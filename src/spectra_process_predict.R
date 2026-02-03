### Function to process uploaded spectra, resampling these to the
### required wavelength range and resolution for prediction using torch DNN models

# DNN model architecture (must match training architecture in mir_torch.R)
dnn_model <- torch::nn_module(
    "DNNModel",
    initialize = function(input_dim,
                          hidden_dims = c(512, 256, 128, 64, 32),
                          dropout = 0.2,
                          n_outputs = 1,
                          activation = torch::nn_relu,
                          use_bn = TRUE,
                          use_dropout = TRUE) {
        layers <- list()
        prev_dim <- input_dim

        for (i in seq_along(hidden_dims)) {
            hdim <- hidden_dims[i]
            layers[[length(layers) + 1]] <- torch::nn_linear(prev_dim, hdim)
            layers[[length(layers) + 1]] <- activation()
            if (use_dropout && dropout > 0) {
                layers[[length(layers) + 1]] <- torch::nn_dropout(p = dropout)
            }
            if (use_bn) {
                layers[[length(layers) + 1]] <- torch::nn_batch_norm1d(hdim)
            }
            prev_dim <- hdim
        }

        layers[[length(layers) + 1]] <- torch::nn_linear(prev_dim, n_outputs)
        self$layers <- torch::nn_sequential(!!!layers)
    },
    forward = function(x) {
        self$layers(x)
    }
)

# Predict a single soil property using a saved torch .pt model
predict_torch <- function(model_path, new_data) {
    saved <- torch::torch_load(model_path)
    arch <- saved$architecture

    model <- dnn_model(
        length(saved$feature_names),
        arch$hidden_dims,
        arch$dropout,
        n_outputs = length(saved$target_names),
        activation = if (arch$activation == "tanh") torch::nn_tanh else torch::nn_relu,
        use_bn = arch$use_bn,
        use_dropout = arch$use_dropout
    )
    model$load_state_dict(saved$model_state)
    model$eval()

    available <- colnames(new_data)
    missing <- setdiff(saved$feature_names, available)
    if (length(missing) > 0) {
        stop(sprintf(
            "Model expects %d features but %d are missing from input (e.g., %s). Available: %d columns.",
            length(saved$feature_names), length(missing),
            paste(head(missing, 5), collapse = ", "), length(available)
        ))
    }
    X_new <- as.matrix(new_data[, saved$feature_names, drop = FALSE])
    X_scaled <- scale(X_new, center = saved$normalization$X_mean, scale = saved$normalization$X_sd)
    X_tensor <- torch::torch_tensor(X_scaled, dtype = torch::torch_float32())

    torch::with_no_grad({
        pred <- model(X_tensor)
        y_mean <- torch::torch_tensor(saved$normalization$y_mean)
        y_sd <- torch::torch_tensor(saved$normalization$y_sd)
        pred_unscaled <- as.numeric((pred * y_sd + y_mean)$cpu())

        # Back-transforms (using flags stored in model)
        if (length(saved$target_names) == 1) {
            if (!is.null(saved$log_transform) && saved$log_transform) {
                pred_unscaled <- expm1(pred_unscaled)
            } else if (!is.null(saved$sqrt_transform) && saved$sqrt_transform) {
                pred_unscaled <- pred_unscaled^2
            }

            if (!is.null(saved$target_scale) && saved$target_scale != 1) {
                pred_unscaled <- pred_unscaled / saved$target_scale
            }

            if (!is.null(saved$y_range)) {
                pred_unscaled <- (pred_unscaled - 0.1) *
                    (saved$y_range$max - saved$y_range$min) / 0.8 +
                    saved$y_range$min
            }
        }
    })

    return(pred_unscaled)
}

process_spectra_predict <- function(spectra_mir = spectral_df, target_wavelengths, selected_vars = NULL) {
    ## Average duplicated spectra based on SSN
    spectra_mir <- spectra_mir[, lapply(.SD, mean), by = SSN, .SDcols = 2:ncol(spectra_mir)]

    ## Reference bands for resampling
    wavebands_ref <- read.table(file.path("data", "wavebands.txt"), header = FALSE)
    wavebands_ref <- as.numeric(wavebands_ref$V1)

    spectra_mat <- as.matrix(spectra_mir[, -1])

    ## Band selection on raw spectra (original column count)
    raw_headers <- as.numeric(colnames(spectra_mat))
    raw_band_sel <- raw_headers >= 601 & raw_headers <= 4001
    spectra_mir_sel <- spectra_mat[, raw_band_sel]

    ## Resample raw to reference wavebands using natural splines (matches reference modeling)
    spectra_mir_sel_resampled <- resample(
        spectra_mir_sel,
        wav = colnames(spectra_mir_sel),
        new.wav = wavebands_ref,
        method = "natural"
    )
    colnames(spectra_mir_sel_resampled) <- paste0("w", wavebands_ref)

    ## SG first derivative on the resampled spectra (matches reference modeling)
    # p=2, w=11, m=1
    spectra_mir_sel_resampled_sg <- savitzkyGolay(
        X = spectra_mir_sel_resampled, p = 2, w = 11, m = 1
    )
    # savitzkyGolay may drop colnames for single-row input
    if (is.null(colnames(spectra_mir_sel_resampled_sg))) {
        # Recover names: SG with w=11 drops 5 cols from each end
        sg_cols <- colnames(spectra_mir_sel_resampled)[6:(ncol(spectra_mir_sel_resampled) - 5)]
        colnames(spectra_mir_sel_resampled_sg) <- sg_cols
    }

    ## Output table
    results_df <- data.frame(SSN = spectra_mir$SSN)

    ## Map soil variables to their torch model files
    all_soilvars <- c("SOC", "TN", "pH", "CEC", "clay", "sand", "silt", "ExCa", "ExMg", "ExK")

    # Use selected variables if provided, otherwise use all
    list_soilvars <- if (is.null(selected_vars)) all_soilvars else selected_vars

    # Find model files (pattern: model_{var}_*.pt)
    model_files <- list.files("models", pattern = "^model_.*\\.pt$", full.names = TRUE)

    for (soilvar in list_soilvars) {
        # Match model file for this variable
        pattern <- paste0("model_", soilvar, "_")
        matched <- model_files[grepl(pattern, model_files)]

        if (length(matched) == 0) {
            warning(paste("Torch model file for", soilvar, "not found in 'models' directory. Skipping."))
            next
        }

        # Use the most recent model if multiple exist
        model_path <- sort(matched, decreasing = TRUE)[1]
        predictions <- round(predict_torch(model_path, spectra_mir_sel_resampled_sg), 2)
        results_df[[soilvar]] <- predictions
    }

    ## Sanity check: TN should be ~1/10 of SOC. If ratio is closer
    ## to 1 than 0.1, the model likely lacks target_scale metadata
    if ("TN" %in% names(results_df) && "SOC" %in% names(results_df)) {
        ratio <- median(results_df$TN / results_df$SOC, na.rm = TRUE)
        if (!is.na(ratio) && ratio > 0.5) {
            results_df$TN <- round(results_df$TN / 10, 2)
        }
    }

    ## Texture closure: Ensure Clay + Sand + Silt = 100%
    texture_vars <- c("clay", "sand", "silt")
    present_texture <- intersect(texture_vars, names(results_df))

    if (length(present_texture) == 3) {
        # Perform closure across all three components
        sums <- rowSums(results_df[, texture_vars, drop = FALSE], na.rm = TRUE)
        # Avoid division by zero
        valid_sums <- !is.na(sums) & sums > 0
        if (any(valid_sums)) {
            for (var in texture_vars) {
                results_df[valid_sums, var] <- round(results_df[valid_sums, var] / sums[valid_sums] * 100, 2)
            }
        }
    }

    ## Output spectra and SG derivatives with SSN
    spectra_mir_sel_resampled <- data.table(SSN = spectra_mir$SSN, spectra_mir_sel_resampled)
    spectra_mir_sel_resampled_sg <- data.table(SSN = spectra_mir$SSN, spectra_mir_sel_resampled_sg)

    return(list(
        predictions = results_df,
        raw_resampled = spectra_mir_sel_resampled,
        deriv_resampled = spectra_mir_sel_resampled_sg
    ))
}
