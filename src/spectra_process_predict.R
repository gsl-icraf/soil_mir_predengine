### Function to process uploaded spectra, resampling these to the
### required wavelength range and resolution for prediction using torch DNN models

# ==============================================================================
# DIRECT STANDARDIZATION (DS) for Alpha -> MIR Harmonization
# ==============================================================================

#' Apply Direct Standardization to transform Alpha spectra to MIR
#' @param ds_model Fitted DS model (from fit_ds_model())
#' @param alpha_new New Alpha spectra matrix (already resampled to MIR grid)
#' @return Transformed spectra matching MIR characteristics
apply_ds_model <- function(ds_model, alpha_new) {
    alpha_centered <- sweep(alpha_new, 2, ds_model$alpha_mean, "-")
    mir_pred <- alpha_centered %*% ds_model$F_matrix
    mir_transformed <- sweep(mir_pred, 2, ds_model$mir_mean, "+")
    return(mir_transformed)
}

# ==============================================================================
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

process_spectra_predict <- function(spectra_mir = spectral_df, target_wavelengths, selected_vars = NULL, is_alpha = FALSE) {
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

    ## Apply Alpha -> MIR harmonization using Direct Standardization (DS)
    ## NOTE: DS model was trained on SG derivatives, so apply AFTER derivative step
    if (is_alpha) {
        ds_model_path <- file.path("models", "spectra_process", "harmonization_models.rds")
        if (file.exists(ds_model_path)) {
            harmonization <- readRDS(ds_model_path)
            ds_model <- harmonization$DS$model

            # Get column names that match the DS model features
            ds_feature_names <- harmonization$feature_names
            matched_cols <- intersect(colnames(spectra_mir_sel_resampled_sg), ds_feature_names)

            if (length(matched_cols) == length(ds_feature_names)) {
                # Apply DS harmonization to SG derivatives
                alpha_mat <- spectra_mir_sel_resampled_sg[, ds_feature_names, drop = FALSE]
                spectra_mir_sel_resampled_sg[, ds_feature_names] <- apply_ds_model(ds_model, alpha_mat)
                message("Applied Direct Standardization harmonization for Alpha spectra")
            } else {
                warning(sprintf(
                    "DS model expects %d features but only %d matched. Skipping harmonization.",
                    length(ds_feature_names), length(matched_cols)
                ))
            }
        } else {
            warning("Alpha DS harmonization model not found at: ", ds_model_path,
                    ". Run train_ds_harmonization.R to create it.")
        }
    }

    ## Output table
    results_df <- data.frame(SSN = spectra_mir$SSN)

    ## Map soil variables to their torch model files
    # Silt is now derived, so it's removed from modeled vars
    all_soilvars <- c("SOC", "TN", "pH", "CEC", "clay", "sand", "ExCa", "ExMg", "ExK")

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

    ## Sanity check: TN should be ~1/10 of SOC.
    if ("TN" %in% names(results_df) && "SOC" %in% names(results_df)) {
        ratio <- median(results_df$TN / results_df$SOC, na.rm = TRUE)
        if (!is.na(ratio) && ratio > 0.5) {
            results_df$TN <- round(results_df$TN / 10, 2)
        }
    }

    ## Sanity check: flag negative predictions as -99 (very uncertain)
    pred_cols <- setdiff(names(results_df), "SSN")
    for (col in pred_cols) {
        neg <- !is.na(results_df[[col]]) & results_df[[col]] < 0
        results_df[[col]][neg] <- -99
    }

    ## Texture derivation and closure
    # silt = 100 - (clay + sand)
    if ("clay" %in% names(results_df) && "sand" %in% names(results_df)) {
        # Identify valid rows (not flagged as uncertain)
        valid <- results_df$clay != -99 & results_df$sand != -99

        # 1. Clip valid Clay and Sand to [0, 100]
        results_df$clay[valid] <- pmax(0, pmin(100, results_df$clay[valid]))
        results_df$sand[valid] <- pmax(0, pmin(100, results_df$sand[valid]))

        # 2. Check sum and scale if > 100 (valid rows only)
        sums_cs <- results_df$clay + results_df$sand
        over_100 <- valid & !is.na(sums_cs) & sums_cs > 100

        if (any(over_100)) {
            # Scale proportionally so clay + sand = 100
            results_df$clay[over_100] <- (results_df$clay[over_100] / sums_cs[over_100]) * 100
            results_df$sand[over_100] <- (results_df$sand[over_100] / sums_cs[over_100]) * 100
        }

        # 3. Derive Silt (only for valid rows; flag invalid as -99)
        results_df$silt <- -99
        results_df$silt[valid] <- round(100 - (results_df$clay[valid] + results_df$sand[valid]), 2)
        results_df$silt[valid] <- pmax(0, results_df$silt[valid])

        # 4. Final rounding for valid rows
        results_df$clay[valid] <- round(results_df$clay[valid], 2)
        results_df$sand[valid] <- round(results_df$sand[valid], 2)
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
