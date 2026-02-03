#' Harmonize Spectra for BRNN Model Prediction
#'
#' Preprocesses raw MIR spectra to match the BRNN model's expected input format.
#' Applies 2nd derivative transformation, resampling, and model-specific normalization.
#'
#' @param spectra_matrix Matrix of raw spectra (rows = samples, cols = wavelengths)
#' @param wavelengths Numeric vector of wavelengths corresponding to columns
#' @param brnn_model BRNN model object (must contain x_base, x_spread, and wavelength info)
#' @param method Preprocessing method: "gap" (default) or "sg" for Savitzky-Golay
#' @param deriv_params List of derivative parameters. Defaults:
#'   For gap: list(m = 2, w = 11, s = 1)
#'   For sg: list(m = 2, p = 2, w = 21)
#'
#' @return Matrix of preprocessed spectra ready for BRNN prediction
#'
#' @examples
#' model <- readRDS("models/SOC_model_brnn.rds")
#' spectra_harmonized <- harmonize_spectra_brnn(
#'   spectra_matrix = my_spectra,
#'   wavelengths = my_wavelengths,
#'   brnn_model = model
#' )
#' predictions <- predict(model, spectra_harmonized)
#'
harmonize_spectra_brnn <- function(
    spectra_matrix,
    wavelengths,
    brnn_model,
    method = c("gap", "sg"),
    deriv_params = NULL) {

    require(prospectr)

    method <- match.arg(method)

    ## Set default parameters based on method
    if (is.null(deriv_params)) {
        deriv_params <- if (method == "gap") {
            list(m = 2, w = 11, s = 1)
        } else {
            list(m = 2, p = 2, w = 21)
        }
    }

    ## Extract target wavelengths from model
    target_wavelengths <- as.numeric(names(brnn_model$x_base))

    ## Ensure spectra_matrix is a matrix
    if (is.vector(spectra_matrix)) {
        spectra_matrix <- matrix(spectra_matrix, nrow = 1)
    }

    ## Filter to relevant wavelength range (with buffer)
    wave_range <- range(target_wavelengths)
    band_sel <- wavelengths >= (wave_range[1] - 20) &
                wavelengths <= (wave_range[2] + 20)

    spectra_sel <- spectra_matrix[, band_sel, drop = FALSE]
    wavelengths_sel <- wavelengths[band_sel]

    ## Apply derivative transformation
    if (method == "gap") {
        spectra_deriv <- gapDer(
            X = spectra_sel,
            m = deriv_params$m,
            w = deriv_params$w,
            s = deriv_params$s
        )
    } else if (method == "sg") {
        spectra_deriv <- savitzkyGolay(
            X = spectra_sel,
            m = deriv_params$m,
            p = deriv_params$p,
            w = deriv_params$w
        )
    }

    ## Adjust wavelengths to match derivative output length
    len_diff <- ncol(spectra_sel) - ncol(spectra_deriv)
    if (len_diff > 0) {
        trim_each_side <- floor(len_diff / 2)
        trim_start <- trim_each_side + 1
        trim_end <- length(wavelengths_sel) - (len_diff - trim_each_side)
        wavelengths_deriv <- wavelengths_sel[trim_start:trim_end]
    } else {
        wavelengths_deriv <- wavelengths_sel
    }

    ## Resample to target wavelengths
    spectra_resampled <- resample(
        X = spectra_deriv,
        wav = wavelengths_deriv,
        new.wav = target_wavelengths
    )

    ## Apply BRNN normalization
    if (!is.null(brnn_model$x_base) && !is.null(brnn_model$x_spread)) {
        spectra_normalized <- sweep(spectra_resampled, 2, brnn_model$x_base, "-")
        spectra_normalized <- sweep(spectra_normalized, 2, brnn_model$x_spread, "/")
    } else {
        stop("BRNN model must contain x_base and x_spread normalization parameters")
    }

    ## Set column names to match model wavelengths
    colnames(spectra_normalized) <- names(brnn_model$x_base)

    return(spectra_normalized)
}


#' Batch Harmonize Spectra from Parquet
#'
#' Load and harmonize spectra from a parquet file for BRNN prediction.
#' Processes spectra in batches to manage memory usage.
#'
#' @param parquet_path Path to parquet file containing spectra
#' @param brnn_model BRNN model object
#' @param batch_size Number of spectra to process at once (default: 1000)
#' @param method Preprocessing method: "gap" or "sg"
#'
#' @return Matrix of harmonized spectra
#'
harmonize_spectra_from_parquet <- function(
    parquet_path,
    brnn_model,
    batch_size = 1000,
    method = "gap") {

    require(arrow)
    require(data.table)

    ## Read parquet
    spectra_raw <- read_parquet(parquet_path)
    spectra_dt <- as.data.table(spectra_raw)

    ## Extract wavelength columns
    wave_cols <- grep("^w[0-9]+", colnames(spectra_dt), value = TRUE)
    wavelengths <- as.numeric(gsub("^w", "", wave_cols))

    ## Extract spectra matrix
    spectra_matrix <- as.matrix(spectra_dt[, ..wave_cols])

    ## Get sample IDs if available
    id_cols <- setdiff(colnames(spectra_dt), wave_cols)
    sample_ids <- if (length(id_cols) > 0) {
        spectra_dt[[id_cols[1]]]
    } else {
        NULL
    }

    cat(sprintf("Processing %d spectra in batches of %d...\n",
                nrow(spectra_matrix), batch_size))

    ## Process in batches
    n_samples <- nrow(spectra_matrix)
    n_batches <- ceiling(n_samples / batch_size)

    results <- vector("list", n_batches)

    for (i in seq_len(n_batches)) {
        start_idx <- (i - 1) * batch_size + 1
        end_idx <- min(i * batch_size, n_samples)

        batch_spectra <- spectra_matrix[start_idx:end_idx, , drop = FALSE]

        results[[i]] <- harmonize_spectra_brnn(
            spectra_matrix = batch_spectra,
            wavelengths = wavelengths,
            brnn_model = brnn_model,
            method = method
        )

        if (i %% 10 == 0 || i == n_batches) {
            cat(sprintf("  Processed batch %d/%d (%d spectra)\n",
                       i, n_batches, end_idx))
        }
    }

    ## Combine results
    harmonized_spectra <- do.call(rbind, results)

    ## Add sample IDs as rownames if available
    if (!is.null(sample_ids)) {
        rownames(harmonized_spectra) <- sample_ids
    }

    cat("✓ Harmonization complete!\n")

    return(harmonized_spectra)
}


#' Quick Test: Verify Harmonization Quality
#'
#' Tests harmonization by comparing a processed spectrum with the model's
#' training data to ensure high correlation.
#'
#' @param spectra_matrix Raw spectra matrix (1 or more samples)
#' @param wavelengths Wavelength vector
#' @param brnn_model BRNN model object
#' @param method Preprocessing method to test
#' @param n_samples Number of samples to test (default: 5)
#'
#' @return List with correlation statistics and plots
#'
test_harmonization <- function(
    spectra_matrix,
    wavelengths,
    brnn_model,
    method = "gap",
    n_samples = 5) {

    require(ggplot2)

    ## Process spectra
    harmonized <- harmonize_spectra_brnn(
        spectra_matrix = spectra_matrix,
        wavelengths = wavelengths,
        brnn_model = brnn_model,
        method = method
    )

    ## Get model training samples for comparison
    model_samples <- brnn_model$x_normalized

    ## Calculate correlations with model samples
    n_test <- min(n_samples, nrow(harmonized))
    n_model <- min(100, nrow(model_samples))

    correlations <- numeric(n_test)

    for (i in seq_len(n_test)) {
        ## Find best matching model sample
        cors <- apply(model_samples[1:n_model, ], 1, function(x) {
            cor(harmonized[i, ], x)
        })
        correlations[i] <- max(cors)
    }

    cat("Harmonization Quality Test:\n")
    cat(sprintf("  Mean correlation: %.4f\n", mean(correlations)))
    cat(sprintf("  Min correlation:  %.4f\n", min(correlations)))
    cat(sprintf("  Max correlation:  %.4f\n", max(correlations)))

    if (mean(correlations) > 0.85) {
        cat("  ✓ EXCELLENT - Harmonization is working well!\n")
    } else if (mean(correlations) > 0.70) {
        cat("  ✓ GOOD - Harmonization looks reasonable\n")
    } else {
        cat("  ✗ WARNING - Low correlation, check preprocessing\n")
    }

    return(list(
        correlations = correlations,
        mean_cor = mean(correlations),
        harmonized_spectra = harmonized
    ))
}
