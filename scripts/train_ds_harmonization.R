#!/usr/bin/env Rscript
#' Train Direct Standardization (DS) Model for Alpha -> MIR Harmonization
#'
#' This script fits a DS model using paired spectra measured on both
#' Alpha and MIR instruments, then saves the model for use in predictions.
#'
#' Usage: Rscript scripts/train_ds_harmonization.R

suppressPackageStartupMessages({
    library(data.table)
    library(prospectr)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Paths to paired spectral data (Alpha and MIR measurements of same samples)
# Data is in the modeling project
MODELING_DATA_DIR <- "/Users/tor/Dropbox/Code/modeling/mir_modeling/data/torch_data"
ALPHA_DATA_PATH <- file.path(MODELING_DATA_DIR, "alpha_data_SOC.csv")
MIR_DATA_PATH <- file.path(MODELING_DATA_DIR, "mir_data_SOC.csv")

# Output path for the DS model
OUTPUT_MODEL_PATH <- "models/spectra_process/alpha_ds_harmonization.rds"

# Ridge regularization parameter (higher = more regularization)
LAMBDA <- 0.01

# ==============================================================================
# FUNCTIONS
# ==============================================================================

#' Fit Direct Standardization transfer model
#' Uses regularized least squares: MIR = Alpha * F + E
#' @param mir_spec MIR spectra matrix (n_samples x n_bands)
#' @param alpha_spec Alpha spectra matrix (same dimensions, resampled to MIR)
#' @param lambda Ridge regularization parameter
#' @return DS model object
fit_ds_model <- function(mir_spec, alpha_spec, lambda = 0.01) {
    cat("\n--- Fitting Direct Standardization Model ---\n")

    n <- nrow(mir_spec)
    p <- ncol(mir_spec)
    cat(sprintf("  Samples: %d, Bands: %d\n", n, p))

    # Centering
    alpha_mean <- colMeans(alpha_spec)
    mir_mean <- colMeans(mir_spec)

    alpha_centered <- scale(alpha_spec, center = TRUE, scale = FALSE)
    mir_centered <- scale(mir_spec, center = TRUE, scale = FALSE)

    # Ridge regression: F = (X'X + lambda*I)^-1 * X'Y
    cat("  Computing transfer matrix (ridge regression)...\n")

    XtX <- crossprod(alpha_centered)
    XtY <- crossprod(alpha_centered, mir_centered)

    # Regularization
    ridge <- diag(lambda * max(diag(XtX)), p)
    F_matrix <- solve(XtX + ridge, XtY)

    # Compute residuals for quality check
    mir_pred <- alpha_centered %*% F_matrix
    residuals <- mir_centered - mir_pred
    rmse <- sqrt(mean(residuals^2))
    cat(sprintf("  Transfer RMSE: %.6f\n", rmse))

    # Per-band R2
    ss_tot <- colSums(mir_centered^2)
    ss_res <- colSums(residuals^2)
    r2_bands <- 1 - ss_res / ss_tot
    cat(sprintf("  Mean band R2: %.3f (range: %.3f - %.3f)\n",
                mean(r2_bands), min(r2_bands), max(r2_bands)))

    list(
        method = "DS",
        F_matrix = F_matrix,
        alpha_mean = alpha_mean,
        mir_mean = mir_mean,
        lambda = lambda,
        rmse = rmse,
        r2_bands = r2_bands,
        n_samples = n,
        n_bands = p,
        created = Sys.time()
    )
}

# ==============================================================================
# MAIN
# ==============================================================================

cat("\n========================================\n")
cat("TRAINING DS HARMONIZATION MODEL\n")
cat("========================================\n")

# Load paired data
cat("\nLoading paired spectra...\n")

if (!file.exists(ALPHA_DATA_PATH)) {
    stop("Alpha data not found: ", ALPHA_DATA_PATH)
}
if (!file.exists(MIR_DATA_PATH)) {
    stop("MIR data not found: ", MIR_DATA_PATH)
}

alpha_data <- fread(ALPHA_DATA_PATH)
mir_data <- fread(MIR_DATA_PATH)

# Normalize SSN for matching
alpha_data[, SSN := tolower(SSN)]
mir_data[, SSN := tolower(SSN)]

# Find paired samples
paired_ssns <- intersect(alpha_data$SSN, mir_data$SSN)
cat(sprintf("  Found %d paired samples\n", length(paired_ssns)))

if (length(paired_ssns) < 50) {
    warning("Few paired samples found. DS model may be unreliable.")
}

# Extract spectral columns
alpha_cols <- names(alpha_data)[grepl("^w", names(alpha_data))]
mir_cols <- names(mir_data)[grepl("^w", names(mir_data))]

alpha_wn <- as.numeric(gsub("^w", "", alpha_cols))
mir_wn <- as.numeric(gsub("^w", "", mir_cols))

cat(sprintf("  Alpha: %d bands (%.0f - %.0f cm-1)\n",
            length(alpha_wn), min(alpha_wn), max(alpha_wn)))
cat(sprintf("  MIR: %d bands (%.0f - %.0f cm-1)\n",
            length(mir_wn), min(mir_wn), max(mir_wn)))

# Subset to paired samples
alpha_paired <- alpha_data[SSN %in% paired_ssns]
mir_paired <- mir_data[SSN %in% paired_ssns]

# Ensure same order
setkey(alpha_paired, SSN)
setkey(mir_paired, SSN)

# Resample Alpha to MIR wavenumber grid
alpha_mat <- as.matrix(alpha_paired[, ..alpha_cols])
ord_alpha <- order(alpha_wn)
ord_mir <- order(mir_wn)

cat("  Resampling Alpha to MIR wavenumber grid...\n")
alpha_resampled <- resample(
    X = alpha_mat[, ord_alpha],
    wav = alpha_wn[ord_alpha],
    new.wav = mir_wn[ord_mir],
    method = "natural"
)
colnames(alpha_resampled) <- mir_cols[ord_mir]
alpha_resampled <- alpha_resampled[, mir_cols]

mir_mat <- as.matrix(mir_paired[, ..mir_cols])

# Fit DS model
ds_model <- fit_ds_model(mir_mat, alpha_resampled, lambda = LAMBDA)

# Save model
dir.create(dirname(OUTPUT_MODEL_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ds_model, OUTPUT_MODEL_PATH)

cat("\n========================================\n")
cat(sprintf("DS model saved to: %s\n", OUTPUT_MODEL_PATH))
cat(sprintf("  Transfer RMSE: %.6f\n", ds_model$rmse))
cat(sprintf("  Mean band R2: %.3f\n", mean(ds_model$r2_bands)))
cat("========================================\n")
