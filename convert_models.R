#!/usr/bin/env Rscript
# ==============================================================================
# convert_models.R
# R script to extract and dump all models (PCA, DS, DNN) to Python-friendly formats.
# ==============================================================================

suppressPackageStartupMessages({
    library(torch)
    library(jsonlite)
})

src_dir <- "/Users/tor/Dropbox/Code/webapps/soil_mir_predengine"
dest_dir <- "/Users/tor/Dropbox/Code/webapps/predengine_python"

# Create destination directories
dir.create(file.path(dest_dir, "pca_model"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(dest_dir, "models", "spectra_process"), recursive = TRUE, showWarnings = FALSE)

# 1. Convert PCA Model
cat("--- Converting PCA Model ---\n")
pca_path <- file.path(src_dir, "pca_model", "mir_pca_model.rds")
if (file.exists(pca_path)) {
    pca <- readRDS(pca_path)
    
    # Save rotation matrix (1751 x 5)
    write.table(pca$rotation, 
                file = file.path(dest_dir, "pca_model", "mir_pca_model_rotation.csv"), 
                sep = ",", row.names = FALSE, col.names = FALSE)
                
    # Save center vector (1751)
    write.table(pca$center, 
                file = file.path(dest_dir, "pca_model", "mir_pca_model_center.csv"), 
                sep = ",", row.names = FALSE, col.names = FALSE)
                
    cat("PCA model successfully saved to CSV files!\n")
} else {
    cat("WARNING: PCA model file not found at", pca_path, "\n")
}

# 2. Convert Direct Standardization Harmonization Model
cat("\n--- Converting DS Harmonization Model ---\n")
ds_path <- file.path(src_dir, "models", "spectra_process", "harmonization_models.rds")
if (file.exists(ds_path)) {
    ds <- readRDS(ds_path)
    ds_model <- ds$DS$model
    
    # Save F_matrix (1700 x 1700)
    write.table(ds_model$F_matrix, 
                file = file.path(dest_dir, "models", "spectra_process", "ds_f_matrix.csv"), 
                sep = ",", row.names = FALSE, col.names = FALSE)
                
    # Save alpha_mean (1700)
    write.table(ds_model$alpha_mean, 
                file = file.path(dest_dir, "models", "spectra_process", "ds_alpha_mean.csv"), 
                sep = ",", row.names = FALSE, col.names = FALSE)
                
    # Save mir_mean (1700)
    write.table(ds_model$mir_mean, 
                file = file.path(dest_dir, "models", "spectra_process", "ds_mir_mean.csv"), 
                sep = ",", row.names = FALSE, col.names = FALSE)
                
    # Save feature wavenumbers (1700)
    write.table(ds$feature_names, 
                file = file.path(dest_dir, "models", "spectra_process", "ds_feature_names.csv"), 
                sep = ",", row.names = FALSE, col.names = FALSE)
                
    cat("DS harmonization model successfully saved to CSV files!\n")
} else {
    cat("WARNING: DS harmonization model not found at", ds_path, "\n")
}

# 3. Convert PyTorch .pt Deep Learning Models
cat("\n--- Converting DNN PyTorch Models ---\n")
model_files <- list.files(file.path(src_dir, "models"), pattern = "^model_.*\\.pt$", full.names = TRUE)
cat(sprintf("Found %d model files to convert.\n", length(model_files)))

for (model_path in model_files) {
    model_name <- basename(model_path)
    dest_json_name <- gsub("\\.pt$", ".json", model_name)
    dest_json_path <- file.path(dest_dir, "models", dest_json_name)
    
    cat(sprintf("Converting %s ... ", model_name))
    
    tryCatch({
        m <- torch::torch_load(model_path)
        
        # Convert state dict tensors to R arrays
        clean_state <- lapply(m$model_state, as.array)
        
        # Ensure architecture list is a clean list of lists
        arch <- m$architecture
        if (!is.null(arch$hidden_dims)) {
            arch$hidden_dims <- as.list(arch$hidden_dims)
        }
        
        # Build python-friendly structure
        clean_list <- list(
            feature_names = as.list(m$feature_names),
            target_names = as.list(m$target_names),
            architecture = arch,
            normalization = list(
                X_mean = as.list(m$normalization$X_mean),
                X_sd = as.list(m$normalization$X_sd),
                y_mean = m$normalization$y_mean,
                y_sd = m$normalization$y_sd
            ),
            log_transform = m$log_transform,
            sqrt_transform = m$sqrt_transform,
            target_scale = m$target_scale,
            y_range = m$y_range,
            model_state = clean_state
        )
        
        # Write to JSON with high numeric precision (16 digits)
        write_json(clean_list, dest_json_path, auto_unbox = TRUE, matrix = "rowmajor", digits = 16)
        cat("DONE!\n")
    }, error = function(e) {
        cat(sprintf("FAILED: %s\n", conditionMessage(e)))
    })
}

cat("\nModel conversion complete!\n")
