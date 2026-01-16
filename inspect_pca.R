mod <- readRDS("models/mir_pca_model.rds")
cat("Class:", class(mod), "\n")
if (inherits(mod, "prcomp")) {
    cat("Rotation dimensions:", dim(mod$rotation), "\n")
    cat("First 5 feature names:\n")
    print(head(rownames(mod$rotation)))
}
