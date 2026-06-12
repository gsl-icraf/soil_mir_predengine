#!/usr/bin/env Rscript
# ==============================================================================
# generate_r_predictions.R
# Computes R predictions on sample spectra to use as validation reference.
# ==============================================================================

library(data.table)
library(prospectr)
library(torch)

source("src/spectra_process_predict.R")

cat("Loading sample spectra CSV...\n")
df <- fread("data/sample_spectra.csv")

# Clean column names by removing 'w' prefix using setnames
names_clean <- colnames(df)
for (i in 2:length(names_clean)) {
    names_clean[i] <- gsub("^w", "", names_clean[i])
}
setnames(df, names_clean)

# We take up to the first 5 samples
sub_df <- df[1:min(5, nrow(df))]
cat(sprintf("Processing %d samples using R...\n", nrow(sub_df)))

results <- process_spectra_predict(
    spectra_mir = sub_df,
    is_alpha = FALSE
)

predictions <- results$predictions

# Write predictions to output
output_path <- "/Users/tor/Dropbox/Code/webapps/predengine_python/r_predictions.csv"
write.csv(predictions, output_path, row.names = FALSE)
cat(sprintf("R predictions saved to: %s\n", output_path))
