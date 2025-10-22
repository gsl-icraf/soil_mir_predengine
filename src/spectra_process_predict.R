### Function to process uploaded spectra, resampling these to the
### required wavelength range and resolution for prediction
process_spectra_predict <- function(spectra_mir = spectral_df, target_wavelengths, soilvar = "SOC") {
    ## Average duplicated spectra based on SSN
    spectra_mir <- spectra_mir[, lapply(.SD, mean), by = SSN, .SDcols = 2:ncol(spectra_mir)]

    ## Output table
    results_df <- data.frame(SSN = spectra_mir$SSN)

    ## Apply Savitzky-Golay smoothing
    mir_zscore <- scale(spectra_mir[, c("SSN") := NULL])

    ##
    spectra_mir_sg <- data.table(savitzkyGolay(X = mir_zscore, p = 2, w = 11, m = 1))

    ## Reference bands for resampling
    wavebands_ref <- read.table("data/wavebands.txt", header = FALSE)
    wavebands_ref <- as.numeric(wavebands_ref$V1)

    # Select bands between 607 nm and 4001 cm-1
    headers_spectra <- colnames(spectra_mir)[2:ncol(spectra_mir)]
    headers_spectra <- as.numeric(headers_spectra)
    band_sel <- headers_spectra >= 617 & headers_spectra <= 3991

    spectra_mir_sel <- spectra_mir_sg[, ..band_sel]
    resampled_spectra <- resample(spectra_mir_sel, wav = colnames(spectra_mir_sel), new.wav = wavebands_ref)
    colnames(resampled_spectra) <- paste0("w", wavebands_ref)

    list_soilvars <- c("SOC", "TN", "pH", "CEC", "clay", "sand", "ExCa", "ExMg", "ExK")

    for (soilvar in list_soilvars) {
        if (!file.exists(paste0("models/", soilvar, "_model_ranger_rf.qs"))) {
            stop(paste("Model file for", soilvar, "not found. Please ensure the model exists in the 'models' directory."))
        }
        # rf_mod <- readRDS(paste0("models/", soilvar, "_model_ranger_rf.rds"))
        rf_mod <- qs::qread(paste0("models/", soilvar, "_model_ranger_rf.qs"))
        predictions <- round(ranger:::predict.ranger(rf_mod, data = resampled_spectra)$predictions, 2)
        results_df[[soilvar]] <- predictions
    }
    return(results_df)
}
