### Function to process uploaded spectra, resampling these to the
### required wavelength range and resolution for prediction
process_spectra_predict <- function(spectra_mir = spectral_df, target_wavelengths, soilvar = "SOC") {
    ## Average duplicated spectra based on SSN
    spectra_mir <- spectra_mir[, lapply(.SD, mean), by = SSN, .SDcols = 2:ncol(spectra_mir)]
    mir_zscore <- scale(spectra_mir[, 2:ncol(spectra_mir)])
    spectra_mir_sg <- data.table(savitzkyGolay(X = mir_zscore, p = 2, w = 11, m = 1))

    ## Reference bands for resampling
    wavebands_ref <- read.table(file.path("data", "wavebands.txt"), header = FALSE)
    wavebands_ref <- as.numeric(wavebands_ref$V1)

    # Select bands between 607 nm and 4001 cm-1
    headers_spectra <- colnames(spectra_mir)[2:ncol(spectra_mir)]
    headers_spectra <- as.numeric(headers_spectra)
    band_sel <- headers_spectra >= 617 & headers_spectra <= 3991

    spectra_mir_sel <- spectra_mir[, ..band_sel]
    spectra_mir_sel_sg <- spectra_mir_sg[, ..band_sel]

    spectra_mir_sel_resampled <- resample(spectra_mir_sel, wav = colnames(spectra_mir_sel), new.wav = wavebands_ref)
    spectra_mir_sel_resampled_sg <- resample(spectra_mir_sel_sg, wav = colnames(spectra_mir_sel_sg), new.wav = wavebands_ref)

    colnames(spectra_mir_sel_resampled) <- paste0("w", wavebands_ref)
    colnames(spectra_mir_sel_resampled_sg) <- paste0("w", wavebands_ref)

    ## Output table
    results_df <- data.frame(SSN = spectra_mir$SSN)

    ## Run prediction(s)
    list_soilvars <- c("SOC", "TN", "pH", "CEC", "clay", "sand", "ExCa", "ExMg", "ExK")

    for (soilvar in list_soilvars) {
        model_path <- file.path("models", paste0(soilvar, "_model_ranger_rf.qs"))
        if (!file.exists(model_path)) {
            stop(paste("Model file for", soilvar, "not found. Please ensure the model exists in the 'models' directory."))
        }
        rf_mod <- qs::qread(model_path)
        predictions <- round(ranger:::predict.ranger(rf_mod, data = spectra_mir_sel_resampled_sg)$predictions, 2)
        results_df[[soilvar]] <- predictions
    }

    ## Output spectra and SG derivatives
    spectra_mir_sel_resampled <- data.table(SSN = spectra_mir$SSN, spectra_mir_sel_resampled)
    spectra_mir_sel_resampled_sg <- data.table(SSN = spectra_mir$SSN, spectra_mir_sel_resampled_sg)


    return(list(
        predictions = results_df,
        raw_resampled = spectra_mir_sel_resampled,
        deriv_resampled = spectra_mir_sel_resampled_sg
    ))
}
