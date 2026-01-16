## Alpha
library(opusreader)

sample_spectra <- list.files(path = "/Users/tor/Dropbox/SRI_LANKA_Spectra/kandy2024", pattern = "*.0", full.names = TRUE)
opus_file <- sample_spectra
s <- opus_read(opus_file)

plot(s$wavenumbers, s$spec, type = "l")

spectra <- read_opus(sample_spectra, parallel = TRUE)

spectra_names <- names(spectra)
wavelengths <- spectra[[spectra_names[1]]]$sc_ref$wavenumbers
absorbance_values <- spectra[[spectra_names[1]]]$sc_ref$data[1:length(wavelengths)]
ssn_absorbance_values <- c(spectra[[spectra_names[1]]]$basic_metadata$opus_sample_name, absorbance_values)
spectral_df <- data.table(t(ssn_absorbance_values))
colnames(spectral_df) <- c("SSN", wavelengths)

plot_data <- melt.data.table(
    spectral_df,
    id.vars = "SSN",
    variable.name = "wavelength",
    value.name = "absorbance_values"
)

## Invenio
sample_spectra <- list.files(path = "/Users/tor/Downloads/invenio", pattern = "*.0", full.names = TRUE)
opus_file <- sample_spectra[1]
s <- opus_read(opus_file)

spectra <- read_opus(sample_spectra, parallel = TRUE)
spectra_names <- names(spectra)
wavelengths <- spectra[[spectra_names[1]]]$ab_no_atm_comp$wavenumbers
absorbance_values <- spectra[[spectra_names[1]]]$ab_no_atm_comp$data[1:length(wavelengths)]
ssn_absorbance_values <- c(spectra[[spectra_names[1]]]$basic_metadata$opus_sample_name, absorbance_values)
spectral_df <- data.table(t(ssn_absorbance_values))
colnames(spectral_df) <- c("SSN", wavelengths)

plot_data <- melt.data.table(
    spectral_df,
    id.vars = "SSN",
    variable.name = "wavelength",
    value.name = "absorbance_values"
)

plot_data <- plot_data[, .(absorbance_values = mean(absorbance_values)), by = .(wavelength, SSN)]
