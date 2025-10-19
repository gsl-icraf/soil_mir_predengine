# CIFOR-ICRAF MIR Prediction Engine

A modern Shiny web application for soil property prediction using Mid-Infrared (MIR) spectroscopy data. Built with advanced machine learning models and a responsive user interface for real-time soil analysis.

## Features

- 🔬 **MIR Spectral Analysis**: Upload and process raw MIR spectral data
- 🤖 **Machine Learning Predictions**: Multiple soil property predictions using Random Forest models
- 📊 **Interactive Visualizations**: Real-time plotting with Plotly integration
- 🎨 **Modern UI**: Bootstrap 5 styling with custom themes via `bslib`
- 📱 **Responsive Design**: Works seamlessly on desktop and mobile devices
- ⚡ **High Performance**: Asynchronous processing with mirai for large datasets
- 🧩 **Modular Architecture**: Clean, maintainable codebase with separate modules

## Soil Properties Predicted

- **Physical Properties**: Sand, Clay content
- **Chemical Properties**: pH, Soil Organic Carbon (SOC), Total Nitrogen (TN), Cation Exchange Capacity (CEC), Exchangeable Calcium (ExCa), Magnesium (ExMg), Potassium (ExK)

## Project Structure

```
soil_mir_predengine/
├── app.R                           # Main Shiny application with routing
├── global.R                        # Global configurations and utilities
├── run_app.R                      # Application runner
├── README.md                      # This file
├── ROUTING.md                     # URL routing documentation
├── modules/                       # Shiny modules
│   ├── home_module.R             # Home dashboard module
│   └── prediction_module.R       # MIR prediction functionality
├── src/                          # Source code
│   └── spectra_process_predict.R # Core spectral processing functions
├── models/                       # Pre-trained ML models
│   ├── CEC_model_ranger_rf.rds   # Cation Exchange Capacity
│   ├── ExCa_model_ranger_rf.rds  # Exchangeable Calcium
│   ├── ExK_model_ranger_rf.rds   # Exchangeable Potassium
│   ├── ExMg_model_ranger_rf.rds  # Exchangeable Magnesium
│   ├── SOC_model_ranger_rf.rds   # Soil Organic Carbon
│   ├── TN_model_ranger_rf.rds    # Total Nitrogen
│   ├── clay_model_ranger_rf.rds  # Clay content
│   ├── pH_model_ranger_rf.rds    # pH
│   └── sand_model_ranger_rf.rds  # Sand content
├── data/                         # Data files and samples
│   ├── sample_spectra.csv        # Example spectral data
│   ├── sample_spectra/           # Individual sample files
│   ├── mir_pca_*_scores.csv     # PCA analysis results
│   ├── wavebands.txt            # MIR wavelength information
│   └── wavebands_ref.csv        # Reference wavelengths
└── www/                         # Static web assets
    ├── css/
    │   └── custom.css           # Custom styling
    ├── js/
    │   └── custom.js            # Custom JavaScript
    ├── images/                  # Application images and logos
    │   ├── icraf_logo.png       # ICRAF institutional logo
    │   ├── spacial_logo.png     # SPACIAL project logo
    │   └── *.jpg                # Soil and field images
    └── favicon.ico              # Site icon
```

### To run the App

```r
source("run_app.R")
```