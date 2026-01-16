# Script to install required packages and run the Shiny app
# Run this file to set up and launch the modular Shiny app

# Check and install required packages
# Core packages needed for the modular SpecPred dashboard
required_packages <- c(
  # "shinydashboard", # Alternative dashboard components
  # "shinyWidgets", # Additional input widgets
  "RColorBrewer",
  "shiny", # Core Shiny framework
  "bslib", # Bootstrap themes and components
  "shinyjs", # JavaScript integration
  "htmltools", # HTML generation utilitie
  "mirai",
  "prospectr", # For spectral data processing
  "ranger",
  "DT", # Interactive data tables
  "plotly", # Interactive plots
  "data.table", # Fast data processing
  "stringr",
  "qs", # Fast serialization
  "remotes",
  "opusreader",
  "arrow"
)

packages_from_github <- c(
  "opusreader" # For reading Opus binary spectral data
)

# Function to check and install packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
    } else {
      cat("Package", pkg, "is already installed\n")
    }
  }
}

# install_r_universe_packages <- function(packages) {
#   for (pkg in packages) {
#     if (!requireNamespace(pkg, quietly = TRUE)) {
#       cat("Installing package from R-universe:", pkg, "\n")
#       install.packages(packages_from_r_universe, repos = c(
#         spectralcockpit = "https://spectral-cockpit.r-universe.dev",
#         CRAN = "https://cloud.r-project.org"
#       ))
#     } else {
#       cat("Package", pkg, "is already installed\n")
#     }
#   }
# }

install_github_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing package from GitHub:", pkg, "\n")
      remotes::install_github("pierreroudier/opusreader")
    } else {
      cat("Package", pkg, "is already installed\n")
    }
  }
}

# Install missing packages
cat("Checking and installing required packages...\n")
install_if_missing(required_packages)
install_github_packages(packages_from_github)
# install_r_universe_packages(packages_from_r_universe)

# Load the main app
cat("\nAll packages installed successfully!\n")
cat("Starting the Shiny app...\n")

# Run the app
options(shiny.autoreload = TRUE)
shiny::runApp(port = 3839)
