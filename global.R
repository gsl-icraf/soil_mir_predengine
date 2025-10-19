# =============================================================================
# Global.R - Shared configurations and functions for Shiny app
# =============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(htmltools)
})

# =============================================================================
# Global Variables
# =============================================================================

# App configuration
APP_VERSION <- "1.0.0"
APP_NAME <- "Spectral Prediction Engine"
APP_DESCRIPTION <- "Soil MIR predictions using ICRAF predictive models"


# Theme configuration
PRIMARY_COLOR <- "#007bff"
SECONDARY_COLOR <- "#6c757d"
SUCCESS_COLOR <- "#28a745"

# =============================================================================
# Utility Functions
# =============================================================================

#' Create a standardized info box
#' @param title Character, the title of the info box
#' @param value Character or numeric, the value to display
#' @param icon Character, the icon class name
#' @param color Character, the color theme
create_info_box <- function(title, value, icon = "info-circle", color = "primary") {
  div(
    class = paste("info-box bg-", color, sep = ""),
    div(
      class = "info-box-icon",
      tags$i(class = paste("fas fa-", icon, sep = ""))
    ),
    div(
      class = "info-box-content",
      div(class = "info-box-title", title),
      div(class = "info-box-value", value)
    )
  )
}

#' Create a loading placeholder
#' @param height Character, height of the placeholder
create_loading_placeholder <- function(height = "200px") {
  div(
    class = "loading-placeholder",
    style = paste("height:", height, "; display: flex; align-items: center; justify-content: center;"),
    div(class = "loading-spinner"),
    span("Loading...", style = "margin-left: 10px;")
  )
}

#' Format numbers with appropriate units
#' @param number Numeric value to format
#' @param digits Number of decimal places
format_number <- function(number, digits = 2) {
  if (is.na(number)) {
    return("N/A")
  }

  if (abs(number) >= 1e9) {
    paste0(round(number / 1e9, digits), "B")
  } else if (abs(number) >= 1e6) {
    paste0(round(number / 1e6, digits), "M")
  } else if (abs(number) >= 1e3) {
    paste0(round(number / 1e3, digits), "K")
  } else {
    as.character(round(number, digits))
  }
}

#' Create a status badge
#' @param status Character, the status text
#' @param type Character, the badge type (success, warning, danger, etc.)
create_status_badge <- function(status, type = "secondary") {
  span(
    class = paste("badge bg-", type, sep = ""),
    status
  )
}

# =============================================================================
# Data Processing Functions
# =============================================================================

#' Safe division function that handles division by zero
#' @param numerator Numeric
#' @param denominator Numeric
safe_divide <- function(numerator, denominator) {
  ifelse(denominator == 0 | is.na(denominator), NA, numerator / denominator)
}

#' Calculate percentage change
#' @param new_value Numeric
#' @param old_value Numeric
percentage_change <- function(new_value, old_value) {
  safe_divide((new_value - old_value), old_value) * 100
}

# =============================================================================
# Validation Functions
# =============================================================================

#' Validate numeric input
#' @param value Input value to validate
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
validate_numeric_input <- function(value, min_val = -Inf, max_val = Inf) {
  if (is.na(value) || !is.numeric(value)) {
    return("Please enter a valid number")
  }
  if (value < min_val) {
    return(paste("Value must be at least", min_val))
  }
  if (value > max_val) {
    return(paste("Value must be at most", max_val))
  }
  return(NULL) # NULL means validation passed
}

# =============================================================================
# App Metadata
# =============================================================================

# Get app information for display
get_app_info <- function() {
  list(
    name = APP_NAME,
    version = APP_VERSION,
    description = APP_DESCRIPTION,
    r_version = R.version.string,
    shiny_version = as.character(packageVersion("shiny")),
    last_updated = Sys.Date()
  )
}

# =============================================================================
# Debug and Logging
# =============================================================================

#' Simple logging function
#' @param message Character, the message to log
#' @param level Character, log level (INFO, WARNING, ERROR)
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] ", level, ": ", message, "\n"))
}

# Enable/disable debug mode based on environment
DEBUG_MODE <- Sys.getenv("SHINY_DEBUG", "FALSE") == "TRUE"

#' Debug print function
#' @param ... Objects to print
debug_print <- function(...) {
  if (DEBUG_MODE) {
    cat("DEBUG:", ...)
    cat("\n")
  }
}

# =============================================================================
# Startup Message
# =============================================================================

cat("=== SpecPred Dashboard ===\n")
cat("Version:", APP_VERSION, "\n")
cat("R Version:", R.version.string, "\n")
cat("Shiny Version:", as.character(packageVersion("shiny")), "\n")
cat("Debug Mode:", DEBUG_MODE, "\n")
cat("Max Upload Size:", round(getOption("shiny.maxRequestSize") / 1024^2, 1), "MB\n")
cat("========================\n\n")
