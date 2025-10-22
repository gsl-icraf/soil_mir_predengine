# Prediction Module UI
prediction_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Prediction Engine", class = "mb-4"),

    # Input controls card with spectral styling
    layout_columns(
      col_widths = c(12),

      # Controls section
      card(
        class = "spectral-card",
        card_header(
          icon("upload"), " Upload soil spectra data!",
          class = "bg-gradient text-white"
        ),
        card_body(
          layout_columns(
            col_widths = c(4, 8),

            # Left side - file upload and controls
            div(
              fileInput(
                ns("spectra_file"),
                "Choose ZIP File with Spectral Data",
                accept = c(".zip")
              ),
              div(
                class = "d-flex align-items-center",
                icon("info-circle", class = "me-2", style = "color: #ffc107;"),
                span("Note that the uploaded ZIP file should contain soil MIR spectral data in Opus binary format (*.0)",
                  style = "color: #ffc107; font-size: 0.9em;"
                )
              ),
              helpText("Upload a ZIP file containing soil spectral data for prediction."),
              # Show unzip button and spectra count after file upload
              conditionalPanel(
                condition = "output.file_uploaded == true",
                ns = ns,
                div(
                  class = "d-flex align-items-center gap-3",
                  actionButton(ns("unzip_btn"), "Unzip Spectra", class = "btn-xs btn-outline-light"),
                  div(
                    class = "p-2 rounded flex-grow-1",
                    style = "background-color: rgba(255,255,255,0.1); border: 1px solid rgba(255,255,255,0.2);",
                    div(
                      class = "d-flex justify-content-between align-items-center",
                      span("Spectra Processed:", style = "color: white; font-weight: 500;"),
                      uiOutput(ns("spectra_count_display"), style = "color: #00ff88; font-weight: bold; font-size: 1.1em;")
                    )
                  )
                )
              ),

              # Helpful message before unzipping
              conditionalPanel(
                condition = "output.spectra_ready == false",
                ns = ns,
                div(
                  class = "mt-3 p-2 rounded",
                  style = "background-color: rgba(255,255,255,0.1); border: 1px solid rgba(255,193,7,0.3);",
                  div(
                    class = "d-flex align-items-center",
                    icon("info-circle", class = "me-2", style = "color: #ffc107;"),
                    span("Upload spectral data and click 'Unzip Spectra' to enable predictions",
                      style = "color: #ffc107; font-size: 0.9em;"
                    )
                  )
                )
              ),

              # Predict button with progress indicators (only show after unzipping)
              conditionalPanel(
                condition = "output.spectra_ready == true",
                ns = ns,
                div(
                  class = "mt-3",
                  div(
                    class = "d-flex align-items-center mb-2",
                    actionButton(
                      ns("predict_btn"),
                      list(icon("flask"), " Predict Soil Properties"),
                      class = "btn-xs btn-outline-light", style = "color: #e67507; width: 100%;"
                    ),
                    uiOutput(ns("prediction_spinner"))
                  ),
                  # Progress bar container
                  uiOutput(ns("prediction_progress_bar"))
                ),

                # Download button (only show after successful predictions)
                conditionalPanel(
                  condition = "output.predictions_ready == true",
                  ns = ns,
                  div(
                    class = "mt-2",
                    downloadButton(
                      ns("download_predictions"),
                      label = list(icon("download"), " Download Results (CSV)"),
                      class = "btn-outline-success btn-sm",
                      style = "width: 100%;"
                    ),
                    p("Downloading the results will reset the app for new predictions.",
                      style = "font-size: 0.8em; color: #6c757d; margin-top: 4px;"
                    )
                  )
                )
              )
            ),

            # Right side - spectral plot
            div(
              div(
                class = "d-flex justify-content-between align-items-center mb-2",
                span(list(icon("wave-square"), " Spectral Data Visualization"), style = "color: white; font-weight: bold;"),
                div(
                  class = "d-flex align-items-center",
                  actionButton(ns("toggle_derivative"), "Show First Derivatives", class = "btn-sm btn-outline-warning me-2"),
                  actionButton(ns("reset_plots"), "Reset View", class = "btn-xs btn-outline-light", style = "font-size: 0.75rem; padding: 0.2rem 0.5rem;")
                )
              ),
              plotlyOutput(ns("spectral_plot_combined"), height = "300px")
            )
          )
        )
      )
    ),

    # Prediction results table (only show when predictions are complete)
    conditionalPanel(
      condition = "output.predictions_ready == true",
      ns = ns,
      div(
        class = "mt-4",
        card(
          class = "spectral-card",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              span(list(icon("table"), " Prediction Results")),
              uiOutput(ns("prediction_count_display"))
            ),
            class = "bg-gradient text-white"
          ),
          card_body(
            DT::dataTableOutput(ns("prediction_table"))
          )
        )
      )
    ),

    # Prediction visualization card with density plots (only show when predictions are complete)
    conditionalPanel(
      condition = "output.predictions_ready == true",
      ns = ns,
      div(
        class = "mt-4",
        card(
          class = "spectral-card",
          card_header(
            icon("chart-area"), " Soil Property Distributions",
            class = "bg-gradient text-white"
          ),
          card_body(
            plotOutput(ns("prediction_plot"), height = "600px")
          )
        )
      )
    ),

    # Additional info section matching home page style
    div(
      class = "mt-4",
      card(
        class = "spectral-card",
        card_header(
          icon("info-circle"), " Analysis Information",
          class = "bg-gradient text-white"
        ),
        card_body(
          div(
            class = "row mt-3",
            div(
              class = "col-md-6",
              div(
                class = "d-flex align-items-start mb-3",
                div(
                  class = "me-3",
                  span(
                    class = "badge bg-primary rounded-pill p-2",
                    icon("microscope")
                  )
                ),
                div(
                  h6("Spectral Analysis", class = "mb-1"),
                  p("This module processes mid-infrared (MIR) spectral data to predict soil properties. Upload your spectral data in ZIP format containing Opus binary files (*.0) for rapid analysis.")
                )
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "d-flex align-items-start mb-3",
                div(
                  class = "me-3",
                  span(
                    class = "badge bg-warning rounded-pill p-2",
                    icon("cogs")
                  )
                ),
                div(
                  h6("Data Processing", class = "mb-1"),
                  p("The system automatically processes your spectral data using Savitzky-Golay filters and derivative calculations to enhance spectral features for accurate predictions.")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Prediction Module Server
prediction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## On load...
    ### Sample spectral data

    sample_spectra <- list.files(path = "data/sample_spectra", pattern = "*.0", full.names = TRUE)
    spectra <- read_opus(sample_spectra, parallel = TRUE)

    spectra_names <- names(spectra)
    wavelengths <- spectra[[spectra_names[1]]]$ab_no_atm_comp$wavenumbers
    absorbance_values <- spectra[[spectra_names[1]]]$ab_no_atm_comp$data[1:length(wavelengths)]
    ssn_absorbance_values <- c(spectra[[spectra_names[1]]]$basic_metadata$opus_sample_name, absorbance_values)
    spectral_df <- data.table(t(ssn_absorbance_values))

    ### Get absorbance values and combine these into one matrix
    for (i in 2:length(spectra)) {
      absorbance_values_add <- spectra[[spectra_names[i]]]$ab_no_atm_comp$data[1:length(wavelengths)]
      ssn_absorbance_values_add <- c(spectra[[spectra_names[i]]]$basic_metadata$opus_sample_name, absorbance_values_add)
      ssn_absorbance_values <- rbind(ssn_absorbance_values, ssn_absorbance_values_add)
    }

    spectral_df <- data.table(ssn_absorbance_values)
    colnames(spectral_df) <- c("SSN", as.character(wavelengths))
    spectral_df[, 2:4840] <- lapply(spectral_df[, 2:4840], as.numeric)

    ## 1st derivatives
    spectral_df_sg <- savitzkyGolay(X = spectral_df[, 2:4840], p = 3, w = 11, m = 1)


    ## Reshape data for plotting
    spectral_df_long <- melt.data.table(
      spectral_df,
      id.vars = "SSN",
      variable.name = "wavelength",
      value.name = "absorbance_values"
    )

    spectral_df_long <- spectral_df_long[, .(absorbance_values = mean(absorbance_values)), by = .(wavelength, SSN)]
    spectral_df_long[, wavelength := as.numeric(as.character(wavelength))]
    spectral_df_long <- spectral_df_long[wavelength >= 617 & wavelength <= 3991, ]
    spectral_df_long <- spectral_df_long[order(SSN, wavelength)]

    spectral_df_sg_long <- melt.data.table(
      data.table(SSN = spectral_df$SSN, spectral_df_sg),
      id.vars = "SSN",
      variable.name = "wavelength",
      value.name = "absorbance_values"
    )

    spectral_df_sg_long <- spectral_df_sg_long[, .(absorbance_values = mean(absorbance_values)), by = .(wavelength, SSN)]
    spectral_df_sg_long[, wavelength := as.numeric(as.character(wavelength))]
    spectral_df_sg_long <- spectral_df_sg_long[wavelength >= 617 & wavelength <= 3991, ]
    spectral_df_sg_long <- spectral_df_sg_long[order(SSN, wavelength)]


    # Reactive value for derivative toggle
    show_derivative <- reactiveVal(FALSE)

    # Toggle derivative button handler
    observeEvent(input$toggle_derivative, {
      current_state <- show_derivative()
      show_derivative(!current_state)

      # Update button text
      if (!current_state) {
        updateActionButton(session, "toggle_derivative",
          label = "Show Original Spectra",
          icon = icon("arrow-left")
        )
      } else {
        updateActionButton(session, "toggle_derivative",
          label = "Show First Derivatives",
          icon = icon("arrow-right")
        )
      }
    })

    ### Render combined plot
    output$spectral_plot_combined <- renderPlotly({
      # Determine which data to use based on toggle state
      use_derivative <- show_derivative()

      if (use_derivative) {
        # Use derivative data
        if (!is.null(uploaded_spectra_df_sg())) {
          # Process uploaded SG data for plotting
          plot_data <- melt.data.table(
            uploaded_spectra_df_sg(),
            id.vars = "SSN",
            variable.name = "wavelength",
            value.name = "absorbance_values"
          )
          plot_data <- plot_data[, .(absorbance_values = mean(absorbance_values)), by = .(wavelength, SSN)]
          plot_data[, wavelength := as.numeric(as.character(wavelength))]
          plot_data <- plot_data[wavelength >= 617 & wavelength <= 3991, ]
          plot_data <- plot_data[order(SSN, wavelength)]
        } else {
          plot_data <- spectral_df_sg_long
        }
        y_title <- "First Derivative"
        plot_title <- "First Derivative Spectra"
      } else {
        # Use original data
        if (!is.null(uploaded_spectra_df())) {
          # Process uploaded data for plotting
          plot_data <- melt.data.table(
            uploaded_spectra_df(),
            id.vars = "SSN",
            variable.name = "wavelength",
            value.name = "absorbance_values"
          )
          plot_data <- plot_data[, .(absorbance_values = mean(absorbance_values)), by = .(wavelength, SSN)]
          plot_data[, wavelength := as.numeric(as.character(wavelength))]
          plot_data <- plot_data[wavelength >= 617 & wavelength <= 3991, ]
          plot_data <- plot_data[order(SSN, wavelength)]
        } else {
          plot_data <- spectral_df_long
        }
        y_title <- "Absorbance"
        plot_title <- "Original Spectra"
      }

      # Create plasma-like color palette
      n_samples <- length(unique(plot_data$SSN))
      plasma_colors <- viridis::plasma(n_samples)

      plot_data |>
        plot_ly(
          x = ~wavelength,
          y = ~absorbance_values,
          color = ~SSN,
          colors = plasma_colors,
          type = "scatter",
          mode = "lines",
          source = "spectral_plot_combined"
        ) |>
        layout(
          title = list(
            text = plot_title,
            font = list(color = "white", size = 16)
          ),
          xaxis = list(
            title = "Wavelength (cm<sup>-1</sup>)",
            autorange = "reversed",
            range = c(3991, 617),
            gridcolor = "#7f8c8d",
            color = "white"
          ),
          yaxis = list(
            title = y_title,
            gridcolor = "#7f8c8d",
            color = "white"
          ),
          dragmode = "zoom",
          plot_bgcolor = "#2c3e50",
          paper_bgcolor = "#34495e",
          font = list(color = "white")
        ) |>
        event_register("plotly_relayout") |>
        event_register("plotly_doubleclick")
    })

    # Reset plot when double-clicking
    observeEvent(event_data("plotly_doubleclick", source = "spectral_plot_combined"), {
      plotlyProxy("spectral_plot_combined", session) %>%
        plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
    })

    # Reset plot when reset button is clicked
    observeEvent(input$reset_plots, {
      plotlyProxy("spectral_plot_combined", session) %>%
        plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
    })

    ### File upload and processing
    # Reactive values to store uploaded spectral data
    uploaded_spectra_df <- reactiveVal(NULL)
    uploaded_spectra_df_sg <- reactiveVal(NULL)
    spectra_count <- reactiveVal(0)

    # Reactive values for prediction
    prediction_loading <- reactiveVal(FALSE)
    prediction_results <- reactiveVal(NULL)
    spectra_processed <- reactiveVal(FALSE) # Track if spectral data has been unzipped

    observeEvent(input$unzip_btn, {
      req(input$spectra_file)

      tryCatch(
        {
          zip_path <- input$spectra_file$datapath

          # Create a temp directory to extract files
          extract_dir <- tempfile()
          dir.create(extract_dir)

          # Unzip contents
          unzip(zip_path, exdir = extract_dir)

          # Find .0 files (Opus binary format)
          opus_files <- list.files(extract_dir, pattern = "*.0", full.names = TRUE, recursive = TRUE)

          if (length(opus_files) == 0) {
            showNotification("No Opus files (*.0) found in the ZIP archive.", type = "warning")
            return()
          }

          showNotification(paste("Processing", length(opus_files), "spectral files..."), type = "message")

          # Process uploaded spectral data
          uploaded_spectra <- read_opus(opus_files, parallel = TRUE)

          if (length(uploaded_spectra) == 0) {
            showNotification("Failed to read spectral data from uploaded files.", type = "error")
            return()
          }

          # Process the uploaded spectra similar to sample data
          uploaded_spectra_names <- names(uploaded_spectra)
          uploaded_wavelengths <- uploaded_spectra[[uploaded_spectra_names[1]]]$ab_no_atm_comp$wavenumbers
          uploaded_absorbance_values <- uploaded_spectra[[uploaded_spectra_names[1]]]$ab_no_atm_comp$data[1:length(uploaded_wavelengths)]
          uploaded_ssn_absorbance_values <- c(uploaded_spectra[[uploaded_spectra_names[1]]]$basic_metadata$opus_sample_name, uploaded_absorbance_values)

          # Combine all uploaded spectra
          for (i in 2:length(uploaded_spectra)) {
            uploaded_absorbance_values_add <- uploaded_spectra[[uploaded_spectra_names[i]]]$ab_no_atm_comp$data[1:length(uploaded_wavelengths)]
            uploaded_ssn_absorbance_values_add <- c(uploaded_spectra[[uploaded_spectra_names[i]]]$basic_metadata$opus_sample_name, uploaded_absorbance_values_add)
            uploaded_ssn_absorbance_values <- rbind(uploaded_ssn_absorbance_values, uploaded_ssn_absorbance_values_add)
          }

          uploaded_spectral_df <- data.table(uploaded_ssn_absorbance_values)
          colnames(uploaded_spectral_df) <- c("SSN", as.character(uploaded_wavelengths))
          uploaded_spectral_df[, 2:ncol(uploaded_spectral_df)] <- lapply(uploaded_spectral_df[, 2:ncol(uploaded_spectral_df)], as.numeric)

          # Calculate 1st derivatives
          # uploaded_spectral_df_sg <- savitzkyGolay(X = uploaded_spectral_df[, 2:ncol(uploaded_spectral_df)], p = 3, w = 11, m = 1)

          # Store processed data in reactive values
          uploaded_spectra_df(uploaded_spectral_df)
          # uploaded_spectra_df_sg(data.table(SSN = uploaded_spectral_df$SSN, uploaded_spectral_df_sg))
          spectra_count(length(uploaded_spectra))
          spectra_processed(TRUE) # Enable predict button

          showNotification(paste("Successfully processed", length(uploaded_spectra), "spectral files!"), type = "success")

          # Clean up temp directory
          unlink(extract_dir, recursive = TRUE)
        },
        error = function(e) {
          showNotification(paste("Error processing files:", e$message), type = "error")
        }
      )
    })

    # Reactive values
    prediction_result <- reactiveVal(NULL)

    # ============================================================================
    # Prediction Handler with Progress Indicators
    # ============================================================================
    # Handles soil property prediction with comprehensive user feedback:
    # - Button state management (disable/enable)
    # - Spinner and progress bar indicators
    # - Success/error notifications
    # ============================================================================

    observeEvent(input$predict_btn, {
      # Check if spectral data is available
      if (is.null(uploaded_spectra_df()) || nrow(uploaded_spectra_df()) == 0) {
        showNotification("Please upload and unzip spectral data first!", type = "warning")
        return()
      }

      # Set loading state and disable button
      prediction_loading(TRUE)
      shinyjs::disable("predict_btn")
      shinyjs::html("predict_btn", "Processing... (this might take a few...)")

      # Use delayed execution to show spinner first
      invalidateLater(100, session)

      tryCatch(
        {
          # Call the actual prediction function
          predictions <- process_spectra_predict(spectra_mir = uploaded_spectra_df())

          # Rename columns for better display
          colnames(predictions) <- c(
            "Sample_ID", "SOC_g/kg", "TN_g/kg", "pH", "CEC_cmolc/kg",
            "Clay_%", "Sand_%", "ExCa_cmolc/kg", "ExMg_cmolc/kg", "ExK_cmolc/kg"
          )

          # Store results and update state
          prediction_results(predictions)
          prediction_loading(FALSE)
          shinyjs::enable("predict_btn")
          shinyjs::html("predict_btn", paste(icon("flask"), " Predict Soil Properties"))
          showNotification(
            paste("✅ Prediction completed successfully for", nrow(predictions), "samples!"),
            type = "success",
            duration = 5
          )
        },
        error = function(e) {
          prediction_loading(FALSE)
          shinyjs::enable("predict_btn")
          shinyjs::html("predict_btn", paste(icon("flask"), " Predict Soil Properties"))
          showNotification(
            paste("❌ Prediction failed:", e$message),
            type = "error",
            duration = 8
          )
        }
      )
    })

    # Model information
    output$model_info <- renderText({
      if (is.null(prediction_result())) {
        "No model run yet"
      } else {
        paste0(
          "Model: ", switch(input$model_type,
            "lm" = "Linear Regression",
            "rf" = "Random Forest",
            "nn" = "Neural Network"
          ),
          "\nParameters: ", input$input_value_1, ", ", input$input_value_2,
          "\nTimestamp: ", Sys.time()
        )
      }
    })

    # Status indicator
    output$status_indicator <- renderUI({
      if (is.null(prediction_result())) {
        span(class = "badge bg-secondary", "Ready")
      } else {
        span(class = "badge bg-success", "Prediction Complete")
      }
    })

    # Soil property density plots
    output$prediction_plot <- renderPlot({
      if (is.null(prediction_results())) {
        # Default plot when no prediction
        par(mfrow = c(1, 1), mar = c(4, 4, 4, 2))
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
        text(1, 1, "Upload spectral data and generate predictions\nto see soil property distributions",
          cex = 1.5, col = "gray60", font = 2
        )
        box(col = "gray80")
      } else {
        # Create density plots for soil properties
        predictions <- prediction_results()

        # Define soil properties to plot (excluding Sample_ID)
        soil_properties <- colnames(predictions)[-1] # Remove Sample_ID column
        n_props <- length(soil_properties)

        # Set up multi-panel layout with more padding
        par(mfrow = c(3, 3), mar = c(5, 5, 4, 2), oma = c(3, 3, 4, 2))

        # Get selected row values if any
        selected_values <- selected_row_values()

        # Create density plot for each soil property
        for (i in 1:n_props) {
          prop_name <- soil_properties[i]
          prop_values <- as.numeric(predictions[[prop_name]])

          # Remove any NA values
          prop_values <- prop_values[!is.na(prop_values)]

          if (length(prop_values) > 1) {
            # Calculate density
            density_data <- density(prop_values, na.rm = TRUE)

            # Set x-axis limits with lower bound of 0 (except for pH)
            xlim_range <- NULL
            if (prop_name %in% c("Clay_%", "Sand_%")) {
              xlim_range <- c(0, 100)
            } else if (prop_name == "pH") {
              # pH can be below 0, so use natural range
              xlim_range <- NULL
            } else {
              # Cut at 0 for all other properties
              xlim_range <- c(0, max(prop_values, na.rm = TRUE) * 1.1)
            }

            # Create the density plot
            plot(density_data,
              main = prop_name,
              xlab = "Value",
              ylab = "Density",
              col = "steelblue",
              lwd = 2,
              cex.main = 1.6,
              cex.lab = 2.0,
              cex.axis = 2.0,
              xlim = xlim_range
            )

            # Fill under the curve
            polygon(density_data$x, density_data$y,
              col = rgb(70 / 255, 130 / 255, 180 / 255, alpha = 0.3),
              border = NA
            )

            # Add vertical line for mean
            abline(v = mean(prop_values), col = "red", lwd = 2, lty = 2)
            abline(v = median(prop_values), col = "blue", lwd = 2, lty = 2)

            # Add vertical line for selected row value
            if (!is.null(selected_values) && prop_name %in% colnames(selected_values)) {
              selected_val <- as.numeric(selected_values[[prop_name]])
              if (!is.na(selected_val)) {
                abline(v = selected_val, col = "orange", lwd = 3, lty = 1)
                # Add a point at the density curve for the selected value
                selected_density <- approx(density_data$x, density_data$y, xout = selected_val)$y
                if (!is.na(selected_density)) {
                  points(selected_val, selected_density, col = "orange", pch = 19, cex = 1.5)
                }
              }
            }

            # Add text showing mean and sd
            mean_val <- round(mean(prop_values), 2)
            median_val <- round(median(prop_values), 2)
            sd_val <- round(sd(prop_values), 2)
            legend_text <- paste0("Mean: ", mean_val, "\nMedian: ", median_val, "\nSD: ", sd_val)

            # Add selected value to legend if available
            if (!is.null(selected_values) && prop_name %in% colnames(selected_values)) {
              selected_val <- as.numeric(selected_values[[prop_name]])
              if (!is.na(selected_val)) {
                legend_text <- paste0(legend_text, "\nSelected: ", round(selected_val, 2))
              }
            }

            text(par("usr")[2] - 0.02 * diff(par("usr")[1:2]),
              par("usr")[4] * 0.55,
              legend_text,
              cex = 1.6, adj = 1, col = "darkblue"
            )
          } else {
            # If only one value or no values, show a simple message
            plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = prop_name)
            text(1, 1, "Insufficient data\nfor density plot", cex = 1.0, col = "gray60")
            box(col = "gray80")
          }
        }

        # Add overall title
        mtext("Distribution of Predicted Soil Properties",
          side = 3, outer = TRUE, cex = 1.6, font = 2
        )
      }
    })

    # Spectra count display
    output$spectra_count_display <- renderUI({
      count <- spectra_count()
      if (count == 0) {
        span("0", style = "color: #ffa500;")
      } else {
        span(as.character(count), style = "color: #00ff88; font-weight: bold;")
      }
    })

    # Prediction count display
    output$prediction_count_display <- renderUI({
      if (is.null(prediction_results())) {
        span("(0 samples)", style = "color: #6c757d; font-size: 0.9em;")
      } else {
        unique_samples <- nrow(prediction_results())
        span(paste0("(", unique_samples, " unique spectra/samples)"),
          style = "color: #ffffff; font-weight: bold; font-size: 1.1em;"
        )
      }
    })

    # Control visibility of unzip button
    output$file_uploaded <- reactive({
      !is.null(input$spectra_file)
    })
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)

    # Control visibility of predict button
    output$spectra_ready <- reactive({
      spectra_processed()
    })
    outputOptions(output, "spectra_ready", suspendWhenHidden = FALSE)

    # Control visibility of download button
    output$predictions_ready <- reactive({
      !is.null(prediction_results()) && nrow(prediction_results()) > 0
    })
    outputOptions(output, "predictions_ready", suspendWhenHidden = FALSE)

    # Download handler for prediction results
    output$download_predictions <- downloadHandler(
      filename = function() {
        paste0("soil_predictions_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(prediction_results())
        write.csv(prediction_results(), file, row.names = FALSE)

        # Reset the app after download
        uploaded_spectra_df(NULL)
        uploaded_spectra_df_sg(NULL)
        spectra_count(0)
        prediction_results(NULL)
        spectra_processed(FALSE)
        prediction_loading(FALSE)
        selected_row_values(NULL)
      },
      contentType = "text/csv"
    )


    # Simple prediction status indicator - removed spinner
    output$prediction_spinner <- renderUI({
      NULL
    })

    # Simple progress bar for prediction process - static bar without animation
    output$prediction_progress_bar <- renderUI({
      if (prediction_loading()) {
        div(
          class = "progress mt-2",
          style = "height: 8px; background-color: rgba(0,0,0,0.1);",
          div(
            class = "progress-bar bg-success",
            role = "progressbar",
            style = "width: 100%; transition: none;"
          )
        )
      } else if (!is.null(prediction_results())) {
        # Show completed bar briefly after success
        div(
          class = "progress mt-2",
          style = "height: 8px; background-color: rgba(0,0,0,0.1);",
          div(
            class = "progress-bar bg-success",
            role = "progressbar",
            style = "width: 100%; transition: none;"
          )
        )
      } else {
        NULL
      }
    })

    # Reactive value to store selected row
    selected_row_values <- reactiveVal(NULL)

    # Prediction results table
    output$prediction_table <- DT::renderDataTable(
      {
        req(prediction_results())

        DT::datatable(
          prediction_results(),
          selection = list(mode = "single", target = "row"),
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel"),
            columnDefs = list(
              list(className = "dt-center", targets = "_all")
            )
          ),
          class = "table-striped table-hover",
          rownames = FALSE
        ) |>
          DT::formatRound(
            columns = c(
              "SOC_g/kg", "TN_g/kg", "pH", "CEC_cmolc/kg",
              "Clay_%", "Sand_%", "ExCa_cmolc/kg", "ExMg_cmolc/kg", "ExK_cmolc/kg"
            ),
            digits = 3
          )
      },
      server = FALSE
    )

    # Observe table row selection
    observeEvent(input$prediction_table_rows_selected, {
      if (length(input$prediction_table_rows_selected) > 0) {
        selected_row <- input$prediction_table_rows_selected[1]
        selected_values <- prediction_results()[selected_row, ]
        selected_row_values(selected_values)
      } else {
        selected_row_values(NULL)
      }
    })
  })
}
