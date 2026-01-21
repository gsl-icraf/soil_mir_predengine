# Prediction Module UI
prediction_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h2("Prediction Engine", class = "mb-4"),
    navset_card_tab(
      id = ns("prediction_tabs"),

      # Tab 1: Spectral Data and Upload
      nav_panel(
        title = list(icon("wave-square"), "Spectral Data"),
        value = "data",
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
                ),
                # Unzip progress bar
                uiOutput(ns("unzip_progress_bar"))
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
              shinycssloaders::withSpinner(
                plotlyOutput(ns("spectral_plot_combined"), height = "300px"),
                type = 6,
                color = "#00ff88",
                size = 0.8
              ),
              conditionalPanel(
                condition = "output.spectra_ready == true",
                ns = ns,
                card(
                  full_screen = TRUE,
                  class = "bg-dark border-0",
                  card_header(
                    div(
                      class = "d-flex justify-content-between align-items-center",
                      span(list(icon("project-diagram"), " Spectral PCA Space"), style = "color: white; font-weight: bold;"),
                    )
                  ),
                  card_body(
                    layout_columns(
                      col_widths = c(4, 4, 4),
                      plotlyOutput(ns("pca_plot_12"), height = "300px"),
                      plotlyOutput(ns("pca_plot_13"), height = "300px"),
                      plotlyOutput(ns("pca_plot_23"), height = "300px")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "output.predictions_ready == true",
                  ns = ns,
                  div(
                    class = "d-flex justify-content-center gap-3 mt-3",
                    downloadButton(ns("download_raw"), "Download Raw Spectra", class = "btn-outline-info", style = "font-size: 1.1rem;"),
                    downloadButton(ns("download_deriv"), "Download 1st Derivatives", class = "btn-outline-warning", style = "font-size: 1.1rem;")
                  )
                )
              )
            )
          )
        )
      ),

      # Tab 2: Prediction Results (conditional)
      nav_panel(
        title = list(icon("table"), "Prediction Results"),
        value = "results",
        card_body(
          # Results table and texture triangle layout
          div(
            class = "mb-4",
            layout_columns(
              col_widths = c(8, 4),

              # Left: Results table card
              card(
                class = "spectral-card",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    span(list(icon("table"), " Prediction Results")),
                    div(
                      class = "d-flex align-items-center gap-3",
                      uiOutput(ns("prediction_count_display")),
                      downloadButton(
                        ns("download_predictions"),
                        label = list(icon("download"), " Download Results (CSV)"),
                        class = "btn-success btn-xs"
                      )
                    )
                  ),
                  class = "bg-gradient text-white"
                ),
                card_body(
                  DT::dataTableOutput(ns("prediction_table"))
                )
              ),

              # Right: Texture triangle card
              card(
                class = "spectral-card",
                card_header(
                  icon("layer-group"), " Soil Texture Triangle",
                  class = "bg-gradient text-white"
                ),
                card_body(
                  plotlyOutput(ns("texture_triangle_plot"), height = "400px")
                )
              )
            )
          ),

          # Density plots card
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
    # Initially hide the results tab
    nav_hide("prediction_tabs", "results")

    # Show/hide results tab based on prediction availability
    observe({
      results_ready <- !is.null(prediction_results()) && nrow(prediction_results()) > 0

      if (results_ready) {
        nav_show("prediction_tabs", "results")
        nav_select("prediction_tabs", "results")
      } else {
        nav_hide("prediction_tabs", "results")
        nav_select("prediction_tabs", "data")
      }
    })

    ## On load...
    ### Sample spectral data

    sample_spectra <- list.files(path = "data/sample_spectra", pattern = "*.0", full.names = TRUE)
    # spectra <- read_opus(sample_spectra, parallel = TRUE)

    # Process uploaded spectral data
    s <- opus_read(sample_spectra[1])
    spectra <- cbind(s$metadata$sample_id, s$spec)

    ## Load additional files
    for (i in 2:length(sample_spectra)) {
      opus_file <- sample_spectra[i]
      s_add <- opus_read(opus_file)
      s_add <- cbind(s_add$metadata$sample_id, s_add$spec)
      spectra <- rbind(spectra, s_add)
    }

    spectral_df <- data.table(spectra)
    setnames(spectral_df, old = "V1", new = "SSN")
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
    # Reactive value for linked selection from PCA
    selected_ssn <- reactiveVal(NULL)

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

          plot_data[, absorbance_values := as.numeric(as.character(absorbance_values))]

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

      p <- plot_data |>
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
            font = list(color = "white", size = 20)
          ),
          xaxis = list(
            title = "Wavelength (cm<sup>-1</sup>)",
            autorange = "reversed",
            range = c(3991, 617),
            gridcolor = "#7f8c8d",
            color = "white",
            titlefont = list(size = 18),
            tickfont = list(size = 16)
          ),
          yaxis = list(
            title = y_title,
            gridcolor = "#7f8c8d",
            color = "white",
            titlefont = list(size = 18),
            tickfont = list(size = 16)
          ),
          dragmode = "zoom",
          plot_bgcolor = "#2d2d2d",
          paper_bgcolor = "#2d2d2d",
          font = list(color = "white"),
          hoverlabel = list(
            font = list(size = 16)
          )
        )

      # Highlight selected SSN if any
      if (!is.null(selected_ssn())) {
        highlight_ssn <- selected_ssn()
        highlight_data <- plot_data[SSN == highlight_ssn]

        if (nrow(highlight_data) > 0) {
          p <- p |>
            add_trace(
              data = highlight_data,
              x = ~wavelength,
              y = ~absorbance_values,
              type = "scatter",
              mode = "lines",
              line = list(color = "#FF0000", width = 3),
              name = paste("Selected:", highlight_ssn),
              hoverinfo = "name",
              inherit = FALSE
            )
        }
      }

      p |>
        event_register("plotly_relayout") |>
        event_register("plotly_doubleclick")
    })

    # Reset plot and selection when double-clicking
    observeEvent(event_data("plotly_doubleclick", source = "spectral_plot_combined"), {
      plotlyProxy("spectral_plot_combined", session) %>%
        plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
      selected_ssn(NULL)
    })

    # Reset selection when reset button is clicked
    observeEvent(input$reset_plots, {
      plotlyProxy("spectral_plot_combined", session) %>%
        plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
      selected_ssn(NULL)
    })

    ### File upload and processing
    # Reactive values to store uploaded spectral data
    uploaded_spectra_df <- reactiveVal(NULL)
    uploaded_spectra_df_sg <- reactiveVal(NULL)
    uploaded_spectra_resampled_df <- reactiveVal(NULL)
    uploaded_spectra_resampled_df_sg <- reactiveVal(NULL)
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

          # Show progress for unzipping (centered modal)
          withProgress(message = "Unzipping files...", value = 0, style = "notification", {
            # Unzip contents
            unzip(zip_path, exdir = extract_dir)
            incProgress(0.2, detail = "Scanning for spectral files...")

            # Find .0 files (Opus binary format)
            opus_files <- list.files(extract_dir, pattern = "*.0", full.names = TRUE, recursive = TRUE)

            if (length(opus_files) == 0) {
              showNotification("No Opus files (*.0) found in the ZIP archive.", type = "warning")
              return()
            }

            incProgress(0.1, detail = paste("Found", length(opus_files), "files"))

            # Process uploaded spectral data
            opus_file <- opus_files[1]
            s <- opus_read(opus_file)
            uploaded_spectra <- cbind(s$metadata$sample_id, s$spec)

            ## Load additional files with progress
            total_files <- length(opus_files)
            for (i in 2:total_files) {
              opus_file <- opus_files[i]
              s_add <- opus_read(opus_file)
              s_add <- cbind(s_add$metadata$sample_id, s_add$spec)
              uploaded_spectra <- rbind(uploaded_spectra, s_add)

              # Update progress every 10 files or at the end
              if (i %% 10 == 0 || i == total_files) {
                incProgress(0.5 / total_files * 10,
                  detail = paste("Processing file", i, "of", total_files)
                )
              }
            }

            incProgress(0.1, detail = "Finalizing data...")

            uploaded_spectra <- data.table(uploaded_spectra)
            uploaded_spectra[, SSN := V1]
            cols_to_convert <- names(uploaded_spectra)[-which(names(uploaded_spectra) == "SSN")]
            uploaded_spectra <- uploaded_spectra[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]
            uploaded_spectral_df <- uploaded_spectra[, lapply(.SD, mean), by = SSN, .SDcols = cols_to_convert]

            # uploaded_spectra <- read_opus(opus_files, parallel = TRUE)

            if (length(uploaded_spectra) == 0) {
              showNotification("Failed to read spectral data from uploaded files.", type = "error")
              return()
            }

            # # Process the uploaded spectra similar to sample data
            # uploaded_spectra_names <- names(uploaded_spectra)
            uploaded_wavelengths <- colnames(uploaded_spectral_df)[2:ncol(uploaded_spectral_df)]
            # uploaded_absorbance_values <- uploaded_spectra[[uploaded_spectra_names[1]]]$ab_no_atm_comp$data[1:length(uploaded_wavelengths)]
            # uploaded_ssn_absorbance_values <- c(uploaded_spectra[[uploaded_spectra_names[1]]]$basic_metadata$opus_sample_name, uploaded_absorbance_values)

            # # Combine all uploaded spectra
            # for (i in 2:length(uploaded_spectra)) {
            #   uploaded_absorbance_values_add <- uploaded_spectra[[uploaded_spectra_names[i]]]$ab_no_atm_comp$data[1:length(uploaded_wavelengths)]
            #   uploaded_ssn_absorbance_values_add <- c(uploaded_spectra[[uploaded_spectra_names[i]]]$basic_metadata$opus_sample_name, uploaded_absorbance_values_add)
            #   uploaded_ssn_absorbance_values <- rbind(uploaded_ssn_absorbance_values, uploaded_ssn_absorbance_values_add)
            # }

            # uploaded_spectral_df <- data.table(uploaded_ssn_absorbance_values)
            colnames(uploaded_spectral_df) <- c("SSN", as.character(uploaded_wavelengths))
            uploaded_spectral_df[, 2:ncol(uploaded_spectral_df)] <- lapply(uploaded_spectral_df[, 2:ncol(uploaded_spectral_df)], as.numeric)

            incProgress(0.1, detail = "Calculating derivatives...")

            # Calculate 1st derivatives
            uploaded_spectral_df_sg <- savitzkyGolay(X = uploaded_spectral_df[, 2:ncol(uploaded_spectral_df)], p = 3, w = 11, m = 1)

            # Store processed data in reactive values
            uploaded_spectra_df(uploaded_spectral_df)
            uploaded_spectra_df_sg(data.table(SSN = uploaded_spectral_df$SSN, uploaded_spectral_df_sg))
            spectra_count(length(unique(uploaded_spectra$SSN)))
            spectra_processed(TRUE) # Enable predict button

            setProgress(1, detail = "Complete!")
          })

          showNotification(paste("Successfully processed", length(unique(uploaded_spectra$SSN)), "spectral files!"), type = "success")

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
    # Progress Indicators
    # ============================================================================
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
          # Log start of prediction
          cat("Starting prediction process...\n")
          cat("Input data dimensions:", nrow(uploaded_spectra_df()), "x", ncol(uploaded_spectra_df()), "\n")

          # Call the actual prediction function
          results <- process_spectra_predict(spectra_mir = uploaded_spectra_df())
          predictions <- results$predictions

          # Store resampled data for downloads
          uploaded_spectra_resampled_df(results$raw_resampled)
          uploaded_spectra_resampled_df_sg(results$deriv_resampled)

          cat("Prediction completed. Output dimensions:", nrow(predictions), "x", ncol(predictions), "\n")

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
          # Log detailed error information
          cat("ERROR in prediction:\n")
          cat("Message:", e$message, "\n")
          cat("Call:", deparse(e$call), "\n")
          cat("Traceback:\n")
          print(sys.calls())

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
        uploaded_spectra_resampled_df(NULL)
        uploaded_spectra_resampled_df_sg(NULL)
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

    # Texture triangle plot (Plotly ternary)
    output$texture_triangle_plot <- renderPlotly({
      req(prediction_results())

      # Prepare data for texture triangle
      predictions <- prediction_results()

      # Calculate Silt % (assuming Clay + Sand + Silt = 100)
      texture_data <- data.frame(
        Sample_ID = predictions$Sample_ID,
        Clay = predictions$`Clay_%`,
        Sand = predictions$`Sand_%`,
        Silt = 100 - predictions$`Clay_%` - predictions$`Sand_%`
      )

      # Remove any rows with negative silt or NA values
      texture_data <- texture_data[texture_data$Silt >= 0 &
        !is.na(texture_data$Clay) &
        !is.na(texture_data$Sand) &
        !is.na(texture_data$Silt), ]

      if (nrow(texture_data) == 0) {
        # Return empty plotly if no valid data
        return(plot_ly() |>
          layout(
            title = list(text = "No valid texture data to display", font = list(color = "white")),
            paper_bgcolor = "#2d2d2d",
            plot_bgcolor = "#2d2d2d"
          ))
      }

      # USDA texture class boundaries for plotly
      # Define boundaries for each class as separate traces
      usda_classes <- list(
        list(name = "Clay", a = c(0, 45, 45, 0, 0), b = c(40, 40, 55, 100, 40), c = c(60, 15, 0, 0, 60)),
        list(name = "Silty Clay", a = c(0, 0, 20, 20, 0), b = c(40, 60, 60, 40, 40), c = c(60, 40, 20, 40, 60)),
        list(name = "Sandy Clay", a = c(45, 65, 65, 45, 45), b = c(35, 35, 55, 55, 35), c = c(20, 0, -20, 0, 20)),
        list(name = "Clay Loam", a = c(20, 45, 52, 52, 20, 20), b = c(27, 27, 35, 40, 40, 27), c = c(53, 28, 13, 8, 40, 53)),
        list(name = "Silty Clay Loam", a = c(0, 20, 20, 0, 0, 0), b = c(27, 27, 40, 40, 27, 27), c = c(73, 53, 40, 60, 73, 73)),
        list(name = "Sandy Clay Loam", a = c(45, 52, 80, 80, 45, 45), b = c(20, 20, 27, 35, 35, 20), c = c(35, 28, -7, -15, 20, 35)),
        list(name = "Loam", a = c(23, 52, 52, 43, 23, 23), b = c(7, 7, 20, 27, 27, 7), c = c(70, 41, 28, 30, 50, 70)),
        list(name = "Silt Loam", a = c(0, 20, 50, 50, 0, 0), b = c(0, 0, 12, 27, 27, 0), c = c(100, 80, 38, 23, 73, 100)),
        list(name = "Sandy Loam", a = c(43, 52, 85, 85, 43, 43), b = c(0, 7, 7, 15, 15, 0), c = c(57, 41, 8, 0, 42, 57)),
        list(name = "Loamy Sand", a = c(70, 85, 90, 70, 70), b = c(0, 0, 15, 15, 0), c = c(30, 15, -5, 15, 30)),
        list(name = "Sand", a = c(85, 90, 100, 85, 85), b = c(0, 0, 10, 10, 0), c = c(15, 10, -10, 5, 15)),
        list(name = "Silt", a = c(0, 0, 20, 0, 0), b = c(0, 12, 12, 0, 0), c = c(100, 88, 68, 100, 100))
      )

      # Create plotly ternary plot
      p <- plot_ly()

      # Add USDA classification boundaries (very subtle)
      for (class_info in usda_classes) {
        p <- p |> add_trace(
          type = "scatterternary",
          mode = "lines",
          a = class_info$a,
          b = class_info$b,
          c = class_info$c,
          line = list(color = "rgba(128, 128, 128, 0.25)", width = 0.5),
          hoverinfo = "text",
          text = class_info$name,
          showlegend = FALSE,
          name = class_info$name
        )
      }

      # Add class labels (positioned at centroids)
      class_labels <- data.frame(
        a = c(92, 80, 55, 35, 10, 8, 25, 65, 55, 20, 8, 15),
        b = c(5, 10, 8, 18, 15, 34, 34, 28, 42, 50, 50, 60),
        c = c(3, 10, 37, 47, 75, 58, 32, 7, 3, 30, 42, 25),
        label = c(
          "Sand", "Loamy Sand", "Sandy Loam", "Loam", "Silt Loam",
          "Silt", "Silty Clay Loam", "Sandy Clay Loam", "Sandy Clay",
          "Clay Loam", "Silty Clay", "Clay"
        )
      )

      p <- p |> add_trace(
        type = "scatterternary",
        mode = "text",
        a = class_labels$a,
        b = class_labels$b,
        c = class_labels$c,
        text = class_labels$label,
        textfont = list(color = "rgba(160, 160, 160, 0.4)", size = 9, family = "Poppins"),
        hoverinfo = "skip",
        showlegend = FALSE
      )

      # Add data points
      p <- p |> add_trace(
        type = "scatterternary",
        mode = "markers",
        a = texture_data$Sand,
        b = texture_data$Clay,
        c = texture_data$Silt,
        text = ~ paste0(
          "<b>", texture_data$Sample_ID, "</b><br>",
          "Sand: ", round(texture_data$Sand, 1), "%<br>",
          "Clay: ", round(texture_data$Clay, 1), "%<br>",
          "Silt: ", round(texture_data$Silt, 1), "%"
        ),
        hoverinfo = "text",
        marker = list(
          size = 8,
          color = "#00ff88",
          opacity = 0.7,
          line = list(color = "#00ff88", width = 1)
        ),
        showlegend = FALSE,
        name = "Samples"
      )

      # Highlight selected row if any
      selected_values <- selected_row_values()
      if (!is.null(selected_values)) {
        selected_silt <- 100 - selected_values$`Clay_%` - selected_values$`Sand_%`

        if (selected_silt >= 0 && !is.na(selected_values$`Clay_%`)) {
          p <- p |> add_trace(
            type = "scatterternary",
            mode = "markers",
            a = selected_values$`Sand_%`,
            b = selected_values$`Clay_%`,
            c = selected_silt,
            text = ~ paste0(
              "<b>SELECTED: ", selected_values$Sample_ID, "</b><br>",
              "Sand: ", round(selected_values$`Sand_%`, 1), "%<br>",
              "Clay: ", round(selected_values$`Clay_%`, 1), "%<br>",
              "Silt: ", round(selected_silt, 1), "%"
            ),
            hoverinfo = "text",
            marker = list(
              size = 14,
              color = "orange",
              opacity = 1,
              line = list(color = "white", width = 2)
            ),
            showlegend = FALSE,
            name = "Selected"
          )
        }
      }

      # Configure layout
      p <- p |> layout(
        ternary = list(
          sum = 100,
          aaxis = list(
            title = list(text = "Sand %", font = list(color = "white", size = 12)),
            tickfont = list(color = "white", size = 10),
            gridcolor = "rgba(128, 128, 128, 0.3)",
            linecolor = "white"
          ),
          baxis = list(
            title = list(text = "Clay %", font = list(color = "white", size = 12)),
            tickfont = list(color = "white", size = 10),
            gridcolor = "rgba(128, 128, 128, 0.3)",
            linecolor = "white"
          ),
          caxis = list(
            title = list(text = "Silt %", font = list(color = "white", size = 12)),
            tickfont = list(color = "white", size = 10),
            gridcolor = "rgba(128, 128, 128, 0.3)",
            linecolor = "white"
          ),
          bgcolor = "#1a1a1a"
        ),
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        hoverlabel = list(
          bgcolor = "#1a1a1a",
          font = list(color = "white", size = 12, family = "Poppins"),
          bordercolor = "#00ff88"
        ),
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )

      p
    })


    # PCA model
    pca_model <- reactive({
      readRDS(file.path("pca_model", "mir_pca_model.rds"))
    })

    # Load PCA scores data
    pca_scores_ref <- reactive({
      fread(file.path("data", "mir_pca_scores_5comp.csv"))
    })

    # Projected scores for uploaded data
    uploaded_pca_scores <- reactive({
      req(uploaded_spectra_df())

      # Re-do preprocessing correctly to match src/spectra_process_predict.R
      spectra_dt <- copy(uploaded_spectra_df())
      ssn_labels <- spectra_dt$SSN
      spectra_dt[, SSN := NULL]

      mir_zscore <- scale(spectra_dt)
      spectra_mir_sg <- data.table(savitzkyGolay(X = mir_zscore, p = 2, w = 11, m = 1))

      # 3. Reference bands for resampling
      wavebands_ref <- read.table(file.path("data", "wavebands.txt"), header = FALSE)
      wavebands_ref <- as.numeric(wavebands_ref$V1)

      # Wavelength selection
      headers_spectra <- as.numeric(colnames(spectra_dt))
      band_sel <- headers_spectra >= 617 & headers_spectra <= 3991
      spectra_mir_sel <- spectra_mir_sg[, ..band_sel]

      resampled_spectra <- resample(spectra_mir_sel, wav = colnames(spectra_mir_sel), new.wav = wavebands_ref)
      colnames(resampled_spectra) <- paste0("w", wavebands_ref)

      # 5. Project using PCA model
      pca_res <- predict(pca_model(), resampled_spectra)

      # Return as data table with SSN
      dt_scores <- as.data.table(pca_res)
      dt_scores[, SSN := ssn_labels]
      dt_scores
    })

    # Helper function for PCA contour plots with overlay
    render_pca_contour <- function(pca_df, x_var, y_var, title, overlay_df = NULL, source_id = NULL) {
      p <- plot_ly(pca_df, x = as.formula(paste0("~", x_var)), y = as.formula(paste0("~", y_var)), source = source_id) |>
        add_histogram2dcontour(
          colorscale = list(
            list(0, "rgba(0, 0, 0, 0)"),
            list(0.1, "rgba(30, 0, 80, 0.3)"),
            list(0.3, "rgba(50, 0, 150, 0.5)"),
            list(0.6, "rgba(100, 50, 200, 0.7)"),
            list(1, "rgba(0, 255, 136, 0.9)")
          ),
          showscale = FALSE,
          ncontours = 25,
          line = list(width = 0.5, color = "rgba(255,255,255,0.1)"),
          hoverinfo = "none",
          name = "Reference Density"
        )

      # Add overlay if provided
      if (!is.null(overlay_df) && nrow(overlay_df) > 0) {
        p <- p |>
          add_trace(
            data = overlay_df,
            x = as.formula(paste0("~", x_var)),
            y = as.formula(paste0("~", y_var)),
            type = "scatter",
            mode = "markers",
            customdata = ~SSN,
            marker = list(
              color = "#ff9100", # ICRAF Orange
              size = 8,
              line = list(color = "white", width = 1),
              opacity = 0.8
            ),
            text = ~SSN,
            hovertemplate = "<b>SSN: %{text}</b><extra></extra>",
            name = "Uploaded Spectra"
          )
      }

      p |>
        layout(
          plot_bgcolor = "#222222",
          paper_bgcolor = "rgba(0,0,0,0)",
          xaxis = list(
            title = x_var,
            gridcolor = "#333333",
            zerolinecolor = "#444444",
            color = "white",
            titlefont = list(size = 16),
            tickfont = list(size = 14)
          ),
          yaxis = list(
            title = y_var,
            gridcolor = "#333333",
            zerolinecolor = "#444444",
            color = "white",
            titlefont = list(size = 16),
            tickfont = list(size = 14)
          ),
          margin = list(l = 50, r = 20, b = 50, t = 60),
          title = list(
            text = title,
            font = list(color = "rgba(255,255,255,0.9)", size = 18),
            y = 0.98
          ),
          hoverlabel = list(
            font = list(size = 16)
          ),
          showlegend = FALSE
        ) |>
        config(displayModeBar = FALSE) |>
        event_register("plotly_click")

      p
    }

    # Observe clicks on PCA plots
    observe({
      click_12 <- event_data("plotly_click", source = "pca_plot_12")
      click_13 <- event_data("plotly_click", source = "pca_plot_13")
      click_23 <- event_data("plotly_click", source = "pca_plot_23")

      clicked_ssn <- NULL
      if (!is.null(click_12)) clicked_ssn <- click_12$customdata
      if (!is.null(click_13)) clicked_ssn <- click_13$customdata
      if (!is.null(click_23)) clicked_ssn <- click_23$customdata

      if (!is.null(clicked_ssn)) {
        selected_ssn(clicked_ssn)
      }
    })

    output$pca_plot_12 <- renderPlotly({
      render_pca_contour(pca_scores_ref(), "PC1", "PC2", "PC1 vs PC2", uploaded_pca_scores(), source_id = "pca_plot_12")
    })

    output$pca_plot_13 <- renderPlotly({
      render_pca_contour(pca_scores_ref(), "PC1", "PC3", "PC1 vs PC3", uploaded_pca_scores(), source_id = "pca_plot_13")
    })

    output$pca_plot_23 <- renderPlotly({
      render_pca_contour(pca_scores_ref(), "PC2", "PC3", "PC2 vs PC3", uploaded_pca_scores(), source_id = "pca_plot_23")
    })

    # ============================================================================
    # Spectra Downloads
    # ============================================================================

    output$download_raw <- downloadHandler(
      filename = function() {
        paste0("raw_spectra_resampled_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(uploaded_spectra_resampled_df())
        fwrite(uploaded_spectra_resampled_df(), file)
      },
      contentType = "text/csv"
    )

    output$download_deriv <- downloadHandler(
      filename = function() {
        paste0("derivative_spectra_resampled_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(uploaded_spectra_resampled_df_sg())
        fwrite(uploaded_spectra_resampled_df_sg(), file)
      },
      contentType = "text/csv"
    )
  })
}
