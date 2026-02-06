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

              # Feature selection for prediction
              conditionalPanel(
                condition = "output.spectra_ready == true",
                ns = ns,
                div(
                  class = "mt-3 p-3 rounded",
                  style = "background-color: rgba(255,255,255,0.05); border: 1px solid rgba(255,255,255,0.1);",
                  h6("Select properties to predict:", style = "color: white; font-weight: bold; font-size: 0.9em;"),
                  checkboxGroupInput(
                    ns("selected_properties"),
                    label = NULL,
                    choices = c(
                      "SOC" = "SOC", "TN" = "TN", "pH" = "pH", "CEC" = "CEC",
                      "Clay" = "clay", "Sand" = "sand", "ExCa" = "ExCa",
                      "ExMg" = "ExMg", "ExK" = "ExK"
                    ),
                    selected = c("SOC", "TN", "pH", "CEC", "clay", "sand", "ExCa", "ExMg", "ExK"),
                    inline = TRUE
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
          # Tip! Card
          card(
            class = "spectral-card mb-3",
            style = "background-color: rgba(0, 255, 136, 0.05); border: 1px dashed rgba(0, 255, 136, 0.3);",
            card_body(
              div(
                class = "d-flex align-items-center",
                icon("lightbulb", class = "me-3", style = "color: #00ff88; font-size: 1.5em;"),
                div(
                  h6("Tip!", class = "mb-1", style = "color: #00ff88;"),
                  p("Clicking on a row in the table will highlight that specific sample in the texture triangle and the distribution plots below.", class = "mb-0 small")
                )
              )
            )
          ),

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
                      ),
                      actionButton(
                        ns("reset_session"),
                        label = list(icon("rotate-left"), " New Session"),
                        class = "btn-outline-warning btn-xs"
                      )
                    )
                  ),
                  class = "bg-gradient text-white"
                ),
                card_body(
                  DT::dataTableOutput(ns("prediction_table"))
                ),
                uiOutput(ns("warning_negative_predictions"))
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
            full_screen = TRUE,
            class = "spectral-card",
            card_header(
              icon("chart-area"), " Soil Property Distributions",
              class = "bg-gradient text-white"
            ),
            card_body(
              plotlyOutput(ns("prediction_plot"), height = "600px")
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
    if (length(sample_spectra) > 0) {
      s_list <- lapply(sample_spectra, function(f) {
        s_res <- opus_read(f)
        cbind(s_res$metadata$sample_id, s_res$spec)
      })
      spectra <- as.data.frame(do.call(rbind, s_list))

      spectral_df <- data.table(spectra)
      setnames(spectral_df, old = colnames(spectral_df)[1], new = "SSN")

      # Convert all spectral columns to numeric
      spectral_cols <- colnames(spectral_df)[-1]
      spectral_df[, (spectral_cols) := lapply(.SD, as.numeric), .SDcols = spectral_cols]

      ## 1st derivatives
      spectral_df_sg <- savitzkyGolay(X = spectral_df[, ..spectral_cols], p = 3, w = 11, m = 1)
    } else {
      spectral_df <- data.table(SSN = character(0))
      spectral_df_sg <- matrix(0, nrow = 0, ncol = 0)
    }


    ## Reshape data for plotting
    spectral_df_long <- melt.data.table(
      spectral_df,
      id.vars = "SSN",
      variable.name = "wavelength",
      value.name = "absorbance_values"
    )

    spectral_df_long <- spectral_df_long[, .(absorbance_values = mean(absorbance_values, na.rm = TRUE)), by = .(wavelength, SSN)]
    spectral_df_long[, wavelength := as.numeric(as.character(wavelength))]
    spectral_df_long <- spectral_df_long[!is.na(wavelength) & wavelength >= 617 & wavelength <= 3991, ]
    spectral_df_long <- spectral_df_long[order(SSN, wavelength)]

    spectral_df_sg_long <- melt.data.table(
      data.table(SSN = spectral_df$SSN, spectral_df_sg),
      id.vars = "SSN",
      variable.name = "wavelength",
      value.name = "absorbance_values"
    )

    spectral_df_sg_long <- spectral_df_sg_long[, .(absorbance_values = mean(absorbance_values, na.rm = TRUE)), by = .(wavelength, SSN)]
    spectral_df_sg_long[, wavelength := as.numeric(as.character(wavelength))]
    spectral_df_sg_long <- spectral_df_sg_long[!is.na(wavelength) & wavelength >= 617 & wavelength <= 3991, ]
    spectral_df_sg_long <- spectral_df_sg_long[order(SSN, wavelength)]

    # Load spectral library percentiles for reference band
    spectral_percentiles <- fread("data/spectral_library_percentiles.csv")


    # Reactive value for derivative toggle
    show_derivative <- reactiveVal(FALSE)
    # Reactive value for linked selection from PCA/Table
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

    # Reactive for current spectral plot data (handles original/derivative toggle)
    current_spectral_plot_data <- reactive({
      use_derivative <- show_derivative()
      orig_df <- uploaded_spectra_df()
      sg_df <- uploaded_spectra_df_sg()

      if (use_derivative) {
        if (!is.null(sg_df)) {
          plot_data <- melt.data.table(sg_df, id.vars = "SSN", variable.name = "wavelength", value.name = "absorbance_values")
          plot_data[, absorbance_values := as.numeric(as.character(absorbance_values))]
          plot_data[, wavelength := as.numeric(as.character(wavelength))]
          plot_data <- plot_data[wavelength >= 617 & wavelength <= 3991][order(SSN, wavelength)]
        } else {
          plot_data <- spectral_df_sg_long
        }
      } else {
        if (!is.null(orig_df)) {
          plot_data <- melt.data.table(orig_df, id.vars = "SSN", variable.name = "wavelength", value.name = "absorbance_values")
          plot_data[, wavelength := as.numeric(as.character(wavelength))]
          plot_data <- plot_data[wavelength >= 617 & wavelength <= 3991][order(SSN, wavelength)]
        } else {
          plot_data <- spectral_df_long
        }
      }
      plot_data
    })

    ### Render combined plot
    output$spectral_plot_combined <- renderPlotly({
      plot_data <- current_spectral_plot_data()
      use_derivative <- show_derivative()
      y_title <- if (use_derivative) "First Derivative" else "Absorbance"

      all_ssns <- unique(plot_data$SSN)
      n_total <- length(all_ssns)
      n_initial <- min(10L, n_total)
      plasma_colors <- viridis::plasma(n_total)

      p <- plot_ly(source = session$ns("spectral_plot_combined"))

      # Add spectral library percentile band (only for raw absorbance)
      if (!use_derivative) {
        p <- p |>
          add_trace(
            data = spectral_percentiles,
            x = ~wavelength, y = ~p10,
            type = "scatter", mode = "lines",
            line = list(color = "transparent"),
            showlegend = FALSE, hoverinfo = "skip",
            name = "p10", inherit = FALSE
          ) |>
          add_trace(
            data = spectral_percentiles,
            x = ~wavelength, y = ~p90,
            type = "scatter", mode = "lines",
            fill = "tonexty",
            fillcolor = "rgba(255, 255, 255, 0.1)",
            line = list(color = "transparent"),
            showlegend = FALSE, hoverinfo = "skip",
            name = "Library P10-P90", inherit = FALSE
          )
      }

      # Add first batch of spectra for fast initial render
      for (i in seq_len(n_initial)) {
        ssn <- all_ssns[i]
        ssn_data <- plot_data[SSN == ssn]
        p <- p |> add_trace(
          x = ssn_data$wavelength,
          y = ssn_data$absorbance_values,
          type = "scatter", mode = "lines",
          line = list(color = plasma_colors[i]),
          customdata = ssn, text = ssn, name = ssn,
          showlegend = FALSE, inherit = FALSE
        )
      }

      p <- p |>
        layout(
          yaxis = list(
            title = y_title,
            gridcolor = "#7f8c8d", color = "white",
            titlefont = list(size = 20), tickfont = list(size = 18)
          ),
          xaxis = list(
            title = "Wavelength (cm<sup>-1</sup>)",
            autorange = "reversed", range = c(3991, 617),
            gridcolor = "#7f8c8d", color = "white",
            titlefont = list(size = 20), tickfont = list(size = 18)
          ),
          dragmode = "zoom",
          plot_bgcolor = "#2d2d2d",
          paper_bgcolor = "#2d2d2d",
          font = list(color = "white", size = 16),
          showlegend = FALSE
        )

      # Highlight trace (always last)
      p <- p |> add_trace(
        x = numeric(0), y = numeric(0),
        type = "scatter", mode = "lines",
        line = list(color = "#FF0000", width = 3),
        name = "Highlight", hoverinfo = "name",
        inherit = FALSE
      )

      # Add remaining spectra via proxy after initial render
      if (n_total > n_initial) {
        remaining_ssns <- all_ssns[(n_initial + 1):n_total]
        remaining_colors <- plasma_colors[(n_initial + 1):n_total]
        n_pct <- if (!use_derivative) 2L else 0L
        insert_at <- n_pct + n_initial # 0-indexed

        session$onFlushed(function() {
          p_proxy <- plotlyProxy("spectral_plot_combined", session)
          traces <- lapply(seq_along(remaining_ssns), function(i) {
            ssn <- remaining_ssns[i]
            ssn_data <- plot_data[SSN == ssn]
            list(
              x = ssn_data$wavelength,
              y = ssn_data$absorbance_values,
              type = "scatter",
              mode = "lines",
              line = list(color = remaining_colors[i]),
              customdata = rep(ssn, nrow(ssn_data)),
              text = rep(ssn, nrow(ssn_data)),
              name = ssn,
              showlegend = FALSE
            )
          })
          indices <- as.integer(
            insert_at + seq_along(traces) - 1L
          )
          p_proxy |> plotlyProxyInvoke("addTraces", traces, indices)
        }, once = TRUE)
      }

      p |>
        event_register("plotly_relayout") |>
        event_register("plotly_doubleclick") |>
        event_register("plotly_click")
    })

    # Efficiently update highlighting using plotlyProxy
    observeEvent(selected_ssn(),
      {
        ssn <- selected_ssn()

        # 1. Update spectral plot via proxy
        p_proxy <- plotlyProxy("spectral_plot_combined", session)
        n_data_traces <- length(unique(current_spectral_plot_data()$SSN))
        # Account for percentile band traces (2 traces) when showing raw absorbance
        n_pct_traces <- if (!show_derivative()) 2L else 0L
        highlight_idx <- as.integer(n_data_traces + n_pct_traces)

        if (is.null(ssn)) {
          p_proxy |> plotlyProxyInvoke("restyle", list(x = list(numeric(0)), y = list(numeric(0))), highlight_idx)
          # Clear detailed selection data
          isolate(selected_row_values(NULL))
        } else {
          h_data <- current_spectral_plot_data()[SSN == ssn]
          if (nrow(h_data) > 0) {
            p_proxy |> plotlyProxyInvoke("restyle", list(
              x = list(h_data$wavelength),
              y = list(h_data$absorbance_values),
              name = list(paste("Selected:", ssn))
            ), highlight_idx)
          }
        }

        # 2. Sync with selected_row_values for distribution plots and triangle
        if (!is.null(ssn) && !is.null(prediction_results())) {
          # Find matching row in results
          results <- prediction_results()
          match <- results[results$SSN == ssn, ]
          if (nrow(match) > 0) {
            # Use isolate to avoid recursive calls
            isolate(selected_row_values(match))
          }
        }
      },
      ignoreInit = TRUE
    )


    observeEvent(event_data("plotly_doubleclick", source = "spectral_plot_combined"), {
      plotlyProxy("spectral_plot_combined", session) %>%
        plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
      selected_ssn(NULL)
      # Also clear table selection
      DT::selectRows(DT::dataTableProxy("prediction_table"), NULL)
    })

    # Reset selection when reset button is clicked
    observeEvent(input$reset_plots, {
      plotlyProxy("spectral_plot_combined", session) %>%
        plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE, "yaxis.autorange" = TRUE))
      selected_ssn(NULL)
      # Also clear table selection
      DT::selectRows(DT::dataTableProxy("prediction_table"), NULL)
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

            # Find .0 files (Opus binary format), excluding macOS resource forks
            opus_files <- list.files(extract_dir, pattern = "\\.0$", full.names = TRUE, recursive = TRUE)
            opus_files <- opus_files[!grepl("__MACOSX", opus_files)]
            opus_files <- opus_files[!grepl("/\\._", opus_files)]

            if (length(opus_files) == 0) {
              showNotification("No Opus files (*.0) found in the ZIP archive.", type = "warning")
              return()
            }

            incProgress(0.1, detail = paste("Found", length(opus_files), "files"))

            # Process uploaded spectral data
            total_files <- length(opus_files)
            spectra_list <- vector("list", total_files)

            for (i in seq_along(opus_files)) {
              tryCatch(
                {
                  s <- opus_read(opus_files[i])
                  dt <- data.table(cbind(s$metadata$sample_id, s$spec))
                  spectra_list[[i]] <- dt
                },
                error = function(e) {
                  warning(paste("Skipping file", basename(opus_files[i]), ":", e$message))
                }
              )

              # Update progress every 10 files or at the end
              if (i %% 10 == 0 || i == total_files) {
                incProgress(0.5 / total_files * min(i, 10),
                  detail = paste("Processing file", i, "of", total_files)
                )
              }
            }

            # Combine spectra - use majority wavenumber layout, skip incompatible files
            spectra_list <- spectra_list[!sapply(spectra_list, is.null)]

            incProgress(0.1, detail = "Finalizing data...")

            if (length(spectra_list) == 0) {
              showNotification("Failed to read spectral data from uploaded files.", type = "error")
              return()
            }

            # Identify the most common wavenumber layout
            col_signatures <- sapply(spectra_list, function(dt) paste(ncol(dt), collapse = "_"))
            layout_counts <- table(col_signatures)
            majority_layout <- names(which.max(layout_counts))
            keep_idx <- which(col_signatures == majority_layout)

            if (length(keep_idx) < length(spectra_list)) {
              n_skipped <- length(spectra_list) - length(keep_idx)
              showNotification(
                paste0(
                  n_skipped, " files skipped (different wavenumber layout from majority). ",
                  "Keeping ", length(keep_idx), " files."
                ),
                type = "warning", duration = 8
              )
              spectra_list <- spectra_list[keep_idx]
            }

            uploaded_spectra <- rbindlist(spectra_list)

            if (nrow(uploaded_spectra) == 0) {
              showNotification("Failed to read spectral data from uploaded files.", type = "error")
              return()
            }

            uploaded_spectra[, SSN := V1]
            uploaded_spectra[, V1 := NULL]
            spectral_cols <- setdiff(names(uploaded_spectra), "SSN")
            uploaded_spectra[, (spectral_cols) := lapply(.SD, as.numeric), .SDcols = spectral_cols]

            # Sort spectral columns by descending wavenumber (opus native order, required for correct SG derivative sign)
            spectral_cols <- spectral_cols[order(as.numeric(spectral_cols), decreasing = TRUE)]
            setcolorder(uploaded_spectra, c("SSN", spectral_cols))

            uploaded_spectral_df <- uploaded_spectra[, lapply(.SD, mean), by = SSN, .SDcols = spectral_cols]

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

          showNotification(paste("Successfully processed", length(unique(uploaded_spectra$SSN)), "spectral files!"), type = "message")

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
          results <- process_spectra_predict(
            spectra_mir = uploaded_spectra_df(),
            selected_vars = input$selected_properties
          )
          predictions <- results$predictions

          # Store resampled data for downloads
          uploaded_spectra_resampled_df(results$raw_resampled)
          uploaded_spectra_resampled_df_sg(results$deriv_resampled)

          cat("Prediction completed. Output dimensions:", nrow(predictions), "x", ncol(predictions), "\n")

          # Dynamic column renaming based on selected properties
          all_units <- c(
            "SOC" = "g_kg", "TN" = "g_kg", "pH" = "", "CEC" = "cmolc_kg",
            "clay" = "%", "silt" = "%", "sand" = "%", "ExCa" = "cmolc_kg", "ExMg" = "cmolc_kg", "ExK" = "cmolc_kg"
          )

          all_display_names <- c(
            "SOC" = "SOC", "TN" = "TN", "pH" = "pH", "CEC" = "CEC",
            "clay" = "Clay", "silt" = "Silt", "sand" = "Sand", "ExCa" = "ExCa", "ExMg" = "ExMg", "ExK" = "ExK"
          )

          new_colnames <- c("SSN")
          for (var in input$selected_properties) {
            if (var %in% names(all_units)) {
              unit <- all_units[var]
              display_name <- all_display_names[var]
              col_name <- if (unit != "") paste0(display_name, "_", unit) else display_name
              new_colnames <- c(new_colnames, col_name)
            }
          }

          # Handle cases where some models might have failed
          actual_vars <- setdiff(colnames(predictions), "SSN")
          final_colnames <- c("SSN")
          for (var in actual_vars) {
            if (var %in% names(all_units)) {
              unit <- all_units[var]
              display_name <- all_display_names[var]
              col_name <- if (unit != "") paste0(display_name, "_", unit) else display_name
              final_colnames <- c(final_colnames, col_name)
            } else {
              final_colnames <- c(final_colnames, var)
            }
          }

          colnames(predictions) <- final_colnames

          # Final sanitization: remove () and replace / with _
          colnames(predictions) <- gsub("[()]", "", colnames(predictions))
          colnames(predictions) <- gsub("/", "_", colnames(predictions))

          # Store results and update state
          prediction_results(predictions)
          prediction_loading(FALSE)
          shinyjs::enable("predict_btn")
          shinyjs::html("predict_btn", paste(icon("flask"), " Predict Soil Properties"))
          showNotification(
            paste("✅ Prediction completed successfully for", nrow(predictions), "samples!"),
            type = "message",
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
    # Soil property density plots
    output$prediction_plot <- renderPlotly({
      req(prediction_results())
      predictions <- prediction_results()

      # Define soil properties to plot (excluding SSN)
      soil_properties <- colnames(predictions)[-1]
      if (length(soil_properties) == 0) {
        return(NULL)
      }

      # Reshape to long format for ggplot
      plot_data <- melt.data.table(as.data.table(predictions),
        id.vars = "SSN",
        variable.name = "Property", value.name = "Value"
      )
      plot_data[, Value := as.numeric(Value)]
      plot_data <- plot_data[!is.na(Value)]

      if (nrow(plot_data) == 0) {
        return(NULL)
      }

      # Desired display order
      display_order <- c(
        "SOC_g_kg", "TN_g_kg", "pH", "CEC_cmolc_kg",
        "ExCa_cmolc_kg", "ExMg_cmolc_kg", "ExK_cmolc_kg",
        "Clay_%", "Sand_%", "Silt_%"
      )
      # Keep only properties present in data, in the desired order
      ordered_props <- intersect(display_order, soil_properties)
      # Append any unexpected properties at the end
      ordered_props <- c(ordered_props, setdiff(soil_properties, ordered_props))
      plot_data[, Property := factor(Property, levels = ordered_props)]

      # Calculate stats for all properties
      stats_df <- plot_data[, .(
        Mean = mean(Value, na.rm = TRUE),
        Median = median(Value, na.rm = TRUE)
      ), by = Property]

      # Force x-axis to 0-100 for texture properties (Sand, Clay, Silt)
      texture_props <- grep("Sand|Clay|Silt", unique(as.character(plot_data$Property)), value = TRUE, ignore.case = TRUE)
      if (length(texture_props) > 0) {
        texture_limits <- data.frame(
          Property = factor(rep(texture_props, each = 2), levels = levels(plot_data$Property)),
          Value = rep(c(0, 100), times = length(texture_props))
        )
      } else {
        texture_limits <- NULL
      }

      # Create ggplot with facets
      p <- ggplot(plot_data, aes(x = Value)) +
        geom_density(fill = "steelblue", alpha = 0.3, color = "steelblue")

      # Add invisible points to set x-axis range for texture properties
      if (!is.null(texture_limits)) {
        p <- p + geom_blank(data = texture_limits, aes(x = Value))
      }

      # Prepare stats for mapping to get a legend
      stats_long <- melt(stats_df,
        id.vars = "Property",
        variable.name = "Statistic", value.name = "StatValue"
      )

      p <- p +
        geom_vline(
          data = stats_long, aes(xintercept = StatValue, color = Statistic, linetype = Statistic),
          size = 0.8
        ) +
        scale_color_manual(values = c("Mean" = "red", "Median" = "#00ff88")) +
        scale_linetype_manual(values = c("Mean" = "dashed", "Median" = "dotted")) +
        facet_wrap(~Property, scales = "free", ncol = 5) +
        theme_minimal(base_size = 16) +
        theme(
          text = element_text(color = "black"),
          axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(color = "black", size = 14),
          axis.title = element_text(color = "black", size = 16),
          panel.grid = element_line(color = "#e0e0e0"),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(color = "black", face = "bold", size = 16),
          strip.background = element_rect(fill = "#f0f0f0", color = NA),
          legend.position = "bottom",
          legend.text = element_text(color = "black", size = 14),
          legend.title = element_blank()
        ) +
        labs(x = NULL, y = "Density")

      # Add highlighting if a row is selected
      vals <- selected_row_values()
      if (!is.null(vals)) {
        sel_df <- data.frame(
          Property = factor(soil_properties, levels = ordered_props),
          Value = suppressWarnings(as.numeric(sapply(soil_properties, function(x) vals[[x]])))
        )
        sel_df <- sel_df[!is.na(sel_df$Value), ]

        if (nrow(sel_df) > 0) {
          p <- p + geom_vline(
            data = sel_df, aes(xintercept = Value),
            color = "orange", size = 1.2, linetype = "solid"
          )
        }
      }

      # Convert to plotly
      ggplotly(p) |>
        layout(
          paper_bgcolor = "white",
          plot_bgcolor = "white",
          margin = list(l = 50, r = 20, b = 100, t = 50),
          legend = list(
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = -0.2,
            font = list(color = "black", size = 14)
          ),
          showlegend = TRUE
        ) |>
        config(displayModeBar = FALSE)
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

    # Warning for -99 values
    output$warning_negative_predictions <- renderUI({
      req(prediction_results())
      predictions <- prediction_results()
      pred_cols <- setdiff(colnames(predictions), "SSN")
      has_neg99 <- any(predictions[, pred_cols] == -99, na.rm = TRUE)
      if (has_neg99) {
        card_footer(
          class = "bg-danger text-white",
          icon("exclamation-triangle"),
          " Some spectra may be noisy or have errors. Where predicted values are very uncertain they have been set to -99."
        )
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

    # Reset session without downloading
    observeEvent(input$reset_session, {
      uploaded_spectra_df(NULL)
      uploaded_spectra_df_sg(NULL)
      uploaded_spectra_resampled_df(NULL)
      uploaded_spectra_resampled_df_sg(NULL)
      spectra_count(0)
      prediction_results(NULL)
      spectra_processed(FALSE)
      prediction_loading(FALSE)
      selected_row_values(NULL)
      shinyjs::reset("spectra_file")
      showNotification("Session reset. Ready for new upload.", type = "message")
    })

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
            columns = setdiff(colnames(prediction_results()), "SSN"),
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

        # Update reactive values
        selected_row_values(selected_values)
        selected_ssn(selected_values$SSN)
      } else {
        selected_row_values(NULL)
        selected_ssn(NULL)
      }
    })

    # Texture triangle highlight proxy update
    observeEvent(selected_row_values(),
      {
        req(prediction_results())
        vals <- selected_row_values()

        predictions <- prediction_results()
        clay_col <- grep("Clay", colnames(predictions), value = TRUE, ignore.case = TRUE)
        sand_col <- grep("Sand", colnames(predictions), value = TRUE, ignore.case = TRUE)
        silt_col <- grep("Silt", colnames(predictions), value = TRUE, ignore.case = TRUE)

        highlight_a <- list(numeric(0))
        highlight_b <- list(numeric(0))
        highlight_c <- list(numeric(0))
        highlight_text <- list("")

        if (!is.null(vals) && length(sand_col) > 0 && length(clay_col) > 0) {
          clay_val <- as.numeric(vals[[clay_col[1]]])
          sand_val <- as.numeric(vals[[sand_col[1]]])
          silt_val <- if (length(silt_col) > 0) as.numeric(vals[[silt_col[1]]]) else (100 - clay_val - sand_val)

          if (!is.na(clay_val) && !is.na(sand_val) && !is.na(silt_val)) {
            highlight_a <- list(list(sand_val))
            highlight_b <- list(list(clay_val))
            highlight_c <- list(list(silt_val))
            highlight_text <- list(list(paste0("<b>SELECTED: ", vals$SSN, "</b>")))
          }
        }

        plotlyProxy("texture_triangle_plot", session) |>
          plotlyProxyInvoke("restyle", list(
            a = highlight_a,
            b = highlight_b,
            c = highlight_c,
            text = highlight_text
          ), 14L)
      },
      ignoreNULL = FALSE
    )

    # Texture triangle plot (Plotly ternary)
    output$texture_triangle_plot <- renderPlotly({
      req(prediction_results())

      predictions <- prediction_results()

      clay_col <- grep("Clay", colnames(predictions), value = TRUE, ignore.case = TRUE)
      silt_col <- grep("Silt", colnames(predictions), value = TRUE, ignore.case = TRUE)
      sand_col <- grep("Sand", colnames(predictions), value = TRUE, ignore.case = TRUE)

      if (length(clay_col) == 0 || length(sand_col) == 0) {
        return(plot_ly() |>
          layout(
            title = list(text = "Texture triangle requires at least Clay and Sand predictions", font = list(color = "white", size = 12)),
            paper_bgcolor = "#2d2d2d",
            plot_bgcolor = "#2d2d2d"
          ))
      }

      texture_data <- data.frame(
        SSN = predictions$SSN,
        Clay = predictions[[clay_col]],
        Sand = predictions[[sand_col]]
      )

      if (length(silt_col) > 0) {
        texture_data$Silt <- predictions[[silt_col]]
      } else {
        texture_data$Silt <- 100 - texture_data$Clay - texture_data$Sand
      }

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
      p <- plot_ly(source = session$ns("texture_triangle_plot"))

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

      # Add actual data points
      p <- p |> add_trace(
        data = texture_data,
        type = "scatterternary",
        mode = "markers",
        a = ~Sand,
        b = ~Clay,
        c = ~Silt,
        text = ~ paste0(
          "<b>SSN: ", SSN, "</b><br>",
          "Sand: ", round(Sand, 1), "%<br>",
          "Clay: ", round(Clay, 1), "%<br>",
          "Silt: ", round(Silt, 1), "%"
        ),
        customdata = ~SSN,
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

      # Add EMPTY selection trace (Trace Index 14)
      p <- p |> add_trace(
        type = "scatterternary",
        mode = "markers",
        a = numeric(0), b = numeric(0), c = numeric(0),
        text = "",
        hoverinfo = "text",
        marker = list(
          size = 14, color = "orange", opacity = 1,
          line = list(color = "white", width = 2)
        ),
        showlegend = FALSE,
        name = "SelectedTrace"
      )

      # Configure layout
      p <- p |> layout(
        ternary = list(
          sum = 100,
          aaxis = list(
            title = list(text = "Sand %", font = list(color = "white", size = 16)),
            tickfont = list(color = "white", size = 14),
            gridcolor = "rgba(128, 128, 128, 0.3)",
            linecolor = "white"
          ),
          baxis = list(
            title = list(text = "Clay %", font = list(color = "white", size = 16)),
            tickfont = list(color = "white", size = 14),
            gridcolor = "rgba(128, 128, 128, 0.3)",
            linecolor = "white"
          ),
          caxis = list(
            title = list(text = "Silt %", font = list(color = "white", size = 16)),
            tickfont = list(color = "white", size = 14),
            gridcolor = "rgba(128, 128, 128, 0.3)",
            linecolor = "white"
          ),
          bgcolor = "#1a1a1a"
        ),
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        hoverlabel = list(
          bgcolor = "#1a1a1a",
          font = list(color = "white", size = 14, family = "Poppins"),
          bordercolor = "#00ff88"
        ),
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )

      p |>
        config(displayModeBar = FALSE) |>
        event_register("plotly_click")
    })

    # Texture triangle highlight proxy update - removed in favor of reactive rendering
    # for better reliability with ternary plots


    # PCA model
    pca_model <- reactive({
      # Note: Filename was fixed from "mir_pca_model " to "mir_pca_model.rds"
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
      p <- plot_ly(pca_df,
        x = as.formula(paste0("~", x_var)),
        y = as.formula(paste0("~", y_var)),
        source = if (!is.null(source_id)) session$ns(source_id) else NULL
      ) |>
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
            titlefont = list(size = 18),
            tickfont = list(size = 16)
          ),
          yaxis = list(
            title = y_var,
            gridcolor = "#333333",
            zerolinecolor = "#444444",
            color = "white",
            titlefont = list(size = 18),
            tickfont = list(size = 16)
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

    # --- CROSS-PLOT LINKAGE ---

    # helper for processing clicks
    process_plot_click <- function(click_data) {
      if (!is.null(click_data) && "customdata" %in% names(click_data)) {
        ssn_val <- click_data$customdata
        clicked_ssn <- if (is.list(ssn_val)) ssn_val[[1]] else ssn_val

        if (!is.null(clicked_ssn) && !is.na(clicked_ssn)) {
          selected_ssn(clicked_ssn)

          # Sync table selection
          if (!is.null(prediction_results())) {
            row_idx <- which(prediction_results()$SSN == clicked_ssn)
            if (length(row_idx) > 0) {
              DT::selectRows(DT::dataTableProxy("prediction_table"), row_idx)
              # Update values for results-specific plots
              selected_row_values(prediction_results()[row_idx[1], ])
            }
          }
        }
      }
    }

    # Individual observers for each click source
    observeEvent(event_data("plotly_click", source = session$ns("pca_plot_12")), {
      process_plot_click(event_data("plotly_click", source = session$ns("pca_plot_12")))
    })
    observeEvent(event_data("plotly_click", source = session$ns("pca_plot_13")), {
      process_plot_click(event_data("plotly_click", source = session$ns("pca_plot_13")))
    })
    observeEvent(event_data("plotly_click", source = session$ns("pca_plot_23")), {
      process_plot_click(event_data("plotly_click", source = session$ns("pca_plot_23")))
    })
    observeEvent(event_data("plotly_click", source = session$ns("spectral_plot_combined")), {
      process_plot_click(event_data("plotly_click", source = session$ns("spectral_plot_combined")))
    })
    observeEvent(event_data("plotly_click", source = session$ns("texture_triangle_plot")), {
      process_plot_click(event_data("plotly_click", source = session$ns("texture_triangle_plot")))
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
