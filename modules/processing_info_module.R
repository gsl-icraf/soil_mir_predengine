#' Spectral Processing Pipeline Module - UI
#' @param id Module ID
processing_info_ui <- function(id) {
    ns <- NS(id)

    tagList(
        tags$head(
            tags$style(HTML("
        .step-card {
          height: 100%;
          border-radius: 12px;
          border: 1px solid rgba(255, 255, 255, 0.15);
          transition: transform 0.2s ease-in-out;
          background-color: #2d2d2d;
        }
        .step-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 15px rgba(0,0,0,0.3);
        }
        .step-header {
          background-color: #1a1a2e !important;
          color: white !important;
          font-weight: 700;
          padding: 1rem 1.25rem;
          font-size: 1.15rem;
          border-radius: 12px 12px 0 0 !important;
        }
        .step-number {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 32px;
          height: 32px;
          border-radius: 50%;
          background-color: #007bff;
          color: white;
          font-weight: 700;
          font-size: 0.95rem;
          margin-right: 10px;
          flex-shrink: 0;
        }
        .step-body {
          font-size: 1.05rem;
          color: #e0e0e0;
          line-height: 1.7;
        }
        .step-body strong {
          color: #ffffff;
        }
        .step-body em {
          color: #a0cfff;
        }
        .step-body li {
          margin-bottom: 6px;
        }
        .tech-badge {
          font-size: 0.85rem;
          background-color: rgba(0, 123, 255, 0.15);
          color: #a0cfff;
          padding: 4px 12px;
          border-radius: 50px;
          display: inline-block;
          border: 1px solid rgba(0, 123, 255, 0.3);
          font-weight: 600;
          margin: 3px;
        }
        .pipeline-arrow {
          text-align: center;
          font-size: 1.5rem;
          color: #007bff;
          padding: 10px 0;
        }
      "))
        ),

        # Hero Section
        div(
            class = "py-5 text-center rounded-3 mb-5",
            style = "background-color: #1a1a2e; border: 1px solid rgba(255,255,255,0.1);",
            h1(class = "display-5 fw-bold", style = "color: white;", "Spectral Processing Pipeline"),
            p(class = "lead mb-0", style = "color: #b0b0b0; font-size: 1.2rem;",
              "How uploaded MIR spectra are processed and transformed into soil property predictions.")
        ),

        # Pipeline overview
        div(
            class = "mb-4 p-4 rounded-3",
            style = "background-color: #2d2d2d; border: 1px solid rgba(255,255,255,0.1); color: #e0e0e0; font-size: 1.1rem;",
            h5(class = "fw-bold mb-3", style = "color: white;", "Pipeline Overview"),
            p("Uploaded Opus binary files pass through a series of spectral processing steps before being fed into deep neural network models. ",
              "Each step is designed to harmonize the input spectra with the reference spectral library used during model training.")
        ),

        # Step cards
        layout_columns(
            col_widths = c(6, 6),

            # Step 1: Upload & Parse
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "1"), "Upload & Parse Opus Files")
                ),
                card_body(
                    class = "step-body",
                    p("Spectral data is uploaded as a ZIP archive containing Bruker Opus binary files (*.0). ",
                      "Each file is parsed using the opusreader package to extract the sample identifier (SSN) and the raw absorbance spectrum."),
                    div(
                        span(class = "tech-badge", "opusreader"),
                        span(class = "tech-badge", "Opus binary format")
                    )
                )
            ),

            # Step 2: Duplicate Averaging
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "2"), "Duplicate Averaging")
                ),
                card_body(
                    class = "step-body",
                    p("If multiple scans exist for the same sample (same SSN), their absorbance values are averaged band-by-band. ",
                      "This reduces measurement noise and produces a single representative spectrum per sample."),
                    div(
                        span(class = "tech-badge", "Mean aggregation by SSN")
                    )
                )
            ),

            # Step 3: Band Selection
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "3"), "Band Selection")
                ),
                card_body(
                    class = "step-body",
                    p("The raw spectrum is trimmed to the mid-infrared fingerprint region ",
                      "between 601 and 4001 cm", tags$sup("-1"), ". ",
                      "Bands outside this range are discarded as they fall outside the region used for model training."),
                    div(
                        span(class = "tech-badge", "601 \u2013 4001 cm\u207B\u00B9")
                    )
                )
            ),

            # Step 4: Resampling
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "4"), "Resampling to Reference Grid")
                ),
                card_body(
                    class = "step-body",
                    p("Different instruments record spectra at slightly different wavenumber positions. ",
                      "The spectra are resampled onto a fixed reference grid of 1764 wavebands using natural cubic spline interpolation, ",
                      "ensuring consistency with the training data."),
                    div(
                        span(class = "tech-badge", "prospectr::resample"),
                        span(class = "tech-badge", "Natural spline"),
                        span(class = "tech-badge", "1764 bands")
                    )
                )
            ),

            # Step 5: Bias Correction
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "5"), "Instrument Bias Correction")
                ),
                card_body(
                    class = "step-body",
                    p("A pre-computed bias correction model is applied to align Alpha instrument spectra with the reference MIR library. ",
                      "The correction subtracts a per-waveband bias vector estimated from paired calibration samples measured on both instruments."),
                    p(tags$em("Corrected = Raw \u2212 Bias")),
                    div(
                        span(class = "tech-badge", "Alpha \u2192 MIR"),
                        span(class = "tech-badge", "1700 matched bands")
                    )
                )
            ),

            # Step 6: Savitzky-Golay Derivative
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "6"), "Savitzky-Golay First Derivative")
                ),
                card_body(
                    class = "step-body",
                    p("A first-order Savitzky-Golay derivative is applied to the resampled spectra. ",
                      "This removes baseline shifts and enhances spectral features related to soil composition, ",
                      "matching the preprocessing used during model training."),
                    div(
                        span(class = "tech-badge", "Polynomial order: 2"),
                        span(class = "tech-badge", "Window: 11 bands"),
                        span(class = "tech-badge", "Derivative: 1st")
                    )
                )
            ),

            # Step 7: DNN Prediction
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "7"), "Deep Neural Network Prediction")
                ),
                card_body(
                    class = "step-body",
                    p("The preprocessed derivatives are fed into property-specific deep neural networks implemented in PyTorch (via the torch R package). ",
                      "Each model has a 5-layer architecture (512 \u2192 256 \u2192 128 \u2192 64 \u2192 32 neurons) with ReLU activation, ",
                      "batch normalization, and dropout regularization."),
                    p("Input features are z-score normalized using training set statistics, and predictions are back-transformed ",
                      "to their original scale (including log/sqrt back-transforms where applicable)."),
                    div(
                        span(class = "tech-badge", "torch"),
                        span(class = "tech-badge", "Separate model per property"),
                        span(class = "tech-badge", "Z-score normalization")
                    )
                )
            ),

            # Step 8: Post-processing
            card(
                class = "step-card mb-3",
                card_header(
                    class = "step-header",
                    div(span(class = "step-number", "8"), "Post-Processing & Quality Control")
                ),
                card_body(
                    class = "step-body",
                    p("Several post-processing steps ensure prediction quality:"),
                    tags$ul(
                        tags$li(tags$strong("Negative values"), " are flagged as uncertain (\u221299)."),
                        tags$li(tags$strong("TN/SOC ratio check"), " \u2014 if the median TN:SOC ratio exceeds 0.5, TN is rescaled."),
                        tags$li(tags$strong("Texture closure"), " \u2014 Clay and Sand are clipped to [0, 100] and scaled so their sum does not exceed 100%. ",
                                "Silt is derived as 100 \u2212 (Clay + Sand).")
                    ),
                    div(
                        span(class = "tech-badge", "Sanity checks"),
                        span(class = "tech-badge", "Compositional closure")
                    )
                )
            )
        )
    )
}

#' Spectral Processing Pipeline Module - Server
#' @param id Module ID
processing_info_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Static documentation module
    })
}
