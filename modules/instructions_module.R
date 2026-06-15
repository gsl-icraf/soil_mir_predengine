#' Instructions Module - UI
#' @param id Module ID
instructions_ui <- function(id) {
    ns <- NS(id)

    tagList(
        tags$head(
            tags$style(HTML("
        .instruction-card {
          margin-bottom: 25px;
          border-radius: var(--card-border-radius);
          border: 1px solid var(--glass-border);
          background-color: #FFFFFF;
          transition: transform 0.2s ease-in-out, box-shadow 0.2s ease-in-out;
        }
        .instruction-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 20px rgba(50, 27, 15, 0.1);
        }
        .instruction-icon {
          font-size: 2.5rem;
          margin-bottom: 15px;
          color: var(--secondary-color);
        }
        .step-number {
          background-color: var(--primary-color);
          color: white;
          width: 30px;
          height: 30px;
          border-radius: 50%;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          margin-right: 10px;
          font-weight: bold;
        }
      "))
        ),

        # Hero Section
        div(
            class = "py-5 text-center rounded-3 mb-5",
            style = "background: linear-gradient(135deg, #FAF6F0 0%, #EFE5DA 100%); border: 1px solid rgba(74, 46, 27, 0.15);",
            h1(class = "display-5 fw-bold", style = "color: var(--primary-color);", "User Guide"),
            p(class = "lead mb-4", style = "color: var(--dark-color);", "Mastering the CIFOR-ICRAF Spectral Prediction Engine"),
            div(
                class = "col-lg-6 mx-auto",
                p(class = "text-muted", "Follow these simple steps to process your MIR spectra and generate soil property predictions.")
            )
        ),

        # Main Content
        layout_columns(
            col_widths = c(6, 6, 6, 6),

            # Step 1: Data Preparation
            card(
                class = "instruction-card",
                card_header(class = "bg-gradient text-white", "1. Data Preparation"),
                card_body(
                    div(class = "instruction-icon text-center", icon("file-zipper")),
                    p("Ensure your MIR spectral data is in the correct format before uploading:"),
                    tags$ul(
                        tags$li("Files must be in ", tags$strong("Bruker Opus binary format"), " (*.0 extension)."),
                        tags$li("All files should be compressed into a single ", tags$strong(".ZIP"), " archive."),
                        tags$li("Sample IDs are automatically extracted from the file metadata.")
                    )
                )
            ),

            # Step 2: Uploading & Unzipping
            card(
                class = "instruction-card",
                card_header(class = "bg-gradient text-white", "2. Uploading & Unzipping"),
                card_body(
                    div(class = "instruction-icon text-center", icon("upload")),
                    p("Navigate to the ", tags$strong("Prediction Engine"), " page:"),
                    tags$ul(
                        tags$li("Click ", tags$strong("Browse..."), " to select your ZIP file."),
                        tags$li("Click the ", tags$strong("Unzip and Process Spectra"), " button."),
                        tags$li("The system will unzip the files and display the raw spectral data.")
                    )
                )
            ),

            # Step 3: Generating Predictions
            card(
                class = "instruction-card",
                card_header(class = "bg-gradient text-white", "3. Generating Predictions"),
                card_body(
                    div(class = "instruction-icon text-center", icon("magic")),
                    p("Once spectra are processed, start the AI models:"),
                    tags$ul(
                        tags$li("The ", tags$strong("Predict Soil Variables"), " button will become active."),
                        tags$li("Click it to run the Random Forest models for all 9 soil variables."),
                        tags$li("Progress will be shown in the notification bar.")
                    )
                )
            ),

            # Step 4: Analysis & Export
            card(
                class = "instruction-card",
                card_header(class = "bg-gradient text-white", "4. Analysis & Export"),
                card_body(
                    div(class = "instruction-icon text-center", icon("download")),
                    p("Review your results and download the data:"),
                    tags$ul(
                        tags$li("View the results in the ", tags$strong("Prediction Results"), " tab."),
                        tags$li("Analyze the data density using the ", tags$strong("PCA Diagnostic"), " plots."),
                        tags$li("Download your predictions in ", tags$strong("CSV, Excel, or Copy"), " formats."),
                        tags$li("Download ", tags$strong("Raw Spectra"), " and ", tags$strong("1st Derivatives"), " CSV files below the preview plot.")
                    )
                )
            )
        ),

        # Pro Tip Section
        div(
            class = "alert mt-4",
            role = "alert",
            style = "background-color: #FAF6F0; border: 2px solid var(--secondary-color); color: var(--dark-color); border-radius: var(--border-radius);",
            h4(class = "alert-heading", style = "color: var(--primary-color); font-weight: 700;", list(icon("lightbulb"), " Pro Tip!")),
            p("You can compare your uploaded samples against the model's reference data in the PCA space. If your samples (orange dots) fall far outside the reference density (contour map), take extra caution when interpreting those specific results as they may represent unique soil types not fully covered by the current models.")
        )
    )
}

#' Instructions Module - Server
#' @param id Module ID
instructions_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # No server-side logic needed for static instructions currently
    })
}
