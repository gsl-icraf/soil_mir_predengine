# Home Module UI
home_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Hero banner with glassmorphism effect
    div(
      class = "hero-banner",
      # Background image as HTML element
      img(
        src = "images/petri.jpg",
        class = "hero-bg-image",
        alt = "Petri Dish Laboratory"
      ),
      # Glass overlay for backdrop filter
      div(class = "glass-overlay"),
      # Spectral particles effect
      div(
        class = "spectral-particles",
        id = ns("particles")
      ),
      # Main glass card content
      div(
        class = "glass-card",
        h1(
          "Welcome!",
          class = "banner-title"
        ),
        p(
          "to the CIFOR-ICRAF Spectral Prediction Engine",
          class = "banner-subtitle"
        ),
        a(
          "Get Started!",
          href = "?page=prediction",
          class = "banner-cta nav-link",
          `data-page` = "prediction"
        )
      )
    ),

    # Row of cards with key metrics/info - enhanced with spectral theme
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        class = "spectral-card",
        card_header(
          icon("microscope"), " Spectral Analysis",
          class = "bg-gradient text-white"
        ),
        card_image(src = "images/soil_sample_load.jpg", alt = "Loading soil samples"),
        card_body(
          p("This is a web-based application for predicting soil properties from mid-infrared (MIR) spectra. Soil spectroscopy is a powerful tool for predicting soil properties from mid-infrared (MIR) spectra. This application uses MIR spectra to predict soil properties such as soil organic carbon (SOC), total nitrogen (TN), pH, and more.")
        )
      ),
      card(
        class = "spectral-card",
        card_header(
          icon("chart-line"), " Predictive Modeling",
          class = "bg-gradient text-white"
        ),
        card_image(src = "images/soil_in_hand.jpg", alt = "Holding soil sample"),
        card_body(
          p("Our predictive models are trained using soil samples collected in the field from different parts of the world, covering a wide range of ecosystems from agricultural to natural ecosystems, and different land use- and cover types.")
        )
      ),
      card(
        class = "spectral-card",
        card_header(
          icon("check-circle"), " Rapid, non-destructive and reproducible!",
          class = "bg-gradient text-white"
        ),
        card_image(src = "images/spectra_3d.jpg", alt = "Soil spectral analysis is rapid and cost-effective"),
        card_body(
          p("One of the benefits of MIR spectroscopy is that it is a non-destructive method, meaning that the same soil sample can be used for multiple analyses. This makes it possible to build large and diverse datasets for model training. It is also a fast and cost-effective method."),
          div(
            class = "d-flex align-items-center mb-2",
            span(class = "badge bg-success me-2", "Online"),
            span("All systems operational")
          ),
          div(
            class = "small text-muted",
            "Last updated: ", format(Sys.time(), "%H:%M:%S")
          )
        )
      )
    ),

    # Additional content area with enhanced spectral theme
    div(
      class = "mt-4",
      card(
        class = "spectral-card",
        card_header(
          icon("rocket"), " Getting Started",
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
                    icon("home")
                  )
                ),
                div(
                  h6("Dashboard Overview", class = "mb-1"),
                  p("This dashboard is a tool for predicting soil properties from mid-infrared (MIR) spectra using machine learning models. The Prediction Engine was developed to facilitate rapid predictions of a range of soil properties based on state-of-the-art spectroscopy and machine learning techniques, and accurate prediction models.")
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
                    icon("chart-line")
                  )
                ),
                div(
                  h6("Prediction Engine", class = "mb-1"),
                  p("The Prediction Engine module allows users to upload their own MIR spectral data (in CSV format) and obtain rapid predictions of various soil properties. Users can visualize the predicted results through interactive plots and download the predicted outputs for further analysis.")
                )
              )
            )
          ),
          hr(),
          div(
            class = "text-center",
            p(
              "Developed by CIFOR-ICRAF | © 2025 All rights reserved."
            ),
            img(
              src = "images/spacial_logo.png",
              alt = "CIFOR-ICRAF Logo",
              style = "height: 40px;"
            )
          )
        )
      )
    )
  )
}

# Home Module Server
home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic for home module
    # Add any reactive logic, observers, or outputs here

    # Example: You could add reactive values or observers
    # observe({
    #   # Module-specific server logic
    # })
  })
}
