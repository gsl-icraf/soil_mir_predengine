#' Soil Properties Module - UI
#' @param id Module ID
property_info_ui <- function(id) {
    ns <- NS(id)

    tagList(
        tags$head(
            tags$style(HTML("
        .property-card {
          height: 100%;
          border-radius: 12px;
          border: 1px solid rgba(0, 170, 255, 0.12);
          background: linear-gradient(135deg, rgba(8,14,45,0.97) 0%, rgba(10,22,58,0.97) 100%);
          transition: transform 0.2s ease-in-out, border-color 0.2s ease;
          display: flex;
          flex-direction: column;
        }
        .property-card:hover {
          transform: translateY(-5px);
          border-color: rgba(0, 229, 204, 0.22);
          box-shadow: 0 8px 24px rgba(0, 60, 180, 0.22);
        }
        .card-header-clean {
          background: linear-gradient(135deg, rgba(6,10,32,0.98) 0%, rgba(8,18,55,0.98) 50%, rgba(8,28,48,0.98) 100%) !important;
          border-bottom: 1px solid rgba(0, 229, 204, 0.12) !important;
          color: white !important;
          font-weight: 700;
          padding: 1.25rem;
          font-size: 1.1rem;
          border-radius: 12px 12px 0 0 !important;
        }
        .unit-badge {
          font-size: 0.8rem;
          background-color: rgba(0, 170, 255, 0.12);
          color: rgba(140, 210, 255, 0.90);
          padding: 4px 12px;
          border-radius: 50px;
          margin-bottom: 15px;
          display: inline-block;
          border: 1px solid rgba(0, 170, 255, 0.20);
          font-weight: 600;
          letter-spacing: 0.5px;
        }
        .property-desc {
          font-size: 0.95rem;
          color: rgba(210, 225, 245, 0.90);
          background-color: rgba(0, 30, 80, 0.40);
          padding: 15px;
          border-radius: 8px;
          line-height: 1.5;
          margin-bottom: 20px;
          flex-grow: 1;
        }
        .range-section {
          background-color: rgba(0, 40, 100, 0.30);
          border-radius: 8px;
          padding: 12px;
          border: 1px solid rgba(0, 170, 255, 0.10);
        }
        .range-label {
          font-weight: 700;
          font-size: 0.85rem;
          color: rgba(140, 200, 255, 0.80);
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin-bottom: 5px;
        }
        .range-value {
          font-size: 0.9rem;
          color: rgba(140, 170, 210, 0.70);
          font-style: italic;
        }
      "))
        ),

        # Hero Section
        div(
            class = "py-5 text-center rounded-3 mb-5",
            style = "background: linear-gradient(135deg, rgba(8,14,45,0.97) 0%, rgba(10,28,55,0.97) 100%); border: 1px solid rgba(0,170,255,0.12);",
            h1(class = "display-5 fw-bold", style = "color: #00e5cc;", "Soil Properties Reference"),
            p(class = "lead mb-0", style = "color: rgba(140,170,210,0.70);", "A comprehensive guide to the soil indicators predicted by the specPred engine.")
        ),

        # Main Content Grid
        layout_columns(
            col_widths = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4),

            # SOC
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Soil Organic Carbon (SOC)"),
                card_body(
                    span(class = "unit-badge", "g/kg"),
                    div(class = "property-desc", "SOC is the carbon component of soil organic matter. It is a critical indicator of soil health, influencing nutrient cycling, water retention, and soil structure."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Target Range"),
                        div(class = "range-value", "Ideally > 20 g/kg for sustainable productivity; variable by soil type.")
                    )
                )
            ),

            # TN
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Total Nitrogen (TN)"),
                card_body(
                    span(class = "unit-badge", "g/kg"),
                    div(class = "property-desc", "TN represents the sum of all nitrogen forms in the soil. It is a primary macronutrient essential for plant growth and protein synthesis."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Common Range"),
                        div(class = "range-value", "Typically 0.5 - 4.0 g/kg. Balanced by C:N ratios.")
                    )
                )
            ),

            # pH
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Soil pH (Water)"),
                card_body(
                    span(class = "unit-badge", "-log [H+]"),
                    div(class = "property-desc", "pH measures the acidity or alkalinity of the soil. It controls nutrient availability and microbial activity."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Optimum Range"),
                        div(class = "range-value", "6.0 - 7.0 for most crops; many tropical soils are 4.5 - 5.5.")
                    )
                )
            ),

            # CEC
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "CEC (cmolc/kg)"),
                card_body(
                    span(class = "unit-badge", "cmol(+)/kg"),
                    div(class = "property-desc", "Cation Exchange Capacity measures the soil's ability to hold and release essential nutrients like calcium, magnesium, and potassium."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Interpretation"),
                        div(class = "range-value", "> 15 is good; < 5 indicates low nutrient holding capacity.")
                    )
                )
            ),

            # Clay
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Clay Content"),
                card_body(
                    span(class = "unit-badge", "%"),
                    div(class = "property-desc", "The proportion of fine mineral particles (< 0.002 mm). Clay determines water holding capacity and nutrient storage."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Typical Range"),
                        div(class = "range-value", "Varies from Sandy (<10%) to Clayey (>40%) soils.")
                    )
                )
            ),

            # Silt
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Silt Content"),
                card_body(
                    span(class = "unit-badge", "%"),
                    div(class = "property-desc", "Mineral particles between 0.002 and 0.05 mm. Silt contributes to soil fertility and moisture retention."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Loamy Textures"),
                        div(class = "range-value", "Typically 15-45% depending on parent material.")
                    )
                )
            ),

            # Sand
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Sand Content"),
                card_body(
                    span(class = "unit-badge", "%"),
                    div(class = "property-desc", "Large mineral particles (0.05 to 2.0 mm). Sand provides aeration and drainage but has low nutrient-holding capacity."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Typical Range"),
                        div(class = "range-value", "Sandy soils > 70%; Clayey soils < 20%.")
                    )
                )
            ),

            # ExCa
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Exch. Calcium (ExCa)"),
                card_body(
                    span(class = "unit-badge", "cmol(+)/kg"),
                    div(class = "property-desc", "Exchangeable calcium is vital for soil structure and plant cell wall development. It is often the dominant exchangeable cation."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Desired Level"),
                        div(class = "range-value", "Should ideally occupy 65-85% of the CEC site occupancy.")
                    )
                )
            ),

            # ExMg
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Exch. Magnesium (ExMg)"),
                card_body(
                    span(class = "unit-badge", "cmol(+)/kg"),
                    div(class = "property-desc", "Magnesium is the central atom in chlorophyll, essential for photosynthesis. It also aids in phosphorus metabolism."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Desired Level"),
                        div(class = "range-value", "Ideally 10-15% of the total CEC occupancy.")
                    )
                )
            ),

            # ExK
            card(
                class = "property-card",
                card_header(class = "card-header-clean", "Exch. Potassium (ExK)"),
                card_body(
                    span(class = "unit-badge", "cmol(+)/kg"),
                    div(class = "property-desc", "Potassium regulates water use in plants, enhances disease resistance, and is crucial for overall plant vigor."),
                    div(
                        class = "range-section",
                        div(class = "range-label", "Desired Level"),
                        div(class = "range-value", "Ideally 2-5% of the total CEC occupancy.")
                    )
                )
            )
        )
    )
}

#' Soil Properties Module - Server
#' @param id Module ID
property_info_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Static documentation module
    })
}
