# Load required libraries
library(shiny)
library(bslib)
library(data.table)
library(plotly)
library(shinyjs)
library(opusreader2)
library(mirai)
library(prospectr)
library(DT)
library(ranger)
library(mirai)


# Source module files
source("modules/home_module.R")
source("modules/prediction_module.R")

# Source prediction processing function
source("src/spectra_process_predict.R")
##
options(shiny.maxRequestSize = 50 * 1024^2)
# Define UI with URL-aware navigation
ui <- function(request) {
  # Parse URL to determine initial page
  url_path <- parseQueryString(request$QUERY_STRING)$page %||% "home"

  fluidPage(
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#007bff",
      base_font = font_google("Poppins")
    ),

    # Include custom CSS and JavaScript
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Poppins"),
      tags$script(src = "js/custom.js"),
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("specPred")
    ),

    # Initialize shinyjs
    useShinyjs(),
    
    # Hidden input to store current page (helps with initialization)
    tags$input(id = "current_page", type = "hidden", value = url_path),

    # URL updating JavaScript
    tags$script(HTML('
      // Global variable to track current page
      window.currentPage = null;
      
      // Determine page from URL immediately
      function getPageFromURL() {
        var urlParams = new URLSearchParams(window.location.search);
        return urlParams.get("page") || "home";
      }
      
      // Set the current page immediately
      window.currentPage = getPageFromURL();
      
      // Initialize routing system
      $(document).ready(function() {
        initializeRouting();
      });
      
      function initializeRouting() {
        var initialPage = getPageFromURL();
        window.currentPage = initialPage;
        
        console.log("Initializing routing:", {
          initialPage: initialPage,
          fullUrl: window.location.href
        });
        
        // Set initial active navigation
        updateActiveNavigation(initialPage);
        
        // Set the hidden input value
        $("#current_page").val(initialPage);
        
        // Ensure Shiny knows the current page (with delay to ensure Shiny is ready)
        setTimeout(function() {
          Shiny.setInputValue("current_page", initialPage, {priority: "event"});
        }, 100);
      }
      
      function updateActiveNavigation(page) {
        $(".nav-link").removeClass("active");
        $(".nav-link[data-page=\\"" + page + "\\"]").addClass("active");
      }
      
      // Handle navigation clicks
      $(document).on("click", ".nav-link", function(e) {
        e.preventDefault();
        var page = $(this).data("page");
        window.currentPage = page;
        
        // Update URL
        var newUrl = window.location.pathname + "?page=" + page;
        window.history.pushState({page: page}, null, newUrl);
        
        // Update hidden input
        $("#current_page").val(page);
        
        // Update Shiny
        Shiny.setInputValue("current_page", page, {priority: "event"});
        
        // Update navigation
        updateActiveNavigation(page);
      });
      
      // Handle browser back/forward buttons
      window.addEventListener("popstate", function(event) {
        var page = getPageFromURL();
        window.currentPage = page;
        
        // Update hidden input
        $("#current_page").val(page);
        
        // Update Shiny
        Shiny.setInputValue("current_page", page, {priority: "event"});
        
        // Update navigation
        updateActiveNavigation(page);
      });
    ')),

    # Navigation bar
    div(
      class = "navbar navbar-expand-lg navbar-dark bg-primary mb-4",
      div(
        class = "container-fluid",
        # Brand
        a(
          class = "navbar-brand nav-link",
          href = "?page=home",
          `data-page` = "home",
          "CIFOR-ICRAF Spectral Prediction Engine"
        ),

        # Navigation links
        div(
          class = "navbar-nav",
          a(
            class = "nav-link",
            href = "?page=home",
            `data-page` = "home",
            icon("home"), " Home"
          ),
          a(
            class = "nav-link",
            href = "?page=prediction",
            `data-page` = "prediction",
            icon("chart-line"), " Prediction Engine"
          )
        )
      )
    ),

    # Page content area
    div(
      class = "container-fluid",

      # Home content
      conditionalPanel(
        condition = "input.current_page == 'home' || (typeof input.current_page === 'undefined' && (typeof window.currentPage === 'undefined' || window.currentPage === 'home'))",
        div(
          class = "router-page",
          home_ui("home")
        )
      ),

      # Prediction content
      conditionalPanel(
        condition = "input.current_page == 'prediction' || (typeof input.current_page === 'undefined' && window.currentPage === 'prediction')",
        div(
          class = "router-page",
          prediction_ui("prediction")
        )
      )
    )
  )
}

# Define server logic
server <- function(input, output, session) {
  # Simple server-side routing - mainly for fallback
  observe({
    # Only intervene if JavaScript hasn't set the page correctly
    if (is.null(input$current_page)) {
      url_query <- parseQueryString(session$clientData$url_search)
      page <- url_query$page %||% "home"
      session$sendInputMessage("current_page", page)
    }
  })

  # Module servers
  home_server("home")
  prediction_server("prediction")
}

# Run the application
shinyApp(ui = ui, server = server)
