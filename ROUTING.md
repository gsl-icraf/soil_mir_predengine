# JavaScript-Based Routing in SpecPred Dashboard

## Overview
This application uses JavaScript-based client-side routing to create a multi-page experience within a single Shiny application. This approach provides:

- **URL bookmarking**: Users can bookmark specific pages
- **Browser navigation**: Back/forward buttons work correctly  
- **Clean URLs**: Routes like `?page=prediction` instead of complex hash routes
- **No external dependencies**: Pure JavaScript without requiring additional R packages

## How It Works

### 1. URL Structure
- Home page: `/?page=home` or just `/`
- Prediction page: `/?page=prediction`

### 2. Navigation Links
Navigation links use `data-page` attributes to specify the target page:

```html
<a class="nav-link" href="?page=home" data-page="home">Home</a>
<a class="nav-link" href="?page=prediction" data-page="prediction">Prediction</a>
```

### 3. JavaScript Router Logic
The routing system consists of three main parts:

#### Click Handler
```javascript
$(document).on("click", ".nav-link", function(e) {
  e.preventDefault();
  var page = $(this).data("page");
  var newUrl = window.location.pathname + "?page=" + page;
  window.history.pushState(null, null, newUrl);
  Shiny.setInputValue("current_page", page, {priority: "event"});
  
  // Update active navigation
  $(".nav-link").removeClass("active");
  $(this).addClass("active");
});
```

#### Popstate Handler (Back/Forward Buttons)
```javascript
window.addEventListener("popstate", function(event) {
  var urlParams = new URLSearchParams(window.location.search);
  var page = urlParams.get("page") || "home";
  Shiny.setInputValue("current_page", page, {priority: "event"});
  
  // Update active navigation
  $(".nav-link").removeClass("active");
  $(".nav-link[data-page=\"" + page + "\"]").addClass("active");
});
```

#### Initial State Setup
```javascript
$(document).ready(function() {
  var urlParams = new URLSearchParams(window.location.search);
  var page = urlParams.get("page") || "home";
  $(".nav-link[data-page=\"" + page + "\"]").addClass("active");
});
```

### 4. Shiny Integration
The UI function accepts a `request` parameter to parse the initial URL:

```r
ui <- function(request) {
  url_path <- parseQueryString(request$QUERY_STRING)$page %||% "home"
  # ... rest of UI
}
```

### 5. Conditional Panels
Pages are shown/hidden using `conditionalPanel` based on the `current_page` input:

```r
# Home content
conditionalPanel(
  condition = "input.current_page == 'home' || typeof input.current_page === 'undefined'",
  home_ui("home")
),

# Prediction content  
conditionalPanel(
  condition = "input.current_page == 'prediction'",
  prediction_ui("prediction")
)
```

## Adding New Pages

To add a new page to the application:

1. **Create the UI module** (e.g., `modules/analytics_module.R`)
2. **Add navigation link** in `app.R`:
   ```r
   a(
     class = "nav-link",
     href = "?page=analytics", 
     `data-page` = "analytics",
     icon("chart-bar"), " Analytics"
   )
   ```
3. **Add conditionalPanel**:
   ```r
   conditionalPanel(
     condition = "input.current_page == 'analytics'",
     div(
       class = "router-page",
       analytics_ui("analytics")
     )
   )
   ```
4. **Add module server** call:
   ```r
   analytics_server("analytics")
   ```

## Benefits

### Compared to shiny.router
- **No external dependencies**: Works with base Shiny
- **Simpler implementation**: Easier to understand and modify
- **Better error handling**: No complex router configuration
- **More control**: Direct access to URL manipulation

### Compared to single-page approach
- **Bookmarkable URLs**: Each page has a unique URL
- **Browser navigation**: Back/forward buttons work naturally
- **Better UX**: Users can share links to specific pages
- **SEO friendly**: Each page can have unique titles and meta tags

## CSS Classes

The routing system uses these CSS classes:

- `.nav-link`: Navigation links
- `.nav-link.active`: Currently active navigation link  
- `.router-page`: Container for page content with transition animations

These classes are styled in `www/css/custom.css` with smooth transitions and hover effects.