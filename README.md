# SpecPred Dashboard

A modern Shiny dashboard for predictions and analytics, built with `bslib` and modular architecture.

## Features

- 🎨 Modern UI with Bootstrap 5 styling via `bslib`
- 📱 Responsive design that works on desktop and mobile
- 🧩 Modular architecture for easy maintenance
- ⚡ Custom CSS and JavaScript enhancements
- 🔧 Reproducible environment with `renv` support

## Project Structure

```
specpred/
├── app.R                    # Main Shiny application
├── global.R                 # Global configurations and utilities
├── load_dependencies.R      # Advanced dependency management
├── install_packages.R       # Simple package installer
├── run_app.R               # Application runner
├── README.md               # This file
├── modules/                # Shiny modules
│   ├── home_module.R       # Home page module
│   └── prediction_module.R # Prediction functionality
└── www/                    # Static web assets
    ├── css/
    │   └── custom.css      # Custom styling
    ├── js/
    │   └── custom.js       # Custom JavaScript
    ├── images/
    │   └── logo.svg        # Application logo
    └── favicon.ico         # Site icon
```

## Quick Start

### 1. Install Dependencies

**Option A: Simple installation**
```r
source("install_packages.R")
```

**Option B: Full setup with renv (recommended)**
```bash
Rscript load_dependencies.R
```

### 2. Run the Application

```r
# In R console
shiny::runApp()

# Or from command line
Rscript -e "shiny::runApp()"
```

### 3. Access the Dashboard

Open your web browser and navigate to the URL shown in the R console (typically `http://127.0.0.1:PORT`).

## Package Dependencies

### Required Packages
- `shiny` - Core Shiny framework
- `bslib` - Bootstrap themes and components
- `htmltools` - HTML generation utilities  
- `shinyjs` - JavaScript integration

### Optional Packages (for enhanced functionality)
- `DT` - Interactive data tables
- `plotly` - Interactive plots
- `shinyWidgets` - Additional input widgets
- `shinycssloaders` - Loading animations
- `waiter` - Loading screens

## Development

### Using renv for Reproducible Environments

This project supports `renv` for package management:

```r
# Restore packages from lockfile
renv::restore()

# Update snapshot after adding packages
renv::snapshot()

# Check package status
renv::status()
```

### Customization

#### Styling
- Edit `www/css/custom.css` to modify the appearance
- CSS variables at the top of the file control the color scheme
- Dark mode support is included

#### JavaScript  
- Add custom functionality in `www/js/custom.js`
- Utility functions for notifications, loading states, and animations are included

#### Modules
- Add new modules in the `modules/` directory
- Follow the pattern of `home_module.R` and `prediction_module.R`
- Update `app.R` to include new modules

## Configuration

### Environment Variables

- `SHINY_DEBUG=TRUE` - Enable debug mode with additional logging
- Set via `.Renviron` file or system environment

### Theme Customization

The app uses a custom Bootstrap theme. Key colors can be modified in:
- `app.R` - `bs_theme()` parameters
- `www/css/custom.css` - CSS variables
- `global.R` - Global color constants

## Features

### Navigation
- Responsive sidebar navigation
- Smooth page transitions
- Keyboard shortcuts (Ctrl/Cmd + H for Home, Ctrl/Cmd + P for Predictions)

### Home Module
- Dashboard overview
- Status indicators
- Feature highlights
- Getting started information

### Prediction Module  
- Interactive parameter inputs
- Real-time predictions
- Visualization charts
- Model information display

### Custom Enhancements
- Loading animations
- Hover effects
- Status badges
- Notification system
- Export functionality
- Performance monitoring

## Deployment

### Local Development
```bash
# Run with specific port
R -e "shiny::runApp(port=3838)"

# Run in background
nohup R -e "shiny::runApp(host='0.0.0.0', port=3838)" &
```

### Production Deployment
Consider using:
- **Shiny Server** - Open source option
- **RStudio Connect** - Commercial platform
- **ShinyProxy** - Container-based deployment
- **Cloud platforms** - AWS, Google Cloud, Azure

## Troubleshooting

### Common Issues

**Dependencies not installing**
```bash
# Update R packages
update.packages(ask=FALSE)

# Clear package cache
remove.packages("packagename")
install.packages("packagename")
```

**JavaScript not loading**
- Check browser console for errors
- Verify file paths in `app.R`
- Ensure `shinyjs` is loaded

**Styling issues**
- Check CSS file syntax
- Verify Bootstrap version compatibility
- Test in different browsers

### Debug Mode
Enable debug mode to see additional logging:
```r
Sys.setenv(SHINY_DEBUG = "TRUE")
shiny::runApp()
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make changes following the existing code style
4. Test your changes locally
5. Submit a pull request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

For questions and support:
- Check the [Issues](../../issues) page
- Review Shiny documentation: https://shiny.rstudio.com/
- bslib documentation: https://rstudio.github.io/bslib/

---

Built with ❤️ using R and Shiny