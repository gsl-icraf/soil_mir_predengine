required_packages <- c(
    'RApiSerialize',
    'stringfish', 
    'BH', 
    'shiny', 
    'bslib', 
    'data.table',
    'plotly', 
    'shinyjs', 
    'remotes',
    'mirai', 
    'prospectr', 
    'DT',
    'ranger', 
    'htmltools', 
    'viridis',
    'shinycssloaders', 
    'torch', 
    'ggplot2',
    'arrow'
)

archived_packages <- c(
  "qs_0.27.3.tar.gz"
)

github_installs <- c(
    'spectral-cockpit/opusreader2', 
    'pierreroudier/opusreader'
)


install.packages(required_packages, repos="https://cran.rstudio.com")

for (pkg in archived_packages){
    install.packages(pkg, repos = NULL, type = "source")
}

for (pkg in github_installs){
    remotes::install_github(pkg)
}