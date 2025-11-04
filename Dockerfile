FROM rocker/shiny:4.4.2

RUN apt-get update -y && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    vim \
    cmake \
    libfontconfig-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    curl \
    libmbedtls-dev \
    libnng-dev \
    xz-utils

WORKDIR /shiny/dashboard
 
COPY . .
 
# Application packages
RUN Rscript -e "install.packages(c('shiny', 'bslib', 'data.table', 'plotly', 'shinyjs', 'remotes', 'mirai', 'prospectr', 'DT', 'ranger', 'htmltools', 'viridis'), repos='https://cran.rstudio.com')"

RUN Rscript -e "remotes::install_github('spectral-cockpit/opusreader2')"

# Port
EXPOSE 3838
 
# Running the container
CMD [ "R", "-e", "shiny::runApp('/shiny/dashboard/app.R', port = 3838, host = '0.0.0.0')" ]