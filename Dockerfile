FROM rocker/shiny:4.4.2

# User configuration
USER root

# System packages
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

# Working directory
WORKDIR /shiny/dashboard
 
COPY . .
 
# Application packages
RUN Rscript -e "install.packages(c('RApiSerialize', 'stringfish', 'BH'), repos='https://cran.rstudio.com')"

# Install cran archived packages
RUN wget https://cran.r-project.org/src/contrib/Archive/qs/qs_0.27.3.tar.gz -O /tmp/qs_0.27.3.tar.gz
RUN Rscript -e "install.packages('/tmp/qs_0.27.3.tar.gz', repos=NULL, type='source')"

# Install upto date cran packages
RUN Rscript -e "install.packages(c( \
    'shiny', 'bslib', 'data.table', \
    'plotly', 'shinyjs', 'remotes', \
    'mirai', 'prospectr', 'DT', \
    'ranger', 'htmltools', 'viridis', \
    'shinycssloaders', 'torch', 'ggplot2'),\ 
    repos='https://cran.rstudio.com')"

## Install R torch package dependencies (libtorch)
RUN Rscript -e "torch::install_torch()"

# R packges from R packages from github
RUN Rscript -e "remotes::install_github(c('spectral-cockpit/opusreader2', 'pierreroudier/opusreader'))" 

# User configuration
RUN useradd -m pred_engine_user
USER pred_engine_user

# Port
EXPOSE 3838
 
# Running the container
CMD [ "R", "-e", "shiny::runApp('/shiny/dashboard/app.R', port = 3838, host = '0.0.0.0')" ]