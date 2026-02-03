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

RUN Rscript -e "install.packages(c('RApiSerialize', 'stringfish', 'BH'), repos='https://cran.rstudio.com')"

RUN wget https://cran.r-project.org/src/contrib/Archive/qs/qs_0.27.3.tar.gz -O /tmp/qs_0.27.3.tar.gz
RUN Rscript -e "install.packages('/tmp/qs_0.27.3.tar.gz', repos=NULL, type='source')"

RUN Rscript -e "install.packages(c('shiny', 'bslib', 'data.table', 'plotly', 'shinyjs', 'remotes', 'mirai', 'prospectr', 'DT', 'ranger', 'htmltools', 'viridis', 'shinycssloaders', 'torch', 'ggplot2'), repos='https://cran.rstudio.com')"
RUN Rscript -e "torch::install_torch()"
RUN Rscript -e "remotes::install_github(c('spectral-cockpit/opusreader2', 'pierreroudier/opusreader'))" 

RUN mv pca_model/mir_pca_model\ pca_model/mir_pca_model.rds

# Port
EXPOSE 3838
 
# Running the container
CMD [ "R", "-e", "shiny::runApp('/shiny/dashboard/app.R', port = 3838, host = '0.0.0.0')" ]