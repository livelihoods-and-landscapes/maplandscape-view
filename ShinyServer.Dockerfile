# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update && apt-get install -y  \
gdal-bin \
git-core \
libcairo2-dev \
libcurl4-openssl-dev \
libgdal-dev \
libgeos-dev \
libgeos++-dev \
libgit2-dev \
libicu-dev \
libpng-dev \
libproj-dev \
libssl-dev \
libudunits2-dev \
libxml2-dev make pandoc pandoc-citeproc \
&& rm -rf /var/lib/apt/lists/*

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
    
# copy necessary files
COPY --chown=shiny:shiny /app/server.R /srv/shiny-server/app/
COPY --chown=shiny:shiny /app/ui.R /srv/shiny-server/app/
COPY --chown=shiny:shiny /app/global.R /srv/shiny-server/app/
COPY --chown=shiny:shiny /app/config.yml /srv/shiny-server/app/
COPY --chown=shiny:shiny /app/www/* /srv/shiny-server/app/www/
COPY --chown=shiny:shiny /app/R/* /srv/shiny-server/app/R/

# copy customised shiny-server config to run on Google Cloud Run
COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

# install required R packages
RUN R -e 'install.packages(c( \
  "bslib", \
  "leaflet", \
  "sf", \
  "shiny", \
  "dplyr", \
  "shinyFeedback", \
  "magrittr", \
  "httr", \
  "stringr", \
  "stringi", \
  "xfun", \
  "waiter", \
  "RColorBrewer", \
  "DT", \
  "Rcpp", \
  "readr", \
  "tidyselect", \
  "fs", \
  "ggmap", \
  "tidyselect", \
  "aws.s3", \
  "config", \
  "ggplot2"), \
  repos="http://cran.rstudio.com/")'

# expose port
EXPOSE 3838

# set user
USER shiny

# run app
CMD ["/usr/bin/shiny-server"]