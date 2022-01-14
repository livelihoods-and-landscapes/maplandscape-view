FROM openanalytics/r-base

# system libraries of general use
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
    
# install required R packages
RUN R -e 'install.packages(c( \
  "bslib", \
  "leaflet", \
  "sf", \
  "shiny", \
  "markdown", \
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

# copy necessary files
RUN mkdir /root/app
COPY /app/server.R /root/app/
COPY /app/ui.R /root/app/
COPY /app/config.yml /root/app/
COPY /app/global.R /root/app/
COPY /app/www/* /root/app/www/
COPY /app/R/* /root/app/R/

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"]