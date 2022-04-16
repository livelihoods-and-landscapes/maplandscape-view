# install packages
# use renv

packages <- c(
  "bslib",
  "leaflet",
  "sf",
  "shiny",
  "dplyr",
  "styler",
  "shinyFeedback",
  "magrittr",
  "httr",
  "stringr",
  "stringi",
  "xfun", 
  "waiter",
  "RColorBrewer",
  "DT",
  "Rcpp",
  "readr",
  "ggmap",
  "aws.s3",
  "config",
  "ggplot2",
  "plotly",
  "livelihoods-and-landscapes/qfieldcloudR",
  "livelihoods-and-landscapes/leafgl")

renv::install(packages)