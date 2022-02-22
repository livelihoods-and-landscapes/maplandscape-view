library(leaflet)
library(shiny)
library(sf)
library(magrittr)
library(dplyr)
library(stringr)
library(httr)
library(bslib)
library(htmltools)
library(httr)
library(xfun)
library(waiter)
library(RColorBrewer)
library(DT)
library(readr)
library(tidyselect)
library(ggmap)
library(aws.s3)
library(config)
library(ggplot2)
library(plotly)
library(qfieldcloudR)

# config
config <- config::get()

# app title
title <- config$title

# OAuth config
oauth_redirect_url <- config$oauth_redirect_url
oauth_key <- config$oauth_key
oauth_secret <- config$oauth_secretl
oauth_scope <- config$oauth_scope

# gcs config
gcs_bucket_name <- config$gcs_bucket

# s3 config 
aws_bucket <- config$aws_bucket
aws_region <- config$aws_region
aws_secret <- config$aws_secret
aws_key <- config$aws_key

# setup S3 storage
Sys.setenv("AWS_ACCESS_KEY_ID" = aws_key,
           "AWS_SECRET_ACCESS_KEY" = aws_secret,
           "AWS_DEFAULT_REGION" = aws_region)

# simple authentication
username <- config$simple_username
password <- config$simple_password

# qfieldcloud
qfieldcloud_url <- config$qfieldcloud
project_id <- config$project_id

# custom xyz basemap
custom_xyz <- config$custom_xyz
custom_xyz_name <- config$custom_xyz_name

# map colour palettes
colour_mappings <- c(
  "red - blue" = "RdBu",
  "brown - green" = "BrBG",
  "purple - orange" = "PuOr",
  "red - yellow - blue" = "RdYlBu",
  "red - yellow - green" = "RdYlGn",
  "blues" = "Blues",
  "blue - green" =  "BuGn",
  "green - blue" = "GnBu",
  "greens" = "Greens",
  "grey" = "Greys",
  "orange" = "Oranges",
  "orange - red" = "OrRd",
  "purple - blue" = "PuBu",
  "red - blue - green" = "PuBuGn",
  "purple - red" = "PuRd",
  "purple" =  "Purples",
  "red - purple" = "RdPu",
  "red" = "Reds",
  "yellow - green" = "YlGn",
  "yellow - green - blue" = "YlGnBu",
  "yellow - orange - brown" = "YlOrBr",
  "yellow - orange - red" = "YlOrRd",
  "accent" = "Accent",
  "pastel 1" = "Pastel1",
  "pastel 2" = "Pastel2",
  "set 1" = "Set1",
  "set 2" = "Set2",
  "set 3" = "Set3"
)

line_colours <- c(
  "blue" = "blue",
  "black" = "black"
)

# waiting screen spinners
map_screen <- tagList(
  waiter::spin_loader(),
  h4("Drawing map...")
)

# waiting screen spinners
loading_screen <- tagList(
  waiter::spin_loader(),
  h4("Loading...")
)