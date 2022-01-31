#' Get GeoPackages from S3 bucket
#'
#' Makes a request to a user specified S3 bucket to get object
#' (GeoPackage) in the bucket.
#'
#' @param bucket name of S3 bucket.
#' @param object name of GeoPackage to get.
#'
#' @return two element list. First element is the path to where the GeoPackage
#'   has been written or character vector with an error message. The second
#'   element is the file name to display to users when selecting the GeoPackage.

get_s3_object <- function(bucket, object) {
  #  get S3 object
  req <- try(aws.s3::get_object(object, bucket))
  
  if (class(req) == "try-error") {
    shiny::showNotification(paste0("Error listing S3 bucket contents"),
                            type = "error",
                            duration = 5)
    return()
  }
  
  s3_gpkg <- tryCatch(
    error = function(cnd) {
      "cannot load GeoPackage from S3"
    },
    {
      s3_gpkg <-
        fs::file_temp(pattern = "",
                      tmp_dir = tempdir(),
                      ext = "gpkg")
      writeBin(req, s3_gpkg)
      
      # check GeoPackage can be read
      check_sf <- try(sf::st_read(s3_gpkg))
      if ("try-error" %in% class(check_sf)) {
        s3_gpkg <- "cannot load GeoPackage from S3"
      }
      
      s3_gpkg <- list(f_path = s3_gpkg,
                      f_name = object)
      
      s3_gpkg
    }
  )
  
  # return a list with path to GeoPackage and name of GeoPackage
  s3_gpkg
}
