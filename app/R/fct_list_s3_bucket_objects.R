#' List GeoPackages in Amazon S3 bucket
#'
#' Makes a request to a user specified Amazon S3 bucket to list all
#' objects in the bucket. Filters the response to return a list of GeoPackages
#' (if any).
#'
#' @param bucket name of Google Cloud Storage bucket to list objects in.
#'
#' @return items a character vector of length 0 or greater listing the names of GeoPacakges in Google Cloud Storage bucket.


list_s3_bucket_objects <- function(bucket) {
  # create a HTTP get request to list objects in the bucket
  req <- try(aws.s3::get_bucket(bucket))

  if (class(req) == "try-error") {
    shiny::showNotification(paste0("Error listing S3 bucket contents"),
      type = "error",
      duration = 5
    )
    return()
  }

  items <- NULL

  if (length(req) > 0) {
    for (i in 1:length(req)) {
      if (stringr::str_detect(req[[i]]$Key, ".gpkg$")) {
        items[i] <- req[[i]]$Key
      }
    }
  }

  if (length(items) <= 0) {
    return()
  }

  items
}
