#' Group by and summarise
#'
#' Perform group by and summarise operations on selected grouping columns and
#' compute summary statistics within each group. This function uses dplyr's
#' \link[dplyr]{group_by} function for creating groups within the data frame and
#' \link[dplyr]{summarise} for computing summary statistics.
#'
#' @param in_df a layer (spatial
#'   (\href{https://r-spatial.github.io/sf/index.html}{sf}) or non-spatial data
#'   frame) to summarise.
#' @param group_var a character vector of column names to group by - uses
#'   dplyr's \link[dplyr]{group_by} function.
#' @param summ_var A character vector of columns names to compute grouped
#'   summary statistics for - uses dplyr's \link[dplyr]{summarise}. For all
#'   numeric columns the mean and sum are computed. For numeric and non-numeric
#'   columns the count of observations within each group is returned.
#'
#' @return a summary table as a data frame (\code{s_df}) object.
#'


# To-do - warning message to handle errors

group_by_summarise <- function(in_df, group_var, summ_var) {
  funs_list_numeric <- list(
    mean = ~ mean(.x, na.rm = TRUE),
    sum = ~ sum(.x, na.rm = TRUE)
  )

  
  if (is.null(summ_var)) {
    s_df <- tryCatch(
      error = function(cnd) NULL,
      in_df %>%
        as.data.frame() %>%
        dplyr::select(!!!group_var) %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(group_var))) %>%
        dplyr::tally()
    )
  } else {
    s_df <- tryCatch(
      error = function(cnd) NULL,
      in_df %>%
        as.data.frame() %>%
        dplyr::select(!!!group_var, !!!summ_var) %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(group_var))) %>%
        dplyr::summarise(dplyr::across(where(is.numeric), funs_list_numeric), n = n(), .groups = "keep")
    )
  }
  s_df
}
