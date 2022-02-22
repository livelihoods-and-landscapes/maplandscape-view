#' Return a ggplot object displaying user selected fill colour palette
#' 
 
make_colour_ramp_viridis <- function(carto_colour) {
  
  df <- data.frame(x = seq(0, 255), y = 1) 
  
  colour_ramp <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = x)) + 
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::scale_fill_viridis_c(option = carto_colour, direction = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
      panel.spacing = unit(0, "cm"),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      axis.ticks.length = unit(0, "cm"),
      legend.position = "none") 
  
  colour_ramp
}

# make_colour_ramp <- function(carto_colour) {
#   
#   df <- data.frame(x = seq(0, 255), y = 1) 
#   
#   colour_ramp <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + 
#     ggplot2::geom_tile(ggplot2::aes(fill = x)) + 
#     ggplot2::labs(x = NULL, y = NULL) +
#     ggplot2::scale_fill_viridis_c(option = carto_colour, direction = 1) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(
#       axis.text.x = ggplot2::element_blank(),
#       axis.text.y = ggplot2::element_blank(),
#       axis.ticks = ggplot2::element_blank(),
#       panel.grid.major = ggplot2::element_blank(), 
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.background = ggplot2::element_rect(fill = "transparent",colour = NA),
#       plot.background = ggplot2::element_rect(fill = "transparent",colour = NA),
#       panel.spacing = unit(0, "cm"),
#       plot.margin = margin(0, 0, 0, 0, "cm"),
#       axis.ticks.length = unit(0, "cm"),
#       legend.position = "none") 
#   
#   colour_ramp
# }


