#' @title A 1d heatmap histogram
#' @description A ggplot2 based function to provide a simple heatmap histogram.
#' @param x the vector of points to be used in the histogram. 
#' @param bins the number of bins for the histogram.  Defaults to 30.
#' @param x_name the name to put on the x-axis.
#' @param narrow defaults to TRUE.  Makes the chart narrow on the y-axis.
#' @param x_limits the range of the x-axis to plot. Defaults to all with NULL.
#' @param ... values that work in ggplot2::scale_x_continous. Mostly for the breaks argument.
#' @examples x_vector <-  rnorm(1000)
#' histheat(x_vector)
#' histheat(x_vector, narrow = FALSE, bins = 20)
#' histheat(x_vector, x_limits = c(-3, 3), breaks = seq(-3,3, by = .5))
#' @export
histheat <- function(x, bins = 30, x_name = "value", narrow = TRUE, x_limits = NULL, ...){
  
  dat <- tidyr::tibble(x = x) %>%
    dplyr::mutate(bins = cut(x, breaks = bins),
                  bins = stringr::str_remove_all(bins, "\\(|\\]")) %>%
    dplyr::count(bins, .drop = FALSE) %>%
    tidyr::separate(bins, into = c("first", "last"), sep = ",", convert = TRUE, remove = FALSE)
  
  if (!is.null(x_limits[1])) {
    dat <- dat %>%
      dplyr::filter(dplyr::between(first, x_limits[1], x_limits[2]))
  }
  
  
  plot_out <- dat %>%
    ggplot2::ggplot(ggplot2::aes(x = first, fill = n, y = 1)) +
    ggplot2::geom_tile() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::scale_x_continuous(...) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::labs(x = x_name, fill = "Count in bin") 
    
  ifelse(narrow, return(plot_out + ggplot2::coord_equal()), return(plot_out))
  
  
} 
