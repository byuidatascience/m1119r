#' @title A 1d heatmap histogram
#' @description A ggplot2 based function to provide a simple heatmap histogram.
#' @param x the vector of points to be used in the histogram. 
#' @examples x_vector <-  rnorm(100)
#' histheat(x_vector)
histheat <- function(x, bins = 30, x_name = "value"){
  
  dat <- tidyr::tibble(x = x) %>%
    dplyr::mutate(bins = cut(x, breaks = bins)) %>%
    dplyr::count(bins)
  
  dat %>%
    ggplot2::ggplot(ggplot2::aes(x = bins, fill = n, y = 1)) +
    ggplot2::geom_tile() +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::labs(x = x_name, fill = "Count in bin")
  
  
} 