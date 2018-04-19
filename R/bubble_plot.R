#' @title Create bubble plots
#'
#' @description Create a bubble for quick checking of MSMS lipid identification from MS-DIAL
#'
#' @param data the dataframe containing the lipids, normally imported results from MS-DIAL
#' @param x what do you want on the x-axis, often the retetnion time
#' @param y what do you want on the y-axis, often the retetnion time
#' @param color_by color the bubbles according to this column
#' @param carbon_group group the bubbles according to this column
#'
#' @return A ggplot2 object
#' 
#' @details This function will create bubble plots from exported MS-DIAL results.
#' It will quickly show you if there any misalignment, bad identifications. This function is still heavily
#' under development and for sure will change in the future. This function assumes the ggplot2 > v2.2.1.9000 is 
#' installed.
#' 
#' @export
#' @importFrom rlang enquo !!
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_text facet_grid xlab ylab guides
#' @importFrom stringi stri_extract
#'
#' @author Rico Derks
bubble_plot <- function(data, x, y, color_by, carbon_group) {
  ## some error checking
  if (length(match.call()) <= 5) {
    stop("Not enough arguments passed... ")
  }
  ## check if data is a data frame
  if (!is(data, "data.frame")) {
    stop("'data' does not appear to be a data frame!")
  }
  ## is x a column in the dataframe
  if (!deparse(substitute(x)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(x)), "' is not the name of a variable in '", deparse(substitute(data)),"'"))
  }
  ## is y a column in the dataframe
  if (!deparse(substitute(y)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(y)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is color_by a column in the dataframe
  if (!deparse(substitute(color_by)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(color_by)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is carbon_group a column in the dataframe
  if (!deparse(substitute(carbon_group)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(carbon_group)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  
  ## quotation stuff
  my_df <- rlang::enquo(data)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_by <- rlang::enquo(color_by)
  carbon_group <- rlang::enquo(carbon_group)
  
  ## create the plot
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = rlang::`!!`(x),
                                 y = rlang::`!!`(y),
                                 color = rlang::`!!`(color_by),
                                 group = rlang::`!!`(carbon_group))) +
    ## assuming this name stays the same
    ggplot2::geom_point(ggplot2::aes(size = DotProduct),
                        alpha = 0.4) +
    ggplot2::scale_size(range = c(1, 10)) +
    ggplot2::geom_line() +
    ggplot2::geom_text(ggplot2::aes(label = stringi::stri_extract(str = ShortLipidName,
                                                                  regex = "[0-9]{2}:[0-9]{1,2}")),
                       size = 2.5,
                       color = "black") +
    ggplot2::facet_grid(LipidClass ~ .,
                        scales = "free") +
    ggplot2::xlab("Retention time [minutes]") +
    ggplot2::ylab(expression(italic("m/z"))) +
    ggplot2::guides(color = FALSE, size = FALSE)
  
  ## return the ggplot2 object
  return(p)
}