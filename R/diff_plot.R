#' @title Create diff plots
#'
#' @description Create a plot to show the difference between two samples.
#'
#' @param data the dataframe containing the lipids, normally imported results from MS-DIAL
#' @param x what do you want on the x-axis, often the difference
#' @param y what do you want on the y-axis, often the lipids
#' @param fill_by factor showing up or down regulation
#' @param facet_y facet by this column in y
#' @param facet_x facet by this column in x
#'
#' @return A ggplot2 object
#' 
#' @details This function will create a difference plot showing up or down regulation.
#' 
#' @export
#' @importFrom rlang enquo !!
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid xlab ylab guides vars
#'
#' @author Rico Derks
diff_plot <- function(data, x, y, fill_by, facet_y, facet_x) {
  ## some error checking
  if (length(match.call()) <= 6) {
    stop("Not enough arguments passed... ")
  }
  ## check if data is a data frame
  if (!is(data, "data.frame")) {
    stop("'data' does not appear to be a data frame!")
  }
  ## is x a column in the dataframe
  if (!deparse(substitute(x)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(x)), "' is not the name of a variable in '",deparse(substitute(data)),"'"))
  }
  ## is y a column in the dataframe
  if (!deparse(substitute(y)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(y)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is fill_by a column in the dataframe
  if (!deparse(substitute(fill_by)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(fill_by)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is facet_y a column in the dataframe
  if (!deparse(substitute(facet_y)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(facet_y)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is facet_x a column in the dataframe
  if (!deparse(substitute(facet_x)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(facet_x)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  
  ## quotation stuff
  my_df <- rlang::enquo(data)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill_by <- rlang::enquo(fill_by)
  facet_y <- rlang::enquo(facet_y)
  facet_x <- rlang::enquo(facet_x)
  
  ## create the plot
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = rlang::`!!`(x),
                                 y = rlang::`!!`(y))) +
    ggplot2::geom_bar(ggplot2::aes(fill = rlang::`!!`(fill_by)),
                      stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(rows = ggplot2::vars(rlang::`!!`(facet_y)),
                        cols = ggplot2::vars(rlang::`!!`(facet_x)),
                        scales = "free_y",
                        space = "free_y") +
    ggplot2::ylab("Difference") +
    ggplot2::xlab("Lipid name") +
    ggplot2::guides(fill = FALSE)
  
  ## return the ggplot2 object
  return(p)
}