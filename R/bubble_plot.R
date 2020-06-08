#' @title Create bubble plots
#' 
#' @description Create bubble plots to have an indication of the quality of the lipid identification from MS-DIAL.
#' 
#' @param data Dataframe containing all information. See details for more information on the structure.
#' @param x what to plot on the x-axis, in general retention time.
#' @param y what to plot on the y-axis, in general m/z.
#' @param color_by color the lines, in general the number carbons.
#' @param carbon_db column containing the total carbon numer and total number of double bounds
#'
#' @return A ggplot2 plot is returned.
#' 
#' @details \code{data} should be data frame which contains at least all the parameters as columns. 
#` No calculations are done within the function. In the data frame there should be two column called: 
#` LipidClass and DotProduct. LipidClass is used for facetting. DotProduct is used for the size of the 
#` bubbles.
#' 
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang enquo !!
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_grid geom_text guides xlab ylab
#' @importFrom methods is
#'
#' @author Rico Derks
#' @examples 
#' my_data <- data.frame(rt = rep(c(seq(7, 10, length.out = 5),
#'                                  seq(3, 6, length.out = 3), 
#'                                  seq(2, 5, length.out = 7),
#'                                  seq(2, 8, length.out = 6),
#'                                  seq(5, 9, length.out = 4),
#'                                  seq(3, 10, length.out = 6)), 3),
#'                       mz = rep(seq(507, 510, by = 0.1), 3),
#'                       carbon = c(rep("43", 5), 
#'                                  rep("41", 3), 
#'                                  rep("44", 7), 
#'                                  rep("46", 6), 
#'                                  rep("42", 4), 
#'                                  rep("45", 6)),
#'                       carbon_db = c(paste0(rep("43", 5), ":", 5:1),
#'                       paste0(rep("41", 3), ":", 3:1),
#'                       paste0(rep("44", 7), ":", 7:1),
#'                       paste0(rep("46", 6), ":", 6:1),
#'                       paste0(rep("42", 4), ":", 4:1),
#'                       paste0(rep("45", 6), ":", 6:1)),
#'                       DotProduct = rnorm(n = 93, mean = 700, sd = 200),
#'                       LipidClass = c(rep("a", 31), rep("b", 31), rep("c", 31)))
#' # create the bubble plot
#' bubble_plot(data = my_data,
#'             x = rt,
#'             y = mz,
#'             color_by = carbon,
#'             carbon_db = carbon_db)
bubble_plot <- function(data, x, y, color_by, carbon_db) {
  ## some error checking
  if (length(match.call()) <= 5) {
    stop("Not enough arguments passed... ")
  }
  ## check if data is a data frame
  if (!is(data, "data.frame")) {
    stop("'data' does not appear to be a data frame!")
  }
  ## is x a column in the data frame
  if (!deparse(substitute(x)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(x)), "' is not the name of a variable in '", deparse(substitute(data)),"'"))
  }
  ## is y a column in the data frame
  if (!deparse(substitute(y)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(y)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is color_by a column in the data frame
  if (!deparse(substitute(color_by)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(color_by)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is carbon_db a column in the dat aframe
  if (!deparse(substitute(carbon_db)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(carbon_db)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }

  ## quotation stuff
  my_df <- enquo(data)
  x <- enquo(x)
  y <- enquo(y)
  color_by <- enquo(color_by)
  carbon_db <- enquo(carbon_db)

  ## create the plot
  p <- data %>%
    ggplot(aes(x = !!x,
               y = !!y,
               color = !!color_by,
               group = !!color_by)) +
    ## assuming this name stays the same
    geom_point(aes(size = DotProduct),
               alpha = 0.4) +
    scale_size(range = c(1, 10)) +
    geom_line() +
    geom_text(aes(label = !!carbon_db),
              size = 2.5,
              color = "black") +
    facet_grid(LipidClass ~ .,
               scales = "free") +
    xlab("Retention time [minutes]") +
    ylab(expression(italic("m/z"))) +
    guides(color = FALSE, size = FALSE)
  
  ## return the ggplot2 object
  return(p)
  
}