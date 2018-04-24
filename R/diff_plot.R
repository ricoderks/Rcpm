#' @title Create difference plots
#' 
#' @description Create difference plots to show up- or down-regulation of certain lipids.
#' 
#' @param data Dataframe containing all information. See details for more information on the structure.
#' @param x what to plot on the x-axis, in general retention time.
#' @param y what to plot on the y-axis, in general m/z.
#' @param fill_by column with factor for filling the bar plot
#' @param facet_x facet in x direction with column
#' @param facet_y facet in y direction with column
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
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid coord_flip guides xlab ylab vars
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
  my_df <- enquo(data)
  x <- enquo(x)
  y <- enquo(y)
  fill_by <- enquo(fill_by)
  facet_y <- enquo(facet_y)
  facet_x <- enquo(facet_x)
  
  ## create the plot
  p <- data %>%
    ggplot(aes(x = !!x,
               y = !!y)) +
    geom_bar(aes(fill = !!fill_by),
             stat = "identity") +
    coord_flip() +
    facet_grid(rows = vars(!!facet_y),
               cols = vars(!!facet_x),
               scales = "free_y",
               space = "free_y") +
    ylab("Difference") +
    xlab("Lipid name") +
    guides(fill = FALSE)
  
  ## return the ggplot2 object
  return(p)
}