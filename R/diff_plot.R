#' @title Create difference plots
#' 
#' @description Create difference plots to show up- or down-regulation of certain lipids.
#' 
#' @param data data frame containing all information. See details for more information on the structure.
#' @param x what to plot on the x-axis, often the variable e.g. lipid
#' @param y what to plot on the y-axis, the difference 
#' @param fill_by column with factor for filling the bar plot
#' @param facet_x facet in x direction with column
#' @param facet_y facet in y direction with column
#'
#' @return A ggplot2 plot is returned.
#' 
#' @details \code{data} should be data frame which contains at least all the parameters as columns. 
#` No calculations are done within the function. 
#' 
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang enquo !!
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid coord_flip guides xlab ylab vars
#' @importFrom methods is
#'
#' @author Rico Derks
#' @examples 
#' set.seed(123)
#' 
#' my_data <- data.frame(lipidname = c(paste("PA ", seq(30, 40, 2), ":0", sep = ""),
#'                                     paste("PA ", seq(30, 40, 2), ":1", sep = ""),
#'                                     paste("PA ", seq(30, 40, 2), ":2", sep = ""),
#'                                     paste("PC ", seq(30, 40, 2), ":0", sep = ""),
#'                                     paste("PC ", seq(30, 40, 2), ":1", sep = ""),
#'                                     paste("PC ", seq(30, 40, 2), ":2", sep = ""),
#'                                     paste("TG ", seq(50, 60, 1), ":0", sep = ""),
#'                                     paste("TG ", seq(50, 60, 1), ":1", sep = ""),
#'                                     paste("TG ", seq(50, 60, 1), ":2", sep = ""),
#'                                     paste("TG ", seq(50, 60, 1), ":3", sep = ""),
#'                                     paste("TG ", seq(50, 60, 1), ":4", sep = "")),
#'                       lipidclass = c(rep("PA", 18),
#'                                      rep("PC", 18),
#'                                      rep("TG", 55)),
#'                       difference = rnorm(n = 91, 
#'                                          mean = 0,
#'                                          sd = 3e4),
#'                       versus = factor(x = "AvsB"))
#' 
#' my_data$diff_grp <- as.factor(ifelse(my_data$difference > 0, "high", "low"))
#' 
#' diff_plot(data = my_data,
#'                 x = lipidname,
#'                 y = difference,
#'                 fill_by = diff_grp,
#'                 facet_x = versus,
#'                 facet_y = lipidclass)
#' 
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