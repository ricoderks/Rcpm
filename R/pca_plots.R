#' @title Create nice PCA plots.
#' 
#' @description Generate different plots for a PCA analysis. Score plot 
#' (includings Hotelling T2 confidence interval ellipse), loading plot or a 
#' summary plot. The summary plot contains a score plot, loadings plot and the 
#' summary of fit plot.
#' 
#' @param data dataframe containing all information. See details for more information on the structure.
#' @param x Principal component for the x-axis.
#' @param y Principal component for the y-axis.
#' @param type Which plot to generate: scores, loadings or summary plot.
#' @param T2 add a Hotteling T2 ellipse to the scores plot. Default is true.
#' @param hotelling Set the Hotelling confidence interval. Default = 0.95.
#' @param colour Set the color of the points for different groups.
#' @param shape Set the shape of the points for different groups.
#' @param size Set the size of the points. Default is 3.
#'
#' @return A ggplot2 plot is returned.
#' 
#' @details \code{data} should be data frame which contains at least 2 variables 
#' for plotting the scores or loadings plot. The name of these 2 variables are 
#' also used as label for the axes. More variables can be added for coloring 
#' or shaping the points. These variables should be factors. See the examples 
#' for a simple example.
#' 
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon labs ggtitle geom_hline geom_vline
#' @importFrom rlang enquo !! as_label
#' @importFrom methods is
#'
#' @author Rico Derks
#' @examples
#' # create a dummy frame 
#' mydata <- data.frame(PC1 = rnorm(40),
#'                      PC2 = rnorm(40),
#'                      groups = as.factor(c(rep(1, 20), rep(2, 20))),
#'                      shapes = as.factor(c(rep(1, 20), rep(2, 20))))
#' p <- pca_plots(data = mydata, 
#'                x = PC1,
#'                y = PC2,
#'                type = "scores",
#'                colour = groups,
#'                shape = shapes)
#' # or
#' library(dplyr)
#' mydata %>%
#'   pca_plots(x = PC1,
#'             y = PC2,
#'             type = "scores",
#'             colour = groups,
#'             shape = shapes)

pca_plots <- function(data, 
                      x, 
                      y, 
                      type = c("scores", "loadings", "summary"), 
                      T2 = TRUE, 
                      hotelling = 0.95, 
                      colour = NULL, 
                      shape = NULL, 
                      size = 3) {
  ## Some sanity checks
  # check if correct type is selected
  type <- match.arg(type)
  ## check if data is a data frame
  if (!is(data, "data.frame")) {
    stop("'data' does not appear to be a data frame!")
  }
  # is x a column in the dataframe
  if (!deparse(substitute(x)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(x)), "' is not the name of a variable in '", deparse(substitute(data)),"'"))
  }
  # is y a column in the dataframe
  if (!deparse(substitute(y)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(y)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  # is colour a column in the dataframe
  if (!deparse(substitute(colour)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(colour)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  # is shape a column in the dataframe
  if (!deparse(substitute(shape)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(shape)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }

  x <- enquo(x)
  y <- enquo(y)
  colour <- enquo(colour)
  shape <- enquo(shape)
  
  # start the plot
  p <- ggplot()
  switch(type,
         scores = {
           if (T2 == TRUE) {
             p <- p + geom_polygon(data = simple_ellipse(x = data[, as_label(x)], 
                                                         y = data[, as_label(y)],
                                                         alpha = hotelling), 
                                   aes(x = x, 
                                       y = y), 
                                   colour = "gray", 
                                   fill = "white", 
                                   alpha = 0.5)
           }
           p <- p + 
             geom_point(data = data, 
                        aes(x = !!x, 
                            y = !!y, 
                            colour = !!colour, 
                            shape = !!shape), 
                        size = size)
           p <- p + 
             ggtitle("Scores plot")
         },
         loadings = {
           p <- p + 
             geom_point(data = data, 
                               aes_string(x = x, 
                                          y = y), 
                               size = size)
           p <- p + 
             ggtitle("Loadings plot")
         },
         summary = {

         }
  )
  p <- p + 
    geom_hline(aes(yintercept = 0), 
               colour = "gray") + 
    geom_vline(aes(xintercept = 0), 
               colour = "gray") + 
    labs(x = x, 
         y = y)
  
  return(p)
}
