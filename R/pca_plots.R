#' @title Create nice PCA plots.
#' 
#' @description Generate different plots for a PCA analysis. Score plot 
#' (includings Hotelling T2 confidence interval ellipse), loading plot or a 
#' summary plot. The summary plot contains a score plot, loadings plot and the 
#' summary of fit plot.
#' 
#' @param data Dataframe containing all information. See details for more information on the structure.
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
#' @importFrom ggplot2 ggplot aes aes_string
#'
#' @author Rico Derks
#' @examples
#' # create a dummy frame 
#' mydata <- data.frame(pc1 = rnorm(40),
#'                      pc2 = rnorm(40),
#'                      groups = as.factor(c(rep(1, 20), rep(2, 20))),
#'                      shapes = as.factor(c(rep(1, 20), rep(2, 20))))
#' p <- pca_plots(data = mydata, 
#'                x = "pc1",
#'                y = "pc2",
#'                type = "scores",
#'                colour = "groups",
#'                shape = "shapes")
pca_plots <- function(data, 
                      x, 
                      y, 
                      type = c("scores", "loadings", "summary"), 
                      T2 = TRUE, 
                      hotelling = 0.95, 
                      colour = NULL, 
                      shape = NULL, 
                      size = 3) {

  type <- match.arg(type)

  # start the plot
  p <- ggplot()
  switch(type,
         scores = {
           if (T2 == TRUE) {
             p <- p + geom_polygon(data = simple_ellipse(x = data[, x], y = data[, y], alpha = hotelling), aes(x = x, y = y), colour = "gray", fill = "white", alpha = 0.5)
           }
           p <- p + geom_point(data = data, aes_string(x = x, y = y, colour = colour, shape = shape), size = size)
           p <- p + ggtitle("Scores plot")
         },
         loadings = {
           p <- p + geom_point(data = data, aes_string(x = x, y = y), size = size)
           p <- p + ggtitle("Loadings plot")
         },
         summary = {

         }
  )
  p <- p + geom_hline(aes(yintercept = 0), colour = "gray")
  p <- p + geom_vline(aes(xintercept = 0), colour = "gray")
  p <- p + xlab(x)
  p <- p + ylab(y)
  return(p)
}
