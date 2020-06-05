#' @title show summary of fit of a PCA model
#' 
#' @description Create a plot which shows a summary of the fit your PCA model.
#'
#' @param data data frame with all data. See details for more information.
#'
#' @return A ggplot2 plot is returned.
#' 
#' @details The data frame \code{data} should contain 3 variables. \code{PC}, 
#' \code{R2cum} and \code{Q2cum}.
#' 
#' @export
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @author Rico Derks
#' 
#' @examples
#' library(pcaMethods)
#' 
#' # create some dummy data
#' my_data <- data.frame(samples = c(rep("a", 10), rep("b", 10)),
#'                       matrix(data = rnorm(400),
#'                              nrow = 20))
#'
#' # as cross validation q2 needs to be selected
#' M1 <- pca(object = my_data,
#'           nPcs = 5,
#'           cv = "q2")
#'           
#' # create data frame with R2 and Q2            
#' sumfit_data <- data.frame(PC = paste("PC", 1:5, sep = ""), 
#'                           R2cum = M1@R2cum, 
#'                           Q2cum = M1@cvstat)
#'
#' # create the summary plot
#' p <- summary_fit(sumfit_data)
summary_fit <- function(data){
  ## sanity checks
  # input checking (is this really necessary?! This is also done in summary_fit_data)
  # data needs to be a data frame
  if (!is.data.frame(data)) {
    stop("data needs to be a data frame!")
  }
  
  # check column names, they need to be PC, R2cum and Q2cum
  col_names <- c("PC", "R2cum", "Q2cum")
  if (length(which(col_names %in% colnames(data))) != 3) {
    stop("data should contain only the column names PC, R2cum and Q2cum!")
  }
  
  # make the data frame long 
  data_long <- data %>% 
    pivot_longer(cols = c(R2cum, Q2cum),
                 names_to = "variable",
                 values_to = "value")
  
  # create the plot
  p <- ggplot(data = data_long, 
              aes(x = PC, 
                  y = value, 
                  fill = variable)) +
    geom_bar(stat = "identity", 
             position = "dodge")
  
  return(p)
}
