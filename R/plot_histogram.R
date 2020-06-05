#' @title Plot nice histogram
#' 
#' @description plot_histogram plots a histogram with a fitted kernel density plot and a normal 
#' density plot. If no \code{variable} (i.e. NULL) is defined, then for each column in the 
#' data frame a histogram is plotted.
#'
#' @param data is a data frame containing all the data
#' @param variable which variables (i.e. columns) to plot (see examples).
#' @param legend is a boolean (default true) used to show the legend.
#'
#' @return ggplot showing a nice histogram
#' 
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_histogram geom_density geom_line aes theme facet_wrap scale_colour_manual
#' @importFrom grDevices nclass.FD
#' @importFrom stats dnorm sd density complete.cases
#' @importFrom dplyr %>% select mutate group_by everything n
#' @importFrom rlang quo_is_null
#'
#' @author Rico Derks
#'
#' @examples
#' set.seed(123)
#' my_df <- data.frame(a = rnorm(100, mean = 10), 
#'                     b = rnorm(100, mean = 40),
#'                     c = rnorm(1000, mean = 123))
#'                     
#' # all variables
#' plot_histogram(data = my_df)
#' 
#' # only one
#' plot_histogram(data = my_df,
#'                variable = a)
#'                
#' # only two
#' plot_histogram(data = my_df,
#'                variable = c(a, b))
#' 
#' # or using tidyverse approach
#' library(dplyr)
#' my_df %>%
#'   plot_histogram()
#'   
plot_histogram <- function(data, variable = NULL, legend = TRUE) {
  ## sanibty checks
  # check if data is a data frame
  if (!is(data, "data.frame")) {
    stop("'data' does not appear to be a data frame!")
  }
  # # is variable a column in the data frame
  # leave out for now, doesn't work if multiple columns are selected
  # if (!quo_is_null(enquo(variable)) & !as.character(deparse(substitute(variable))) %in% names(data)) {
  #   stop(paste0("'", deparse(substitute(variable)), "' is not the name of a variable in '", deparse(substitute(data)),"'"))
  # }

  variable <- enquo(variable)
  
  # if columns are set, select them here
  if (!quo_is_null(variable)) {
    data <- data %>% 
      select(!!variable)
  }
  
  long_data <- data %>% 
    pivot_longer(cols = everything(),
                 names_to = "my_var",
                 values_to = "value") %>% 
    group_by(my_var) %>% 
    mutate(xfit = seq(min(value, na.rm = TRUE),
                      max(value, na.rm = TRUE),
                      length = n()),
           yfit = dnorm(x = xfit,
                        mean = mean(value, na.rm = TRUE),
                        sd = sd(value, na.rm = TRUE)))
  
  long_data %>% 
    ggplot() +
    # create histograms
    geom_histogram(aes(x = value,
                       y = ..density..,
                       group = my_var),
                   binwidth = function(x) (max(x) - min(x)) / nclass.FD(x)) +
    # add the density fit
    geom_density(aes(x = value,
                     colour = "density fit"),
                 show.legend = FALSE) +
    # add the normal distribution density plot
    geom_line(aes(x = xfit,
                  y = yfit,
                  colour = "normal fit"),
              show.legend = legend) +
    scale_colour_manual(values = c("blue", "red"), 
                        guide = guide_legend(title = NULL)) + 
    theme(axis.title.x = element_blank(),
          legend.position = "bottom") + 
    facet_wrap(~ my_var,
               scales = "free")
}
