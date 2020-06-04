#' @title Create QC plots
#' 
#' @description Create Shewhary Chart like QC plots for our metabolomics workflow.
#' 
#' @param data Dataframe containing all information. See details for more information on the structure.
#' @param x what to plot on the x-axis, in general something with a time component in it.
#' @param y what to plot on the y-axis, in general something like peak area or signal height.
#' @param color_by color the lines and facet according to this column.
#' @param avg the column with the average per compound.
#' @param stdev the column with the standard deviation per compound.
#' @param rsd the column with the RSD / CV per compound, this will be used in the title of the facets.
#' @param xlabel set the label of the x-axis.
#' @param ylabel set the label of the y-axis.
#'
#' @return A ggplot2 plot is returned.
#' 
#' @details \code{data} should be data frame which contains at least all the parameters as columns. 
#` No calculations are done within the function.
#' 
#' @export
#' @importFrom rlang enquo !!
#' @importFrom dplyr mutate select filter %>%
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap labeller guides labs vars
#' @importFrom methods is
#'
#' @author Rico Derks
#' @examples
#' require(dplyr)
#' set.seed(11)
#' # create a dummy frame 
#' my_data <- data.frame(x = rep(1:8, 5),
#'                       y = rnorm(40, mean = 10),
#'                       group = as.factor(rep(1:5, 8)))
#' # calculate average / sd / rsd                       
#' my_data <- my_data %>%
#'   group_by(group) %>%
#'   mutate(avg = mean(y),
#'          stdev = sd(y),
#'          rsd = stdev / avg * 100)
#' # make the QC plot          
#' p <- qc_plot(data = my_data,
#'              x = x,
#'              y = y,
#'              color_by = group,
#'              avg = avg,
#'              stdev = stdev,
#'              rsd = rsd,
#'              xlabel = "Measurement index",
#'              ylabel = "peak area")
qc_plot <- function(data, x, y, color_by, avg, stdev, rsd, xlabel = "", ylabel = "") {
  # some error checking
  if (length(match.call()) <= 5) {
    stop("Not enough arguments passed... ")
  }
  
  if (!is(data, "data.frame")) {
    stop("'data' does not appear to be a data frame!")
  }
  ## is x a column in the data frame
  if (!deparse(substitute(x)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(x)), "' is not the name of a variable in '",deparse(substitute(data)),"'"))
  }
  ## is y a column in the data frame
  if (!deparse(substitute(y)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(y)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  ## is color_by a column in the data frame
  if (!deparse(substitute(color_by)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(color_by)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  
  ## quotation stuff
  my_df <- enquo(data)
  x <- enquo(x)
  y <- enquo(y)
  color_by <- enquo(color_by)
  avg <- enquo(avg)
  stdev <- enquo(stdev)
  rsd <- enquo(rsd)
  
  ## Create custom labels for the strips of the facets.
  new_labels <- data %>%
    mutate(new_labels = paste(as.character(!!color_by), ":", format(!!rsd, nsmall = 1, digits = 1), "%")) %>%
    select(!!color_by, new_labels) %>%
    unique()
  
  ## And make a named vector of it.
  new_labels <- setNames(new_labels$new_labels, as.character(new_labels[[1]]))
  
  ## create the plot
  p <- data %>%
    # filter out some NA's
    filter(!is.na(!!x),
           !is.na(!!y)) %>% 
    ggplot(aes(x = !!x,
               y = !!y,
               color = !!color_by)) +
    ## average
    geom_line(aes(x = !!x,
                  y = !!avg),
              color = "black",
              linetype = 2) +
    geom_label(aes(x =max(!!x),
                   y = !!avg),
               label = "Avg.",
               size = 3,
               color = "gray") +
    ## sd lines
    geom_line(aes(x = !!x,
                  y = (!!avg + !!stdev)),
              color = "green",
              linetype = 2) +
    geom_line(aes(x = !!x,
                  y = (!!avg - !!stdev)),
              color = "green",
              linetype = 2) +
    geom_label(aes(x = max(!!x),
                   y = (!!avg + 1 * !!stdev)),
               label = "1.sd",
               size = 3,
               color = "gray") +
    geom_line(aes(x = !!x,
                  y = (!!avg + 2 * !!stdev)),
              color = "orange",
              linetype = 2) +
    geom_line(aes(x = !!x,
                  y = (!!avg - 2 * !!stdev)),
              color = "orange",
              linetype = 2) +
    geom_label(aes(x = max(!!(x)),
                   y = (!!avg + 2 * !!stdev)),
               label = "2.sd",
               size = 3,
               color = "gray") +
    geom_line() +
    geom_point() +
    facet_wrap(vars(!!color_by),
               scales = "free_y",
               labeller = labeller(.cols = new_labels)) +
    guides(color = FALSE) +
    labs(title = "QC chart peak area",
         caption = "NOTE: The percentage is the relative standard deviation or CV.",
         x = xlabel,
         y = ylabel)
  
  ## return the ggplot2 object
  return(p)
}
