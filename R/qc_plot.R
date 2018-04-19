#' @title Create shewart chart like plots
#'
#' @description Create Shewart chart like plots
#'
#' @param data the dataframe containing the lipids, normally imported results from MS-DIAL
#' @param x what do you want on the x-axis, often the retetnion time
#' @param y what do you want on the y-axis, often the retetnion time
#' @param color_by color used if you have multiple variables to show, it will also be used for facetting
#' @param avg column which contains the average values
#' @param stdev column which contains the standard devation values
#' @param rsd column which contains the values for the RSD / CV
#' @param xlabel define the label for the x-axis
#' @param ylabel define the label for the y-axis
#'
#' @return A ggplot2 object
#' 
#' @details This function will give facetted Shewart chart like plots. With color_by each 
#' line (variable) will get a different color. This will also used for facetting. 
#' This function is still heavily under development and for sure will change in the future. 
#' This function assumes the ggplot2 > v2.2.1.9000 is installed.
#' 
#' @export
#' @importFrom rlang enquo !!
#' @importFrom dplyr mutate select filter
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_label facet_wrap labeller labs xlab ylab ggtitle guides vars
#' @importFrom stats setNames
#'
#' @author Rico Derks
qc_plot <- function(data, x, y, color_by, avg, stdev, rsd, xlabel = "", ylabel = "") {
  ## some error checking
  if (length(match.call()) <= 9) {
    stop("Not enough arguments passed... ")
  }
  
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
  ## is color_by a column in the dataframe
  if (!deparse(substitute(color_by)) %in% names(data)) {
    stop(paste0("'", deparse(substitute(color_by)), "' is not the name of a variable in '", deparse(substitute(data)), "'"))
  }
  
  ## quotation stuff
  my_df <- rlang::enquo(data)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_by <- rlang::enquo(color_by)
  avg <- rlang::enquo(avg)
  stdev <- rlang::enquo(stdev)
  rsd <- rlang::enquo(rsd)
  
  new_labels <- data %>%
    dplyr::mutate(new_labels = paste(rlang::`!!`(color_by), ":", format(area_rsd, nsmall = 1, digits = 1), "%")) %>%
    dplyr::select(rlang::`!!`(color_by), new_labels) %>%
    unique()
  
  ## and make a named vector of it
  new_labels <- setNames(new_labels$new_labels, new_labels[, 1])
  
  ## create the plot (at the moment this uses the development version of ggplot2 2.2.1.9000)
  p <- data %>%
    # filter out some NA's
    dplyr::filter(!is.na(rlang::`!!`(x)),
                  !is.na(rlang::`!!`(y))) %>% 
    ggplot2::ggplot(ggplot2::aes(x = rlang::`!!`(x),
                                 y = rlang::`!!`(y),
                                 color = rlang::`!!`(color_by))) +
    ## average
    ggplot2::geom_line(ggplot2::aes(x = rlang::`!!`(x),
                                    y = rlang::`!!`(avg)),
                       color = "black",
                       linetype = 2) +
    ggplot2::geom_label(ggplot2::aes(x =max(rlang::`!!`(x)),
                                     y = rlang::`!!`(avg)),
                        label = "Avg.",
                        size = 3,
                        color = "gray") +
    ## sd lines
    ggplot2::geom_line(ggplot2::aes(x = rlang::`!!`(x),
                                    y = (rlang::`!!`(avg) + rlang::`!!`(stdev))),
                       color = "green",
                       linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(x = rlang::`!!`(x),
                                    y = (rlang::`!!`(avg) - rlang::`!!`(stdev))),
                       color = "green",
                       linetype = 2) +
    ggplot2::geom_label(ggplot2::aes(x = max(rlang::`!!`(x)),
                                     y = (rlang::`!!`(avg) + 1 * rlang::`!!`(stdev))),
                        label = "1.sd",
                        size = 3,
                        color = "gray") +
    ggplot2::geom_line(ggplot2::aes(x = rlang::`!!`(x),
                                    y = (rlang::`!!`(avg) + 2 * rlang::`!!`(stdev))),
                       color = "orange",
                       linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(x = rlang::`!!`(x),
                                    y = (rlang::`!!`(avg) - 2 * rlang::`!!`(stdev))),
                       color = "orange",
                       linetype = 2) +
    ggplot2::geom_label(ggplot2::aes(x = max(rlang::`!!`(x)),
                                     y = (rlang::`!!`(avg) + 2 * rlang::`!!`(stdev))),
                        label = "2.sd",
                        size = 3,
                        color = "gray") +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(ggplot2::vars(rlang::`!!`(color_by)),
                        scales = "free_y",
                        labeller = ggplot2::labeller(.cols = new_labels)) +
    ggplot2::guides(color = FALSE) +
    ggplot2::ggtitle("QC chart peak area") +
    ggplot2::labs(caption = "NOTE: The percentage is the relative standard deviation or CV.") +
    ggplot2::xlab(xlabel) +
    ggplot2::ylab(ylabel)
  
  ## return the ggplot2 object
  return(p)
}