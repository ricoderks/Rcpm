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
#' @examples 
#` my_data <- structure(list(LipidName = c("FA 16:1; [M-H]-", "FA 16:0; [M-H]-", 
#` "FA 17:1; [M-H]-", "FA 17:0; [M-H]-", "FA 18:2; [M-H]-", "FA 18:1; [M-H]-", 
#` "FA 18:0; [M-H]-", "FA 20:4; [M-H]-", "FA 20:2; [M-H]-", "FA 20:1; [M-H]-", 
#` "FA 20:0; [M-H]-", "FA 22:7; [M-H]-", "FA 22:7; [M-H]-", "FA 22:6; [M-H]-", 
#` "FA 22:4; [M-H]-", "FA 22:3; [M-H]-", "FA 22:1; [M-H]-", "FA 16:1; [M-H]-", 
#` "FA 16:0; [M-H]-", "FA 17:1; [M-H]-", "FA 17:0; [M-H]-", "FA 18:2; [M-H]-", 
#` "FA 18:1; [M-H]-", "FA 18:0; [M-H]-", "FA 20:4; [M-H]-", "FA 20:2; [M-H]-", 
#` "FA 20:1; [M-H]-", "FA 20:0; [M-H]-", "FA 22:7; [M-H]-", "FA 22:7; [M-H]-", 
#` "FA 22:6; [M-H]-", "FA 22:4; [M-H]-", "FA 22:3; [M-H]-", "FA 22:1; [M-H]-"),
#` LipidClass = c("FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", 
#` "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", 
#` "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", "FA", 
#` "FA", "FA", "FA", "FA"),
#` ShortLipidName = c("FA 16:1", "FA 16:0", 
#` "FA 17:1", "FA 17:0", "FA 18:2", "FA 18:1", "FA 18:0", "FA 20:4", 
#` "FA 20:2", "FA 20:1", "FA 20:0", "FA 22:7", "FA 22:7", "FA 22:6", 
#` "FA 22:4", "FA 22:3", "FA 22:1", "FA 16:1", "FA 16:0", "FA 17:1", 
#` "FA 17:0", "FA 18:2", "FA 18:1", "FA 18:0", "FA 20:4", "FA 20:2", 
#` "FA 20:1", "FA 20:0", "FA 22:7", "FA 22:7", "FA 22:6", "FA 22:4", 
#` "FA 22:3", "FA 22:1"), 
#` LongLipidName = c("FA 16:1", "FA 16:0", 
#` "FA 17:1", "FA 17:0", "FA 18:2", "FA 18:1", "FA 18:0", "FA 20:4", 
#` "FA 20:2", "FA 20:1", "FA 20:0", "FA 22:7", "FA 22:7", "FA 22:6", 
#` "FA 22:4", "FA 22:3", "FA 22:1", "FA 16:1", "FA 16:0", "FA 17:1", 
#` "FA 17:0", "FA 18:2", "FA 18:1", "FA 18:0", "FA 20:4", "FA 20:2", 
#` "FA 20:1", "FA 20:0", "FA 22:7", "FA 22:7", "FA 22:6", "FA 22:4", 
#` "FA 22:3", "FA 22:1"), 
#` versus = structure(c(1L, 1L, 1L, 1L, 1L, 
#` 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
#` 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), 
#` .Label = c("Fraction 3 vs Fraction 5", 
#` "Fraction 4 vs Fraction 5"), 
#` class = "factor"), 
#` difference = c(77957.9,  1189200, -4187.35, 26.6800000000003, 12450.92, 1817329, 2183241, 
#` 2616300, 1276.5, -4171.5, 3428.03, -224791, 23952.7487, 19128138, 
#` 1342603, 5555.65, 9585.93, -121608.1723, -2217825.7, -8818.316, 
#` -24195.53, -88400.4798, -3598409.7, -3971887.5, -23671620, -58950.298, 
#` -188427.61, -19481.2875, -786391.7, -108.66547, -7209072.9, -1208392.3, 
#` -18368.65343, -22453.98234),
#` diff_grp = structure(c(2L, 2L, 1L, 
#` 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 
#` 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
#` .Label = c("low", 
#` "high"), 
#` class = "factor")), 
#` class = "data.frame", 
#` row.names = c(NA, -34L), 
#` .Names = c("LipidName", "LipidClass", "ShortLipidName", 
#` "LongLipidName", "versus", "difference", "diff_grp"))
#' # create the diff plot
#' diff_plot(data = my_data,
#'             x = LongLipidName,
#'             y = difference,
#'             fill_by = diff_grp,
#'             facet_y = LipidClass,
#'             facet_x = versus)
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