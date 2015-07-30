#' create PCA plots
#' @description Generate different plots for a PCA analysis. Score plot
#'   (includings Hotelling T2 confidence interval ellipse), loading plot or a
#'   summary plot. The summary plot contains a score plot, loadings plot and the
#'   summary of fit plot.
#' @param mydata Dataframe containing all information. See details for more
#'   information on the structure.
#' @param xPC Principal component for the x-axis.
#' @param yPC Principal component for the y-axis.
#' @param type Which plot to generate: scores, loadings or summary plot.
#' @param T2 add a Hotteling T2 ellipse to the scores plot. Default is true.
#' @param hotelling Set the Hotelling confidence interval. Default = 0.95.
#' @param show.groups color the score plot according to the groups. Default is false.
#' @param group.name the name of the group in the data frame.
#' @return A ggplot2 plot is returned.
#' @details Your data frame should contain the variable names \code{PC1} and
#'   \code{PC2}. For every extra principal component use \code{PC3}, \code{PC4},
#'   etc. In the score plot the scores can be colored by group.name. Set show.groups to true.
#' @author Rico Derks
pca_plots <- function(mydata, xPC=1, yPC=2, type=c("scores", "loadings", "summary"), T2=TRUE, hotelling=0.95, show.groups=FALSE, group.name=NULL, ...) {
  type <- match.arg(type);

  # start the plot
  p <- ggplot2::ggplot();
  switch(type,
         scores = {
           if (T2 == TRUE) {
             el <- simple_ellipse(x=mydata[, paste("PC", xPC, sep="")], y=mydata[, paste("PC", yPC, sep="")], alpha=hotelling);
             p <- p + ggplot2::geom_path(data=el, ggplot2::aes(x=x, y=y), colour="gray")
           }
           if (show.groups == FALSE) {
             p <- p + ggplot2::geom_point(data=mydata, ggplot2::aes_string(x=paste("PC", xPC, sep=""), y=paste("PC", yPC, sep="")), size=3);
           } else {
              if (!is.null(group.name)) {
                p <- p + ggplot2::geom_point(data=mydata, ggplot2::aes_string(x=paste("PC", xPC, sep=""), y=paste("PC", yPC, sep=""), colour=group.name), size=3);
              } else {
                stop("No group name specified!");
              }
           }
           p <- p + ggplot2::ggtitle("Scores plot");
         },
         loadings = {
           p <- p + ggplot2::geom_point(data=mydata, ggplot2::aes_string(x=paste("PC", xPC, sep=""), y=paste("PC", yPC, sep="")), size=3);
           p <- p + ggplot2::ggtitle("Loadings plot");
         },
         summary = {

         }
  );
  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept=0), colour="gray");
  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept=0), colour="gray");
  p <- p + ggplot2::xlab(paste("PC", xPC, sep=" "));
  p <- p + ggplot2::ylab(paste("PC", yPC, sep=" "));
  return(p);
}
