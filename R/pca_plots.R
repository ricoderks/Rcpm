#' create PCA plots
#' @description Generate different plots for a PCA analysis. Score plot
#'   (includings Hotelling T2 confidence interval ellipse), loading plot or a
#'   summary plot. The summary plot contains a score plot, loadings plot and the
#'   summary of fit plot.
#' @param data Dataframe containing all information. See details for more
#'   information on the structure.
#' @param xPC Principal component for the x-axis.
#' @param yPC Principal component for the y-axis.
#' @param type Which plot to generate: scores, loadings or summary plot.
#' @param T2 add a Hotteling T2 ellipse to the scores plot. Default is true.
#' @param hotelling Set the Hotelling confidence interval. Default = 0.95.
#' @details Your data frame should contain the variable names \code{pc1} and \code{pc2}.
#' @author Rico Derks
pca_plots <- function(data, xPC=1, yPC=2, type=c("scores", "loadings", "summary"), T2=TRUE, hotelling=0.95, show.groups=FALSE, group.names, group.ids, ...) {
  type <- match.arg(type);
  if (is.null(type)) {
    type <- "summary";
  }

  switch(type,
         scores = {
           p <- ggplot();
           p <- p + geom_point(data=data, aes_string(x=paste("pc", xPC, sep=""), y=paste("pc", yPC, sep="")), size=3);
           p <- p + geom_hline(aes(yintercept=0), colour="gray");
           p <- p + geom_vline(aes(xintercept=0), colour="gray");
           if (T2 == TRUE) {
             el <- simple_ellipse(x=data[, paste("pc", xPC, sep="")], y=data[, paste("pc", yPC, sep="")], alpha=hotelling);
             p <- p + geom_path(data=el, aes(x=x, y=y), colour="gray")
           }
         },
         loadings = {

         },
         summary = {

         }
  );
  return(p);
}
