pca_plots <- function(data, x, y, type = c("scores", "loadings", "summary"), T2 = TRUE, hotelling = 0.95, colour = NULL, shape = NULL, size = 3) {

  type <- match.arg(type)

  # start the plot
  p <- ggplot2::ggplot()
  switch(type,
         scores = {
           if (T2 == TRUE) {
             el <- simple_ellipse(x = data[, x], y = data[, y], alpha = hotelling)
             p <- p + ggplot2::geom_path(data = el, ggplot2::aes(x = x, y = y), colour = "gray")
           }
           p <- p + ggplot2::geom_point(data = data, ggplot2::aes_string(x = x, y = y, colour = colour, shape = shape), size = size)
           p <- p + ggplot2::ggtitle("Scores plot")
         },
         loadings = {
           p <- p + ggplot2::geom_point(data = data, ggplot2::aes_string(x = x, y = y), size = size)
           p <- p + ggplot2::ggtitle("Loadings plot")
         },
         summary = {

         }
  )
  p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "gray")
  p <- p + ggplot2::geom_vline(ggplot2::aes(xintercept = 0), colour = "gray")
  p <- p + ggplot2::xlab(x)
  p <- p + ggplot2::ylab(y)
  return(p)
}
