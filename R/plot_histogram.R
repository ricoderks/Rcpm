plot_histogram <- function(DataSet, Metabolite = NULL, Legend = TRUE) {
  if (is.null(Metabolite)) {
    # calculate bin width according to Freedman-Diaconis rule for eache metabolite
    bw <- lapply(DataSet, function(x) (max(x) - min(x)) / nclass.FD(x))

    # create function to fit
    fit_func <- function(x) {
      xfit <- seq(min(x, na.rm = TRUE),	max(x, na.rm = TRUE),	length = 100)
      yfit <- dnorm(xfit,	mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
      return(data.frame(xfit = xfit, yfit = yfit))
    }
    fit <- lapply(DataSet, fit_func)
    fit_melt <- reshape2::melt(fit, id.vars = c("xfit", "yfit"))
    # melt gives it name L1, change to variable for ggplot
    colnames(fit_melt)[3] <- "variable"
    
    FAdata_melt <- reshape2::melt(DataSet,
                                  measure.vars = colnames(DataSet))

    # get the names of the metabolites
    met_names <- names(bw)
    
    h <- ggplot2::ggplot()
    # to create histograms with different binwidth they need to be in different layers
    for (a in 1:length(met_names)) {
      h <- h + ggplot2::geom_histogram(data = subset(FAdata_melt, variable == met_names[a]), 
                                       ggplot2::aes(x = value, 
                                                    y = ..density..), 
                                       binwidth = bw[[a]])
    }
    h <- h + ggplot2::geom_density(data = FAdata_melt, 
                                   ggplot2::aes(x = value, 
                                                colour = "density fit"), 
                                   show.legend = FALSE)
    h <- h + ggplot2::geom_line(data = fit_melt, 
                                ggplot2::aes(x = xfit, 
                                             y = yfit, 
                                             colour = "normal fit"), 
                                show.legend = Legend)
    h <- h + ggplot2::scale_colour_manual(values = c("blue", "red"), 
                                          guide = ggplot2::guide_legend(title = NULL))
    h <- h + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    h <- h + ggplot2::facet_wrap(~ variable, 
                                 scales = "free", 
                                 ncol = 2)
  } else {
    x <- as.matrix(subset(DataSet, select = Metabolite))
    # fit a density plot
    xfit <- seq(min(x, na.rm = TRUE),	
                max(x, na.rm = TRUE),	
                length = 100)
    yfit <- dnorm(xfit,	
                  mean = mean(x, na.rm = TRUE), 
                  sd = sd(x, na.rm = TRUE))
    fit <- data.frame(xfit = xfit, 
                      yfit = yfit)
    # density plot for normal distribution
    d <- density(x[complete.cases(x)], 
                 from = min(x[complete.cases(x)]), 
                 to = max(x[complete.cases(x)]))
    
    # calculate bin width according to Freedman-Diaconis rule
    bw <- (max(x) - min(x)) / nclass.FD(x)

    h <- ggplot2::ggplot()
    h <- h + ggplot2::geom_histogram(data = DataSet, 
                                     ggplot2::aes_string(x = Metabolite, 
                                                         y = "..density.."), 
                                     binwidth = bw)
    h <- h + ggplot2::geom_density(data = DataSet, 
                                   ggplot2::aes_string(x = Metabolite, 
                                                       colour = shQuote("density fit")), 
                                   show.legend = FALSE)
    h <- h + ggplot2::geom_line(data = fit, 
                                ggplot2::aes(x = xfit, 
                                             y = yfit, 
                                             colour = "normal fit"), 
                                show.legend = Legend)
    h <- h + ggplot2::scale_colour_manual(values = c("blue", "red"), 
                                          guide = ggplot2::guide_legend(title = NULL))
  }
  return(h)
}
