.onLoad <- function(libname = find.package("Rcpm"), pkgname = "Rcpm") {
  # CRAN Notes avoidance
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(# from magrittr
        ".", 
        ## Some of my own things
        # bubble_lot
        "DotProduct",
        # plot_histogram
        "my_var",
        "value",
        "..density..",
        "xfit",
        "yfit",
        # summary_fit
        "PC",
        "value",
        "variable",
        "R2cum",
        "Q2cum")
    )
  }
  invisible()
}