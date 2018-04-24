.onLoad <- function(libname = find.package("Rcpm"), pkgname = "Rcpm") {
  # CRAN Notes avoidance
  if(getRversion() >= "2.15.1") {
    utils::globalVariables(
      c(# from magrittr
        ".", 
        # one of my own things
        "DotProduct")
    )
  }
  invisible()
}