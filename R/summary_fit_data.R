summary_fit_data <- function(mydata) {
  # input checking
  # mydata needs to be a data frame
  if (!is.data.frame(mydata)) {
    stop("mydata needs to be a data frame!")
  }
  
  # colnames need to be PC, R2cum and Q2cum
  col_names <- c("PC", "R2cum", "Q2cum")
  if (length(which(col_names %in% colnames(mydata))) != 3) {
    stop("mydata should contain only the column names PC, R2cum and Q2cum!")
  }
  
	return(reshape2::melt(mydata, id.vars = "PC"))
}
