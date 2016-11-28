summary_fit <- function(mydata){
  # input checking (is this really nescessary?! This is also done in summary_fit_data)
  # mydata needs to be a data frame
  if (!is.data.frame(mydata)) {
    stop("mydata needs to be a data frame!")
  }
  
  # colnames need to be PC, R2cum and Q2cum
  col_names <- c("PC", "R2cum", "Q2cum")
  if (length(which(col_names %in% colnames(mydata))) != 3) {
    stop("mydata should contain only the column names PC, R2cum and Q2cum!")
  }
  
  # melt the data
  mydata.long <- summary_fit_data(mydata);
  
  # create the plot
  p <- ggplot2::ggplot(data=mydata.long, ggplot2::aes(x=PC, y=value, fill=variable));
  p <- p + ggplot2::geom_bar(stat="identity", position="dodge");
  return(p);
}
