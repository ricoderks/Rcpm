summary_fit <- function(mydata){
  mydata.long <- summary_fit_data(mydata);
  p <- ggplot2::ggplot(data=mydata.long, ggplot2::aes(x=PC, y=value, fill=variable));
  p <- p + ggplot2::geom_bar(stat="identity", position="dodge");
  return(p);
}
