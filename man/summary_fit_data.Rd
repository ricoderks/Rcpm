\name{summary_fit_data}
\alias{summary_fit_data}
\title{Reshape data frame for the summary_fit function.}
\usage{
summary_fit_data(mydata)
}
\arguments{
\item{mydata}{The data frame to be reshaped to a long data fram}
}
\value{
reshaped data frame. A long data frame is created.
}
\description{
This functions reshapes the data frame for making a bar plot
  graph with ggplot2.
}
\details{
It is not nescessary to call this function. This function is called
  from summary_fit. The reshaping is done with the \link{melt} function from the
  package reshape2.
}
\author{
Rico Derks
}

