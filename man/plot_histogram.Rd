\name{plot_histogram}
\alias{plot_histogram}
\title{plot nice histogram}
\usage{
plot_histogram(DataSet, Metabolite = NULL, Legend = TRUE)
}
\arguments{
\item{DataSet}{is a dataframe containing all the data}

\item{Metabolite}{is a character containing the name of the metabolite (see details).}

\item{Legend}{is a boolean (default true) used to show the legend.}
}
\value{
ggplot showing the nice histogram
}
\description{
plot_histogram plots a histogram with a fitted kernel density plot and a normal density plot. If no \code{Metabolite} (i.e. NULL) is defined for each column in the dataframe a histogram is plotted.
}
\author{
Rico Derks
}
\example{
df <- data.frame(a=rnorm(100, mean=10), b=rnorm(100, mean=20))
plot_histogram(DataSet=df)
}

