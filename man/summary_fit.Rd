\name{summary_fit}
\alias{summary_fit}
\title{show summary of fit of a PCA model}
\usage{
summary_fit(mydata)
}
\arguments{
\item{mydata}{data frame holding all data. See details for}
}
\value{
A ggplot2 plot is returned.
}
\description{
Create a plot which shows a summary of the fit your PCA model.
}
\details{
The data frame mydata should contain 3 variables. \code{PC},
  \code{R2cum} and \code{Q2cum}.
}
\examples{
\dontrun{
   # Here a PCA model (M1) was build with package pcaMethods
   sumfit <- data.frame(PC=paste("PC", 1:5, sep=""), R2cum=M1@R2cum, Q2cum=M1@cvstat);
   p <- summary_fit(sumfit);
}
}
\author{
Rico Derks
}

