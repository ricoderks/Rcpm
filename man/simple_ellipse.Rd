\name{simple_ellipse}
\alias{simple_ellipse}
\title{Create a simple ellipse for a PCA score plot}
\usage{
simple_ellipse(x, y, alpha = 0.95, len = 200)
}
\arguments{
\item{x}{x vector}

\item{y}{y vector}

\item{alpha}{confidence interval}

\item{len}{number of points to create the ellipse}
}
\value{
A data frame is returned with the points to create the ellipse
}
\description{
This function can be used to create a confidence (Hotteling T2) interval for a
  PCA score plot.
}
\author{
Rico Derks
}

