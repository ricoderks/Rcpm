\name{preprocess}
\alias{preprocess}
\title{Center and scale your data}
\usage{
preprocess(data, center = TRUE, scale = c("none", "uv", "pareto"),
  reverse = FALSE, simple = TRUE)
}
\arguments{
\item{data}{matrix to preprocess}

\item{center}{center the data (default is TRUE)}

\item{scale}{which scaling to use \code{c("none", "uv", "pareto")}}

\item{reverse}{reverse the scaling \\ centering. Default is false.}

\item{simple}{only return the preprocessed matrix. If false also the original
data will be returned as a list. Default is true.}
}
\value{
The centered and scaled matrix will be returned.
}
\description{
With this function you can center and scale your data.
}

