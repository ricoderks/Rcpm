\name{m_pow_x}
\alias{m_pow_x}
\title{Modeling power}
\usage{
m_pow_x(data, pca_model)
}
\arguments{
\item{data}{is a matrix of the original data (centered and scaled)}

\item{pca_model}{is the pca model from the package pcaMethods}

\value{
vector of the modeling power for each variable
}
\description{
Calculates the modeling power of each variable. This is from Simca.
}
\author{
Rico Derks
}