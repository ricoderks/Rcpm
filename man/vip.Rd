\name{vip}
\alias{vip}
\title{Implementation of VIP (Variable Importance in Projection) for the pls package}
\usage{
vip(object)
}
\arguments{
\item{object}{a pls object}
}
\value{
vip returns all VIP values for all variables and all number of components
}
\description{
vip is an implementation of the Variance Importance in Projection, as described in Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of some variable selection methods when multicollinearity is present, Chemometrics and Intelligent Laboratory Systems 78, 103-112. It currently only works with single-response ortghogonal scores plsr models.
}
\author{
Bjorn-Helge Mevik
}
\references{
http://mevik.net/work/software/pls.html

As described in Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of some variable selection methods when multicollinearity is present, Chemometrics and Intelligent Laboratory Systems 78, 103-112.
}

