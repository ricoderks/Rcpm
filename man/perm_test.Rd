\name{perm_test}
\alias{perm_test}
\title{Permutation test for PLS and PLS-DA models}
\usage{
perm_test(pls_model, perm=200, scale=FALSE)
}
\arguments{
\item{pls_model}{The PLS or PLS-DA model created by the pls package}
\item{perm}{number of permutations (default = 200)}
\item{scale}{should scaling be used. Use the same settings as used in the original model (see details).}
}
\description{
Permutation test for PLS and PLS-DA models. The Y variable is permutated and a model is then fitted. From this model the R2Ycum and Q2cum are calculated.
}
\details{
The scaling for the PLS / PLS-DA model has to be given to the function. At the moment this can not be retrieved from the original model.
}
\value{
Output is a plot showing the results.
}
\author{
Rico Derks
}