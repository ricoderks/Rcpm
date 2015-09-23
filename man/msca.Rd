\name{msca}
\alias{msca}
\title{Multilevel Simultaneous Component Analysis (MSCA)}
\usage{
msca(X, persID)
}
\arguments{
\item{X}{a data table with structure ((I x K) x J), see details for more information}

\item{persID}{vector length (IxK) person ids (numeric e.g. for the situation with 4 samples per person 1 1 1 1 2 2 2 2 3 3 3 3, or factor)}
}
\value{
A data frame with the following structure :
       \item{$between$data}{table used for the between-individual level}
       \item{$between$scores}{between-individual scores}
       \item{$between$loadings}{between-individual loadings}
       \item{$between$percexp}{percentage of variation explained in the between-individual model}

       \item{$within$data}{table used for the within-individual level}
       \item{$within$scores}{within-individual scores}
       \item{$within$loadings}{within-individual loadings}
       \item{$within$percexp}{percentage of variation explained in the within-individual model}
}
\details{
X needs to have the following data table structure ((I x K) x J). Where I are the individuals, J are the variables, and K are samples of an individual.

Note: msca can NOT handle NA's!

Based on the algorithm Jansen et al. ANALYTICA CHIMICA ACTA (2005) 530(2):173-183
Based on MatLab code available here http://www.bdagroup.nl/content/Downloads/software/software.php
}
\description{
Multilevel Simultaneous Component Analysis (MSCA)
}
\author{
E.Nevedomskaya
}
\references{
1.Jansen et al. ANALYTICA CHIMICA ACTA (2005) 530(2):173-183
}
