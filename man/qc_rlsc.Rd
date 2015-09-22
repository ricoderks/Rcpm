\name{qc_rlsc}
\alias{qc_rlsc}
\title{Perform QC-RLSC for batch correction of chromatographic signal}
\usage{
qc_rlsc(tab, colv, or)
}
\arguments{
\item{tab}{table N*K (row * column) with N samples and K variables}

\item{colv}{vector N of numbers: 1 for QC samples and 2 for other samples}

\item{or}{vector of measuring order (see details)}
}
\value{
corrected table N*K
}
\description{
Perform QC-RLSC for batch correction of chromatographic signal
}
\details{
Make sure that everything is sorted in measurement order!!!
}
\author{
E. Nevedomskaya
}
\references{
1.Dunn et al. Nature Protocols 6, 1060-1083 (2011)
}

