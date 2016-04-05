\name{check_alignment}
\alias{check_alignment}
\title{Check the aligned mzXML files}
\usage{
check_alignment(files)
}
\arguments{
\item{files}{A character vector containing all the mzXML file locations.}
}
\value{
The function gives a message with the file name of the problematic mzXML file.
}
\details{
msalign2 sometimes creates wrong retention times. The retention time of a scan is sometimes
smaller then its previous scan. 
}
\value{
Returns true or false.
}
\description{
Check the aligned mzXML files for problematic retention times
}
\author{
Rico Derks
}