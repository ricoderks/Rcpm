\name{get_mzxml_spectra}
\alias{get_mzxml_spectra}
\title{Retrieve the spectrums }
\usage{
get_mzxml_spectra(file)
}
\arguments{
\item{file}{mzXML filename}
}
\value{
The function returns a list of spectra. A spectrum is a matrix where the first column is m/z and the second column is intensity.
}
\details{
This is first version for retrieving spectra from mzxml files. For now it only works on MS1 data, there is no error checking. It assumes that the 
presicion is 64bit, endian is network and the compression is zlib. 
}
\description{
Retrieve all MS1 spectra from a mzXML file
}
\author{
Rico Derks
}