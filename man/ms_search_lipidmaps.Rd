\name{ms_search_lipidmaps}

\alias{ms_search_lipidmaps}

\title{Do a MS search on the LMSD database from LipidMaps }

\usage{
  ms_search_lipidmaps(ExactMass = NULL, ExactMassOffSet = 0.01, Name = NULL, Formula = NULL, CoreClass = NULL)
}
\arguments{
  \item{ExactMass}{Tthe exact mass to search.}
  \item{ExactMassOffSet}{The search window in dalton.}
  \item{Name}{Optional parameter to filter the search results. Name is search in any part of common name.}
  \item{Formula}{Optional parameter to filter the search results. Formula is search in any part of formula.}
  \item{CoreClass}{Optional parameter to filter the search results. At the moment there are 8 core classes defined (1-8). See detail section for more information.}
}

\value{
  The function returns a dataframe with the search results. If there is a connection problem NULL is returned and a warning message showing the status code of the connection.
}

\details{
  This function uses the Programmatic Access functionality of LipidMaps to search the LMSD database. 
  There are 8 core classes defined :
  \describe{
    \item{1:}{Fatty Acids [FA]}
    \item{2:}{Glycerolipids [GL]}
    \item{3:}{Glycerophospholipids [GP]}
    \item{4:}{Sphingolipids [SP]}
    \item{5:}{Sterol Lipids [ST]}
    \item{6:}{Prenol Lipids [PR]}
    \item{7:}{Saccharolipids [SL]}
    \item{8:}{Polyketides [PK]}
  }
}

\description{
  Search the LMSD database from LipidMaps (http://www.lipidmaps.org/).
}

\author{
  Rico Derks
}

\examples{
results <- ms_search_lipidmaps(ExactMass = 537.37944, ExactMassOffSet = 0.01)
}