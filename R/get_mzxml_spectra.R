#' @title Retrieve MS1 spectra from a mzXML file
#'
#' @description Retrieve all MS1 spectra from a mzXML file.
#'
#' @param file mzXML file
#'
#' @return The function returns a list of spectra. A spectrum is a matrix where the first column is \emph{m/z} and the second column is intensity.
#' @details This is first version for retrieving spectra from mzXML files. For now it only works on MS1 data, there is no error checking. It assumes that the 
#' presicion is 64bit, endian is network and the compression is zlib. 
#' 
#' @export
#' @importFrom xml2 read_xml xml_text xml_find_all xml_ns
#' @importFrom base64enc base64decode
#' 
#' @author Rico Derks
get_mzxml_spectra <- function(file) {
	data_xml <- xml2::read_xml(file)
	# give the namespace and use it to find the peaks
	scans <- xml2::xml_text(xml2::xml_find_all(x = data_xml, 
	                                           xpath = "//d1:peaks", 
	                                           ns = xml2::xml_ns(data_xml)))

	# initialize list to hold all the scans (MS1)
	spectra <- list()
	
	for (a in 1:length(scans)) {
		# the order of these steps I got from the package readMzXmlData!
		# first base64decode 
		scan_decode <- base64enc::base64decode(scans[a])
		# decompress, for zlib use gzip
		scan_decom <- memDecompress(from = scan_decode, 
		                            type = "gzip")
		# read binary data and convert to readable data :-)
		scan_raw <- readBin(scan_decom, 
		                    what = "double", 
		                    n = length(scan_decom) %/% 8, 
		                    size = 8, 
		                    signed = TRUE, 
		                    endian = "big")
		# scan contains the m/z and intensity as :
		# m/z intensity m/z intensity etc.
		spectra[[a]] <- matrix(scan_raw, 
		                       ncol = 2, 
		                       byrow = TRUE, 
		                       dimnames = list(1:(length(scan_raw) / 2), 
		                                       c("Mz", "Intensity"))) # first column  =  m/z, second column  =  intensity
	}
	return(spectra)
}