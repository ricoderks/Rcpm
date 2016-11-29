get_mzxml_spectra <- function(file) {
	data_xml <- xml2::read_xml(file)
	# give the namespace and use it to find the peaks
	scans <- xml2::xml_text(xml2::xml_find_all(data_xml, "//d1:peaks", ns = xml2::xml_ns(data_xml)))

	# initialize list to hold all the scans (MS1)
	spectra <- list()
	
	for (a in 1:length(scans)) {
		# the order of these steps I got from the package readMzXmlData!
		# first base64decode 
		scan_decode <- base64enc::base64decode(scans[a])
		# decompress, for zlib use gzip
		scan_decom <- memDecompress(from = scan_decode, type = "gzip")
		# read binary data and convert to readable data :-)
		scan_raw <- readBin(scan_decom, 
		                    what = "double", 
		                    n = length(scan_decom)%/%8, 
		                    size = 8, 
		                    signed = TRUE, 
		                    endian = "big")
		# scan contains the m/z and intensity as :
		# m/z intensity m/z intensity etc.
		spectra[[a]] <- matrix(scan_raw, 
		                       ncol = 2, 
		                       byrow = TRUE, 
		                       dimnames = list(1:(length(scan_raw)/2), 
		                                       c("Mz", "Intensity"))) # first column  =  m/z, second column  =  intensity
	}
	return(spectra)
}