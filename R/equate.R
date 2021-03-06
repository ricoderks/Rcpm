#' @title Quantile equating
#'
#' @description Equating is based on the function *normalize.quantiles* from the library preprocessCore from BioConductor.
#'
#' @param data is a matrix containing all the data, rows correspond to protein, columns correspond to samples.
#' @param sequence is a vector containing the sequence number of each sample.
#' @param test is a boolean (default false) used to run a test if the equating was succesful. No real equating is done, but the original matrix is exported again.
#'
#' @return a matrix with the equated data
#' 
#' @export
#' @importFrom preprocessCore normalize.quantiles
#' @references Draisma, H. H. M. et al. Equating, or Correction for Between-Block Effects with Application to Body Fluid LC-MS 
#' and NMR Metabolomics Data Sets. Anal. Chem. 82, 1039-1046 (2010).
#' @author Rico Derks
equate <- function(data, sequence, test = FALSE) {
	if (requireNamespace("preprocessCore", quietly = TRUE) == TRUE) {

		num.sequences <- max(sequence)				# number of sequences
		# determine number of samples in a sequence
		num.samples <- matrix(ncol = 1, nrow = num.sequences)
		for (i in 1:num.sequences) {
			num.samples[i] <- length(which(sequence == i))
		}
		num.bins <- nrow(data)					# how many bins/variables are there

		# data contains only the area's, columns correspond to samples, rows correspond to features/proteins/variables
		# declare some memory
		results.quantile <- matrix(nrow = num.bins, ncol = sum(num.samples))
		peak.area <- matrix(ncol = 1, nrow = num.bins * max(num.samples) * num.sequences)
		peak.area.quantile <- matrix(ncol = 1, nrow = num.bins * max(num.samples) * num.sequences)
		dim(peak.area) <- c(num.bins, max(num.samples), num.sequences)
		dim(peak.area.quantile) <- c(num.bins, max(num.samples), num.sequences)

		# restructure the data into a 3-dimensional array
		# per compound a matrix is made with samples and sequences
		samp.in.seq <- matrix(ncol = num.sequences, nrow = sum(num.samples))
		
		for (a in 1:max(sequence)) {	# this is slow so keep it outside large loops
			samp.in.seq[, a] <- sequence == a
		}
		for (c in 1:num.bins) {
			for (a in 1:max(sequence)) {
				peak.area[c, 1:num.samples[a], a] <- data[c, samp.in.seq[, a]]
			}
		}

		# quantile equate per compound
		for (a in 1:num.bins) {
			if (test == FALSE) {
				peak.area.quantile[a, , ] <- preprocessCore::normalize.quantiles(peak.area[a, , ])		# this one does the quating
			} else {
				peak.area.quantile[a, , ] <- peak.area[a, , ]				# check if everything is ok
			}
		}
		# restructure back to a 2-dimsional array (table) so it is suited for export
		for (c in 1:num.bins) {
			for (a in 1:max(sequence)) {
					results.quantile[c, samp.in.seq[, a]] <- peak.area.quantile[c, 1:num.samples[a], a]
			}
		}

		# get row names if present
		if (!is.null(rownames(data))) {
			rownames(results.quantile) <- rownames(data)
		}

		# return results
		equate <- results.quantile
	} else {
		cat("Could not load library preprocessCore.\nInstall from BioConductor")
	}
}
