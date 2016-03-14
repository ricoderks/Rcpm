flip_peaklist <- function(peaklist, xset) {
	# xcms peaklist can be from the functions diffreport (11 columns) or peakTable (7 columns)
	# columns are added depending on the number of classes
	# peakTable in general used if there are no classes, if there is no class 1 column is added
	# check if peaklist is data frame and xset is xcmsSet object
	# if more then 2 classes are used diffreport gives extra column anova
	
	# simple sanitation
	if (!is.data.frame(peaklist)) {
		stop("Peaklist is not a data frame")
	}
	if (class(xset) != "xcmsSet") {
		stop("xset is not a xcmsSet object")
	}
	
	# get the number of classes
	nclass <- length(levels(xset@phenoData$class))
	
	# how many columns are there besides the real data
	if (length(which(colnames(peaklist) == "anova")) >= 1 | length(which(colnames(peaklist) == "pvalue")) >= 1) {
		if (length(which(colnames(peaklist) == "anova")) >= 1) {
			# diffreport was used with more then 2 classes
			ncols <- 12 + nclass
		} else {
			# diffreport was used with 2 classes
			ncols <- 11 + nclass
		}
		var_names <- peaklist$name
	} else {
		# peakTable function was used
		ncols <- 7 + nclass
		var_names <- xcms::groupnames(xset)
	}
	# transpose the data
	# rownames are now observation names
	peaklist_t <- t(peaklist[, -c(1:ncols)])
	colnames(peaklist_t) <- var_names
	
	peaklist_t <- data.frame(peaklist_t)
	return(peaklist_t)
}