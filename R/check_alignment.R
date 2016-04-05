check_alignment <- function(files) {
	# sanity check
	if ((is.vector(files) && is.list(files)) && !is.character(files)) {
		stop("files is not a character vector!!")
	}
	# check if files exist
	if (!all(file.exists(files))) {
		stop("File doesn't exist!")
	}
	# assume all is fine
	all_fine <- TRUE
	cat("Checking....")
	for (a in 1:length(files)) {
		tmp <- xml2::read_xml(files[a])
		all_rt <- xml2::xml_text(xml2::xml_find_all(tmp, "//@retentionTime"))
		all_rt_num <- as.numeric(regmatches(m=regexpr(pattern="[0-9]{1,3}.[0-9]{1,7}", text=all_rt), x=all_rt))
		if (length(which(diff(all_rt_num) >= 0)) < (length(all_rt_num) - 1)) {
			cat("Something wrong with : ", files[a], "\n"); flush.console()
			all_fine <- FALSE
		}
	}
	cat("....done\n")
	return(all_fine)
}