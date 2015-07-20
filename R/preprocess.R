preprocess <- function (data, center=TRUE, scale=c("none", "uv", "pareto"), reverse=FALSE, simple=TRUE) {
	data <- as.matrix(data);
	# do arguments exist, if not set a default value
	if (is.null(center)) {
		center <- FALSE;
	}
	if (is.null(scale)) {
		scale <- "none";
	}
	if (is.null(reverse)) {
		reverse <- FALSE;
	}
	if (is.null(simple)) {
		simple <- TRUE;
	}

	### centering ###
	if (is.logical(center)) {
		if (center[1]) {
			center <- colMeans(data, na.rm=TRUE);
		} else {
			center <- rep(0, ncol(data));
		}
	}
	if (length(center) != ncol(data)) {
		stop("center does not match matrix dimensions!");
	}
	if (!reverse) {
		data <- sweep(data, 2, center, "-");
	}

	### scaling ###
	if (is.character(scale[1])) {
		scale <- match.arg(scale);
		if (scale == "none") {
			scale <- rep(1, ncol(data));
		} else if (scale == "uv") {
			scale <- apply(data, 2, sd, na.rm=TRUE);
		} else if (scale == "pareto") {
			scale <- sqrt(apply(data, 2, sd, na.rm=TRUE));
		}
	}
	if (length(scale) != ncol(data)) {
		stop("scale does not match matrix dimensions!");
	}
	if (!reverse) {
		data <- sweep(data, 2, scale, "/");
	}

	### reverse ###
	if (reverse) {
		data <- sweep(data, 2, scale, "*");
		data <- sweep(data, 2, center, "+");
	}

	### return result ###
	if (simple) {
		return(data);
	} else {
		return(list(data=data, center=center, scale=scale));
	}
}
