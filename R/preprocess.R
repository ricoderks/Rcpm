#' @title Center and scale your data
#' 
#' @description With this function you can center and scale your data.
#' 
#' @param data matrix to preprocess
#' @param center center the data (default is TRUE)
#' @param scale which scaling to use \code{c("none", "uv", "pareto")}
#' @param simple only return the preprocessed matrix. If false also the original 
#' data will be returned as a list. Default is true.
#'
#' @return The centered and scaled matrix will be returned if simple is true. If simple is false also the center and scale vectors are returned.
#' 
#' @export
#' @importFrom stats sd
#'
#' @author Rico Derks
preprocess <- function (data, 
                        center = TRUE, 
                        scale = c("none", "uv", "pareto"), 
                        simple = TRUE) {
  # convert the data to a matrix
	data <- as.matrix(data)
	# check if correct scaling type is selected
	scale <- match.arg(scale)
	
	# do arguments exist, if not set a default value
	if (is.null(center)) {
		center <- FALSE
	}

	if (is.null(simple)) {
		simple <- TRUE
	}

	### centering ###
	if (is.logical(center)) {
		if (center[1]) {
			center <- colMeans(data, na.rm = TRUE)
		} else {
			center <- rep(0, ncol(data))
		}
	}
	
	if (length(center) != ncol(data)) {
		stop("center does not match matrix dimensions!")
	}
	# do the actual centering
	data <- sweep(data, 2, center, "-")

	### scaling ###
		switch(scale,
		       none = { scale <- rep(1, ncol(data)) },
		       uv = { scale <- apply(data, 2, stats::sd, na.rm = TRUE) },
		       pareto = { scale <- sqrt(apply(data, 2, stats::sd, na.rm = TRUE)) }
		)

	# check if the length of scale is the same as the number of columns
	if (length(scale) != ncol(data)) {
		stop("scale does not match matrix dimensions!")
	}
	# do the actual scaling
	data <- sweep(data, 2, scale, "/")

	### return result ###
	if (simple) {
		return(data)
	} else {
		return(list(data = data, center = center, scale = scale))
	}
}
