lead <- function(x, na = NA) c(tail(x, -1), na)


##  ----------------------------------------------------------------------------
#  align_hlines
#  ==========
#' @title Align horizontal lines
#' @description Align the horizontal lines of a dataframe.
#' @param x an object of class dataframe.
#' @param tol a threshold of how many distance can be between line segments
#' which should still be handled as connected.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return Returns a df where all horizontal line segments
#' with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_hlines <- function(x, tol = 1, ...) {
	if (!requireNamespace("data.table", quietly = TRUE)) {
    	stop("Package \"data.table\" needed for this function to work. 
    		Please install it.",
      	call. = FALSE)
  	}
  	
	hl <- x[x$horizontal, ]
	hl <- hl[order(hl$y0, hl$x0), ]

	same_row <- (abs(hl$y0 - lead(hl$y0)) < tol)
	same_end <- (abs(hl$x1 - lead(hl$x0)) < tol)
	
	same_group <- (same_row & same_end)

	hl$group <- c(1, head(cumsum(!same_group) + 1, -1))

	pid <- NULL
	linewidth <- NULL
	x0 <- NULL
	x1 <- NULL
	y0 <- NULL
	y1 <- NULL
	horizontal <- NULL
	vertical <- NULL
	group <- NULL


	cl <- hl[, 
	list(data.table::first(pid), data.table::first(linewidth), 
		 data.table::first(x0), data.table::first(y0), data.table::last(x1), 
		 data.table::first(y1), data.table::first(horizontal), 
		 data.table::first(vertical)), 
	group]
	cl$group = NULL
	colnames(cl) <- colnames(x)
	
	cl
}

##  ----------------------------------------------------------------------------
#  align_vlines
#  ==========
#' @title Align vertical lines
#' @description Align the vertical lines of a dataframe.
#' @param x an object of class dataframe.
#' @param tol a threshold of how many distance can be between line segments
#' which should still be handled as connected.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return Returns a df where all vertical line segments
#' with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_vlines <- function(x, tol = 1, ...) {
	if (!requireNamespace("data.table", quietly = TRUE)) {
    	stop("Package \"data.table\" needed for this function to work. 
    		Please install it.",
      	call. = FALSE)
  	}

	vl <- x[x$vertical, ]
	vl <- vl[order(vl$x0, vl$y0), ]

	same_col <- (abs(vl$x0 - lead(vl$x0)) < tol)
	same_end <- (abs(vl$y1 - lead(vl$y0)) < tol)
	
	same_group <- (same_col & same_end)

	vl$group <- c(1, head(cumsum(!same_group) + 1, -1))

	pid <- NULL
	linewidth <- NULL
	x0 <- NULL
	x1 <- NULL
	y0 <- NULL
	y1 <- NULL
	horizontal <- NULL
	vertical <- NULL
	group <- NULL


	cl <- vl[, 
	list(data.table::first(pid), data.table::first(linewidth), 
		 data.table::first(x0), data.table::first(y0), data.table::last(x1), 
		 data.table::first(y1), data.table::first(horizontal), 
		 data.table::first(vertical)), 
	group]
	cl$group <- NULL
	colnames(cl) <- colnames(x)
	
	cl
}

##  ----------------------------------------------------------------------------
#  align_lines
#  ==========
#' @title Align all lines
#' @description Align the all (horizontal + vertical) lines of a dataframe.
#' @param x an object of class dataframe.
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a df where all line segments (which belong to the same
#' horizontal or vertical) with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_lines <- function(x, ...) {
	rbind(align_hlines(x), align_vlines(x))
}