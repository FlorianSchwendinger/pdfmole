lead <- function(x, na = NA) c(tail(x, -1), na)


##  ----------------------------------------------------------------------------
#  align_hlines
#  ==========
#' @title Align horizontal lines
#' @description Align the horizontal lines of a dataframe.
#' @param x an object of class dataframe.
#' @param tol a threshold of how many distance can be between line segments
#' which should still be handled as connected.
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a df where all horizontal line segments
#' with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_hlines <- function(x, tol = 1) {
	hl <- x[x$horizontal, ]
	hl <- hl[order(hl$ystart, hl$xstart), ]

	same_row <- (abs(hl$ystart - lead(hl$ystart)) < tol)
	same_end <- (abs(hl$xend - lead(hl$xstart)) < tol)
	
	same_group <- (same_row & same_end)

	hl$group <- c(1, head(cumsum(!same_group) + 1, -1))

	cl <- hl[, list(first(linewidth), first(xstart), first(ystart), last(xend), 
			first(yend), first(horizontal), first(vertical)), group]
	cl[, group := NULL]
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
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a df where all horizontal line segments
#' with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_vlines <- function(x, tol = 1) {
	vl <- x[x$vertical, ]
	vl <- vl[order(vl$xstart, vl$ystart), ]

	same_col <- (abs(vl$xstart - lead(vl$xstart)) < tol)
	same_end <- (abs(vl$yend - lead(vl$ystart)) < tol)
	
	same_group <- (same_col & same_end)

	vl$group <- c(1, head(cumsum(!same_group) + 1, -1))

	cl <- vl[, list(first(linewidth), first(xstart), first(ystart), first(xend), 
			last(yend), first(horizontal), first(vertical)), group]
	cl[, group := NULL]
	colnames(cl) <- colnames(x)
	
	cl
}

##  ----------------------------------------------------------------------------
#  align_lines
#  ==========
#' @title Align all lines
#' @description Align the all (horizontal + vertical) lines of a dataframe.
#' @param x an object of class dataframe.
#' @param tol a threshold of how many distance can be between line segments
#' which should still be handled as connected.
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a df where all line segments (which belong to the same
#' horizontal or vertical) with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_lines <- function(x) {
	rbind(align_hlines(x), align_vlines(x))
}