#' @importFrom graphics text abline

##  ----------------------------------------------------------------------------
#  textplot
#  ============
#' @title textplot
#' @description TODO
#' @param x an object inheriting from \code{data.frame}.
#' @param split_points an vector containing the x-coordinates for splitting
#' the page.
#' @param page the number of the page which shall be plotted.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export
##  ----------------------------------------------------------------------------

textplot <- function(x, split_points = NULL, page = 0L) UseMethod("textplot", x)

textplot.pdf_document <- function(x, split_points = NULL, page = 0L) {
	x <- rm_na(as.data.frame(x))

	stopifnot(page %in% x$page)
	x <- x[x$page == page, ]
	
	stopifnot(any(c("xstart", "xend", "ystart", "yend") %in% colnames(x)))
	v <- as.numeric(unlist(x[, c('xstart','xend')]))

	plot(	c(	min(x$xstart, na.rm = TRUE), 
				max(x$xend, na.rm = TRUE)), 
			c(	min(x$ystart, na.rm = TRUE), 
				max(x$yend, na.rm = TRUE)),
		type = "n", xlab = "", ylab = "", xaxt = "n",
		main = sprintf("Page %d", page))

  	graphics::text((x$xstart + x$xend)/2, 
  				   (x$ystart+ x$yend)/2,
  				   x$text, cex = 0.8)

	xgrid <- seq(from = min(v, na.rm = TRUE), 
                 to = max(v, na.rm = TRUE), 
                 by = 20)
	xlines <- seq(from = min(v, na.rm = TRUE),
				  to = max(v, na.rm = TRUE),
				  by = 10)
                 #length.out = grid_len)

    axis(1, at = xgrid, labels = format(round(xgrid,0)), las = 2)


  	if (!is.null(breaks)) {
  		abline(v = split_points, col = "red")
  	} else {
  		abline(v = xlines, col = "red", lty = 5)
  	}
}

#' @noRd
#' @export
textplot.data.frame <- textplot.pdf_document