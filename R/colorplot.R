#' @importFrom graphics rect abline plot

##  ----------------------------------------------------------------------------
#  colorplot
#  ============
#' @title colorplot
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

colorplot <- function(x, split_points = NULL, 
	page = 1, ...) UseMethod("colorplot", x)

#' @noRd
#' @export
colorplot.pdf_document <- function(x, split_points = NULL,
								   page = 1L, grid_len = 20, ...) {
	x <- rm_na(as.data.frame(x))
	stopifnot(page %in% x$pid)
	x <- x[x$pid == page, ]
	stopifnot(any(c("x0", "x1", "y0", "y1") %in% colnames(x)))
	v <- as.numeric(unlist(x[, c('x0','x1')]))

	if (!"col" %in% colnames(x)) {
		x$col <- 0
	}

	plot(	c(	min(x$x0, na.rm = TRUE), 
				max(x$x1, na.rm = TRUE)), 
			c(	min(x$y0, na.rm = TRUE), 
				max(x$y1, na.rm = TRUE)),
		type = "n", xlab = "", ylab = "", xaxt = "n",
		main = sprintf("Page %d", page))

  	graphics::rect(xleft = x$x0, xright = x$x1, 
  		ytop = x$y0, ybottom = x$y1, 
  		border = x$col+1)

	xgrid <- seq(from = min(v, na.rm = TRUE), 
                 to = max(v, na.rm = TRUE), 
                 by = 20)
	xlines <- seq(from = min(v, na.rm = TRUE),
				  to = max(v, na.rm = TRUE),
				  by = 10)
                 #length.out = grid_len)

    axis(1, at = xgrid, labels = format(round(xgrid,0)), las = 2)


  	if (!is.null(split_points)) {
  		abline(v = split_points, col = "red")
  	} else {
  		abline(v = xlines, col = "red", lty = 5)
  	}
}

#' @noRd
#' @export
colorplot.data.frame <- colorplot.pdf_document