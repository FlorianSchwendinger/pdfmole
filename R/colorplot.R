#' @importFrom graphics rect abline

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
	page = 0) UseMethod("colorplot", x)

#' @noRd
#' @export
colorplot.pdf_document <- function(x, split_points = NULL,
								   page = 0L, grid_len = 20) {
	x <- rm_na(as.data.frame(x))
	stopifnot(page %in% x$page)
	x <- x[x$page == page, ]
	stopifnot(any(c("xstart", "xend", "ystart", "yend") %in% colnames(x)))
	v <- as.numeric(unlist(x[, c('xstart','xend')]))

	if (!"col" %in% colnames(x)) {
		x$col <- 0
	}

	plot(	c(	min(x$xstart, na.rm = TRUE), 
				max(x$xend, na.rm = TRUE)), 
			c(	min(x$ystart, na.rm = TRUE), 
				max(x$yend, na.rm = TRUE)),
		type = "n", xlab = "", ylab = "", xaxt = "n",
		main = sprintf("Page %d", page))

  	graphics::rect(xleft = x$xstart, xright = x$xend, 
  		ytop = x$ystart, ybottom = x$yend, 
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