##
## view_intervals
##

##  ----------------------------------------------------------------------------
#  intervalplot
#  ============
#' @title Intervalplot
#' @description TODO
#' @param x an object inheriting from \code{data.frame}.
#' @param by TODO
#' @param offset TODO
#' @param breaks TODO
#' @param grid_len TODO
#' @param widths TODO
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
##  ----------------------------------------------------------------------------
intervalplot <- function(x, by = 0.1, offset = NULL, breaks = 500,
    grid_len = 20, widths = NULL, ...) UseMethod("intervalplot", x)

#' @noRd
#' @export
intervalplot.pdf_document <- function(x, by = 0.1, offset = NULL, breaks = 500,
                                      grid_len = 20, widths = NULL, font = NULL) {
    x <- rm_na(as.data.frame(x))
    if ( !is.null(offset) ) x <- x[x[,'ystart'] < offset,]
    if ( !is.null(font) )   x <- x[x$font %in% font, ]
    v <- as.numeric(unlist(x[, c('xstart','xend')]))
    se <- unlist(mapply(seq, x[, 'xstart'], x[, 'xend'],
                        MoreArgs = list(by = by)))
    hist(se, breaks = breaks, axes = FALSE, xlab = "", main = "")
    xgrid <- seq(from = min(v, na.rm = TRUE), 
                 to = max(v, na.rm = TRUE), 
                 length.out = grid_len)
    if ( !is.null(widths) ) xgrid <- widths
    axis(1, at = xgrid, labels = format(round(xgrid,2)), las = 2)
    abline(v = xgrid, col = "gray", lty = 5)
    invisible(NULL)
}

#' @noRd
#' @export
intervalplot.pdf_page <- intervalplot.pdf_document

#' @noRd
#' @export
intervalplot.data.frame <- intervalplot.pdf_document
