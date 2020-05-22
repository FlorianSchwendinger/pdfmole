##
## view_intervals
##

##  ----------------------------------------------------------------------------
#  intervalplot
#  ============
#' @title Intervalplot
#' @description TODO
#' @param x an object inheriting from \code{data.frame}.
#' @param breaks TODO
#' @param by TODO
#' @param offset TODO
#' @param num_breaks TODO
#' @param grid_len TODO
#' @param widths TODO
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export 
##  ----------------------------------------------------------------------------
intervalplot <- function(x, breaks = NULL, by = 0.1, offset = NULL, 
    num_breaks = 500, grid_len = 20, widths = NULL, ...) UseMethod("intervalplot", x)

#' @noRd
#' @export
intervalplot.pdf_document <- function(x, breaks = NULL, by = 0.1, 
    offset = NULL, num_breaks = 500, grid_len = 20, widths = NULL, font = NULL, ...) {
    
    x <- rm_na(as.data.frame(x))
    if ( !is.null(offset) ) x <- x[x[,'y0'] < offset,]
    if ( !is.null(font) )   x <- x[x$font %in% font, ]
    v <- as.numeric(unlist(x[, c('x0','x1')]))
    se <- unlist(mapply(seq, x[, 'x0'], x[, 'x1'],
                        MoreArgs = list(by = by)))
    hist(se, breaks = num_breaks, axes = FALSE, xlab = "", main = "")
    xgrid <- seq(from = min(v, na.rm = TRUE), 
                 to = max(v, na.rm = TRUE), 
                 length.out = grid_len)
    if ( !is.null(widths) ) xgrid <- widths
    # axis(1, at = xgrid, labels = format(round(xgrid,2)), las = 2)
    axis(1, at = xgrid, labels = format(round(xgrid,0)), las = 2)

    if (!is.null(breaks)) {
        abline(v = breaks, col = "red")
    } else {
        abline(v = xgrid, col = "gray", lty = 5)
    }

    invisible(NULL)
}

#' @noRd
#' @export
intervalplot.data.frame <- intervalplot.pdf_document
