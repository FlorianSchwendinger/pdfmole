#' @importFrom graphics rect abline

#
#
# intervalplot
#
#


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
#' @export 
##  ----------------------------------------------------------------------------
intervalplot <- function(x, by = 0.1, offset = NULL, breaks = 500,
    grid_len = 20, widths = NULL, ...) UseMethod("intervalplot", x)

#' @noRd
#' @export
intervalplot.pdf_document <- function(x, by = 0.1, offset = NULL, breaks = 500,
                                      grid_len = 20, widths = NULL, font = NULL) {
    x <- rm_na(as.data.frame(x))
    if ( !is.null(offset) ) x <- x[x[,'y0'] < offset,]
    if ( !is.null(font) )   x <- x[x$font %in% font, ]
    v <- as.numeric(unlist(x[, c('x0','x1')]))
    se <- unlist(mapply(seq, x[, 'x0'], x[, 'x1'],
                        MoreArgs = list(by = by)))
    hist(se, breaks = breaks, axes = FALSE, xlab = "", main = "")
    xgrid <- seq(from = min(v, na.rm = TRUE), 
                 to = max(v, na.rm = TRUE), 
                 length.out = grid_len)
    if ( !is.null(widths) ) xgrid <- widths
    # axis(1, at = xgrid, labels = format(round(xgrid,2)), las = 2)
    axis(1, at = xgrid, labels = format(round(xgrid,0)), las = 2)
    abline(v = xgrid, col = "gray", lty = 5)
    invisible(NULL)
}

#' @noRd
#' @export
intervalplot.pdf_page <- intervalplot.pdf_document

#' @noRd
#' @export
intervalplot.data.frame <- intervalplot.pdf_document


#
#
# colorplot
#
#


##  ----------------------------------------------------------------------------
#  colorplot
#  ============
#' @title colorplot
#' @description TODO
#' @param x an object inheriting from \code{data.frame}.
#' @param split_points an vector containing the x-coordinates for splitting
#' the page.
#' @param pid the number of the page which shall be plotted.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export
##  ----------------------------------------------------------------------------

colorplot <- function(x, split_points = NULL, pid = 0) UseMethod("colorplot", x)

#' @noRd
#' @export
colorplot.pdf_document <- function(x, split_points = NULL, pid = 0L, grid_len = 20) {
    x <- rm_na(as.data.frame(x))
    stopifnot(pid %in% x$pid)
    x <- x[x$pid == pid, ]
    stopifnot(any(c("x0", "x1", "y0", "y1") %in% colnames(x)))
    v <- as.numeric(unlist(x[, c('x0','x1')]))

    if (!"col" %in% colnames(x)) {
        x$col <- 0
    }

    plot(   c(  min(x$x0, na.rm = TRUE), 
                max(x$x1, na.rm = TRUE)), 
            c(  min(x$y0, na.rm = TRUE), 
                max(x$y1, na.rm = TRUE)),
        type = "n", xlab = "", ylab = "", xaxt = "n",
        main = sprintf("Page %d", pid))

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


#
#
# textplot
#
#


##  ----------------------------------------------------------------------------
#  textplot
#  ============
#' @title textplot
#' @description TODO
#' @param x an object inheriting from \code{data.frame}.
#' @param split_points an vector containing the x-coordinates for splitting the columns.
#' @param pid the number of the page which shall be plotted.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export
##  ----------------------------------------------------------------------------

textplot <- function(x, split_points = NULL, pid = 0L) UseMethod("textplot", x)

textplot.pdf_document <- function(x, split_points = NULL, pid = 0L) {
    x <- rm_na(as.data.frame(x))

    stopifnot(pid %in% x$pid)
    x <- x[x$pid == pid, ]
    
    stopifnot(any(c("x0", "x1", "y0", "y1") %in% colnames(x)))
    v <- as.numeric(unlist(x[, c('x0','x1')]))

    plot(   c(  min(x$x0, na.rm = TRUE), 
                max(x$x1, na.rm = TRUE)), 
            c(  min(x$y0, na.rm = TRUE), 
                max(x$y1, na.rm = TRUE)),
        type = "n", xlab = "", ylab = "", xaxt = "n",
        main = sprintf("Page %d", pid))

    graphics::text((x$x0 + x$x1) / 2, (x$y0+ x$y1) / 2, x$text, cex = 0.8)

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
textplot.data.frame <- textplot.pdf_document

