#' @importFrom graphics rect abline


##  ----------------------------------------------------------------------------
#  pixelplot
#  ============
#' @title Pixelplot
#' @description Plot the count of letters 
#' @param x an object inheriting from \code{data.frame}.
#' @param scale a double giving the scale factor.
#' @param pids an integer vector giving the pages to be considered in the pixelplot.
#' @param las labels are parallel (=0) or perpendicular(=2) to axis
#' @param cex.axis size of axis annotation relative to cex
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @export 
#' @examples
#' pdf_file <- system.file(file.path("pdfs","cars.pdf"), package = "pdfmole")
#' pdf <- pdfminer::read.pdf(pdf_file, pages = 1:2, maxpages = 2L)
#'
#' df <- pdf$text
#' head(df, 20)
#'
#' pixelplot(df, scale = 0.5)
##  ----------------------------------------------------------------------------
pixelplot <- function(x, scale = 1, pids = integer(), las = 2, cex.axis = 0.7, ...) 
    UseMethod("pixelplot", x)

#' @noRd
#' @export
pixelplot.data.frame <- function(x, scale = 1, pids = integer(), las = 2, cex.axis = 0.7, ...) {
    assert_contains_columns(x, c("pid", "text", "x0", "x1"))
    x <- x[is.finite(x$x0) & is.finite(x$x1),]
    x <- x[!grepl("^\\s*$", x$text),]
    if ( length(pids) > 0 ) {
        x <- x[x$pid %in% pids,]
    }
    pixel <- round(unlist(mapply(seq, floor(scale * x$x0), ceiling(scale * x$x1))) / scale)
    plot(table(pixel), xlab = "x - coordinates", ylab = "count", las = las, cex.axis = cex.axis, ...)
}


##  ----------------------------------------------------------------------------
#  bboxplot
#  ============
#' @title bboxplot
#' @description Plot the detected bounding boxes
#' @param x an object inheriting from \code{data.frame}.
#' @param split_points an vector containing the x-coordinates for splitting
#' the page.
#' @param pid the number of the page which shall be plotted.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export
#' @examples
#' pdf_file <- system.file(file.path("pdfs","cars.pdf"), package = "pdfmole")
#' pdf <- pdfminer::read.pdf(pdf_file, pages = 1:2, maxpages = 2L)
#'
#' df <- pdf$text
#' head(df, 20)
#'
#' bboxplot(df, pid = 1L)
##  ----------------------------------------------------------------------------
bboxplot <- function(x, split_points = NULL, pid = 1L, ...) 
    UseMethod("bboxplot", x)

#' @noRd
#' @export
bboxplot.data.frame <- function(x, split_points = NULL, pid = 1L, grid_len = 20, ...) {
    assert_contains_columns(x, c("pid", "x0", "x1", "y0", "y1"))
    x <- rm_na(x)
    x <- x[x$pid == pid, ]
    v <- as.numeric(unlist(x[, c('x0','x1')]))

    if (!"col" %in% colnames(x)) {
        x$col <- 0
    }

    plot(c(min(x$x0, na.rm = TRUE), max(x$x1, na.rm = TRUE)), 
         c(min(x$y0, na.rm = TRUE), max(x$y1, na.rm = TRUE)),
         type = "n", xlab = "", ylab = "", xaxt = "n", ...)

    graphics::rect(xleft = x$x0, xright = x$x1, ytop = x$y0, ybottom = x$y1, 
                   border = x$col + 1)

    xgrid <- seq(from = min(v, na.rm = TRUE), to = max(v, na.rm = TRUE), by = 20)
    xlines <- seq(from = min(v, na.rm = TRUE), to = max(v, na.rm = TRUE), by = 10)

    axis(1, at = xgrid, labels = format(round(xgrid,0)), las = 2)

    if (!is.null(split_points)) {
        abline(v = split_points, col = "red")
    } else {
        abline(v = xlines, col = "red", lty = 5)
    }
}


##  ----------------------------------------------------------------------------
#  textplot
#  ============
#' @title textplot
#' @description Plot the detected text
#' @param x an object inheriting from \code{data.frame}.
#' @param split_points an vector containing the x-coordinates for splitting
#' the page.
#' @param pid the number of the page which shall be plotted.
#' @param ... additional arguments (currently not used).
#' @details Some details to be written.
#' @return TODO
#' @export
#' @examples
#' pdf_file <- system.file(file.path("pdfs","cars.pdf"), package = "pdfmole")
#' pdf <- pdfminer::read.pdf(pdf_file, pages = 1:2, maxpages = 2L)
#'
#' df <- pdf$text
#' head(df, 20)
#'
#' textplot(df, pid = 1L)
##  ----------------------------------------------------------------------------
textplot <- function(x, split_points = NULL, pid = 1L, ...)
    UseMethod("textplot", x)

#' @noRd
#' @export
textplot.data.frame <- function(x, split_points = NULL, pid = 1L, ...) {
    assert_contains_columns(x, c("pid", "text", "x0", "x1", "y0", "y1"))
    x <- rm_na(x)
    x <- x[x$pid == pid, ]
    v <- as.numeric(unlist(x[, c('x0','x1')]))

    plot(c(min(x$x0, na.rm = TRUE), max(x$x1, na.rm = TRUE)), 
         c(min(x$y0, na.rm = TRUE), max(x$y1, na.rm = TRUE)),
         type = "n", xlab = "", ylab = "", xaxt = "n", ...)

    graphics::text((x$x0 + x$x1) / 2, (x$y0+ x$y1) / 2, x$text, cex = 0.8,
        adj = c(0.5, 0.5))

    xgrid <- seq(from = min(v, na.rm = TRUE), to = max(v, na.rm = TRUE), by = 20)
    xlines <- seq(from = min(v, na.rm = TRUE), to = max(v, na.rm = TRUE), by = 10)

    axis(1, at = xgrid, labels = format(round(xgrid,0)), las = 2)

    if (!is.null(split_points)) {
        abline(v = split_points, col = "red")
    } else {
        abline(v = xlines, col = "red", lty = 5)
    }
}
