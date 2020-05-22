 
## ----------------------------------------------------------------------------
#  align_columns
#  =============
#' @title Align Columns
#' @description TODO
#' @param x an object inheriting from \code{'data.frame'}.
#' @param method a character string giving the method.
#' @param ... additional arguments
#' @return Returns an object inheriting from \code{'data.frame'}.
#' @export
##  ----------------------------------------------------------------------------
align_columns <- function(x, method = c("fixed_width", "auto"), ...) {
    method <- match.arg(method)
    kwargs <- list(...)
    switch(method, 
        auto = align_columns_fixed_width(x, find_breaks(x)),
        fixed_width = align_columns_fixed_width(x, kwargs[["split_points"]]))
}

align_columns_fixed_width <- function(x, split_points) {
    x$col <- NA_integer_
    xmean <- rowMeans(x[, c("x0", "x1")])
    for ( i in seq_along(split_points) ) {
        x$col[is.na(x$col) & (xmean < split_points[i])] <- i
    }
    x$col[is.na(x$col)] <- length(split_points) + 1L
    
    x
}

##  ----------------------------------------------------------------------------
#  find_breaks
# =============
#' @title Find Breaks
#' @description TODO
#' @param x an object inheriting from \code{'data.frame'}.
#' @param lower_bound a lower bound indicating with which ...
#' @return Returns an vector containing the column breaks.
#' @export
##  ----------------------------------------------------------------------------
find_breaks <- function(x, lower_bound = 0.25) {
    se <- unlist(mapply(seq, x[, 'x0'], x[, 'x1'], MoreArgs = list(by = 0.1)))
    h <- hist(se, breaks = 500, plot = FALSE)
    low <- quantile(h$counts, lower_bound)
    h$counts[h$counts < low] <- 0L
    group <- (cumsum(h$counts != 0L) * (h$counts == 0L))
    b <- !duplicated(group, fromLast = TRUE) & group > 0
    # breaks <- head(tail(h$breaks, -1)[b], -1)
    breaks <- tail(h$breaks, -1)[b]
    breaks
}

##  ----------------------------------------------------------------------------
#  distribute_breaks
# =============
#' @title Distributes breaks
#' @description Distributes equidistantly the given number of columns.
#' @param x an object inheriting from \code{'data.frame'}.
#' @param ncols the nubmer of columns of the document.
#' @return Returns an vector containing the column breaks.
#' @export 
##  ----------------------------------------------------------------------------
distribute_breaks <- function(x, ncols) {
    min_x <- min(x$x0)
    max_x <- max(x$x1)

    x_length <- max_x - min_x

    breaks <- round(seq(1, x_length, by = x_length / ncols))[-1]
    breaks <- breaks + min_x

    breaks
}
