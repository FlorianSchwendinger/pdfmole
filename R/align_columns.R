
##  ----------------------------------------------------------------------------
#  align_columns
#  =============
#' @title Align Columns
#' @description TODO
#' @param x a object inheriting from \code{'data.frame'}.
#' @param method a character string giving the method.
#' @param ... additional arguments
#' @return Returns an object inheriting from \code{'data.frame'}.
#' @export
##  ----------------------------------------------------------------------------
align_columns <- function(x, method = c("fixed_width"), ...) {
    method <- match.arg(method)
    kwargs <- list(...)
    switch(method, 
        fixed_width = align_columns_fixed_width(x, kwargs[["split_points"]]))
}

align_columns_fixed_width <- function(x, split_points) {
    x$col <- NA_integer_
    xmean <- rowMeans(x[, c("xstart", "xend")])
    for ( i in seq_along(split_points) ) {
        x$col[is.na(x$col) & (xmean < split_points[i])] <- i
    }
    x$col[is.na(x$col)] <- length(split_points) + 1L
    x
}

find_breaks <- function(x) {
    se <- unlist(mapply(seq, x[, 'xstart'], x[, 'xend'], MoreArgs = list(by = 0.1)))
    h <- hist(se, breaks = 500, plot = FALSE)
    low <- quantile(h$counts, 0.25)
    h$counts[h$counts < low] <- 0L
    group <- (cumsum(h$counts != 0L) * (h$counts == 0L))
    b <- !duplicated(group, fromLast = TRUE) & group > 0
    breaks <- head(tail(h$breaks, -1)[b], -1)
    breaks
}

