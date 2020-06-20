##  ----------------------------------------------------------------------------
#  align_rows
#  ==========
#' @title Align Rows
#' @description Align the rows of an object inheriting from \code{'data.frame'}.
#' @param x an object of class \code{'data.frame'}.
#' @param method a character string giving the name of the method. This must be 
#' one of "exact_match", "hclust" or "fixed_width".
#' @param ... additional arguments
#' @details Available methods for column aligning are:
#'   
#' exact_match:   
#' Assigns row values based on exact matches of possible row values.
#' The matching is performed on the start values of y-coordinates (y0).
#' 
#' hclust:  
#' Uses hierachical clustering for determining the rows.
#' 
#' fixed_width:  
#' Aligns the i-th row to a segment based on whether its center 
#' ((y0 + y1) / 2) is below this i-th split point.
#' @return Returns an object inheriting from \code{'data.frame'}.
#' @examples
#' @export
##  ----------------------------------------------------------------------------
align_rows <- function(x, method = c("exact_match", "hclust", "fixed_width"), ...) {
    method <- match.arg(method)
    kwargs <- list(...)
    switch(method, 
        exact_match = align_rows_exact_match(x, ...),
        hclust = align_rows_hclust(x, ...),
        fixed_width = align_rows_fixed_width(x, kwargs[["split_points"]]))
}


align_rows_exact_match <- function(x, ...) {
    x$row <- match(x$y0, sort(unique(x$y0), decreasing = TRUE))
    x
}


align_rows_hclust <- function(x, ...) {
    kwargs <- list(...)
    .align_rows_hclust <- function(x, h) {
        ymean <- x$y1 + x$y0 / 2
        distance_matrix <- dist(ymean)
        hierarchical_cluster <- hclust(distance_matrix, method = "single")
        cut_height <- if ( is.null(h) ) min(x$y1 - x$y0) else h
        cutree(hierarchical_cluster, h = cut_height)
    }
    x$row <- NA_integer_
    for ( i in unique(x$pid) ) {
        b <- (x$pid == i)
        x$row[b] <- .align_rows_hclust(x[b,], h = kwargs$h)
    }

    x <- x[order(x$y0, decreasing = TRUE),]
    x$row <- match(x$row, unique(x$row))
    
    x
}


align_rows_fixed_width <- function(x, split_points) {
    x$row <- NA_integer_
    xmean <- rowMeans(x[, c("y0", "y1")])
    for ( i in seq_along(split_points) ) {
        x$row[is.na(x$row) & (xmean < split_points[i])] <- i
    }
    x$row[is.na(x$row)] <- length(split_points) + 1L

    x$row <- (length(split_points) + 2) - x$row
    x
}



## ----------------------------------------------------------------------------
#  align_columns
#  =============
#' @title Align Columns
#' @description Align the columns of an object inheriting from 
#' \code{'data.frame'}.
#' @param x an object inheriting from \code{'data.frame'}.
#' @param method a character string giving the name of the method. This must be 
#' one of "fixed_width" or "auto".
#' @param ... additional arguments
#' @details Available methods for column aligning are:
#'   
#' fixed_width:
#' Aligns the i-th column to a segment based on whether its center 
#' ((x0 + x1) / 2) is below this i-th split point.
#'   
#' auto:
#' Uses find_breaks to determine the split points for columns aligning. 
#' Afterwards uses fixed_width with the found split points.
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
#' @description Detect the breaks for columns aligning.
#' @param x an object inheriting from \code{'data.frame'}.
#' @param lower_bound a lower bound indicating with which ...
#' @param min_diff a minimum threshold for the distance between the breaks.
#' @return Returns an vector containing the column breaks.
#' @export
##  ----------------------------------------------------------------------------
find_breaks <- function(x, lower_bound = 0.25, min_diff = 10) {
    se <- unlist(mapply(seq, x[, 'x0'], x[, 'x1'], MoreArgs = list(by = 0.1)))
    h <- hist(se, breaks = 500, plot = FALSE)
    low <- quantile(h$counts, lower_bound)
    h$counts[h$counts < low] <- 0L
    group <- (cumsum(h$counts != 0L) * (h$counts == 0L))
    b <- !duplicated(group, fromLast = TRUE) & group > 0
    # breaks <- head(tail(h$breaks, -1)[b], -1)
    breaks <- tail(h$breaks, -1)[b]

    # apply min_diff
    breaks <- breaks[c(diff(breaks) > min_diff, TRUE)]
    breaks
}

