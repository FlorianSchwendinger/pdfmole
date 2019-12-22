
##  ----------------------------------------------------------------------------
#  align_rows
#  ==========
#' @title Align Rows
#' @description Align the rows of an object of class \code{pdf\_df}.
#' @param x an object of class \code{pdf\_df}.
#' @param method a character string giving the name of the method.
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a object of class \code{"pdf\_df"}.
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
    x$row <- match(x$ystart, sort(unique(x$ystart), decreasing = TRUE))
    
    x
}

align_rows_hclust <- function(x, ...) {
    kwargs <- list(...)
    .align_rows_hclust <- function(x, h) {
        ymean <- x$yend + x$ystart / 2
        distance_matrix <- dist(ymean)
        hierarchical_cluster <- hclust(distance_matrix, method = "single")
        cut_height <- if ( is.null(h) ) min(x$yend - x$ystart) else h
        cutree(hierarchical_cluster, h = cut_height)
    }
    x$row <- NA_integer_
    for ( i in unique(x$page) ) {
        b <- (x$page == i)
        x$row[b] <- .align_rows_hclust(x[b,], h = kwargs$h)
    }

    ## reorder
    x <- x[order(x$ystart, decreasing = TRUE),]
    x$row <- match(x$row, unique(x$row))
    
    x
}

align_rows_fixed_width <- function(x, split_points) {
    x$row <- NA_integer_
    xmean <- rowMeans(x[, c("ystart", "yend")])
    for ( i in seq_along(split_points) ) {
        x$row[is.na(x$row) & (xmean < split_points[i])] <- i
    }
    x$row[is.na(x$row)] <- length(split_points) + 1L

    x$row <- (length(split_points) + 2) - x$row
    x
}
