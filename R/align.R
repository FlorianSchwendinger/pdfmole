#
# 
#   align rows
#
#


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

    ## reorder
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


#
# 
#   align columns
#
#

 
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


#
# 
#   align lines
#
#


lead <- function(x, na = NA) c(tail(x, -1), na)


##  ----------------------------------------------------------------------------
#  align_hlines
#  ==========
#' @title Align horizontal lines
#' @description Align the horizontal lines of a dataframe.
#' @param x an object of class dataframe.
#' @param tol a threshold of how many distance can be between line segments
#' which should still be handled as connected.
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a df where all horizontal line segments
#' with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_hlines <- function(x, tol = 1) {
    hl <- x[x$horizontal, ]
    hl <- hl[order(hl$y0, hl$x0), ]

    same_row <- (abs(hl$y0 - lead(hl$y0)) < tol)
    same_end <- (abs(hl$x1 - lead(hl$x0)) < tol)
    
    same_group <- (same_row & same_end)

    hl$group <- c(1, head(cumsum(!same_group) + 1, -1))

    cl <- hl[, list(first(linewidth), first(x0), first(y0), last(x1), 
            first(y1), first(horizontal), first(vertical)), group]
    cl[, group := NULL]
    colnames(cl) <- colnames(x)
    
    cl
}

##  ----------------------------------------------------------------------------
#  align_vlines
#  ==========
#' @title Align vertical lines
#' @description Align the vertical lines of a dataframe.
#' @param x an object of class dataframe.
#' @param tol a threshold of how many distance can be between line segments
#' which should still be handled as connected.
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a df where all horizontal line segments
#' with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_vlines <- function(x, tol = 1) {
    vl <- x[x$vertical, ]
    vl <- vl[order(vl$x0, vl$y0), ]

    same_col <- (abs(vl$x0 - lead(vl$x0)) < tol)
    same_end <- (abs(vl$y1 - lead(vl$y0)) < tol)
    
    same_group <- (same_col & same_end)

    vl$group <- c(1, head(cumsum(!same_group) + 1, -1))

    cl <- vl[, list(first(linewidth), first(x0), first(y0), first(x1), 
            last(y1), first(horizontal), first(vertical)), group]
    cl[, group := NULL]
    colnames(cl) <- colnames(x)
    
    cl
}

##  ----------------------------------------------------------------------------
#  align_lines
#  ==========
#' @title Align all lines
#' @description Align the all (horizontal + vertical) lines of a dataframe.
#' @param x an object of class dataframe.
#' @param tol a threshold of how many distance can be between line segments
#' which should still be handled as connected.
#' @param ... additional arguments
#' @details Some details to be written.
#' @return Returns a df where all line segments (which belong to the same
#' horizontal or vertical) with connecting end and start point are concatened.
#' @export
##  ----------------------------------------------------------------------------
align_lines <- function(x) {
    rbind(align_hlines(x), align_vlines(x))
}

