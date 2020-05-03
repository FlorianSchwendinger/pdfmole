
# returns the first element of a vector with block specific warning
block_first <- function(key, val) {
    if ( length(unique(val)) != 1L ) {
        warning("'%s' contains blocks that are not unique", key)
    }
    head(val, 1)
}

unify_blocks <- function(x, collapse = "") {
    x$block <- NULL
    if ( nrow(x) == 1L ) {
        return(x)
    }
    y <- setNames(vector("list", ncol(x)), colnames(x))
    for ( i in seq_along(x) ) {
        k <- colnames(x)[i]
        unifun <- switch(k,
            block_first,
            x0 = function(key, val) min(val, na.rm = TRUE),
            x1 = function(key, val) max(val, na.rm = TRUE),
            text = function(key, val) paste(val, collapse = collapse))
        y[[i]] <- unifun(k, x[[i]])
    }
    return(as.data.frame(y, stringsAsFactors = FALSE))
}

##  ----------------------------------------------------------------------------
#  group_blocks
#  ============
#' @title Group Blocks
#' @description TODO
#' @param x an object inheriting from \code{'data.frame'}.
#' @return Returns an object inheriting from \code{'data.frame'}.
#' @export
##  ----------------------------------------------------------------------------
group_blocks  <- function(x, collapse = "") {
    assert_contains_columns(x, c("block"))

    x <- x[order(x$block, x$x0),]
    x <- split(x, x$block) # the split drops the NA values
    x <- lapply(x, unify_blocks, collapse = collapse)
    x$stringsAsFactors <- FALSE
    do.call(rbind.data.frame, x)
}

##  ----------------------------------------------------------------------------
#  group_columns
#  ============
#' @title Group Columns
#' @description TODO
#' @param x an object inheriting from \code{'data.frame'}.
#' @return Returns an object inheriting from \code{'data.frame'}.
#' @export
##  ----------------------------------------------------------------------------
group_columns <- function(x, collapse = "") {
    assert_contains_columns(x, c("pid", "row", "col", "x0", "x1", "text"))
    bb_x_center <- (x$x0 + x$x1) / 2
    x <- x[order(bb_x_center),]
    x <- aggregate(text ~ pid + row + col, data = x, FUN = paste, collapse = collapse)
    x <- x[with(x, order(pid, row, col)),]
    rownames(x) <- NULL
    attr(x, "ordered") <- TRUE
    x
}

