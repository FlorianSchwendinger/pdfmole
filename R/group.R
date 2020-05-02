
##  ----------------------------------------------------------------------------
#  group_blocks
#  ============
#' @title Group Blocks
#' @description TODO
#' @param x an object inheriting from \code{'data.frame'}.
#' @return Returns an object inheriting from \code{'data.frame'}.
#' @export
##  ----------------------------------------------------------------------------
## NOTE: check if the stuff has also to be ordered!!
group_blocks <- function(x) {
    assert_contains_columns(x, c("pid", "x0", "x1", "y0", "y1", "text", "block", ""))
    
    x <- rm_na(x)
    x <- split(x, x$block)
    group_block <- function(x) {
        if ( nrow(x) == 0L ) return(NULL)
        if ( length(unique(x$pid)) != 1L ) warning("block pid is not unique")
        if ( length(unique(x$font)) != 1L ) warning("block font is not unique")
        if ( length(unique(x$y0)) != 1L ) warning("block y0 is not unique")
        if ( length(unique(x$y1)) != 1L ) warning("block y1 is not unique")
        if ( length(unique(x$size)) != 1L ) warning("block size is not unique")
        data.frame(pid = x$pid[1], text = paste(x$text, collapse = ""),
                   font = x$font[1], x0 = min(x$x0), y0 = x$y0[1], 
                   x1 = max(x$x1), y1 = x$y1[1], size = x$size[1], 
                   stringsAsFactors = FALSE)
    }
    do.call(rbind, lapply(x, group_block))
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

