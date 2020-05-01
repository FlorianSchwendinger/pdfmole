
rm_na <- function(x) {
    x[!is.na(x$x0),]
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
## NOTE: check if the stuff has also to be ordered!!
group_blocks <- function(x) {
    stopifnot(inherits(x, "data.frame"), "block" %in% colnames(x))
    
    x <- rm_na(x)
    x <- split(x, x$block)
    group_block <- function(x) {
        if ( nrow(x) == 0L ) return(NULL)
        if ( length(unique(x$pid)) != 1L ) warning("block pid is not unique")
        if ( length(unique(x$font)) != 1L ) warning("block font is not unique")
        if ( length(unique(x$y0)) != 1L ) warning("block y0 is not unique")
        if ( length(unique(x$y1)) != 1L ) warning("block y1 is not unique")
        if ( length(unique(x$size)) != 1L ) warning("block size is not unique")
        as_df(list(pid = x$pid[1], text = paste(x$text, collapse = ""),
                   font = x$font[1], x0 = min(x$x0), y0 = x$y0[1], 
                   x1 = max(x$x1), y1 = x$y1[1], size = x$size[1]))
    }
    do.call(rbind, lapply(x, group_block))
}
