
rm_na <- function(x) {
    x[!is.na(x$xstart),]
}

##  ----------------------------------------------------------------------------
#  group_blocks
#  ============
#' @title Group Blocks
#' @description TODO
#' @param x a object inheriting from \code{'data.frame'}.
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
        if ( length(unique(x$page)) != 1L ) warning("block page is not unique")
        if ( length(unique(x$font)) != 1L ) warning("block font is not unique")
        if ( length(unique(x$ystart)) != 1L ) warning("block ystart is not unique")
        if ( length(unique(x$yend)) != 1L ) warning("block yend is not unique")
        if ( length(unique(x$size)) != 1L ) warning("block size is not unique")
        as_df(list(page = x$page[1], text = paste(x$text, collapse = ""),
                   font = x$font[1], xstart = min(x$xstart), ystart = x$ystart[1], 
                   xend = max(x$xend), yend = x$yend[1], size = x$size[1]))
    }
    do.call(rbind, lapply(x, group_block))
}
