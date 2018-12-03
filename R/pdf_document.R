##
## This file implements the generic functions for 
## the S3 classes "pdf_document" and "pdf_page".
##

#' @noRd
#' @export
print.pdf_document <- function(x, ...) {
    if ( length(x) == 1L ) {
        writeLines(sprintf("A pdf document with %s page.", length(x)))
    } else {
        writeLines(sprintf("A pdf document with %s pages.", length(x)))
    }
}

#' @noRd
#' @export
as.data.frame.pdf_document <- function(x, ...) {
    df <- vector("list", length(x))
    for (i in seq_along(df)) {
        df[[i]] <- as.data.frame(x[[i]])
    }
    df <- do.call(rbind, df)
    df$block <- cumsum(is.na(df$xstart)) + 1L 
    class(df) <- c("pdf_df", class(df))
    df
}

#' @noRd
#' @export
print.pdf_page <- function(x, ...) {
    writeLines(sprintf("pdf page number %i.", as.integer(x$meta$id)))
}

#' @noRd
#' @export
as.data.frame.pdf_page <- function(x, ...) {
    cbind(page = as.integer(x$meta$id), x$text)
}
