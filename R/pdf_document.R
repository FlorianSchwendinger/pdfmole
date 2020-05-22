##
## This file implements the generic functions for 
## the S3 classes "pdf_document" and "pdf_page".
##

#' @noRd
#' @export
as.data.frame.pdf_document <- function(x, ...) {
    as.data.frame(x[["text"]], ...)
}
