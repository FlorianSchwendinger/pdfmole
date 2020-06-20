#' @noRd
#' @export
print.pdf_document <- function(x, ...) {
    n_pages <- max(x$metainfo$pid)
    if ( n_pages == 1L ) {
        writeLines(sprintf("A pdf document with %s page and", n_pages))
    } else {
        writeLines(sprintf("A pdf document with %s pages and", n_pages))
    }
    print(data.frame(lapply(x, nrow)))
    writeLines("elements.")
}

##  ----------------------------------------------------------------------------
#  from_tesseract
#  ==============
#' @title Convert Data from Tesseract
#' @description Convert data obtained by \code{tesseract::ocr_data} into
#' an format usable by \pkg{pdfmole}.
#' @param x a tibble obtained by \code{tesseract::ocr_data}.
#' @return a \code{data.frame} usable by \pkg{pdfmole}.
#' @examples
#' # ocr_data <- tesseract::ocr_data("my_file".png)
#' # data("ocr_data")
#' # add page number
#' # ocr_data$pid <- 1L
#' # from_tesseract(ocr_data)
#' @export 
##  ----------------------------------------------------------------------------
from_tesseract <- function(x) {
    bbox <- do.call(rbind, strsplit(x$bbox, split = ",", fixed = TRUE))
    bbox <- as.data.frame(apply(bbox, 2, as.integer), stringsAsFactors=FALSE)
    colnames(bbox) <- c("x0", "y0", "x1", "y1")
    x$bbox <- NULL
    max_y <- max(max(bbox$y0), max(bbox$y1))
    bbox$y0 <- max_y - bbox$y0
    bbox$y1 <- max_y - bbox$y1
    x <- cbind(as.data.frame(x, stringsAsFactors = FALSE), bbox)
    colnames(x)[colnames(x) == "word"] <- "text"
    x
}
