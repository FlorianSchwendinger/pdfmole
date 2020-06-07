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


ocr_image <- function(file, pid, engine) {
    d <- tesseract::ocr_data(file, engine = engine)
    bbox <- do.call(rbind, strsplit(d$bbox, split = ",", fixed = TRUE))
    bbox <- as.data.frame(apply(bbox, 2, as.integer), stringsAsFactors=FALSE)
    colnames(bbox) <- c("x0", "y0", "x1", "y1")
    d$bbox <- NULL
    d <- cbind(pid = pid, as.data.frame(d, stringsAsFactors = FALSE), bbox)
    colnames(d)[2L] <- "text"
    return(d)
}



##  ----------------------------------------------------------------------------
#  simplify
#  ========
#' @title TODO
#' @description TODO
#' @param x an 
#' @param ... optional arguments
#' @export 
##  ----------------------------------------------------------------------------
tesseract_read.pdf <- function(file, pages = integer(), dpi = 400L, engine = tesseract("eng"), 
    temp_dir = tempdir(), temp_format = "tiff") {
    
    pdftools_is_installed_ <- requireNamespace("pdftools", quietly = TRUE)
    tesseract_is_installed <- requireNamespace("tesseract0", quietly = TRUE)
    stopifnot(pdftools_is_installed_, tesseract_is_installed)

    if ( length(pages) == 0L ) {
        pages <- seq_len(pdftools::pdf_info(pdffile)$pages)
    }
    
    wd <- getwd()
    setwd(temp_dir)
    status <- tryCatch({
        img_files <- pdftools::pdf_convert(file, format = temp_format, pages = pages, 
                                           dpi = dpi, verbose = FALSE)
        TRUE
        }, error = function(e) e)
    setwd(wd)
    if ( !isTRUE(status) ) {
        unlink(temp_dir)
        stop(status)
    }
    
    img_files <- file.path(temp_dir, img_files)
    status <- tryCatch({
        d <- mapply(ocr_image, img_files, pages, MoreArgs = list(engine = engine), 
                    SIMPLIFY = FALSE, USE.NAMES = FALSE)
        TRUE
        }, error = function(e) e)
    unlink(temp_dir)
    if ( !isTRUE(status) ) stop(status)
    metainfo <- data.frame(pid = pages)
    dat <- list(metainfo=metainfo, text=do.call(rbind, d))
    class(dat) <- "pdf_document"
    return(dat)
}
