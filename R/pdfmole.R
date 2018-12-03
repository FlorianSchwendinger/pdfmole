#' @importFrom PythonInR pyExec pyExecfile pyCall
#' @importFrom checkmate assert check_file_exists check_directory_exists check_integerish check_character check_logical
#' @importFrom graphics abline axis hist
#' @importFrom stats cutree df dist hclust quantile
#' @importFrom utils download.file head tail unzip

# @importFrom intervals


##
## meta is a list with "rotate", "id" and "bbox"
##
simplify_list <- function(x) {
    if ( is.null(x) ) {
        NULL
    } else {
        lapply(x, unlist)
    }
}


bbox_names <- function() c("xstart", "ystart", "xend", "yend")

char_names <- function() c("text", "font", bbox_names(), "size")

split_string <- function(x) as.double(unlist(strsplit(x, ",", fixed = TRUE)))

## The input x is an list where each element represents a single character
## with meta data about the font, position and size in some cases only
## the character is given (e.g. \n).
text_to_df <- function(x) {
    char_to_df <- function(text = NA_character_, font = NA_character_, 
        bbox = rep.int(NA_real_, 4), size = NA_real_) {
        y <- c(list(text), font, bbox, size)
        names(y) <- char_names()
        as_df(y)
    }
    do.call(rbind, lapply(x, function(y) do.call(char_to_df, y)))
}

textbox_to_df <- function(x) {
    box_to_df <- function(wmode = NA_character_, id = NA_real_, bbox = rep.int(NA_real_, 4)) {
        if ( is.character(bbox) ) {
            bbox <- as.double(unlist(strsplit(bbox, ",", fixed = TRUE)))
        }
        names(bbox) <- bbox_names()
        as_df(as.list(c(wmode = wmode, id = id, bbox)))
    }
    do.call(rbind, lapply(x, function(y) do.call(box_to_df, y)))
}

textline_to_df <- function(x) {
    to_df <- function(x) {
        y <- as.list(as.numeric(unlist(strsplit(x$bbox, ",", fixed = TRUE))))
        names(y) <- bbox_names()
        as_df(y)
    }
    do.call(rbind, lapply(x, to_df))
}

curve_to_list <- function(x) {
    to_list <- function(x) {
        list(linewidth = x$linewidth, pts = split_string(x$pts),
             bbox = split_string(x$bbox))
    }
    lapply(x, to_list)
}

line_to_list <- function(x) {
    to_list <- function(x) {
        list(linewidth = x$linewidth, bbox = split_string(x$bbox))
    }
    lapply(x, to_list)
}


## transform a single page in to an object of class pdf_document
to_pdf_page <- function(x) {
    page <- list()
    # "figure"
    # "text"
    page$text <- text_to_df(x$text)
    # "image"
    # "curve"
    page$curve <- curve_to_list(x$curve)
    # "metainfo"
    page$meta <- simplify_list(x$metainfo)
    # "line"
    page$line <- line_to_list(x$line)
    # "textline"
    page$textline <- textline_to_df(x$textline)
    # "rect"

    # "textbox"
    page$textbox <- textbox_to_df(x$textbox)
    class(page) <- "pdf_page"
    page
}

to_pdf_document <- function(x) {
    doc <- lapply(x, to_pdf_page)
    class(doc) <- "pdf_document"
    doc
}

#  ---------------------------------------------------------
#  read.pdf
#  ========
#' @title Read a \code{PDF} document.
#' @description Extract  \code{PDF} document
#' @param file a character giving the name of the \code{PDF}-file the data are to be read from.
#' @param pages an integer giving the pages pages which should be extracted.
#' @param encoding a character string giving the encoding of the output.
#' @param maxpages an integer giving the maximum number of pages to be extracted.
#' @param check_extractable a logical indicating if a check should be performend
#'        to verify that the text is extractable.
#' @param password a character string giving the password necessary to access the \code{PDF}.
#' @param caching a logical if \code{TRUE} \pkg{pdfmole} is faster but uses more memory.
#' @param layout_page_number an integer giving number of the page where the layout
#'        analysis should be performed.
# @examples
# ifi <- system.file("pdfs/cars.pdf", package = "pdfmole")
# read.pdf(ifi)
# as.data.frame(read.pdf(ifi))
#
#' @return Returns a object of class \code{"pdf_document"}.
#'         A object of class \code{"pdf_document"} is a list where
#'         each element of the list is an object of class \code{"pdf_page"}.
#' @export
##  ---------------------------------------------------------
read.pdf <- function(file, pages = integer(), encoding = 'utf8', maxpages = Inf,
                     check_extractable = TRUE, password = '', caching = TRUE, 
                     layout_page_number = 1L) {

    if (is.infinite(maxpages)) maxpages <- 0L

    assert(check_file_exists(file), check_integerish(pages), 
           check_character(encoding), check_integerish(maxpages), 
           check_logical(check_extractable), check_character(password),
           check_logical(caching), check_integerish(layout_page_number))

    ## prepare kwargs
    kwargs <- list(path = file, pagenos = as.list(pages - 1L), codec = encoding,
                   maxpages = maxpages, check_extractable = check_extractable,
                   password = password, caching = caching, 
                   pageno = layout_page_number - 1L)

    doc <- pyCall("pdf2list", kwargs = kwargs, simplify = FALSE)
    to_pdf_document(doc)
}

.to_matrix <- function(x, collapse = " ") {
    nrow <- max(x$row)
    ncol <- max(x$col)
    M <- matrix("", nrow, ncol)
    x <- x[order(x$row, -x$xstart, decreasing = TRUE),]
    for (i in seq_len(nrow)) {
        m <- which(x$row == i)
        if ( length(m) ) {
            tmp <- x[m,]
            for (j in seq_len(ncol)) {
                n <- which(tmp$col == j)
                if ( length(n) ) {
                    M[i, j] <- paste(tmp[n, "text"], collapse = collapse)
                }
            }
        }
    }
    M
}

to_matrix <- function(x, collapse = " ") {
    x <- x[order(x$page),]
    x <- split(x, x$page)
    lapply(x, .to_matrix, collapse = collapse)
}

df_to_matrix <- function(x) {
    urow <- sort(unique(x$row))
    ucol <- sort(unique(x$col))
    M <- matrix("", length(urow), length(ucol))

    x <- x[order(x$ystart, -x$xstart, decreasing = TRUE),]
    for ( i in seq_along(urow) ) {
        bi <- x$row == urow[i]
        for ( j in seq_along(ucol) ) {
            bj <- x$col == ucol[j]
            rec <- x[(bi & bj), ]
            if ( nrow(rec) > 0 ) {
                M[i, j] <- paste(rec$text, collapse=" ")
            }
        }
    }
    rownames(M) <- urow
    M
}




