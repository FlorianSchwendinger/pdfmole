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


bbox_names <- function() c("x0", "y0", "x1", "y1")

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

.to_matrix <- function(x, collapse = " ") {
    nrow <- max(x$row)
    ncol <- max(x$col)
    M <- matrix("", nrow, ncol)
    x <- x[order(x$row, -x$x0, decreasing = TRUE),]
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
    x <- x[order(x$pid),]
    x <- split(x, x$pid)
    lapply(x, .to_matrix, collapse = collapse)
}

df_to_matrix <- function(x) {
    urow <- sort(unique(x$row))
    ucol <- sort(unique(x$col))
    M <- matrix("", length(urow), length(ucol))

    x <- x[order(x$y0, -x$x0, decreasing = TRUE),]
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


##  ----------------------------------------------------------------------------
#  extract_lines
#  ============
#' @title extract_lines
#' @description TODO
#' @param x an object inheriting from \code{pdf_document}.
#' @param tol a threshold for aligning lines.
#' @details Some details to be written.
#' @return TODO
#' @export 
##  ----------------------------------------------------------------------------
extract_lines <- function(x, tol = 0.05) {
    stopifnot(inherits(x, 'pdf_document'))
    stopifnot(length(x$line) > 0)

    res <- x$line

    res$horizontal <- FALSE
    res$vertical <- FALSE

    res$horizontal[abs(res$y1 - res$y0) < tol] <- TRUE
    res$vertical[abs(res$x1 - res$x0) < tol] <- TRUE

    res
}
