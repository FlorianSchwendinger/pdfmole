#' @importFrom checkmate assert check_file_exists check_directory_exists check_integerish check_character check_logical
#' @importFrom graphics abline axis hist plot
#' @importFrom stats cutree df dist hclust quantile aggregate setNames
#' @importFrom utils download.file head tail unzip

# @importFrom intervals


##  ----------------------------------------------------------------------------
#  mole
#  ====
#' @title Convert into Rectangular Format.
#' @description Convert the data from a sparse sparse simple triplet representation
#'  into a dense list of vectors format.
#' @param x an object inheriting from \code{data.frame}.
#' @param header a logical indicating whether the first line contains the names 
#'  of the variables.
#' @param simplify a logical giving if the results should be simplified.
#'  If \code{FALSE} all vectors are character vectors, if \code{TRUE}
#'  the correct types are inferred by \pkg{pdfmole}.
#' @param keep a logical giving if the variables \code{"pid"} and \code{"row"}
#'  should be kept in the output.
#' @return
#'  Returns an object of class \code{"mole"}.
#' @export 
##  ----------------------------------------------------------------------------
mole  <- function(x, header = FALSE, simplify = FALSE, keep = FALSE) {
    assert_contains_columns(x, c("pid", "row", "col", "text"))
    if (!isTRUE(attr(x, "ordered"))) {
        x <- x[with(x, order(pid, row, col)),]
    }

    b <- !duplicated(x[c("pid", "row")])
    x$rowid <- cumsum(b)
    nrows <- tail(x$rowid, 1)
    ncolumns <- max(x$col)
    col_names <- character(ncolumns)
    
    d <- lapply(rep.int(nrows, ncolumns), character)
    for (i in seq_len(ncolumns)) {
        y <- x[which(x$col == i),]
        d[[i]][y$rowid] <- y$text
        if ( header & (nrows > 0) ) {
            col_names[i] <- d[[i]][1L]
            if ( simplify ) {
                d[[i]] <- simplify(d[[i]][-1L])
            } else {
                d[[i]] <- d[[i]][-1L]
            }
        }
    }

    if (keep) {
        d[[length(d) + 1L]] <- x[b, "row"]
        d[[length(d) + 1L]] <- x[b, "pid"]
        col_names <- make.names(c(col_names, "row", "pid"), unique = TRUE)
        j <- seq(length(d) - 1L, length(d))
        names(d) <- rep.int("", length(d))
        names(d)[j] <- col_names[j]
    }

    if (header) {
        names(d) <- col_names
        b <- nchar(names(d)) == 0
        names(d)[b] <- sprintf("X%i", seq_along(d))[b]
    }

    class(d) <- "mole"
    d
}


#' @export
#' @noRd
as.matrix.mole <- function(x, ...) do.call(cbind, x)


#' @export
#' @noRd
as.data.frame.mole <- function(x, ...) {
    class(x) <- "list"
    if ( isTRUE(list(...)$stringsAsFactors) ) {
        as.data.frame(x, ...)
    } else {
        as.data.frame(x, stringsAsFactors = FALSE)
    }
}


cutoff_text <- function(x, nc = 10L) {
    y <- sprintf("%s ~", substr(x, 1, nc - 2L))
    format_1 <- sprintf("%%-%is", nc)
    ifelse(nchar(x) <= nc , sprintf(format_1, x), y)
}


#' @export
#' @noRd
print.mole <- function(x, hide = TRUE, ...) {
    ncolumns <- length(x)
    nrows <- length(x[[1L]])
    nchar_header <- if (is.null(names(x))) double(ncolumns) else nchar(names(x))
    nchar_body <- sapply(x, function(y) max(nchar(y)))
    nchar_max <- pmax(nchar_header, nchar_body)
    x <- lapply(x, as.character)
    nprint <- 30L
    nhead <- ntail <- 6L
    nbody <- nprint - nhead - ntail
    max_col_width <- 8L
    if ( nrows > nprint ) {
        i <- c(seq_len(nhead), sort(sample(seq(nhead + 1L, nrows - ntail), nbody)),
               seq(nrows - ntail + 1L, nrows))
        for (j in seq_along(x)) {
            x[[j]] <- x[[j]][i]
            x[[j]][nhead + 1L] <- "..."
            x[[j]][nprint - ntail] <- "..."
        }
    }

    do_cutoff <- sum(nchar_max) + 2 * length(nchar_max) >  unlist(options("width"))
    for (j in seq_along(x)) {
        if ( !is.null(names(x)) ) {
                x[[j]] <- c(names(x)[j], x[[j]])
        }
        if ( (nchar_max[j] > max_col_width) &  do_cutoff ) {
            x[[j]] <- cutoff_text(x[[j]], max_col_width)
        }
        fmt <- sprintf("%%-%is", max(nchar(x[[j]])) + 2)
        x[[j]] <- sprintf(fmt, x[[j]])
    }
    
    txt <- do.call(paste0, x)
    writeLines(txt)
}

