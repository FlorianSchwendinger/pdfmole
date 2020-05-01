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


#-- bbox_names <- function() c("x0", "y0", "x1", "y1")

#-- char_names <- function() c("text", "font", bbox_names(), "size")

split_string <- function(x) as.double(unlist(strsplit(x, ",", fixed = TRUE)))

## The input x is an list where each element represents a single character
## with meta data about the font, position and size in some cases only
## the character is given (e.g. \n).
#-- text_to_df <- function(x) {
#--     char_to_df <- function(text = NA_character_, font = NA_character_, 
#--         bbox = rep.int(NA_real_, 4), size = NA_real_) {
#--         y <- c(list(text), font, bbox, size)
#--         names(y) <- char_names()
#--         as_df(y)
#--     }
#--     do.call(rbind, lapply(x, function(y) do.call(char_to_df, y)))
#-- }

#-- textbox_to_df <- function(x) {
#--     box_to_df <- function(wmode = NA_character_, id = NA_real_, bbox = rep.int(NA_real_, 4)) {
#--         if ( is.character(bbox) ) {
#--             bbox <- as.double(unlist(strsplit(bbox, ",", fixed = TRUE)))
#--         }
#--         names(bbox) <- bbox_names()
#--         as_df(as.list(c(wmode = wmode, id = id, bbox)))
#--     }
#--     do.call(rbind, lapply(x, function(y) do.call(box_to_df, y)))
#-- }

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

#' @noRd
#' @export
extract_lines <- function(x) {
    stopifnot(inherits(x, 'pdf_page'))
    stopifnot(length(x$line) > 0)

    res <- as.data.frame(do.call(rbind, simplify_list(x$line)))
    colnames(res) <- c('linewidth', 'x0', 'y0', 'x1', 'y1')

    res$horizontal <- FALSE
    res$vertical <- FALSE

    res$horizontal[(res$y1 - res$y0) < 0.05] <- TRUE
    res$vertical[(res$x1 - res$x0) < 0.05] <- TRUE

    res
}
