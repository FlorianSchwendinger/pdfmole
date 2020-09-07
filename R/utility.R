
assert_contains_columns <- function(x, columns) {
    b <- columns %in% colnames(x)
    if (!all(b)) {
        plural <- if (sum(!b) == 1L) "" else "s"
        msg <- sprintf("object does not contain the required column%s %s", 
            plural, deparse(columns[!b]))
        stop(msg)
    }
}

rm_na <- function(x) {
    x[!is.na(x$x0),]
}


as_df <- function(x) as.data.frame(x, stringsAsFactors = FALSE)

rm_empty_rows <- function(x) {
    stopifnot(is.matrix(x) & is.character(x))
    b <- apply(x == "", 1, all)
    x[!b,]
}

rm_rows_regex <- function(x, pattern, ignore.case = FALSE, perl = FALSE, 
    fixed = FALSE, useBytes = FALSE, invert = FALSE) {
    i <- grep(pattern = pattern, x$text, ignore.case = ignore.case, 
              perl = perl, fixed = fixed, useBytes = useBytes, invert = invert)
    x[!x$row %in% unique(x[i, "row"]),]
}

rm_fonts <- function(x, fonts, invert = FALSE) {
    if ( invert ) {
        b <- (x$font %in% fonts)
    } else {
        b <- !(x$font %in% fonts)
    }
    x[b,]
}

rm_size <- function(x, sizes, invert = FALSE, min_size = 0L, max_size = Inf, digits = 2L) {
    x$size <- round(x$size, digits)
    if ( !missing(sizes) ) {
        if ( invert ) {
            b <- (x$size %in% sizes)
        } else {
            b <- !(x$size %in% sizes)
        }
        x <- x[b,]
    }
    b <- (min_size <= x$size) & (x$size <= max_size)
    x[b,]
}

keep_only_most_common_size <- function(x) {
    tab <- table(round(x$size, 2))
    sizes <- as.double(names(tab))[which.max(tab)]
    rm_size(x, sizes = sizes, invert = TRUE)
}

rm_rows_char_count <- function(x, min_char = 0L, max_char = Inf) {
    id <- paste(x$pid, x$row, sep = "_")
    char_count <- sapply(split(x$text, id), function(x) sum(nchar(x)))
    i <- match(id, names(char_count))
    char_count <- char_count[i]
    b <- (min_char <= char_count) & (char_count <= max_char)
    x[b,]
}

keep_only_most_common_font <- function(x) {
    rm_fonts(x, fonts = names(which.max(table(df$font))), invert = TRUE)
}



rm_char <- function(x, c) {
    x[x$text != c,]
}

rm_bound <- function(x, xmin, xmax, ymin, ymax) {
    x[x$x0 > xmin & x$x1 < xmax &
      x$y0 > ymin & x$y1 < ymax,]
}

