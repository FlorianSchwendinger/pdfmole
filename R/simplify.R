

str_table <- function(x) {
    table(unlist(strsplit(x, "", fixed = TRUE)))
}


guess_class <- function(x) {
    x <- trimws(x)
    stab <- str_table(x)
    if ( length(stab) == 8L ) {
        true_false <- c("TRUE", "FALSE")
        if ( all(tolower(names(stab)) %in% names(str_table(tolower(true_false)))) ) {
            xtab <- table(toupper(x))
            if ( all(names(xtab) %in% true_false) ) {
                return("logical")
            }
        }
    }
    if ( length(stab) <= 10L ) {
        if ( all(names(stab) %in% as.character(c(0:9))) ) {
            return("integer")
        }
    }
    if ( length(stab) <= 11L ) {
        if ( all(names(stab) %in% c(0:9, ".", ",")) ) {
            return("double")
        }
    }
    return("character")
}


#' @export
#' @noRd
simplify <- function(x, ...) UseMethod("simplify", x)


#' @export
#' @noRd
simplify.character <- function(x, ...) {
    xtype <- guess_class(x)
    as_class <- switch(xtype,
        logical = as.logical, 
        integer = as.integer,
        double = as.double,
        default = identity)
    as_class(x)
}

