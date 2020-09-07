library("pdfmole")

test_pdfboxr <- function() {
    pdf_folder <- system.file("pdfs", package = "pdfmole")
    pdf_file <- file.path(pdf_folder, "cars.pdf")
    if ( file.exists(pdf_file) & requireNamespace("pdfboxr") ) {
        pdf <- pdfboxr::read_chars(pdf_file)
        pdf
    } else {
        data("pdfmole")
        pdf <- data_pdfboxr
        pdf
    }

    d <- pdf$text
    head(d, 20)
    
    d <- align_rows(d)
    head(d, 20)

    # pixelplot(d)
    d <- align_columns(d, split_points = c(78, 126, 220))
    head(d, 20)

    d <- d[grep("Courier", d$font),]
    head(d, 20)

    d <- group_cells(d)

    df <- mole(d, header = TRUE, simplify = TRUE)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df$X1 <- NULL
    
    stopifnot(all.equal(dim(cars), dim(df)))
    stopifnot(all.equal(colnames(cars), colnames(df)))
    stopifnot(all.equal(cars[,1], df[,1]))
}


test_pdfminer <- function() {
    pdf_folder <- system.file("pdfs", package = "pdfmole")
    pdf_file <- file.path(pdf_folder, "cars.pdf")
    if ( file.exists(pdf_file) & requireNamespace("pdfminer") ) {
        pdf <- pdfboxr::read_chars(pdf_file)
        pdf
    } else {
        data("pdfmole")
        pdf <- data_pdfboxr
        pdf
    }

    d <- pdf$text
    head(d, 20)
    
    d <- align_rows(d)
    head(d, 20)

    # pixelplot(d)
    d <- align_columns(d, split_points = c(78, 126, 220))
    head(d, 20)

    d <- d[grep("Courier", d$font),]
    head(d, 20)

    d <- group_cells(d)

    df <- mole(d, header = TRUE, simplify = TRUE,keep = TRUE)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df$X1 <- NULL
    
    stopifnot(all.equal(dim(cars), dim(df)))
    stopifnot(all.equal(colnames(cars), colnames(df)))
    stopifnot(all.equal(cars[,1], df[,1]))
}


test_pdfboxr <- function() {
    pdf_folder <- system.file("pdfs", package = "pdfmole")
    pdf_file <- file.path(pdf_folder, "cars.pdf")
    if ( file.exists(pdf_file) & requireNamespace("pdfboxr") ) {
        pdf <- pdfboxr::read_chars(pdf_file)
        pdf
    } else {
        data("pdfmole")
        pdf <- data_pdfboxr
        pdf
    }

    d <- pdf$text
    head(d, 20)
    
    d <- align_rows(d)
    head(d, 20)

    # pixelplot(d)
    d <- align_columns(d, split_points = c(78, 126, 220))
    head(d, 20)

    d <- d[grep("Courier", d$font),]
    head(d, 20)

    d <- group_cells(d)

    df <- mole(d, header = TRUE, simplify = TRUE)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df$X1 <- NULL
    
    stopifnot(all.equal(dim(cars), dim(df)))
    stopifnot(all.equal(colnames(cars), colnames(df)))
    stopifnot(all.equal(cars[,1], df[,1]))
}


test_pdfboxr()
test_pdfboxr()
test_pdfboxr()
