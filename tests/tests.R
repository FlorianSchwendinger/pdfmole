if (FALSE) {
    q("no")
    R
}

suppressPackageStartupMessages(library(pdfmole))

read_pdf_cars <- function() {
    pdf_folder <- system.file("pdfs", package = "pdfmole")
    pdf_file <- file.path(pdf_folder, "cars.pdf")
    if ( file.exists(pdf_file) ) {

        pdf <- read.pdf(pdf_file, pages = 1:2, maxpages = 2L)
        pdf

        d <- as.data.frame(pdf)
        head(d, 20)
        
        d <- group_blocks(d)
        head(d, 20)
        
        d <- align_rows(d)
        head(d, 20)

        intervalplot(d, 1)
        d <- align_columns(d, split_points = c(88, 130, 220))
        head(d, 20)

        d <- d[grep("Courier", d$font),]
        head(d, 20)

        M <- do.call(rbind, to_matrix(d))
        M <- rm_empty_rows(M)
        M
        
        cars_new <- as.data.frame(M[-1, -1], stringsAsFactors = FALSE)
        colnames(cars_new) <- M[1, -1]
        cars_new

        stopifnot(all.equal(dim(cars), dim(cars_new)))
        stopifnot(all.equal(colnames(cars), colnames(cars_new)))
        stopifnot(all.equal(cars[,1], as.integer(cars_new[,1])))
    }
}

read_pdf_mtcars <- function() {
    pdf_folder <- system.file("pdfs", package = "pdfmole")
    pdf_file <- file.path(pdf_folder, "mtcars.pdf")
    
    if ( file.exists(pdf_file) ) {
        pdf <- read.pdf(pdf_file, pages = 1, maxpages = 1L)
        pdf

        df <- as.data.frame(pdf)
        head(df, 20)

        blocks <- group_blocks(df)
        head(blocks, 20)
    
        breaks <- find_breaks(blocks)
        breaks
    
        rows <- align_rows(blocks, method = "hclust")
        head(rows, 20)

        intervalplot(rows)
    
        cols <- align_columns(rows, method = "auto") 
        head(cols, 20)
    
        colorplot(cols, breaks)
        textplot(cols, breaks)
    
        M <- do.call(rbind, to_matrix(cols))
        M <- rm_empty_rows(M)
        M
    
        mtcars_new <- as.data.frame(M[-1, -1], stringsAsFactors = FALSE)
        colnames(mtcars_new) <- M[1, -1]
        rownames(mtcars_new) <- M[-1, 1]
        mtcars_new
    
        stopifnot(all.equal(dim(mtcars), dim(mtcars_new)))
        stopifnot(all.equal(colnames(mtcars), colnames(mtcars_new)))
        stopifnot(all.equal(mtcars[, 1], as.numeric(mtcars_new[, 1])))
        stopifnot(all.equal(mtcars[, 2], as.numeric(mtcars_new[, 2])))
        stopifnot(all.equal(mtcars[, 3], as.numeric(mtcars_new[, 3])))
        stopifnot(all.equal(mtcars[, 4], as.numeric(mtcars_new[, 4])))
        stopifnot(all.equal(mtcars[, 5], as.numeric(mtcars_new[, 5])))
        stopifnot(all.equal(mtcars[, 6], as.numeric(mtcars_new[, 6])))
        stopifnot(all.equal(mtcars[, 7], as.numeric(mtcars_new[, 7])))
        stopifnot(all.equal(mtcars[, 8], as.numeric(mtcars_new[, 8])))
        stopifnot(all.equal(mtcars[, 9], as.numeric(mtcars_new[, 9])))
        stopifnot(all.equal(mtcars[,10], as.numeric(mtcars_new[,10])))
        stopifnot(all.equal(mtcars[,11], as.numeric(mtcars_new[,11])))
    }
}

read_vnalf <- function() {
    pdf_folder <- system.file("pdfs", package = "pdfmole")
    pdf_file <- file.path(pdf_folder, "vnalf2011graz.pdf")
    
    if ( file.exists(pdf_file) ) {
        pdf <- read.pdf(pdf_file, pages = 1, maxpages = 1L)
        pdf

        df <- as.data.frame(pdf)
        head(df, 20)

        df_rm <- rm_char(df, ' ')
        df_rm <- rm_char(df_rm, '\n')
        head(df_rm, 20)

        select_area(df_rm)
        df_sort <- rm_bound(df_rm, 37.1, 565, 49.2, 780.4)
        textplot(df_sort)

        breaks <- find_breaks(df_sort)
        colorplot(df_sort, breaks)
        
        breaks <- c(60, 120, 210, 255, 300, 340, 400, 485, 530)
        colorplot(df_sort, split_points = breaks)
        textplot(df_sort, split_points = breaks)

        rows <- align_rows(df_sort, method = "exact_match")
        head(rows, 20)
    
        cols <- align_columns(rows, method = "fw", split_points = breaks) 
        head(cols, 20)
    
        colorplot(cols, breaks)

        M <- do.call(rbind, to_matrix(cols))
        M <- rm_empty_rows(M)
        M
    
        View(M)
    }
}


read_pdf_agstat <- function() {
    pdf_folder <- system.file("pdfs", package = "pdfmole")
    pdf_file <- file.path(pdf_folder, "agstat.pdf")
    
    if ( file.exists(pdf_file) ) {
        pdf <- read.pdf(pdf_file, pages = 1, maxpages = 1L)
        pdf

        df <- as.data.frame(pdf)
        head(df, 20)

        lines <- extract_lines(pdf[[1]])
        head(lines, 20)

        plot_boxes(lines)
        hlines <- lines[lines$horizontal,]
        plot_boxes(hlines)
        vlines <- lines[lines$vertical,]
        plot_boxes(vlines))

  
    }
}

if (FALSE) {
    read_pdf_cars()
    read_pdf_mtcars()
}

