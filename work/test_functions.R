if ( FALSE ) {
    q("no")
    R
}


library(pdfminer)
library(pdfmole)

pdf_folder <- system.file("pdfs", package = "pdfmole")
pdf_file <- file.path(pdf_folder, "cars.pdf")


pdf <- read.pdf(pdf_file, pages = 1:2, maxpages = 2L)
pdf

d <- pdf$text
head(d, 20)

d <- d[!is.na(d$block),]

d <- group_blocks(d)
head(d, 20)

d <- align_rows(d)
head(d, 20)


# plots
intervalplot(d, 1)


# alignment
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

