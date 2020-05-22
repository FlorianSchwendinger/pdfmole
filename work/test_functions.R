if ( FALSE ) {
    q("no")
    R
}

# options(stringsAsFactors=FALSE)

library(pdfminer)
library(pdfmole)

pdf_folder <- system.file("pdfs", package = "pdfmole")
pdf_file <- file.path(pdf_folder, "cars.pdf")

pdf <- read.pdf(pdf_file, pages = 1:2, maxpages = 2L)
pdf$meta

d <- pdf$text
d <- group_blocks(d)
d <- align_rows(d)

# plots
pixelplot(d, scale = 0.5)
bboxplot(d, pid = 1L)
textplot(d, pid = 1L)

# alignment
d <- align_columns(d, split_points = c(78, 126, 220))
d <- d[grep("Courier", d$font),]

x <- mole(d, header = TRUE, simplify = TRUE)
x
all.equal(as.data.frame(x)[,-1], cars)






