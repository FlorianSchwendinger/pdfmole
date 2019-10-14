rm(list = ls())
library(pdfmole)
library(magrittr)

pdf_folder <- system.file("pdfs", package = "pdfmole")

##  column_span_1.pdf ----------------------------------------------------------
pdf_file <- file.path(pdf_folder, "column_span_1.pdf")
pdf <- read.pdf(pdf_file, pages = 1, maxpages = 1L)
pdf

df <- as.data.frame(pdf)
head(df, 20)

blocks <- group_blocks(df)
head(blocks, 20)

rows <- align_rows(blocks, method = "hclust")
rows <- rows[rows$row > 1,]
head(rows, 20)

# notice increase of quantile lower_bound
breaks <- find_breaks(rows, lower_bound = 0.5)
breaks

cols <- align_columns(rows, method = "fixed_width", split_points = breaks) 
head(cols, 20)
colorplot(cols, breaks)
intervalplot(cols)

M <- do.call(rbind, to_matrix(cols))
M <- rm_empty_rows(M)
M

df_new <- as_df(M[-1,])
colnames(df_new) <- M[1,]
head(df_new)


##  cars.pdf -------------------------------------------------------------------

pdf_folder <- system.file("pdfs", package = "pdfmole")
pdf_file <- file.path(pdf_folder, "cars.pdf")

pdf <- read.pdf(pdf_file, pages = 1:2, maxpages = 1L)
pdf

d <- as.data.frame(pdf)
head(d, 20)
    
d <- group_blocks(d)
head(d, 20)
    
d <- align_rows(d)
head(d, 20)

intervalplot(d)
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

## agstat.pdf ------------------------------------------------------------------

pdf_file <- file.path(pdf_folder, "agstat.pdf")
pdf <- read.pdf(pdf_file, pages = 1, maxpages = 1L)
pdf

df <- as.data.frame(pdf)
head(df, 20)

blocks <- group_blocks(df)
head(blocks, 20)

rows <- align_rows(blocks, method = "hclust")
rows <- rows[rows$row > 1,]
head(rows, 20)

# notice increase of quantile lower_bound
breaks <- find_breaks(rows, lower_bound = 0.5)
breaks

cols <- align_columns(rows, method = "fixed_width", split_points = breaks) 
head(cols, 20)
colorplot(cols, breaks)