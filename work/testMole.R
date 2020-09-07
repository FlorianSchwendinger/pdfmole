rm(list = ls())
library(pdfminer)
library(pdfmole)

devtools::load_all()

pdf_folder <- system.file("pdfs", package = "pdfmole")

##  cars.pdf -------------------------------------------------------------------

pdf_file <- file.path(pdf_folder, "cars.pdf")

pdf <- read_chars(pdf_file, pages = 1:2, maxpages = 2L)
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


## agstat.pdf ------------------------------------------------------------------

pdf_file <- file.path(pdf_folder, "agstat.pdf")

pdf <- read_chars(pdf_file, pages = 1, maxpages = 1L)
pdf$meta

d <- pdf$text
d <- group_blocks(d)
d <- align_rows(d, method = "hclust")

# plots
pixelplot(d, scale = 0.5)
bboxplot(d, pid = 1L)
textplot(d, pid = 1L)

# alignment
splits <- find_breaks(d, 0.5)
bboxplot(d, pid = 1L, splits)

d <- align_columns(d, split_points = splits)
d <- d[grep("Arial", d$font),]

x <- mole(d, header = F, simplify = T)
x

## vnalf2011graz.pdf  ---------------------------------------------------------

pdf_file <- file.path(pdf_folder, "vnalf2011graz.pdf")
    
pdf <- read_chars(pdf_file, pages = 1, maxpages = 1L)
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


## agstat.pdf  ---------------------------------------------------------------

pdf_file <- file.path(pdf_folder, "agstat.pdf")
    
pdf <- read_chars(pdf_file, pages = 1, maxpages = 1L)
pdf

df <- as.data.frame(pdf)
head(df, 20)

lines <- as.data.table(extract_lines(pdf[[1]]))
head(lines, 20)

cl <- align_lines(lines)

plot_boxes(cl)

row_breaks <- unique(unlist(cl[cl$horizontal, c('ystart')], use.names = F))
col_breaks <- unique(unlist(cl[cl$vertical, c('xstart')], use.names = F))

rows <- align_rows(df, method = "fw", split_points = row_breaks)
head(rows, 20)

cols <- align_columns(rows, method = "fw", split_points = col_breaks)
head(cols, 20)

M <- do.call(rbind, to_matrix(cols))
M <- rm_empty_rows(M)
View(M)

## column_span_1.pdf ----------------------------------------------------------

pdf_file <- file.path(pdf_folder, "column_span_1.pdf")

pdf <- read_chars(pdf_file, pages = 1, maxpages = 1L)
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
textplot(rows, breaks)

cols <- align_columns(rows, method = "fw", split_points = breaks) 
head(cols, 20)

M <- do.call(rbind, to_matrix(cols))
M <- rm_empty_rows(M)
View(M)