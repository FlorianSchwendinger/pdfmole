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
class(pdf)

pdf$meta

d <- pdf$text
head(d, 20)

d <- d[!is.na(d$block),]

d <- group_blocks(d)
head(d, 20)

d <- align_rows(d)
head(d, 20)
tail(d)

# plots
pixelplot(d, scale = 0.5)
pixelplot(d, scale = 0.5, 2)

bboxplot(d, pid = 2L)

textplot(d, pid = 1L)


select_area(d)

# alignment
d <- align_columns(d, split_points = c(78, 126, 220))
head(d, 20)

d <- d[grep("Courier", d$font),]
head(d, 20)


M <- do.call(rbind, pdfmole:::to_matrix(d))
M <- rm_empty_rows(M)
M

cars_new <- as.data.frame(M[-1, -1], stringsAsFactors = FALSE)
colnames(cars_new) <- M[1, -1]
cars_new






#
# devel
#
if ( FALSE ) {
    q("no")
    R
}

library(pdfminer)
library(pdfmole)

pdf_folder <- system.file("pdfs", package = "pdfmole")
pdf_file <- file.path(pdf_folder, "cars.pdf")


pdf <- read.pdf(pdf_file, pages = 1:2, maxpages = 2L)
pdf$meta

d <- pdf$text
head(d, 20)

d <- d[!is.na(d$block),]
# d <- group_blocks(d)
d <- align_rows(d)
d <- align_columns(d, split_points = c(78, 126, 220))
d <- d[grep("Courier", d$font),]

x <- d
x <- x[sample(seq_len(nrow(x))),]
x <- group_columns(x)
head(x)
all(with(x, order(pid, row, col)) == seq_len(nrow(x)))


df <- mole(x, header = T, simplify = T)
df
all(as.data.frame(df)[,-1] == cars)
df <- mole(x, T)
df
df <- mole(x)
df
x <- df 

lapply(df, guess_type)

x <- df[[2L]]

x <- sample(c("TRUE", "FALSE"), 10, TRUE)




df[1:4,]

x

format_columns

head(d)


z <- names(options())
cutoff_text(z, 10L)

ifelse(nchar(z) <= 10, z, cutoff_text(z, 10L))

cutoff_text(, 10)

sprintf("%-6s", "a")


sprintf("|%-10s|", "Hello");

options("width")



nchar(names(x))



"~"




