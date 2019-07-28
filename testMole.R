rm(list = ls())
library(pdfmole)
library(magrittr)
setwd("/home/ben/github/pdfmole/inst/pdfs/")

pdf <- read.pdf("mtcars.pdf")

blocks <- pdf %>% as.data.frame() %>% group_blocks()

breaks <- find_breaks(blocks)

cols <- blocks %>% align_columns(method = "automatic")
rows <- cols %>% align_rows(method = "hclust")

