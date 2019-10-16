# pdfmole

The **`R`** package **pdfmole** can used to read in tables from 'PDF'-files.

### Installation

To install the package, run the following code:

``` r
devtools::install_github("ben-schwen/pdfmole")
``` 

### Backend

**pdfmole** is using **PDFMiner**

### Usage

There is a simple pipeline for extracting tables from pdfs.

#### Identify boundaries of table.

This is an optional step if your table takes up the whole space.

To select the boundaries visually, you can make usage of the select_area\1 function,
which makes a call to shiny GUI for selecting the boundaries.

``` r
select_area(x)
```

#### Remove unnecessary characters

This is an optional step.

#### Decide whether usage of lines or whitespaces as cell separator

##### Lines as separator

##### Whitespaces as separator



#### Identify rows

#### Identify columns