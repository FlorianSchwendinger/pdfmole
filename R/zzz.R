.onAttach <- function( libname, pkgname ) {

    pdfminer_missing_message <- function() {
        packageStartupMessage("'pdfminer' sources could not be found. ",
            "Information about the installation of 'pdfminer' can be found in the README.")
    }

    pyExec("import os")
    
    ##
    ## In the best case pdfminer is installed an working!
    ##
    pdfminer_home <- Sys.getenv("PDFMINER_HOME")
    if ( nchar(pdfminer_home) > 0L ) {
        if ( dir.exists(file.path(pdfminer_home, "pdfminer")) ) {
            cwd <- getwd()
            on.exit(setwd(cwd))
            setwd(pdfminer_home)
            pyExecfile("pdf2list.py")
        } else {
            pdfminer_missing_message()
        }
    } else {
        pdfmole_path <- system.file("python", package = "pdfmole")
        pdfminer_wrapper <- system.file("python/pdf2list.py", package = "pdfmole")
   
        if ( nchar(pdfminer_wrapper) > 0L & file.exists(pdfminer_wrapper) ) {
            pyExecfile(pdfminer_wrapper)
        } else {
            pdfminer_missing_message()
        }
    }
}
