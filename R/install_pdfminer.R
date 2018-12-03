
set_params <- function(x, ...) {
    params <- list(...)
    for (i in seq_along(params)) {
        x <- gsub(sprintf("@%s", names(params)[i]),
                  as.character(params[[i]]), x, fixed = TRUE)
    }
    x
}


## "https://api.github.com/repos/user/repo.rb/zipball"
## "https://api.github.com/repos/user/repo.rb/tarball"
## @params user a character string giving the user name
## @params repo a character string giving the name of reo
## @return a character vector giving the github url
github_url <- function(user, repo) {
    base_url <- "https://github.com/@user/@repo/archive/master.zip"
    set_params(base_url, user = user, repo = repo)
}


## user <- "euske"
## repo <- "pdfminer.six"
## https://github.com/pdfminer/pdfminer.six/archive/master.zip
download_github <- function(user, repo, destfile, ...) {
    url <- github_url(user, repo)
    args <- c(list(url = url, destfile = destfile), list(...))
    do.call(download.file, args)
}


#  -----------------------------------------------------------------------------
#  install_pdfminer
#  ================
#' @title Install \pkg{pdfminer}
#' @description Install pdfminer from \R.
#' @param pdfminer_dir a character string giving the path where \pkg{pdfminer} 
#'   should be installed.
#' @param user a character string giving the user name of the \pkg{pdfminer} 
#'   github repository owner, default is \code{"pdfminer"}.
#' @param repo a character string giving the name of the \pkg{pdfminer} github 
#'   repository, default is \code{"pdfminer.six"}.
#' @param ... additional arguments passed to \code{download.file}.
#' @details To use \pkg{pdfmole} the \code{Python} package \pkg{pdfminer}
#'   needs to be available. There exist two versions of \pkg{pdfminer},
#'   \pkg{pdfminer} and \pkg{pdfminer.six}, where \pkg{pdfminer.six} is a fork
#'   from \pkg{pdfminer}. For more information can be found in the \pkg{pdfmole}
#'   \code{README} file.
#' @examples
#' \dontrun{
#' install_pdfminer("~/Software/pdfminer")
#' }
#' @export
install_pdfminer <- function(pdfminer_dir, user = "pdfminer", repo = "pdfminer.six", ...) {
    assert(check_character(pdfminer_dir), check_character(user), check_character(repo))
    pdfminer_dir <- normalizePath(pdfminer_dir)
    assert(check_directory_exists(pdfminer_dir))

    destfile <- file.path(pdfminer_dir, "master.zip")
    download_github(user, repo, destfile, ...)
    
    pdfminer_files <- unzip(destfile, exdir = pdfminer_dir)
    unlink(destfile)

    repo_dir <- file.path(pdfminer_dir, sprintf("%s-master", repo))
    source_dir <- file.path(repo_dir, "pdfminer")
    file.copy(source_dir, pdfminer_dir, recursive = TRUE)
    file.copy(file.path(repo_dir, c("LICENSE", "README.md")), file.path(pdfminer_dir, "pdfminer"))
    unlink(repo_dir, recursive = TRUE)

    pyfiles <- file.path("python", c("__init__.py", "pdf2list.py"))
    pyfiles <- system.file(pyfiles, package = "pdfmole")
    file.copy(pyfiles, pdfminer_dir)    
}

#  -----------------------------------------------------------------------------
#  pdfminer_set_environment_variable
#  =================================
#' @title Set \pkg{pdfminer} Environment Variable
#' @description Set the environment variable \code{"PDFMINER\_HOME"}.
#' @param pdfminer_dir a character string giving the path where \pkg{pdfminer} 
#'   should be installed.
#' @details The environment variable \code{"PDFMINER\_HOME"} contains the 
#'   path to \pkg{pdfminer}, this information if needed in order to load
#'   \pkg{pdfminer} from \pkg{pdfmole}.
#' @examples
#' \dontrun{
#' pdfminer_set_environment_variable("~/Software/pdfminer")
#' }
pdfminer_set_environment_variable <- function(pdfminer_dir) {
    pdfminer_dir <- normalizePath(pdfminer_dir)
    assert(check_directory_exists(pdfminer_dir))
    env_folder <- normalizePath("~")
    env_path <- file.path(env_folder, ".Renviron")
    x <- if (file.exists(env_path)) readLines(env_path) else character()
    env_var <- sprintf("PDFMINER_HOME=%s", shQuote(pdfminer_dir))
    x <- unique(c(x, env_var))
    writeLines(x, env_path)
}

