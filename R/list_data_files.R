#' @name list_data_files
#'
#' @alias list_data_files
#'
#' @title Get a list of data files for processing
#'
#' @description Look in current directory for files with specified prefixes
#' and extension
#'
#' @param path path to look for files
#' @param prefixes prefix(es) for matching files, defaults ot "CNR", "IPS", "PurpleAir", "SidePak", "DustTrak"
#' @param extension extension for matching files, defaults to "csv"
#' @param full.names logical, whether to return full names with paths, default to TRUE
#' @param recursive logical, whether to look into subdirectories recursively to match files, defaults to TRUE
#'
#'
#' @return list of data file names matching prefix and extension
#'
#' @author Neil Klepeis
#'
#' @details  By default looks recursively in sub-directories and returns full names
#' with the file path relative to current directory
#'
# -----------------------------------------------

list_data_files <- function (path=".",
                             prefixes=c("CNR","IPS","SidePak","DustTrak"),
                             middle="",
                             extension="csv", full.names=TRUE,
                             recursive=TRUE) {

  pattern <- paste0("^",paste0(prefixes, collapse="|"),
                    ".*?",middle,".*?\\.", extension,"$")

  cat("Search pattern: ", pattern, "\n")

  list.files(path=path, pattern=pattern, full.names = full.names,
             recursive=recursive)

}

