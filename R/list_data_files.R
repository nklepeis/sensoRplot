#' @name list_data_files
#'
#' @alias list_data_files
#'
#' @title Get a list of data files for processing
#'
#' @description Look in current directory for files with specified prefixes
#' and extension
#'
#' @param prefixes defaults ot "CNR", "IPS", "PurpleAir", "SidePak", "DustTrak"
#' @param extension defaults to "csv"
#'
#' @return list of full data file names matching prefix and extension
#'
#' @author Neil Klepeis
#'
#' @details  Looks recursively in sub-directories and returns full names
#' with the file path relative to current directory
#'
# -----------------------------------------------

list_data_files <- function (prefixes=c("CNR","IPS","SidePak","DustTrak"),
                             extension="csv") {

  pattern <- paste0("^",paste0(prefixes, collapse="|"),
                    ".*?\\.", extension,"$")

  cat("Search pattern: ", pattern, "\n")

  list.files(pattern=pattern, full.names = TRUE,
             recursive=TRUE)

}

