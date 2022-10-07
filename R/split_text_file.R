#' @name split_text_file
#'
#' @alias split_text_file
#'
#' @title Split a text file on matched character string
#'
#' @description Read in a text file and split it into pieces based on
#' a specified string of characters
#'
#' @param file the text file to be split into pieces
#' @param split_pattern string pattern of characters to use in splitting the file
#' @param comment_pattern string pattern to replace with the comment character
#' @param comment_char defaults to "#"
#'
#' @return Returns a list of character strings resulting from splitting the original
#' text file
#'
#' @author Neil Klepeis
#'
#' @details The list of character string can be read into a tibble using read_csv
#'  This function is useful for pre-processing a text file of sensor data
#'  that contains multiple experiments or runs that are separated by a fixed character string (tag)
#'
#'
# -----------------------------------------------

split_text_file <- function(file, split_pattern, comment_pattern=NULL,
                            comment_character="#") {

  data <- read_file(file)

  # Add comment to lines with, e.g., pattern= "MM/dd/yyyy" for DustTrak
  if (!is.null(comment_pattern))
    data <- str_replace_all(string=data, pattern=comment_pattern,
                            replacement = comment_character)

  #  split into list
  str_split_fixed(string = data, pattern=split_pattern,
                  n = Inf)

}
