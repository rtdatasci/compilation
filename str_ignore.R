#' str_ignore function - opposite of str_subset - excludes elements
#'     of a string vector that have specific pattern
#' 
#' @param string an input character string
#' @param pattern regular expression for string pattern
#' 
#' @export
str_ignore <- function(string, pattern) {
  string[!str_detect(string, pattern)]
}

