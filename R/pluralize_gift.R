#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(item){

  if (str_detect(item, "oo")) {
    item <- str_replace(item, "oo", "ee")
  } else if (str_detect(item, "y")) {
    item <- str_replace(item, "y", "ies")
  } else {
    item <- paste0(item, "s")
  }
}
