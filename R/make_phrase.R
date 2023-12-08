#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export

make_phrase <- function(num = 10,
                        num_word = "ten",
                        item = "lords",
                        verb = "a-leaping",
                        adjective = "",
                        location = "") {

  ## Replace NAs with blank strings
  item <- replace_na(item, "")
  verb <- replace_na(verb, "")
  adjective <- replace_na(adjective, "")
  location <- replace_na(location, "")

  ## Pluralize if needed, or change num word to "a/an"
  vowel_start <- str_sub(item, 1, 1) %>% str_detect("[aeiou]")
  if (num > 1) {
    item <- pluralize_gift(item)
  } else if (vowel_start) {
    num_word <- "an"
  } else {
    num_word <- "a"
  }

  glue::glue("{num_word} {adjective} {item} {verb} {location}") %>%
    str_squish()
}

