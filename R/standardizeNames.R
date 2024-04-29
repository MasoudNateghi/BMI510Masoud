#' Standardize variable names in a tibble to "small_camel" case
#'
#' @param data A tibble
#' @return The input tibble with variable names in "small_camel" case
#' @import dplyr janitor snakecase
#' @examples
#' data = tibble(MyVar = 1:5, 'Another Var' = 6:10)
#' standardizeNames(data)
#'
#' @export
standardizeNames = function(data) {
  library(dplyr)
  library(janitor)
  library(snakecase)
  library(tibble)

  data = rename_with(data, ~ to_snake_case(make_clean_names(.)), everything())
  return(data)
}
