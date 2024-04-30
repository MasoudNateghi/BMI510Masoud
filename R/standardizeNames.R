#' Standardize variable names in a tibble to "small_camel" case
#'
#' @param data A tibble
#' @return The input tibble with variable names in "small_camel" case
#' @import dplyr
#' @import janitor
#' @import snakecase
#' @examples
#' data = tibble(MyVar = 1:5, 'Another Var' = 6:10)
#' standardizeNames(data)
#'
#' @export
standardizeNames <- function(data) {
  data <- dplyr::rename_with(data, ~ snakecase::to_snake_case(.x), everything())
  data <- dplyr::mutate_all(data, ~ janitor::make_clean_names(.))
  return(data)
}


