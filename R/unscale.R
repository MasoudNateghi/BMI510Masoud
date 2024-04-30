#' Reverse scaling and centering of a vector
#'
#' This function reverses the scaling and centering operation performed on a vector using the scale() function in R.
#'
#' @param x A vector that has been scaled and centered using the scale() function
#' @return The original vector before scaling and centering
#'
#' @examples
#' x = scale(1:10)
#' unscale(x)
#'
#' @export
unscale = function(x) {
  # Retrieve the scaling information
  center = attr(x, "scaled:center")
  scale = attr(x, "scaled:scale")
  # Reverse the scaling
  unscaled = x * scale + center
  return(unscaled)
}
