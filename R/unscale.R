#' Reverse scaling and centering of a vector
#'
#' @param x A vector that has been scaled and centered
#' @return The original vector before scaling and centering
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
