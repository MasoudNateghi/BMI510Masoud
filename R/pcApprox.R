#' Approximate data based on a specified number of principal components
#'
#' @param x The original data
#' @param npc The number of principal components to use for approximation
#' @return The approximated data
#' @examples
#' x = matrix(rnorm(50), nrow = 10)
#' approx_x = pcApprox(x, npc = 3)
#'
#' @export
pcApprox = function(x, npc) {
  # Compute principal components
  pca = prcomp(x, center = TRUE, scale. = TRUE)

  # Extract PCs and scores
  pc_scores = pca$x[, 1:npc]
  pc_loadings = pca$rotation[, 1:npc]

  # Reconstruct data using npc principal components
  approx_data = pc_scores %*% t(pc_loadings)

  # Re-scale and recenter approximated data
  approx_data = approx_data * rep(pca$scale, each = nrow(x)) + rep(pca$center, each = nrow(x))

  return(approx_data)
}
