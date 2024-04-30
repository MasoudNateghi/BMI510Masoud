#' Calculate maximum likelihood estimate of p for Bernoulli data
#'
#' This function calculates the maximum likelihood estimate of the parameter p
#' for Bernoulli data, which is represented as a vector of 0s and 1s.
#'
#' @param data A vector of 0s and 1s
#' @return The value of p that maximizes the log-likelihood
#' @examples
#' # logLikBernoulli(c(1,0,0,0,1,1,1))
#' @export

logLikBernoulli = function(data) {
  p_grid = seq(0, 1, by = 0.001)
  log_likelihoods = sapply(p_grid, function(p) sum(log(p^data * (1 - p)^(1 - data))))
  p_hat = p_grid[which.max(log_likelihoods)]
  return(p_hat)
}
