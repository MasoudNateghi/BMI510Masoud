#' Calculate minimum sample size for a t-test
#'
#' @param x1 A vector of preliminary data (required)
#' @param x2 An optional second vector of preliminary data
#' @return The minimum sample size needed for a t-test with 0.8 power at alpha=0.05
#' @import pwr
#' @examples
#' x1 = rnorm(100)
#' minimumN(x1)
#' x2 = rnorm(150)
#' minimumN(x1, x2)
#'
#' @export

minimumN = function(x1, x2 = NULL) {
  alpha = 0.05  # Significance level
  power = 0.80  # Desired power

  if (is.null(x2)) {
    # One-sample t-test
    # Calculate the effect size based on the sample mean and standard deviation
    d = (mean(x1) - 0) / sd(x1)  # Assuming mu (null hypothesis) is 0
    result = pwr.t.test(d = d, sig.level = alpha, power = power, type = "one.sample")
  } else {
    # Two-sample t-test
    # Calculate the effect size based on the difference of means and pooled standard deviation
    n1 = length(x1)
    n2 = length(x2)
    sp = sqrt(((n1 - 1) * sd(x1)^2 + (n2 - 1) * sd(x2)^2) / (n1 + n2 - 2))
    d = abs(mean(x1) - mean(x2)) / sp
    result = pwr.t.test(d = d, sig.level = alpha, power = power, type = "two.sample", alternative = "two.sided")
  }

  return(ceiling(result$n))  # Returning the ceiling of the sample size to ensure it is an integer
}

