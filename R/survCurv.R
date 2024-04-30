#' Plot survival curve
#'
#' This function plots the survival curve based on the input status and time data.
#'
#' @param status A vector indicating the status of each individual (1 for event occurrence, 0 for censoring).
#' @param time A vector of time points corresponding to each individual's event or censoring time.
#'
#' @return None (plot is generated).
#'
#' @examples
#' data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
#'
#' @export

survCurv = function(status, time) {
  # Combine status and time into a data frame
  data = data.frame(time, status)

  # Sort data by time
  data = data[order(data$time),]

  # Calculate survival probabilities
  n = nrow(data)
  survival = numeric(n)
  risk_set = n

  for (i in 1:n) {
    if (i == 1) {
      survival[i] = ifelse(data$status[i] == 1, (n - 1) / n, 1)
    } else {
      survival[i] = survival[i - 1] * ifelse(data$status[i] == 1, (risk_set - 1) / risk_set, 1)
    }
    risk_set = risk_set - 1
  }

  data$survival = survival

  # Plot the survival curve
  plot(data$time, data$survival, type = "s", lwd = 2, xlab = "Time", ylab = "Survival Probability", main = "Survival Curve")
}

