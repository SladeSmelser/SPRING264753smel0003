#' Simulate a Binomial Distribution
#'
#' Simulates a Binomial experiment by running \code{iter} independent
#' experiments, each consisting of \code{n} Bernoulli trials with success
#' probability \code{p}. Plots the simulated proportions as a bar chart and
#' returns the proportion table.
#'
#' @param iter Integer. Number of simulation iterations. Default is 100.
#' @param n    Integer. Number of Bernoulli trials per experiment. Default is 10.
#' @param p    Numeric. Probability of success on each trial (0 < p < 1). Default is 0.5.
#'
#' @return A named numeric vector of proportions for each outcome 0 through n.
#'
#' @examples
#' mybin(iter = 1000, n = 10, p = 0.7)
#'
#' @export
mybin <- function(iter = 100, n = 10, p = 0.5) {
  # Matrix to store all samples (n rows x iter columns)
  sam.mat <- matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  succ <- c()

  for (i in 1:iter) {
    # Sample n Bernoulli trials (1=success, 0=failure)
    sam.mat[, i] <- sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))
    # Count successes in this experiment
    succ[i] <- sum(sam.mat[, i])
  }

  # Build a frequency table over all possible outcomes 0:n
  succ.tab <- table(factor(succ, levels = 0:n))

  # Plot proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
