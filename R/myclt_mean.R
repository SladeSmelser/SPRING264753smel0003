#' Central Limit Theorem Simulation for Sample Means (Uniform Distribution)
#'
#' Simulates the sampling distribution of the sample mean by drawing repeated
#' samples from a Uniform(a, b) distribution. Produces a histogram of the
#' simulated sample means overlaid with a theoretical normal curve based on
#' the Central Limit Theorem.
#'
#' @param n Integer. Sample size for each iteration. Default is 10.
#' @param iter Integer. Number of iterations (simulated samples). Default is 10000.
#' @param a Numeric. Lower bound of the uniform distribution. Default is 0.
#' @param b Numeric. Upper bound of the uniform distribution. Default is 5.
#'
#' @return A numeric vector of length \code{iter} containing the simulated sample means.
#' A histogram is produced as a side effect.
#'
#' @details
#' For a Uniform(a, b) distribution, the population mean is \eqn{\mu = (a+b)/2}
#' and the population variance is \eqn{\sigma^2 = (b-a)^2/12}. By the Central
#' Limit Theorem, the distribution of the sample mean \eqn{\bar{Y}} approaches
#' \eqn{N(\mu, \sigma^2/n)} as \eqn{n} increases.
#'
#' @examples
#' # Basic usage with defaults
#' w <- myclt_mean()
#'
#' # Custom parameters
#' w <- myclt_mean(n = 30, iter = 10000, a = 0, b = 10)
#' mean(w)
#' var(w)
#'
#' @export
myclt_mean <- function(n = 10, iter = 10000, a = 0, b = 5) {

  # Generate n*iter random values from Uniform(a, b)
  y <- runif(n * iter, a, b)

  # Arrange into a matrix: rows = sample size, cols = iterations
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Compute the sample mean for each iteration (column)
  sm <- apply(data, 2, mean)

  # Build histogram without plotting to get density info
  h <- hist(sm, plot = FALSE)

  # Set y-axis limit with 10% buffer above max density
  ymax <- 1.1 * max(h$density)

  # Plot histogram of sample means
  hist(sm,
       col    = rainbow(length(h$mids)),
       freq   = FALSE,
       ylim   = c(0, ymax),
       main   = paste("CLT: Distribution of Sample Mean\n",
                      "n = ", n, ", iter = ", iter,
                      ", Uniform(", a, ", ", b, ")", sep = ""),
       xlab   = "Sample Mean",
       ylab   = "Density")

  # Overlay theoretical normal curve (CLT approximation)
  curve(dnorm(x, mean = (a + b) / 2, sd = (b - a) / sqrt(12 * n)),
        add = TRUE, col = "Blue", lty = 2, lwd = 3)

  # Add legend
  legend("topright",
         legend = c("Theoretical Normal (CLT)"),
         col    = "Blue",
         lty    = 2,
         lwd    = 3,
         bty    = "n")

  # Invisibly return the vector of sample means
  invisible(sm)
}
