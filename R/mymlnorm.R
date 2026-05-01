#' Maximum Likelihood Estimates for a Normal Distribution
#'
#' Uses a grid search to find the maximum likelihood estimates for the
#' mean and standard deviation of a normally distributed sample.
#' Produces a contour plot of the likelihood surface L(mu, sigma).
#'
#' @param x A numeric vector of sample data
#' @param mu A numeric vector of candidate values for the mean
#' @param sig A numeric vector of candidate values for the standard deviation
#' @param ... Additional arguments passed to \code{contour()}
#'
#' @return A list with the following components:
#' \describe{
#'   \item{x}{The original data vector}
#'   \item{coord}{Matrix indices of the maximum likelihood on the grid}
#'   \item{maxl}{The maximum likelihood value}
#'   \item{muest}{The MLE for the mean}
#'   \item{sigest}{The MLE for the standard deviation}
#' }
#'
#' @export
#'
#' @examples
#' mymlnorm(
#'   x   = c(10, 12, 13, 15, 12, 11, 10),
#'   mu  = seq(8, 18,  length = 1000),
#'   sig = seq(0.1, 5, length = 1000)
#' )
mymlnorm <- function(x, mu, sig, ...) {
  nmu  <- length(mu)
  nsig <- length(sig)
  n    <- length(x)
  zz   <- c()
  lfun <- function(x, m, p) log(dnorm(x, mean = m, sd = p))
  for (j in 1:nsig) {
    z  <- outer(x, mu, lfun, p = sig[j])
    y  <- apply(z, 2, sum)
    zz <- cbind(zz, y)
  }
  maxl  <- max(exp(zz))
  coord <- which(exp(zz) == maxl, arr.ind = TRUE)
  contour(mu, sig, exp(zz),
          xlab = expression(mu),
          ylab = expression(sigma),
          main = expression(paste("L(", mu, ",", sigma, ")")), ...)
  abline(v = mean(x),               lwd = 2, col = "Green")
  abline(h = sqrt((n-1)/n) * sd(x), lwd = 2, col = "Red")
  muest  <- mu[coord[1]]
  sigest <- sig[coord[2]]
  abline(v = muest, h = sigest)
  return(list(x = x, coord = coord, maxl = maxl,
              muest = muest, sigest = sigest))
}
