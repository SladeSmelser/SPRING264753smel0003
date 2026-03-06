#' Plot a Normal Curve and Shade Area up to a
#'
#' Plots a Normal distribution curve with mean \code{mu} and standard deviation
#' \code{sigma}, shades the area under the curve from -Inf to \code{a}, and
#' returns the probability P(X <= a).
#'
#' @param mu Numeric. The mean of the normal distribution.
#' @param sigma Numeric. The standard deviation of the normal distribution.
#' @param a Numeric. The upper bound for the shaded region P(X <= a).
#'
#' @return A named list with components:
#' \describe{
#'   \item{mu}{The mean used.}
#'   \item{sigma}{The standard deviation used.}
#'   \item{area}{The computed probability P(X <= a), rounded to 4 decimal places.}
#' }
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
#'
#' @export
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean=mu, sd=sigma), xlim=c(mu - 3*sigma, mu + 3*sigma))

  xcurve <- seq(mu - 3*sigma, a, length=1000)
  ycurve <- dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col="Red")

  area <- round(pnorm(a, mean=mu, sd=sigma), 4)
  text(mu - 1.5*sigma, max(ycurve)/2, paste("Area =", area))

  list(mu=mu, sigma=sigma, area=area)
}
