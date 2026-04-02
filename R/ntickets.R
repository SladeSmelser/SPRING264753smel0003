#' Calculate Number of Tickets to Sell for Airline Overbooking
#'
#' @description
#' Calculates the optimal number of tickets to sell for a flight using both
#' the discrete binomial distribution and the normal approximation.
#' Also produces two plots of the objective function vs n.
#'
#' @param N Integer. Number of seats on the flight.
#' @param gamma Numeric. Probability that the plane will be truly overbooked
#'   (more people show than there are seats). Must be between 0 and 1.
#' @param p Numeric. Probability that a ticketed passenger will show up.
#'   Must be between 0 and 1.
#'
#' @return A named list with the following elements:
#'   \item{nd}{Number of tickets to sell discrete binomial method}
#'   \item{nc}{Number of tickets to sell normal approximation method}
#'   \item{N}{Number of seats on the flight}
#'   \item{p}{Probability of a passenger showing up}
#'   \item{gamma}{Probability of overbooking}
#'
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#'
#' @export
ntickets <- function(N, gamma, p) {

  # Discrete method using binomial distribution
  #Searching over a range of n values above N
  n_range <- N:(N + 50) #This creates a buffer for finding the optimal n

  discrete <- 1 - gamma - pbinom(N, n_range, p)

  # nd: the n where the absolute value of objective is minimized
  nd <- n_range[which.min(abs(discrete))] #which.min() finds the first occurrence of the minimum value

  # Normal approximation method
  #  1 - gamma - pnorm(N + 0.5, n*p, sqrt(n*p*(1-p))) = 0
  continuous <- function(n) {
    1 - gamma - pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p)))
  }

  nc_result <- uniroot(continuous, interval = c(N, N + ceiling(N * 0.2) + 20)) #Finds the zero of a contious interval
  nc <- ceiling(nc_result$root) #ceiling rounds up to the nearest integer

  # Plot for Discrete result
  obj_vals_d <- 1 - gamma - pbinom(N, n_range, p)

  plot(n_range, obj_vals_d,
       type = "b",
       pch = 21,
       bg = "blue",
       col = "blue",
       xlab = "n",
       ylab = "Objective",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",
                     nd, ") gamma= ", gamma, " N=", N, " discrete"))
  abline(v = nd, col = "red", lwd = 2)
  abline(h = 0, col = "black", lwd = 1)

  # Plot for Continuous
  n_seq <- seq(N, N + ceiling(N * 0.1) + 10, by = 0.1) #creates the grid of n values
  obj_vals_c <- continuous(n_seq) #calculates objective at each point

  plot(n_seq, obj_vals_c,
       type = "l",
       col = "black",
       lwd = 2,
       xlab = "n",
       ylab = "Objective",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",
                     round(nc_result$root, 6), ") gamma= ", gamma,
                     " N=", N, " continuous"))
  abline(v = nc_result$root, col = "red", lwd = 2)
  abline(h = 0, col = "black", lwd = 1)

  #Return named list of results and numbers
  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(result)
  invisible(result)
}

