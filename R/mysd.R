
#' My Standard Deviation Function
#'
#' @param x Numeric vector.
#' @param na.rm Logical indicating whether to remove NA values before computation.
#' @importFrom stats var
#'
#' @returns Standard deviation of the numeric vector
#' @export
#'
#' @examples
#' mysd(c(1, 2, 3, 4, 5))
utils::globalVariables(c("filename", "size_kb", "kb", "object"))
mysd <- function(x, na.rm = TRUE) {
  if (na.rm){
    x <- x[!is.na(x)]
  }
  sqrt(var(x))
}
