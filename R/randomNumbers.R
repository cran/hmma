#' Creates a random distribution over numbers.
#'
#' The \code{randomNumbers} function returns a vector of numbers that are random
#' within some bandwidth. The count of all the numbers is equal to \code{sumTo},
#' which defaults to 1.
#'
#' @param amount The amount of numbers in the vector.
#' @param sumTo When all numbers in the returned vector are sumemd, they sum up
#'   to sumTo.
#' @param seed A seed when required.
#'
#' @keywords internal
#'
randomNumbers <- function(amount, sumTo = 1, seed) {
  # Set seed when instructed
  if (!missing(seed)) {
    set.seed(seed)
  }

  # Create a vector of numbers based on Dirichlet distribution
  numbers <- MCMCpack::rdirichlet(1, rep(1, amount))
  numbers * sumTo
}
