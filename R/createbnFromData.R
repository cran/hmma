#' Creates a Bayesian network from a datafile
#'
#' The \code{createbnFromData} creates a Bayesian network from the datafile that
#' is supplied. The \code{$x} parameter of the datafile should contain the
#' actual data, while the $N component should contain the observation lengths.
#'
#' The possible outputs of a node are based on the unique values in the dataset.
#' All unique values from a column (a random variable) are converted to possible
#' outputs of the node in the BN.
#'
#' @param data The datafile as specified above.
#' @param seed Seed.
#' @param debug Switches debug mode on/off.
#'
#' @keywords internal
#'
createbnFromData <- function(data, seed, debug = FALSE) {
  # Get columns
  columns <- colnames(data$x)

  # For each column, find its factors
  factorsPerColumns <- list()
  for (i in 1:length(columns)) {
    factors <- levels(data$x[,i])
    factors
    factorsPerColumns[[i]] <- factors
  }

  # Create the bn using the createbn function
  bn <- createbn(nodeNames = columns,
                 emissionOutputPerNode = factorsPerColumns,
                 seed = seed)

  # Return the bn
  bn
}
