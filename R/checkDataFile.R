#' Check the data file
#'
#' This function checks if the data file has the required format.
#'
#' @param data The data file.
#'
#' @keywords internal
#'
checkDataFile <- function(data) {
  # Check if data file is a list
  if (!(is.list(data))) {
    stop("The datafile is not a list. See the documentation for the required format.")
  }

  # Check if datafile has an $x component of class dataframe
  if (!(is.data.frame(data$x))) {
    stop("The datafile's $x component is not a dataframe.")
  }

  # Check if datafile has an $N component of class integer
  if (!(is.numeric(data$N))) {
    stop("The datafile's $N component is not an integer vector.")
  }

  # Verify sum of $N equals length of $x
  sumN <- sum(data$N)
  lengthX <- nrow(data$x)

  if (!(sumN == lengthX)) {
    stop("The observation sequence ($N) has a different amount of observations than the $x component.")
  }
}
