#' Learna a HMM-A from data
#'
#' The \code{learnModel} function, learns a HMM-A from the supplied data file.
#' The function first creates a random model: the initial and transition
#' distributions are initialized using a Dirichlet
#' \url{https://en.wikipedia.org/wiki/Dirichlet_distribution} distribution.
#' Thereafter the model is maximised for the datafile that is supplied.
#'
#' The \code{learnModel} makes use of the \code{mhsmm}
#' \code{\link[mhsmm]{hmmfit}} function. An example of the structure of the
#' datafile can be found in \code{\link{hmmaExampleData}}.
#'
#' @param data The datafile. The datafile should be a list containing a
#'   dataframe with the data as its $x component and contain the lengths of the
#'   observations a the $N component (see details).
#' @param amountOfStates The amount of states.
#' @param maxit The maximum amount of iterations.
#' @param seed Seed (optional).
#' @param iss The Imaginary Sample Size (iss), also called priors, to add data.
#' @param debug Debugmode.
#'
#' @return The output of the function is an asymmetric hidden Markov model. This
#'   model contains the amount of states, the initial distribution, the
#'   transition distribution and the emission distribution (Bayesian networks in
#'   the different states).
#'
#'   The model can quicly be visualised with the \code{\link{visualise}} method.
#'   The \code{\link{visualise}} method does not show the Bayesian networks
#'   within the states as this would result in unreadable graphs. Instead, the
#'   \code{bnlearn} \code{\link[bnlearn]{graphviz.plot}} method can be used (see
#'   the examples below).
#'
#' @examples
#' fit <- learnModel(data = hmmaExampleData, amountOfStates = 3, seed = 1234)
#' visualise(fit)
#'
#' # See bn in first state
#' library(bnlearn)
#' graphviz.plot(fit$parms.emission[[1]])
#'
#' @useDynLib hmma, .registration = TRUE
#'
#' @export
learnModel <- function(data, amountOfStates = 2, maxit = 50, seed, iss = 0.0001, debug = FALSE) {
  # Use seed when supplied
  if (!missing(seed)) {
    set.seed(seed = seed)
  }

  # Check datafile
  checkDataFile(data)

  # Create an initial distribution
  init0 <- randomNumbers(amountOfStates)

  # Create the transition distribution
  trans0 <- matrix(nrow = amountOfStates, ncol = amountOfStates)
  for (i in 1:amountOfStates) {
    trans0[i,] <- randomNumbers(amountOfStates)
  }

  if (debug) {
    cat("Init distribution:", init0, "\n")
    cat("Transition distribution:\n")
    print(trans0)
  }

  # Create a BN based on the data.
  bn <- createbnFromData(data)

  # Add default (basic) BN to each state
  bns0 <- list()
  for (i in 1:amountOfStates) {
    bns0[[i]] <- bn
  }

  # Create mstep function.
  mstep <- function(x,wt) {
    mstep.bnlearn(x = x, wt = wt, blacklist = NULL, iss = iss, debug = debug)
  }

  # Create the start model
  startModel <- createHmma(init = init0,
                           trans = trans0,
                           bns = bns0)

  # Use the data, the startmodel, the mstep and the max amount of iterations to
  # fit a model to the data.
  foundModel <- mhsmm::hmmfit(data,
                              start.val = startModel,
                              mstep = mstep,
                              maxit = maxit)

  # Return ONLY the found model
  foundModel$model
}
