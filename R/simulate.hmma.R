#' Simulates a HMM-A.
#'
#' \code{simulate.hmma} simulates a HMM-A. The only difference between
#' \code{simulate.hmma} and \code{simulate.hmmspec} is that this function adds
#' the names of the nodes of the Bayesian networks to the results. The
#' \code{simulate.hmmspec} function only uses numbers, instead of names.
#'
#' @param object A hmma object
#' @param nsim An integer or vector of integers (for multiple sequences)
#'   specifying the length of the sequence(s)
#' @param seed \code{seed} for the random number generator
#' @param ...	Further arguments passed to or from other methods.
#'
#' @return A simulation with the following values \itemize{ \item \code{s} The
#'   states of the model. \item \code{x} The observations. \item \code{n} The
#'   length of the observation. }
#'
#' @export
#'
#' @import stats
#'
#' @examples
#' # Get a HMM-A (e.g. from learnModel or createHmma)
#' model <- learnModel(data = hmmaExampleData, amountOfStates = 2, seed = 1234)
#'
#' # Simulate the data, results in 100 observation sequences of length 20.
#' data <- simulate(model, nsim = c(rep(20, 100)))
#'
simulate.hmma <- function(object, nsim, seed = NULL, ...) {
  simulation <- mhsmm::simulate.hmmspec(object, nsim, seed, rand.emission = rbn.hssm, ...)

  # Steps to get the node names
  # 1. Get a BN from any state (we select the first)
  # 2. Extract nodes with the bnlearn::nodes() functio
  # 3. Add nodenames to the simulated data.
  #
  # The bnlearn::nodes() functions is used as this is the same function that is
  # used in the custom implementation of rbn.hssm.

  bn <- object$parms.emission[[1]]
  nodes <- bnlearn::nodes(bn)
  colnames(simulation$x) <- nodes

  simulation
}
