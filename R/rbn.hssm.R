#' Random emission from Bayesian network.
#'
#' \code{rbn.hsmm} returns a random sample from the Bayesian network in the
#' given state for the given model.
#'
#' @param state The state of the model.
#' @param model The model.
#'
#' @keywords internal
#'
#' @return A vector with the samples of each node in the Bayesian network.
#'
rbn.hssm <- function(state, model) {
  bn <- model$parms.emission[[state]]
  sample <- bnlearn::cpdist(bn, nodes = bnlearn::nodes(bn), evidence = TRUE, n = 1)

  as.vector(as.matrix(sample))
}
