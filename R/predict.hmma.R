#' Predict sequence of data on HMMA
#'
#' \code{predict.hmma} is simply a wrapper function for the mhsmm
#' \code{\link[mhsmm]{predict.hmmspec}} function. It generates the predicted
#' state sequence for dat data, given the model.
#'
#' From the mhsmm documentation: If method="viterbi", this technique applies the
#' Viterbi algorithm for HMMs, producing the most likely sequence of states
#' given the observed data. If method="smoothed", then the individually most
#' likely (or smoothed) state sequence is produced, along with a matrix with the
#' respective probabilities for each state. This function differs from
#' predict.hmm in that it takes the output from hmmspec ie. this is useful when
#' users already know their parameters and wish to make predictions.
#'
#' @param object The HMMA
#' @param data  The data file
#' @param method The prediction method (viterbi or smoothed, see details)
#' @param ... Futher arguments.
#'
#' @return The return object contains the data, a vector `s` with the
#'   reconstructed state sequence, the vector N with the lengths of the
#'   sequences, a matrix p with the probabilities of the states (only with
#'   smoothed). For more details see \code{\link[mhsmm]{predict.hmmspec}}.
#'
#'   The loglikelihood is returned as the \code{$loglik} component.
#'
#' @seealso \code{\link[mhsmm]{predict.hmmspec}} for more information.
#'
#' @export
#'
predict.hmma <- function(object, data, method = "viterbi", ...) {
  mhsmm::predict.hmmspec(object, newdata = data, method = method, ...)
}
