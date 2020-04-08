#' Create an asymmetric hidden Markov model.
#'
#' The \code{createHmma} method creates a HMM-A from the specifications
#' provided. The amount of states are implicitly derived from the initial
#' distribution. Bayesian networks can be created using the \code{bnlearn}
#' package.
#'
#' @param init This initial distribution as a vector.
#' @param trans The transition distribution as a matrix.
#' @param bns The Bayesian networks for the states.
#'
#' @export
#'
#' @seealso \code{\link{bnlearn}} for more information regarding the creation of
#'   Bayesian networks
#'
#' @examples
#' # Start by creating the initial and transition distribution
#' init <- c(0.3, 0.7)
#' trans <- c(0.4, 0.7, 0.6, 0.3)
#' dim(trans) <- c(2,2)
#'
#' # Create a Baysian network for each state using 'bnlearn'
#' library(bnlearn)
#' struc <- model2network("[X1][X2]")
#' cptX1 <- matrix(c(0.15, 0.85), ncol = 2, dimnames = list(NULL, c("TRUE", "FALSE")))
#' cptX2 <- matrix(c(0.7, 0.3), ncol = 2, dimnames = list(NULL, c("TRUE", "FALSE")))
#'
#' bn1 <- custom.fit(struc, dist = list(X1 = cptX1,
#'                                      X2 = cptX2))
#'
#' struc <- model2network("[X2|X1][X1]")
#' cptX1 <- matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("TRUE", "FALSE")))
#' cptX2 <- matrix(c(0.9, 0.1, 0.5, 0.5), nrow = 2, ncol = 2)
#' dimnames(cptX2) <- list("X2" = c("TRUE", "FALSE"),
#'                         "X1" = c("TRUE", "FALSE"))
#'
#' bn2 <- custom.fit(struc, dist = list(X1 = cptX1,
#'                                      X2 = cptX2))
#'
#' bns <- list()
#' bns[[1]] <- bn1
#' bns[[2]] <- bn2
#'
#' # Create the model
#' hmma <- createHmma(init = init, trans = trans, bns = bns)
#'
createHmma <- function(init, trans, bns) {
  # The model is created by using the mhsmm package function
  model <- mhsmm::hmmspec(init = init,
                          trans = trans,
                          parms.emission = bns,
                          dens.emission = dbn.hssm,
                          rand.emission = rbn.hssm)

  # The class 'hmma' is added for S3 generics
  class(model) <- c("hmma", class(model))

  model
}
