#' Creates a Bayesian network without arcs.
#'
#' The \code{createbn} function create a Bayesian network without any arcs. The
#' nodenames supplied as argument are used to determine the amount of nodes and
#' their names. Furthermore, the possible emissions from the nodes are begin
#' retrieved from the \code{emissionOutputPerNode} argument.
#'
#' @param nodeNames The names that the nodes must have.
#' @param emissionOutputPerNode The possible outputs per node.
#' @param seed The seed for the randomized functions.
#'
#' @keywords internal
#'
createbn <- function(nodeNames, emissionOutputPerNode, seed) {
  # Check names
  if (missing(nodeNames)) {
    stop("Each node must be given a name.")
  }

  # Check the emissionOutputPerNode
  if (missing(emissionOutputPerNode)) {
    stop("The emissions for each node must be supplied.")
  }

  # Check if the lengths are equal
  if (length(nodeNames) != length(emissionOutputPerNode)) {
    stop("The amount of emissions is not equal to the amount of nodes.")
  }

  # Set seed, if a seed is supplied
  if(!missing(seed)) {
    set.seed(seed = seed)
  }

  # Create a structurestring
  networkstring <- paste(nodeNames, sep = "", collapse = "][")
  networkstring <- paste("[", networkstring, "]", sep = "", collapse = "")

  # Assign each node its corresponding emission output together with a cpt
  # (random distributed).
  cpts <- list()
  for(i in 1:length(nodeNames)) {
    amountOfEmissions = length(emissionOutputPerNode[[i]])
    randoms <- randomNumbers(amountOfEmissions)

    cpts[[i]] <- matrix(randoms,
                        ncol = amountOfEmissions,
                        dimnames = list(NULL, emissionOutputPerNode[[i]]))
  }
  names(cpts) <- nodeNames

  # Create structure of Bayesian network
  bn = bnlearn::model2network(networkstring)

  # Add conditional probability tables to the bn
  bn = bnlearn::custom.fit(bn, dist = cpts)

  # Return the bn
  bn
}
