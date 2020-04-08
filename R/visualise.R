#' Visualise the HMM-A
#'
#' The function \cite{visualise} shows a graphical representation of the found
#' HMM-A. Only the initial distribution and transition distribution are shown.
#' Each state contains a Bayesian network, these would be unreadble when
#' displayed within each state. The \code{bnlearn}
#' \code{\link[bnlearn]{graphviz.plot}} method is used to display the bayesian
#' networks.
#'
#' @param model The model.
#' @param numDigitsRound The amount of decimals that should be presented in the
#'   graph.
#'
#' @examples
#' # First, we need a model that we want to visualise, we create
#' # one using the learnModel function.
#' fit <- learnModel(data = hmmaExampleData, amountOfStates = 3)
#'
#' # To visualise the states and transitions, we use the visualise method
#' visualise(fit)
#'
#' # To visualise the BNs within the states, use the code below
#' library(bnlearn)
#' graphviz.plot(fit$parms.emission[[1]])
#'
#' @export
#'
visualise <- function(model, numDigitsRound = 2) {
  check.and.load.package("Rgraphviz")

  # Define attributes variables
  # nAttr: node attributes
  # eAttr: edge attributes
  eAttrs <- c()
  nAttrs <- c()

  # We start by creating the main structure (all nodes, without start)
  stateNum <- length(model$parms.emission)
  nodes <- as.character(1:stateNum)

  # Create graph with nodes
  graph <- methods::new("graphNEL", nodes = nodes, edgemode = "directed")

  # Add edges between nodes
  for (i in 1:length(nodes)) {
    for (j in 1:length(nodes)) {
      weight <- round(model$transition[i, j], digits = numDigitsRound)
      graph <- graph::addEdge(nodes[i], nodes[j], graph, weight)
    }
  }

  # Add initial distribution
  graph <- graph::addNode("start", graph)
  nAttrs$color <- c(nAttrs$color, start = "green")
  for (i in 1:stateNum) {
    weight <- round(model$init[i], digits = numDigitsRound)
    graph <- graph::addEdge("start", nodes[i], graph, weights = weight)
    edgeName <- paste("start", nodes[i], sep = "~")
    eAttrs$color <- c(eAttrs$color, "green")
    names(eAttrs$color) <- c(names(eAttrs$color[1:(length(eAttrs$color)-1)]), edgeName)
  }


  # Add labels
  ew <- as.character(unlist(graph::edgeWeights(graph)))
  names(ew) <- graph::edgeNames(graph, recipEdges = "distinct")
  eAttrs$label <- ew
  attrs <- list(node=list(shape="ellipse", fixedsize=FALSE))
  attrs$edge$fontsize <- 10

  # Plot model
  plotObject <- Rgraphviz::layoutGraph(graph, nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrs, recipEdges = "distinct")
  Rgraphviz::renderGraph(plotObject)
}
