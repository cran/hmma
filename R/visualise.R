#' Visualise the HMM-A
#'
#' The function \cite{visualise} shows a graphical representation of the found
#' HMM-A. Only the initial distribution and transition distribution are shown.
#' Each state contains a Bayesian network, these would be unreadble when
#' displayed within each state. The \code{bnlearn}
#' \code{\link[bnlearn]{graphviz.plot}} method can be used to display the
#' bayesian networks.
#'
#' Per default, all transitions (initial and transition) are drawn on the
#' canvas. For larger HMM-As, the visualisation can become cluttered with arcs
#' that have a low probability. By setting the \code{minProb}, an arc is only
#' drawn when it is equal to or greater than the specified \code{minProb}. The
#' default value for \code{minProb} is 0.00 (all arcs are drawn).
#'
#' To further improve the visualisation, edge line width can be related to the
#' respective weight of the edge. When \code{relateWeightWithThickness} is
#' \code{TRUE}, an edge with a higher weight will be drawn with a wider line.
#' The minimum width of a line and the maximum width can be supplied via the
#' respective parameters.
#'
#' The \code{minWidth} is associated with a weight of 0.00, the maxWidth is
#' related to the weight 1.00. When \code{widthType} is set to 'linear', the
#' width of a line is linearly related to the weight. When \code{widthType} is
#' set to 'sigmoid', the width of a line is determined via a sigmoid of the
#' weight.
#'
#' @param model The model.
#' @param minProb The minimum probability of an arc (initial or transition) to
#'   be drawn in the visualisation (see details).
#' @param numDigitsRound The amount of decimals that should be presented in the
#'   graph.
#' @param relateWidthWithWeight An edge with a high weight will be drawn with a
#'   thicker line compared to an edge with a lower weight.
#' @param widthType The thickness function for the lines. Defaults to linear
#'   function. When \code{sigmoid} is used, a sigmoid function is used to
#'   determine the thickness.
#' @param minWidth The minimum thickness for a line. Only used when
#'   \code{relateWidthWithWeight} is \code{TRUE}
#' @param maxWidth The maximum thickness for a line. Only used when
#'   \code{relateWidthWithWeight} is \code{TRUE}
#'
#' @examples
#' # First, we need a model that we want to visualise, we create
#' # one using the learnModel function.
#' fit <- learnModel(data = hmmaExampleData, amountOfStates = 3)
#'
#' # To visualise the states and transitions, we use the visualise method.
#' # Only lines with a weight of 0.10 are drawn.
#' visualise(fit, minProb = 0.10)
#'
#' # When it is not desired to relate the width of a line with its weight,
#' # this can be disabled:
#' visualise(fit, minProb = 0.10, relateWidthWithWeight = FALSE)
#'
#' # Finally, it is possible to use a sigmoid instead of a linear relation:
#' visualise(fit, minProb = 0.10, widthType = 'sigmoid')
#'
#' # To visualise the BNs within the states, use the code below
#' library(bnlearn)
#' graphviz.plot(fit$parms.emission[[1]])
#'
#' @export
#'
visualise <- function(model,
                      minProb = 0.00,
                      numDigitsRound = 2,
                      relateWidthWithWeight = TRUE,
                      widthType = 'linear',
                      minWidth = 0.05,
                      maxWidth = 5.00) {
  check.and.load.package("Rgraphviz")

  # Select thickness function, defaults to linear
  thicknessFunction <- linearThickness
  if (widthType == 'sigmoid') {
    thicknessFunction <- sigmoidThickness
  }

  # Line width information
  widths <- c()

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

      # Only add the edge when >= the minProb
      if (weight >= minProb) {
        graph <- graph::addEdge(nodes[i], nodes[j], graph, weight)

        # Determine and set line width
        lineWidth <- thicknessFunction(weight,
                                       minWidth = minWidth,
                                       maxWidth = maxWidth)
        widths <- c(widths, lineWidth)
      }
    }
  }

  # Add initial distribution
  graph <- graph::addNode("start", graph)
  nAttrs$color <- c(nAttrs$color, start = "green")
  for (i in 1:stateNum) {
    weight <- round(model$init[i], digits = numDigitsRound)
    # Only add the edge when >= the minProb
    if (weight >= minProb) {
      graph <- graph::addEdge("start", nodes[i], graph, weights = weight)
      edgeName <- paste("start", nodes[i], sep = "~")
      eAttrs$color <- c(eAttrs$color, "green")
      names(eAttrs$color) <- c(names(eAttrs$color[1:(length(eAttrs$color)-1)]), edgeName)

      # Determine and set line width
      lineWidth <- thicknessFunction(weight,
                                     minWidth = minWidth,
                                     maxWidth = maxWidth)
      widths <- c(widths, lineWidth)
    }
  }

  # Add textual labels to the arcs, which must be equal to the weights.
  # 1. We get the weights from the graph and convert them to characters
  ew <- as.character(unlist(graph::edgeWeights(graph)))
  # 2. We assign edgeNames as names to the stringified weights (labels)
  names(ew) <- graph::edgeNames(graph, recipEdges = "distinct")
  # 3. We add this ew (edge weight) object to eAttrs (edge attributes)
  eAttrs$label <- ew
  attrs <- list(node=list(shape="ellipse", fixedsize=FALSE))
  attrs$edge$fontsize <- 10

  # We use the names of the 'ew' variables, so we know which weight belongs
  # to which arc.
  names(widths) <- names(ew)

  # The code below (in comment) should work, but does not. A bit of explanation.
  # When we call the edgeRenderInfo function, it used the edgeNames function
  # internally. The problem is that reciprocating edges are not always correctly
  # found. So, when we have start, 1 and 2 as nodes, with arrows from start -> 1,
  # start -> 2, 1 -> 1, 1 -> 2, 2 -> 1, 2 -> 2. In that case, '2 -> 1' would not
  # be found. It would not give it a 'lwd' and therefore, the whole graph would
  # be incorrectly rendered.
  #graph::edgeRenderInfo(graph) <- list(lwd = widths)

  # This however, can be fixed by directly editing the graph slots. We just fill
  # in the widths array.
  if (relateWidthWithWeight) {
    graph@renderInfo@edges$lwd <- widths
  }

  # Plot model
  plotObject <- Rgraphviz::layoutGraph(graph, nodeAttrs = nAttrs, edgeAttrs = eAttrs, attrs = attrs, recipEdges = "distinct")
  Rgraphviz::renderGraph(plotObject)
}

# This is a helper function for the thickness of the lines
# It uses a sigmoid for determining the thickness.
sigmoidThickness <- function(weight, minWidth = 0.01, maxWidth = 5.0) {
  sigmoid <- 1 / (1 + exp(-8 * (weight - 0.5)))

  thickNess <- (maxWidth - minWidth) * sigmoid + minWidth

  thickNess
}

# This is a helper function for the thickness of the lines
# It uses a linear functions for determining the thickness.
linearThickness <- function(weight, minWidth = 0.01, maxWidth = 5.0) {
  thickNess <- weight * (maxWidth - minWidth) + minWidth

  thickNess
}
