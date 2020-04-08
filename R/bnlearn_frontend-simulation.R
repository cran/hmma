# generate random data from a bayesian network.
rbn = function(x, n = 1, ...) {

  UseMethod("rbn")

}#RBN

# catch-all, fallback method.
rbn.default = function(x, n = 1, ...) {

  check.bn.or.fit(x)

}#RBN.DEFAULT

# generate a random graph.
random.graph = function(nodes, num = 1, method = "ordered", ..., debug = FALSE) {

  # check the generation method.
  check.label(method, choices = graph.generation.algorithms,
    labels = graph.generation.labels, argname = "graph generation method",
    see = "random.graph")
  # check the node labels.
  check.nodes(nodes)
  # check the number of graph to generate.
  if (!is.positive.integer(num))
    stop(" the number of graphs to generate must be a positive integer number.")

  # expand and sanitize method-specific arguments.
  extra.args = check.graph.generation.args(method = method,
                 nodes = nodes, extra.args = list(...))

  random.graph.backend(num = num, nodes = nodes, method = method,
    extra.args = extra.args, debug = debug)

}#RANDOM.GRAPH

# create an empty graph from a given set of nodes.
empty.graph = function(nodes, num = 1) {

  random.graph(nodes = nodes, num = num, method = "empty", debug = FALSE)

}#EMPTY.GRAPH
