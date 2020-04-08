
# construct a fake markov blanket using all the nodes within distance 2.
fake.markov.blanket = function(learn, target) {

  mb = c(unlist(lapply(learn[[target]]$nbr,
         function(cur) learn[[cur]]$nbr)), learn[[target]]$nbr)
  mb = setdiff(unique(mb), target)

  return(mb)

}#FAKE.MARKOV.BLANKET

# include v-structures in the network, setting the corresponding arc directions.
vstruct.apply = function(arcs, vs, nodes, strict, debug = FALSE) {

  if (debug)
    cat("----------------------------------------------------------------\n")

  for (i in seq(nrow(vs))) {

    x = vs[i, "x"]
    y = vs[i, "y"]
    z = vs[i, "z"]
    max_a = vs[i, "max_a"]

    # check whether the network already includes conflicting v-structures.
    if (!(is.listed(arcs, c(y, x)) && is.listed(arcs, c(z, x)))) {

      if (debug)
        cat("* not applying v-structure", y, "->", x, "<-", z, "(", max_a, ")\n")

      if (strict)
        stop("vstructure ", y, " -> ", x, " <- ", z, " is not applicable, ",
          "because one or both arcs are oriented in the opposite direction.")
      else
        warning("vstructure ", y, " -> ", x, " <- ", z, " is not applicable, ",
          "because one or both arcs are oriented in the opposite direction.")

      next

    }#THEN

    # tentatively add the arcs that make up the v-structure.
    temp = set.arc.direction(y, x, arcs)
    temp = set.arc.direction(z, x, temp)

    # check whether the network is acyclic.
    if (!is.acyclic(temp, nodes, directed = TRUE)) {

      if (debug)
        cat("* not applying v-structure", y, "->", x, "<-", z, "(", max_a, ")\n")

      if (strict)
        stop("vstructure ", y, " -> ", x, " <- ", z, " is not applicable, ",
          "because one or both arcs introduce cycles in the graph.")
      else
        warning("vstructure ", y, " -> ", x, " <- ", z, " is not applicable, ",
          "because one or both arcs introduce cycles in the graph.")

      next

    }#THEN

    if (debug)
      cat("* applying v-structure", y, "->", x, "<-", z, "(", max_a, ")\n")

    # save the updated arc set.
    arcs = temp

  }#FOR

  return(arcs)

}#VSTRUCT.APPLY

# emergency measures for markov blanket and neighbourhood recovery.
bn.recovery = function(bn, nodes, strict, filter = "AND", mb = FALSE,
    debug = FALSE) {

  .Call("bn_recovery",
        bn = bn,
        strict = strict,
        mb = mb,
        filter = match(filter, c("OR", "AND")),
        debug = debug)

}#BN.RECOVERY

# explore the structure of the network using its arc set.
cache.structure = function(nodes, arcs, amat = NULL, debug = FALSE) {

  # rebuild the adjacency matrix only if it's not available.
  if (is.null(amat))
    amat = arcs2amat(arcs, nodes)

  .Call("cache_structure",
        nodes = nodes,
        amat = amat,
        debug = debug)

}#CACHE.STRUCTURE

# explore the structure of the neighbourhood of a target node.
cache.partial.structure = function(nodes, target, arcs, amat = NULL,
    debug = FALSE) {

  # rebuild the adjacency matrix only if it's not available.
  if (is.null(amat))
    amat = arcs2amat(arcs, nodes)

  .Call("cache_partial_structure",
        nodes = nodes,
        target = target,
        amat = amat,
        debug = debug)

}#CACHE.PARTIAL.STRUCTURE
