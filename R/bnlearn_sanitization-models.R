
# check an object of class bn.
check.bn = function(bn) {

  if (missing(bn))
    stop("an object of class 'bn' is required.")
  if (!is(bn, "bn")) {

    stop(sprintf("%s must be an object of class 'bn'.",
           deparse(substitute(bn))))

  }#THEN

}#CHECK.BN

# check two bn's against each other.
match.bn = function(bn1, bn2) {

  # the two networks must have the same node set.
  nodes1 = names(bn1$nodes)
  nodes2 = names(bn2$nodes)

  equal = setequal(nodes1, nodes2) && (length(nodes1) == length(nodes2))

  if (!equal)
    stop("the two networks have different node sets.")

}#MATCH.BN

# check an object of class bn or bn.fit.
check.bn.or.fit = function(bn) {

  if (missing(bn))
    stop("an object of class 'bn' or 'bn.fit' is required.")
  if (!is(bn, "bn") && !is(bn, "bn.fit")) {

    stop(sprintf("%s must be an object of class 'bn' or 'bn.fit'.",
           deparse(substitute(bn))))

  }#THEN

}#CHECK.BN.OR.FIT

# check an object of class bn.fit.
check.fit = function(bn) {

  if (missing(bn))
    stop("an object of class 'bn.fit' is required.")
  if (!is(bn, "bn.fit")) {

    stop(sprintf("%s must be an object of class 'bn.fit'.",
           deparse(substitute(bn))))

  }#THEN

}#CHECK.FIT
