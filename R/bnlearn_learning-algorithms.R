# score-based learning algorithms.
greedy.search = function(x, start = NULL, whitelist = NULL, blacklist = NULL,
    score = NULL, heuristic = "hc", ..., optimized = TRUE, debug = FALSE) {

  # check the data are there.
  check.data(x)
  # check the algorithm.
  check.learning.algorithm(heuristic, class = "score")
  # check the score label.
  score = check.score(score, x)
  # check debug.
  check.logical(debug)

  # check unused arguments in misc.args.
  extra = list(...)
  misc.args = extra[names(extra) %in% method.extra.args[[heuristic]]]
  extra.args = extra[names(extra) %in% score.extra.args[[score]]]
  check.unused.args(extra, c(method.extra.args[[heuristic]], score.extra.args[[score]]))

  # expand and check the max.iter parameter (common to all algorithms).
  max.iter = check.max.iter(misc.args$max.iter)
  # expand and check the maxp parameter (common to all algorithms).
  maxp = check.maxp(misc.args$maxp, data = x)

  if (heuristic == "hc") {

    # expand and check the number of random restarts.
    restart = check.restart(misc.args$restart)
    # expand and check the magnitude of the perturbation when random restarts
    # are effectively used.
    perturb = ifelse((restart > 0), check.perturb(misc.args$perturb), 0)

  }#THEN
  else if (heuristic == "tabu") {

    # expand and check the arguments related to the tabu list.
    tabu = check.tabu(misc.args$tabu)
    max.tabu = check.max.tabu(misc.args$max.tabu, tabu)

  }#THEN

  # sanitize whitelist and blacklist, if any.
  whitelist = build.whitelist(whitelist, nodes = names(x), data = x,
                algo = heuristic, criterion = score)
  blacklist = build.blacklist(blacklist, whitelist, names(x), algo = heuristic)
  # if there is no preseeded network, use an empty one.
  if (is.null(start))
    start = empty.graph(nodes = names(x))
  else {

    stop("This part should NOT be reached: bnlearn_learning-algorithms.R")

  }#ELSE

  # apply the whitelist to the preseeded network; undirected arcs are allowed
  # but applied as directed to interoperate with bnlearn() in hybrid.search().
  if (!is.null(whitelist)) {

    for (i in 1:nrow(whitelist))
      start$arcs = set.arc.direction(whitelist[i, "from"],
                       whitelist[i, "to"], start$arcs)

  }#THEN

  # apply the blacklist to the preseeded network.
  if (!is.null(blacklist)) {

    blacklisted = apply(start$arcs, 1, function(x){ is.blacklisted(blacklist, x) })
    start$arcs = start$arcs[!blacklisted, , drop = FALSE]

  }#THEN

  # be sure the graph structure is up to date.
  start$nodes = cache.structure(names(start$nodes), arcs = start$arcs)
  # no party if the graph is partially directed.
  if (is.pdag(start$arcs, names(start$nodes)))
    stop("the graph is only partially directed.")
  # check whether the graph is acyclic.
  if (!is.acyclic(arcs = start$arcs, nodes = names(start$nodes)))
    stop("the preseeded graph contains cycles.")

  # sanitize score-specific arguments.
  extra.args = check.score.args(score = score, network = start,
                 data = x, extra.args = extra.args, learning = TRUE)

  # reset the test counter.
  reset.test.counter()

  # call the right backend.
  if (heuristic == "hc") {

    # COMMENTED OUT DUE TO NOT NECESSARY
    stop("This part should NOT be reached: bnlearn_learning-algorithms.R")
    # res = hill.climbing(x = x, start = start, whitelist = whitelist,
    #   blacklist = blacklist, score = score, extra.args = extra.args,
    #   restart = restart, perturb = perturb, max.iter = max.iter,
    #   maxp = maxp, optimized = optimized, debug = debug)

  }#THEN
  else if (heuristic == "tabu"){


    res = tabu.search(x = x, start = start, whitelist = whitelist,
      blacklist = blacklist, score = score, extra.args = extra.args,
      max.iter = max.iter, optimized = optimized, tabu = tabu,
      maxp = maxp, debug = debug)

  }#THEN

  # set the metadata of the network in one stroke.
  res$learning = list(whitelist = whitelist, blacklist = blacklist,
    test = score, ntests = test.counter(),
    algo = heuristic, args = extra.args, optimized = optimized,
    illegal = check.arcs.against.assumptions(NULL, x, score))

  invisible(res)

}#GREEDY.SEARCH
