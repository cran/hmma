# Hill Climbing greedy search frontend.
hc = function(x, start = NULL, whitelist = NULL, blacklist = NULL,
    score = NULL, ..., debug = FALSE, restart = 0, perturb = 1,
    max.iter = Inf, maxp = Inf, optimized = TRUE) {

  greedy.search(x = x, start = start, whitelist = whitelist,
    blacklist = blacklist, score = score, heuristic = "hc", debug = debug,
    ..., restart = restart, perturb = perturb,
    max.iter = max.iter, maxp = maxp, optimized = optimized)

}#HC

# TABU list greedy search frontend.
tabu = function(x, start = NULL, whitelist = NULL, blacklist = NULL,
    score = NULL, ..., debug = FALSE, tabu = 10, max.tabu = tabu,
    max.iter = Inf, maxp = Inf, optimized = TRUE) {

  greedy.search(x = x, start = start, whitelist = whitelist,
    blacklist = blacklist, score = score, heuristic = "tabu", debug = debug,
    ..., max.iter = max.iter, tabu = tabu, max.tabu = max.tabu,
    maxp = maxp, optimized = optimized)

}#TABU
