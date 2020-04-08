# "Density emission", function assumes discrete BNs
dbn.hssm <- function(x, j, model) {
  # first bn just to retrieve the nodes and dimnames
  bn = model$parms.emission[[1]]
  nodes = bnlearn::nodes(bn)
  cnodes = nodes[which(lapply(bn[nodes], class) != "bn.fit.dnode")]

  obs = list()
  for(i in 1:length(nodes)) {
    node = nodes[i]
    if(node %in% cnodes) {
      obs[[node]] = as.numeric(x[,i])
    } else {
      # Changed 'i' to 'node' for dataframes.
      obs[[node]] = factor(x=x[,node], levels=dimnames(bn[[node]]$prob)[[1]])
    }
  }
  obs = as.data.frame(obs)

  bn = model$parms.emission[[j]]
  exp(stats::logLik(bn, obs, by.sample=TRUE))
}
