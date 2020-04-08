# Mstep function for HMM-As
mstep.bnlearn <- function(x, wt, blacklist, iss, debug) {
  k = ncol(wt)
  bns = list()
  dd = as.data.frame(x)
  for(i in 1:k) {
    # The line below decides which 'tabu' method is used!
    # If bnlearn implements weighting, this can be changed back to bnlearn,
    bn = tabu(dd, score="aic-w", weights=wt[,i], blacklist=blacklist)
    fit = bn.fit.weight_cont(x = bn, data = dd, weights = wt[,i], iss = iss, debug = debug)
    bns[[i]] = fit
  }
  bns
}

