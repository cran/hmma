# AIC of fit, smaller is better!
aic.hmmbn <- function(fit) {
  states = fit$model$J
  params = (states-1) + states*(states-1)

  for(i in 1:length(fit$model$parms.emission)) {
    params = params + bnlearn::nparams(fit$model$parms.emission[[i]])
  }

  loglik = fit$loglik[length(fit$loglik)]
  2*params - 2*loglik
}
