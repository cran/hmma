# Fit a bnlearn object from weighted data
bn.fit.weight_cont <- function(x, data, weights, iss, debug=FALSE) {
  w = list(weight = weights)
  dd = cbind(data, w)

  dist = list()

  # Show weights
  if (debug) {
    cat("The weights ", weights, "\n")
  }

  for(node in bnlearn::nodes(x)) {
    if(debug) {
      cat("Handling node ", node, '\n')
    }

    parents = bnlearn::parents(x, node)

    if(is.factor(data[,node])) {
      # Discrete case

      joinf = paste(c(node, parents), collapse="+")
      formula = stats::as.formula(paste("weight ~ ", joinf))
      tab = stats::xtabs(formula, data=dd)
      # Include prior
      tab = tab + iss / prod(dim(tab))
      tab = prop.table(tab, margin = seq(length(parents) + 1)[-1])

      dist[[node]] = tab
    } else {
      # Continuous case

      # Determine Gaussian parents (gparents) and discrete parents (dparents)
      if(length(parents) == 0) {
        gparents = c()
        dparents = c()
      } else {
        numtypes = sapply(parents, function(x) {is.numeric(data[[x]])} )
        gparents = parents[which(numtypes)]
        dparents = parents[which(!numtypes)]
      }

      # determine the formula used for linear regression
      if(length(gparents) == 0) {
        lmstring = paste(node,'~1')
      } else {
        preds = paste(gparents, collapse="+")
        lmstring = paste(node,"~",preds)
      }

      if(debug) {
        cat(paste('formulastring passed to lm: ', lmstring,'\n'))
      }

      if(length(dparents) == 0) {
        # Gaussian case

        m = stats::lm(lmstring, data=data, weights =data$weight)
        dist[[node]] = list(coef = coef(m), sd = summary(m)$sigma)

      } else {
        # Conditional Gaussian case

        configs = expand.grid(lapply(dparents, function(x) { levels(data[[x]]) } ))
        coef = c()
        sd = c()
        for(i in 1:nrow(configs)) {
          # determine relevant data for this configuration of discrete parents
          if(length(dparents) == 1) {
            indices = data[,dparents] == configs[i,]
          } else {
            indices = apply(data[,dparents], 1, function(x) { all(x == configs[i,]) } )
          }
          m = stats::lm(lmstring, data=data[indices,], weights = data$weight)

          coef = c(coef, coef(m))
          sd = c(sd, summary(m)$sigma)
        }
        coef = matrix(coef, nrow=length(gparents)+1)
        if(debug) {
          cat("coef ",paste(coef), "\n")
          cat("sd ",paste(sd), "\n")
        }
        dist[[node]] = list(coef = coef, sd = sd)
      }
    }
  }
  bnlearn::custom.fit(x,dist)
}

