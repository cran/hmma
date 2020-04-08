
<!-- README.md is generated from README.Rmd. Please edit that file -->

# How to use

Travis-ci status: ![Travis
status](https://travis-ci.com/jthibaudier/hmma.svg?token=ecAEbMnQHRcuqKgskFfr&branch=master)

This R package can be used for the analysis of asymmetric hidden Markov
models. The files together form a package.

After the package has been installed, you can load it using:

    library(hmma)

After the package has been loaded, you find its description, functios
and vignettes by:

    ?hmma

All package functions and files must be organised in the R folder. To
use and test the package, place files in the Development folder.

A demonstration:

    # Show part of the data set
    head(hmmaExampleData$x, 10)
    
    # We specify the amount of states and fit the model
    amountOfStates <- 3
    fit <- learnModel(hmmaExampleData, amountOfStates = amountOfStates)
    
    # Visualise the model
    visualise(fit)
    
    # Load required libraries and files for visualising the Bayesian networks
    library(bnlearn)
    for (i in 1:amountOfStates) {
      graphviz.plot(fit$parms.emission[[i]])
    }

The datafile must comply with the required format (more on that later).
An example dataset is provided in `hmmaExampleData`.

For a full demonstration, please refer to the introduction vignette that
can be found by:

    browseVignettes(package = "hmma")
