## -----------------------------------------------------------------------------
library(hmma)

## -----------------------------------------------------------------------------
head(hmmaExampleData$x, 5)

## -----------------------------------------------------------------------------
fit <- learnModel(data = hmmaExampleData, amountOfStates = 3, seed = 1234)

## -----------------------------------------------------------------------------
visualise(fit)

## -----------------------------------------------------------------------------
library(bnlearn)
fit$parms.emission[[1]]
graphviz.plot(fit$parms.emission[[1]])

## -----------------------------------------------------------------------------
training <- list()
training$x <- hmmaExampleData$x[1:40,]
training$N <- c(20, 20)

validation <- list()
validation$x <- hmmaExampleData$x[41:60,]
validation$N <- c(20)

## -----------------------------------------------------------------------------
fit2 <- learnModel(training, amountOfStates = 2, seed = 1234)
loglikelihood <- predict(fit2, data = validation)$loglik
loglikelihood

## -----------------------------------------------------------------------------
age <- c("Y", "Y", "I", "I", "Y", "I", "O", "Y")
data <- list()
data$x <- data.frame(age)
data$N <- c(8)

## -----------------------------------------------------------------------------
training <- list()
age <- c("Y", "Y", "I", "I", "Y")
training$x <- data.frame(age, stringsAsFactors = TRUE)
training$N <- c(5)

## -----------------------------------------------------------------------------
fit <- learnModel(data = training, amountOfStates = 2, seed = 1234)
visualise(fit)

## -----------------------------------------------------------------------------
validation <- list()
age <- c("I", "O", "Y")
validation$x <- data.frame(age, stringsAsFactors = TRUE)
validation$N <- c(3)

## -----------------------------------------------------------------------------
fit$parms.emission[[1]]

## -----------------------------------------------------------------------------
init <- c(0.6, 0.4)
init

## -----------------------------------------------------------------------------
trans <- matrix(c(0.7, 0.3, 0.1, 0.9), nrow = 2, ncol = 2, byrow = TRUE)
trans

## -----------------------------------------------------------------------------
library(bnlearn)
struc <- model2network("[X1][X2]")
cptX1 <- matrix(c(0.15, 0.85), ncol = 2, dimnames = list(NULL, c("TRUE", "FALSE")))
cptX2 <- matrix(c(0.7, 0.3), ncol = 2, dimnames = list(NULL, c("TRUE", "FALSE")))

bn1 <- custom.fit(struc, dist = list(X1 = cptX1,
                                     X2 = cptX2))

struc <- model2network("[X2|X1][X1]")
cptX1 <- matrix(c(0.4, 0.6), ncol = 2, dimnames = list(NULL, c("TRUE", "FALSE")))
cptX2 <- matrix(c(0.9, 0.1, 0.5, 0.5), nrow = 2, ncol = 2)
dimnames(cptX2) <- list("X2" = c("TRUE", "FALSE"),
                        "X1" = c("TRUE", "FALSE"))

bn2 <- custom.fit(struc, dist = list(X1 = cptX1,
                                     X2 = cptX2))

bns <- list()
bns[[1]] <- bn1
bns[[2]] <- bn2

## -----------------------------------------------------------------------------
model <- createHmma(init = init, trans = trans, bns = bns)

## -----------------------------------------------------------------------------
visualise(model = model)

## -----------------------------------------------------------------------------
graphviz.plot(model$parms.emission[[1]])
graphviz.plot(model$parms.emission[[2]])

## -----------------------------------------------------------------------------
data <- simulate(model, nsim = c(20, 20, 20, 20), seed = 1234)
data

