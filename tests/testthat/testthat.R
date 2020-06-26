test_that("Found model is consistent", {
  model <- learnModel(data = hmmaExampleData, amountOfStates = 3, seed = 1234)

  # Verify that inits counts to 1
  inits <- model$init
  sum <- sum(inits)
  expect_equal(sum, 1)

  # Verify that trans counts to 1
  trans <- model$transition
  for (i in 1:nrow(trans)) {
    sum <- sum(trans[i,])
    expect_equal(sum, 1)
  }
})

test_that("Same seeds finds same model", {
  seeds <- c(1111, 2345, 7890)

  for (seed in seeds) {
    model1 <- learnModel(data = hmmaExampleData, amountOfStates = 2, seed = seed)
    model2 <- learnModel(data = hmmaExampleData, amountOfStates = 2, seed = seed)

    expect_equal(model1$init, model2$init)
    expect_equal(model1$transition, model2$transition)
  }

})

test_that("Correct data is accepted", {
  data <- list()
  data$x <- data.frame("A" = c("TRUE", "TRUE", "FALSE", "FALSE"),
                       "B" = c("FALSE", "FALSE", "TRUE", "FALSE"),
                       "C" = c("FALSE", "TRUE", "TRUE", "TRUE"),
                       stringsAsFactors = TRUE)
  data$N <- c(2, 2)

  fit <- learnModel(data = data, amountOfStates = 2, seed = 3244)

  # We do not expect warnings, so datafile is accepted
  # The check below only comples in case of no errors
  expect_equal(1, 1)
})

test_that("Incorrect data is not accepted", {
  data <- list()
  data$x <- data.frame("A" = c("TRUE", "TRUE", "FALSE", "FALSE"),
                       "B" = c("FALSE", "FALSE", "FALSE", "FALSE"),
                       "C" = c("FALSE", "TRUE", "TRUE", "TRUE"),
                       stringsAsFactors = TRUE)
  data$N <- c(2, 3)

  expect_error(learnModel(data = data, amountOfStates = 2, seed = 3244))
})
