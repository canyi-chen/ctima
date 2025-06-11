\donttest{
  ## Load libraries
  library(ctima)

  ## Example 1
  ## This example shows the use of ctYlm.Mglm function for linear model on the mediator.


  ## Set up parameters
  M.family <- gaussian()
  Y.family <- gaussian()

  simulation <- function(alpha_S = 0, beta_M = 0) {
    data <- generate_all_data(
      n = 200,
      alpha_S = alpha_S,
      beta_M = beta_M,
      M.family = M.family,
      Y.family = Y.family
    )
    S <- data$S
    M <- data$M
    Y <- data$Y
    X <- data$X

    out <- ctYlm.Mglm(S, M, Y, X)
    out
  }

  set.seed(1)
  simulation(1 / 4, 1 / 4)

  simulation(0, 0)

  ## Example 2
  ## This example shows the use of ctYlm.Mglm function for generalized linear model on the mediator.

  ## Set up parameters
  M.family <- binomial()
  Y.family <- gaussian()

  simulation <- function(alpha_S = 0, beta_M = 0) {
    data <- generate_all_data(
      n = 200,
      alpha_S = alpha_S,
      beta_M = beta_M,
      M.family = M.family,
      Y.family = Y.family
    )
    S <- data$S
    M <- data$M
    Y <- data$Y
    X <- data$X

    out <- ctYlm.Mglm(S, M, Y, X, M.family = M.family)
    out
  }


  set.seed(1)
  simulation(1 / 4, 1 / 4)

  simulation(0, 0)



}
