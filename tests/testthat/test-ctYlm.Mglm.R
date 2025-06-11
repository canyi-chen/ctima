test_that("ctYlm.Mglm works", {
  expect_equal({
    set.seed(1)
    n <- 200
    S <- rnorm(n)
    M <- S + rnorm(n)
    Y <- S + M + rnorm(n)
    out <- ctYlm.Mglm(S = S, M = M, Y = Y, s = 1, s_star = 0)
    out$p_value_NIE
  },
  0
  )
})
