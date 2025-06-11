#' Cross Test in Mediation Analysis with (generalized) Linear Models
#'
#' @description
#' \code{ctYlm.Mglm} performs an cross test in mediation analysis in which mediator is modeled by a generalized linear model and outcome Y are modeled by a linear model.
#'
#' @param S an n-by-1 vector for exposure.
#' @param M an n-by-1 matrix for mediator.
#' @param Y an n-by-1 vector for outcome.
#' @param X an n-by-p matrix for p confounders. Do not include intercept in matrix X. In the absence of confounders, matrix is not required to be specified.
#' @param s exposure level with default set at 1.
#' @param s_star baseline exposure level with default set at 0.
#' @param M.family the error distribution and link function for the mediator model. The default family is \code{gaussian()}.
#'
#' @returns NIE refers to natural indirect effect, which is the estimated when a change of exposure S occurs from s_star to s. This return is absent when M.family is not \code{gaussian()}.
#' @returns p_value_NIE refers to the AB p value for NIE.
#' @returns NDE refers to natural direct effect, which is estimated when exposure S changes from s_star to s. This return is absent when M.family is not \code{gaussian()}.
#' @returns p_value_NDE is the p value for NDE. This return is absent when M.family is not \code{gaussian()}.
#' @returns NTE is the estimated natural total effect when exposure S changes from s_star to s. This return is absent when M.family is not \code{gaussian()}.
#' @returns p_value_NTE is the p value for NTE. This return is absent when M.family is not \code{gaussian()}.
#' @references Chen, C., Zhou, L., Song, P.X.K. (2025), "An Efficient Two-Way Cross Test for Mediation Effects with High Sensitivity to Weak Signals" arXiv preprint.
#' @example man/examples/example_ctYlm.Mglm.R
#'
#' @export
ctYlm.Mglm <- function(S, M, Y, X = NULL, s = 1, s_star = 0, M.family = stats::gaussian()) {

  # ============================================================ #
  # Parameters checking and cleaning
  # ============================================================ #
  S <- as.matrix(S)
  M <- as.matrix(M)
  Y <- as.matrix(Y)
  # X <- as.matrix(X)

  # Check dimensions to ensure matrices have the same number of rows
  stopifnot(nrow(S) == nrow(M), nrow(M) == nrow(Y))

  # Ensure scalar exposure and outcome
  stopifnot(ncol(S) == 1, ncol(Y) == 1)

  if(is.null(X)) {
    X <- matrix(1, nrow(S), 1)
  } else {
    # Check dimensions to ensure matrices have the same number of rows
    stopifnot(nrow(Y) == nrow(X))
    X <- cbind(1, as.matrix(X))  # Add intercept column if it is not present
  }

  # ============================================================ #
  # Main computation based on the number of mediators
  # ============================================================ #
  if (ncol(M) > 1) {
    error("ctYlm.Mlm is not implemented for multiple mediators.")
  } else {
    out <- cross_test(S = S, M = M, Y = Y, X = X, M.family = M.family)
  }

  # ============================================================ #
  # Return structured output
  # ============================================================ #
  if( M.family$family != "gaussian") {
    return(structure(list(p_value_NIE = out$p_value_NIE),
           class = "ctYlm.Mglm.Result"))
  } else {
    return(structure(list(
      NIE = as.numeric(out$NIE) * (s - s_star),
      p_value_NIE = out$p_value_NIE,
      NDE = as.numeric(out$NDE) * (s - s_star),
      p_value_NDE = out$p_value_NDE,
      NTE = as.numeric(out$NTE) * (s - s_star),
      p_value_NTE = out$p_value_NTE
    ), class = "ctYlm.Mglm.Result"))
  }
}
