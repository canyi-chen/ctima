ptwohalfnorm <- function(x, sigma = 1) {
  (pnorm(x/sqrt(2)/sigma) - pnorm(-x/sqrt(2)/sigma))^2
}
cross_test <- function(S, M, Y, X, prop = 0.5, M.family = stats::gaussian()) {
  data <- data.frame(S = S,
                     M = M,
                     Y = Y,
                     X = X)
  n <- nrow(data)
  p <- ncol(X)
  # Input Validation
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")
  if (ncol(data) < 4) stop("Input 'data' must have at least 4 columns.")
  if (any(is.na(data))) stop("Input 'data' contains NA values, please clean the data first.")
  if (prop <= 0 || prop >= 1) stop("Prop must be in the range (0, 1).")

  colnames(data) <- c("S", "M", "Y", paste(paste("X", 0:(p - 1), sep = "")))
  # Define the regression formulas
  if(p > 1) {
    med.fit.formula <- stats::as.formula(paste0("M~S+", paste(paste("X", 1:(
      p - 1
    ), sep = ""), collapse = "+")))
  } else if (p== 1) {
    med.fit.formula <- stats::as.formula("M~S")
  } else {
    stop("Input 'X' must have at least one column.")
  }
  out.fit.formula <- stats::as.formula("Y~.-1")

  # Extract Helper Function to Reduce Redundant Logic
  Split_help_function <- function(data_subset) {
    if(M.family$family == "gaussian") {
      med.fit <- lm(med.fit.formula, data = data_subset)
    } else {
      med.fit <- glm(med.fit.formula, data = data_subset, family = M.family)
    }
    out.fit <- lm(out.fit.formula, data = data_subset)

    t_alpha_S_hat <- summary(med.fit)$coefficients['S', 3]
    t_beta_M_hat <- summary(out.fit)$coefficients['M', 3]
    alpha_S_hat <- summary(med.fit)$coefficients['S', 1]
    beta_M_hat <- summary(out.fit)$coefficients['M', 1]

    return(list(
      estimate = c(alpha_S_hat, beta_M_hat),
      t_statistics = c(t_alpha_S_hat, t_beta_M_hat)
    ))
  }

  n <- nrow(data)
  if (n < 2) stop("Input 'data' must have at least 2 rows.")

  ind1 <- 1:floor(n * prop)
  ind2 <- -ind1

  out1 <- Split_help_function(data[ind1, ])
  out2 <- Split_help_function(data[ind2, ])

  # Calculate Statistics for the First Group
  jj1 <- which.min(abs(out1$estimate))
  statistic1 <- out2$t_statistics[jj1]

  # Calculate Statistics for the Second Group
  jj2 <- which.min(abs(out2$estimate))
  statistic2 <- out1$t_statistics[jj2]

  # Statistic and P-value Calculation
  statistic <- (abs(statistic2) + abs(statistic1))
  p_value_out <- (1 - ptwohalfnorm(abs(statistic)))

  if(M.family$family != "gaussian") {
    return(list(p_value_NIE = p_value_out))
  }

  # Estimate the mediation effect from the original data
  med.fit <- stats::lm(med.fit.formula, data = data)
  out.fit <- stats::lm(out.fit.formula, data = data)
  alpha_S_hat <- stats::coef(med.fit)['S']
  beta_M_hat <- stats::coef(out.fit)['M']
  NIE_hat <- alpha_S_hat * beta_M_hat

  NDE_hat <- stats::coef(out.fit)['S']
  p_value_NDE <- summary(out.fit)$coefficients['S', 4]

  if(p>1) {
    NTE.fit.formula <- stats::as.formula(paste0("Y~S+", paste(paste("X", 1:(
      p - 1
    ), sep = ""), collapse = "+")))
  } else if (p == 1) {
    NTE.fit.formula <- stats::as.formula("Y~S")
  } else {
    stop("Input 'X' must have at least one column.")
  }
  NTE.fit <- stats::lm(NTE.fit.formula, data = data)
  NTE_hat <- stats::coef(NTE.fit)['S']
  p_value_NTE <- summary(NTE.fit)$coefficients['S', 4]

  return(list(NIE = NIE_hat, p_value_NIE = p_value_out,
              NDE = NDE_hat, p_value_NDE = p_value_NDE,
              NTE = NTE_hat, p_value_NTE = p_value_NTE))
}
