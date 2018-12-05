# Bootstrap functions for point estimation and CI
# Jasper Ginn

boot_ttest <- function(x, y, R = 1000) {

  # Take values and store them in inputs
  inputs <- list(
    "x" = x,
    "y" = y,
    "n_x" = length(x),
    "n_y" = length(y),
    "R" = R
  )

  # Calculate original statistic
  theta_hat <- t.test(inputs$x, inputs$y, var.equal = TRUE)$statistic

  # Zip data and run bootstrap
  theta_boot <- unname(replicate(R, boots(inputs)))

  # Calculate standard error
  se_boot <- sd(theta_boot)

  # Calculate bias
  bias <- mean(theta_hat - theta_boot)

  # (p. 190)
  bias_adj <- abs(bias) / se_boot

  # Confidence intervals
  # Sort the output vector
  theta_boot_sorted <- sort(theta_boot)

  ## Get lower and upper values of the 95% CI

  ## Percentile bootstrap
  alpha <- 0.05
  lower_ci_ind <- (alpha/2) * R
  upper_ci_ind <- 1 + (1 - (1/2) * alpha) * R

  # Select
  upper_ci <- theta_boot_sorted[upper_ci_ind]
  lower_ci <- theta_boot_sorted[lower_ci_ind]

  # Add to output data
  out <- list(
    "inputs" = inputs,
    "result" = list(
      "data" = res,
      "sd" =
    )
  )

  # Add class
  class(out) <- "boottest"

  # Return
  return(out)

}

# Bootstrapping function for a single run
boots <- function(data) {

  # Unroll data
  x <- data$x
  y <- data$y
  n_x <- data$n_x
  n_y <- data$n_y

  # Sample indices
  ind_x <- sample(1:n_x, size=n_x, replace=TRUE)
  ind_y <- sample(1:n_y, size=n_y, replace=TRUE)

  # Subset
  x_tmp <- x[ind_x]
  y_tmp <- y[ind_y]

  # T-test
  tst <- t.test(x_tmp, y_tmp, var.equal = TRUE)
  return(
    tst$statistic
  )

}

# SE generic
se <- function(x) UseMethod("se", x)
se.boottest <- function(obj) {

  sd(obj$result$data)

}

# Get confidence interval



