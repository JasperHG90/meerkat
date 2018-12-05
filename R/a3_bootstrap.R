# Bootstrap functions for point estimation and CI
# Jasper Ginn

#' Calculate bootstrap statistics for difference-of-means t-test
#'
#' This function calculates R bootstrapped t-test statistics between x and y. It also calculates a variety of statistics such as percentile confidence intervals, bias, standard error and jackknife-after-bootstrap estimates.
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param alpha significance level. used to calculate confidence intervals
#' @param R number of replications
#'
#' @return
#' \itemize{
#'    \item{inputs: }{list containing user inputs}
#'    \item{boot: }{list containing results of bootstrapping, indices used for the pooled sample of x and y, t-statistics for every sample drawn, bias, standard error, jackknife-after-bootstrap}
#'    \item{CI: }{returned if R >= 200. Lower and upper percentile confidence intervals.}
#' }
#'
#' @seealso Rizzo, Maria L. "Statistical Computing with R". Chapman and Hall/CRC, 2007. Chapter 7
#' @export
boot_ttest <- function(x, y, alpha = 0.05, R = 1000) {

  # Take values and store them in inputs
  inputs <- list(
    "x" = x,
    "y" = y,
    "n_x" = length(x),
    "n_y" = length(y),
    "alpha" = alpha,
    "R" = R
  )

  # Outputs
  outputs <- list()

  # Calculate original statistic
  outputs$theta_hat <- t.test(inputs$x, inputs$y, var.equal = TRUE)$statistic

  # Zip data and run bootstrap
  theta_boot <- lapply(1:inputs$R, function(x) boots(inputs))

  # Retrieve indices
  outputs$indices <- t(sapply(theta_boot, function(x) x$indices))

  # Boot values
  outputs$theta_boot <- unname(sapply(theta_boot, function(x) x$stat))

  # Calculate standard error
  outputs$se_boot <- sd(outputs$theta_boot)

  # Calculate jackknife after bootstrap
  outputs$se_JAB <- JAB(inputs$n_x + inputs$n_y,
                       outputs$indices,
                       outputs$theta_boot)

  # Calculate bias
  outputs$bias <- mean(outputs$theta_boot) - outputs$theta_hat

  # Add to output data
  out <- list(
    "inputs" = inputs,
    "boot" = outputs
  )

  # CI
  if(R >= 200) {
    CI <- boot_ci(inputs, outputs)
    out$CI <- CI
  } else {
    warning("'R' is below 200, which is too few to calculate confidence intervals")
  }

  # Add class
  class(out) <- "boottest"

  # Return
  return(out)

}

# Bootstrapping function for a single run
#
# @param data list of inputs defined by the user
#
# @return list containing indices from the pooled sample and t-statistic
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
    list(
      "indices" = c(ind_x, ind_y),
      "stat" = tst$statistic
    )
  )

}

# Calculate jackknife-after-bootstrap
#
# @param n number of observations
# @param matrix of indices indices of length R from the pooled sample returned by boots()
# @param theta_boot vector of length R containing t-statistics returned by boots()
#
# @return jackknife-after-bootstrap estimate
JAB <- function(n, indices, theta_boot) {

  # Calculate the jackknife-after-bootstrap
  se_jack <- numeric(n)
  for( i in 1:n ) {

    # Remove the index
    keep <- apply(indices, 1, function(k) !(i %in% k))

    # Se estimate of all samples that don't contain the index we want to remove
    se_jack[i] <- sd(theta_boot[keep])

  }

  # Return
  se_jack <- sqrt(((n-1) * mean((se_jack - mean(se_jack))^2))) # Standard error of standard error
  return(se_jack)

}

# Calculate percentile confidence intervals
#
# @param inputs as specified by user
# @param outputs bootstrap results
#
# @return list containing upper and lower confidence interval
boot_ci <- function(inputs, outputs) {

  # Confidence intervals
  # Sort the output vector
  theta_boot_sorted <- sort(outputs$theta_boot)

  ## Get lower and upper values of the 95% CI

  ## Percentile bootstrap
  alpha <- inputs$alpha
  lower_ci_ind <- (alpha/2) * inputs$R
  upper_ci_ind <- 1 + (1 - (1/2) * alpha) * inputs$R

  # Select
  upper_ci <- theta_boot_sorted[upper_ci_ind]
  lower_ci <- theta_boot_sorted[lower_ci_ind]

  # Return
  list(
    "lower" = lower_ci,
    "upper" = upper_ci
  )

}

#' Print method
#' @export
print.boottest <- function(obj) {

  R <- obj$inputs$R

  msg <- paste0(
    R, " bootstrapped two-tailed t-statistic values\n\n",
    "Bootstrap statistics :\n",
    "\toriginal\tbias\t\tstd.error\tjack-after-boot\n",
    "\t",round(obj$boot$theta_hat, digits=2),"\t\t",round(obj$boot$bias, digits=2),"\t\t",
    round(obj$boot$se_boot, digits=2), "\t\t", round(obj$boot$se_JAB, digits=2), "\n\n",
    "Percentile bootstrap values :\n",
    "\talpha\t\tlower\t\tupper\n",
    "\t",round(obj$inputs$alpha, digits=2),"\t\t",round(obj$CI$lower, digits=2),"\t\t",
    round(obj$CI$upper, digits=2)

  )

  cat(msg)


}

#' Plot method
#' @export
plot.boottest <- function(obj) {

  hist(obj$boot$theta_boot,
       main = "Bootstrap distribution of replicates",
       breaks="scott",
       freq = FALSE,
       xlab = "Theta")
  abline(v = obj$boot$theta_hat, col = 2)
  abline(v = obj$CI, col = 3, lty=2)

}

