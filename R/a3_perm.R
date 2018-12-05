# Functions for assignment 3, part II

#' Permutation t-test for inference
#'
#' This function computes the t-test on the inputs R times and samples the pooled data (i.e. x and y together) without replacement. The function calculates the p-value by
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param R number of resamples. Must be between 99 and 999.
#' @param use_sample logical. Use the sample() function? If FALSE, this function will use the runif() function to draw random samples.
#' @param ... optional (named) parameters
#' \itemize{
#'    \item{alternative: }{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter. See ?t.test() for more information}
#'    \item{var.equal: }{a logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used. See ?t.test() for more information.}
#'    \item{tolerance: }{defines the error tolerance bounds around the proportion of indices. Smaller tolerance means smaller bounds and more resampling to get the 'right' proportion}
#' }
#'
#' @return List containing
#' \itemize{
#'     \item{data: }{list containing input parameters}
#'     \item{results: }{result of the permutation test. Contains indices for each run, p-values, options passed to the t-tests}
#' }
#' @seealso Rizzo, Maria L. "Statistical Computing with R". Chapman and Hall/CRC, 2007. pp. 183-188
#' @export
permutation_test <- function(x, y, R, use_sample = TRUE, ...) {

  # Get options
  opts <- list(...)
  alternative <- ifelse("alternative" %in% names(opts), opts$alternative, "two.sided")
  # This must be valid
  if(!alternative %in% c("two.sided", "greater", "less")) {
    stop("'alternative' must be one of 'two.sided', 'greater' or 'less'. See ?t.test() for more information")
  }
  variance_equal <- ifelse("var.equal" %in% names(opts), opts$var.equal, TRUE)
  tolerance <- ifelse("tolerance" %in% names(opts), opts$tolerance, 1)

  # Preparation ----

  # R must be between 99 and 999
  if(R < 99 | R > 999) {

    stop("Number of permutations R must be between 99 and 999")

  }

  # Record the number of observations in x and y
  n_x <- length(x)
  n_y <- length(y)

  # Get the name of the vector with the most observations. If tie, always choose one.
  # This is the 'base category' from which indices are chosen in the permutations below.
  nams <- c("x", "y")
  if(n_x == n_y) {
    basecase <- nams[1]
  } else {
    # Get the maximum value
    basecase <- nams[which(c(n_x, n_y) == max(c(n_x, n_y)))]
  }

  # Total number of observations
  K <- n_x + n_y

  # Store data
  data <- list(
    "x" = x,
    "y" = y,
    "basecase" = basecase,
    "n_x" = n_x,
    "n_y" = n_y,
    "K" = K,
    "R" = R
  )

  # Permutation test ----

  res <- permute(data, use_sample, variance_equal = variance_equal,
                 alternative = alternative, tolerance = tolerance)

  # TODO: Make one and two-tailed test

  # Pull list together
  ret <- list(
    "data" = data,
    "results" = res
  )

  # Add class
  class(ret) <- "permutation_test"

  # Return values
  return(
    ret
  )

}

# Permutation test
#
# See explanation under 'permutation_test'
#
# @param inputs list of inputs passed by the user
# @param use_sample see permutation_test()
# @param variance_equal see permutation_test()
# @param alternative see permutation_test()
# @param tolerance see permutation_test()
#
# @return results of the permutation test
permute <- function(inputs, use_sample, variance_equal, alternative, tolerance) {

  # Unroll data
  R <- inputs$R
  n_x <- inputs$n_x
  n_y <- inputs$n_y
  K <- inputs$K
  # Pool the sample
  z <- c(inputs$x, inputs$y)

  # Compute the t-statistic between x and y
  t_xy <- t.test(inputs$x, inputs$y,
                 var.equal = variance_equal,
                 alternative = alternative)$statistic

  # Vector to store the results
  res <- numeric(R)

  # Matrix to store the indices used for each random subsample
  # We do this so we can reproduce the results
  # If not use sample() then open list --> we cannot be sure that the sample will draw exactly the same number of values every time
  if(use_sample) {
    ind <- matrix(0L, ncol = max(n_x, n_y), nrow = R)
  } else {
    ind <- vector("list", length = R)
  }

  # Permute
  for(i in 1:R) {

    # Sample k indices
    if(use_sample) {
      k <- sample(K, size = max(n_x, n_y), replace = FALSE)
      # Save indices
      ind[i,] <- k
    } else {
      # Use the uniform distribution to sample data randomly
      K_ind <- 1:K
      # Make sure that length of k is never more than 1 SE away from the proportion

      # Proportion to be drawn for the variable with the most observations
      p <- (max(n_x, n_y) / length(K_ind))
      # Standard error for this proportion
      se <- sqrt((p * (1-p))/length(K_ind))
      k <- draw(p, se, tolerance, K_ind)
      ind[[i]] <- k
    }

    # Subset
    xt <- z[k]
    yt <- z[-k]

    # Compute two-tailed t statistic
    res[i] <- t.test(xt, yt,
                     var.equal = variance_equal,
                     alternative = alternative)$statistic

  }

  # Calculate the empirical p-value of the original sample (this counts as one permutation) and the other permutations
  p <- mean(c(t_xy, res) >= t_xy)
  # Times two if p <=0.5, otherwise times 2(1-p) --> only if two-tailed

  if(alternative == "two.sided") {
    p <- ifelse(p <= 0.5, 2*(p), 2*(1-p))
  } else {
    p <- p
  }

  # To list and return
  list(
    "variance_equal" = variance_equal,
    "alternative" = alternative,
    "t_sample" = t_xy,
    "indices" = ind,
    "tstats" = res,
    "pvalue" = p
  )

}

# Draw k indices from a total of K with proportion p
#
# @param p proportion of indices to sample from K_ind
# @param se standard error of p
# @param tolerance defines the error tolerance bounds around the proportion of indices. Smaller tolerance means smaller bounds and more resampling to get the 'right' proportion
# @parm K_ind indices from which to sample
#
# @return indices sampled from K_ind. If not within the bounds of abs(p+se) then the function calls itself
draw <- function(p, se, tolerance, K_ind) {
  # Get indices
  k <- K_ind[runif(length(K_ind), min = 0, max = 1) <= p]
  # If out of bounds, call self
  prop_ind <- (length(k) / length(K_ind))
  if(prop_ind < (p - tolerance * se) | prop_ind > (p + tolerance * se)) {
    draw(p,se, tolerance, K_ind)
  } else {
    return(k)
  }
}

#' Plot method
#' @export
plot.permutation_test <- function(object) {

  # Plot a histogram of distribution
  hist(object$results$tstats, main = "permutation distribution of replicates",
       freq = FALSE,
       xlab = paste0("T (p = ", round(object$results$p, digits=3),")"),
       breaks = "scott")
  # Add sample t-statistic as a point
  points(object$results$t_sample, 0, cex = 1, pch=16, col="red")

}

#' Print method
#' @export
print.permutation_test <- function(object) {

  p <- object$results$pvalue
  t0 <- object$results$t_sample
  K <- object$data$K
  R <- object$data$R

  msg <- paste0("Two-tailed permutation test using ", K, " observations and ", R, " permutations", "\n\n",
                "K\t", "R\t", "t\t\t", "p\n",
                "-----------------------------------\n",
                K, "\t", R, "\t", round(t0, digits=2), "\t", round(p, digits=3))

  cat(msg)

}
