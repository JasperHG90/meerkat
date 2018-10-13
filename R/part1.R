## Functions for assignment 2, part I
#
# Functions:
#  - t_test: exported function that performs a t-test for two variables. Returns an S3 method of class t_test
#  - print.t_test: S3 print method for 't_test' class
#  - t_statistic: helper function. Not exported. Returns t-statistic given inputs.
#  - pooled_variance: helper function. Not exported. Returns pooled variance given inputs.

# Main ----

#' Perform a two-tailed t-test between two variables
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param variance_equal boolean (TRUE/FALSE) indicating if variance of x and y are equal or not. If variances are equal, the function will calculate the pooled variance. Else, it will use the formula for unequal variances.
#'
#' @author Jasper Ginn
#'
#' @return list of class 't_test' containing input variables, summary statistics & test statistics
#' @export
t_test <- function(x, y, variance_equal = TRUE) {

  ## Checks

  # Both x, y must be vectors
  if(!is.vector(x) || !is.vector(y)) {

    stop("Both inputs (x and y) must be numeric vectors")

  }

  # Both x, y must be numeric
  if(!is.numeric(x) || !is.numeric(y)) {

    stop("Both inputs (x and y) must be numeric vectors")

  }

  # If NA values, emit warning
  if(sum(is.na(x)) + sum(is.na(y)) > 0) {

    warning("One or more of your variables contains missing values. These will be removed in the test")

    # Save indices for missing values
    missing <- list(
      "x" = which(is.na(x)),
      "y" = which(is.na(y))
    )

    # Remove missing values
    x <- na.omit(x)
    y <- na.omit(y)

  } else {

    # No missing values.
    missing <- NULL

  }

  ## Calculate statistics needed to perform t-test

  # Observations
  n_x <- length(x)
  n_y <- length(y)

  # Degrees of freedom
  df <- (n_x + n_y) - 2

  # Means
  mu_x <- 1/n_x * sum(x)
  mu_y <- 1/n_y * sum(y)

  # Variances
  var_x <- sum( (x-mu_x)^2 ) / ( n_x - 1 )
  var_y <- sum( (y-mu_y)^2 ) / ( n_y - 1 )

  ## Calculate t-statistic
  if( variance_equal ) {

    # Pooled variance
    s <- pooled_variance(var_x, var_y,
                         n_x, n_y)

    # t-stat (equal variances)
    tt_result <- t_statistic(mu_x, mu_y, n_x, n_y, poolvar = s)

  } else {

    # t-stat (unequal variances)
    tt_result <- t_statistic(mu_x, mu_y, n_x, n_y, var_x = var_x, var_y = var_y)

  }

  # P-value (make this two-tailed)
  pval <- 2 * pt( tt_result, df, lower.tail = FALSE )

  ## Make list of results
  res <- list(
    "inputs" = list(
      "x" = x,
      "y" = y,
      "variance_equal" = variance_equal
    ),
    "summary_statistics" = list(
      "mu_x" = mu_x,
      "mu_y" = mu_y,
      "var_x" = var_x,
      "var_y" = var_y,
      "n_x" = n_x,
      "n_y" = n_y
    ),
    "test" = list(
      "tstat" = tt_result,
      "df" = df,
      "pval" = pval
    )
  )

  ## If equal variances, add pooled variance to results
  if(variance_equal) {

    res$summary_statistics$pooled_variance <- s

  }

  ## If NA values were omitted, add indices of these values
  if(!is.null(missing)) {

    res$missing_values <- missing

  }

  ## Add class name
  class(res) <- "t_test"

  ## Return
  return(res)

}

# Documentation is not necessary for this print method
#' @export
print.t_test <- function(x) {

  ## Use paste to create a summary of the data

  msg <- paste0(
    # Print whether test is equal/unequal variances
    "Two-sided t-test with",
    ifelse(x$inputs$variance_equal, " equal ", " unequal "),
    "variances.",
    "\n\n",
    # Print summary statistics
    "Summary stats:\n\tx\ty\n",
    "n\t",
    x$summary_statistics$n_x,"\t",
    x$summary_statistics$n_y,"\n",
    "mean\t",
    round(x$summary_statistics$mu_x, digits=2), "\t",
    round(x$summary_statistics$mu_y, digits=2) ,"\n",
    "var\t",
    round(x$summary_statistics$var_x, digits=2), "\t",
    round(x$summary_statistics$var_y, digits=2),
    # If equal variances, print pooled variance
    ifelse(x$inputs$variance_equal,
           paste0("\n", "------------------------", "\n",
                  "Pooled variance: ", round(x$summary_statistics$pooled_variance,
                                             digits=2)),
           ""),
    "\n\n",
    # Print t-statistic, degrees of freedom & p-values
    "t-statistic\tdf\tp-value\t\n",
    round(x$test$tstat, digits=2), "\t\t",
    x$test$df, "\t",
    round(x$test$pval, digits=3))

  ## Use cat() to print the message + formatting to the console
  cat(msg)

}

# Helper functions ----

# Calculate the t-statistic for two variables
#
# @param mu_x, mu_y the means of variables x, y
# @param n_x, n_y number of observations for x, y
# @param ... optional (named) arguments. Accepted arguments are 'poolvar' (for equal variances) or 'var_x' & 'var_y' (for unequal variances)
#
# @return t-statistic for x, y
t_statistic <- function(mu_x, mu_y, n_x, n_y, ...) {

  # Store optional arguments in list
  opts <- list(...)

  # If 'poolvar' in names of opts --> equal variances
  if("poolvar" %in% names(opts)) {

    # Retrieve value
    pooled_var <- opts$poolvar

    # Apply the t-test formula
    (mu_x - mu_y) / (sqrt(pooled_var * (1/n_x + 1/n_y)))

  } else { # unequal variances

    # Retrieve values
    var_x <- opts$var_x
    var_y <- opts$var_y

    # Apply the t-test formula
    (mu_x - mu_y) / sqrt(var_x/n_x + var_y/n_y)

  }

}

# Calculate the pooled variance of two variables
#
# @param var_x, var_y the variances of variables x, y
# @param n_x, n_y number of observations for x, y
#
# @return pooled variance of variables x, y
pooled_variance <- function(var_x, var_y, n_x, n_y) {

  # Apply the formula for pooled variance
  nom <- (n_x - 1) * var_x + (n_y -1) * var_y
  denom <- n_x + n_y - 2
  # Return
  return(nom/denom)

}
