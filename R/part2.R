## Functions for assignment 2, part II
#
# Functions:

# Main functions ----

#' Fit a linear regression model
#'
#' Use linear_model() to fit a linear regression model. The function computes the QR decomposition and returns this as part of the output list.
#'
#' @param formula an object of class 'formula' or a string that can be coerced to this class. The formula is a symbolic description of the data and should be passed as 'DV ~ IV1 + IV2 + ...' (where DV stands for 'dependent variable' and the IVs are the independent variables). You are also allowed to pass 'DV ~ .' indicating that you will use all all variables in the data as predictors.
#' @param data data frame containing the variables of interest
#'
#' @return
#'
#' @seealso
#'
#' @author Jasper Ginn
#'
#' @export
linear_model <- function(formula, data) {

  ## Checks & initialize data
  init <- perform_checks(formula, data)

  ## Summary statistics
  ss <- summary_statistics(init$inputs$X)

  ## Linear model
  coeff <- linear_regression(init$inputs$X,
                             init$inputs$y)

  ## Tests

  ## Set class
  attr(init, "linear_model")

  ## Return
  return(init)

}

## Methods for the 'linear_model' class

summary.linear_model <- function(x) {



}

print.linear_model <- function(x) {


}

plot.linear_model <- function(x) {



}

coef.linear_model <- function(x) {}

residuals.linear_model <- function(x) {}

resid.linear_model <- function(x) {

  residuals.linear_model(x)

}

# Helper functions ----

## Perform checks for the model

check_formula <- function(varnames, formula) {

  ## Coerce to character
  formula <- as.character(formula)

  ## Retrieve DV
  DV <- formula[2]

  ## Make sure DV is in variables
  if( !(DV %in% varnames) ) {

    stop("Dependent variable not found in dataset")

  }

  ## Retrieve IVs
  IV <- formula[3]

  ## If IV == ., move on
  if( IV == '.' ) {

    ## Store results in list
    res <- list(
      "DV" = DV,
      "IV" = varnames[which(varnames != DV)]
    )

  } else if( grepl("\\+", IV) ) {

    ## If '+' in IVs, then multiple IVs

    # Split the IVs
    IVs <- strsplit(IV,"(\\s)?\\+(\\s)?")[[1]]

    # Check for each if in dataset
    if( !all(IVs %in% varnames) ) {

      stop("Not all independent variables found in dataset")

    }

    # Store results in a list
    res <- list(
      "DV" = DV,
      "IV" = IVs
    )

  } else {

    ## We have the situation that there is only one IV

    if( !(IV %in% varnames) ) {

      stop("Independent variable not found in dataset.")

    }

    ## Store results in list
    res <- list(
      "DV" = DV,
      "IV" = IV
    )

  }

  ## Return the DV / IV list
  return(res)

}

perform_checks <- function(formula, data) {

  ## Check if formula is formula or if it can be coerced to one
  if( !(is(formula)[1] == "formula") ) formula <- as.formula(formula)

  ## Check if data is data frame or matrix
  if( !(is.data.frame(data)) ) {

    stop("'data' must be a data frame")

  }

  ## Retrieve variable names from data
  varnames <- colnames(data)

  ## Check if formula correct and all variable names present
  vars <- check_formula(varnames, formula)

  ## Retrieve X & y matrix / vectors
  y <- data[,vars$DV]

  ## Assert that the outcome variable is a numeric vector
  if( !is.numeric(y) ) {

    stop("outcome vector y must be numeric")

  }

  ## Create design matrix
  X <- model.matrix(formula, data)

  ## Put in list & return
  res <- list(
    "inputs" = list(
      "formula" = formula,
      "variables" = vars,
      "y" = y,
      "X" = X
    )
  )

  return(res)

}

## Calculate summary statistics

summary_statistics <- function(X) {

  ## Number of observations
  n <- nrow(X)
  ## Number of columns
  m <- ncol(X)
  ## Columns names
  cnames <- colnames(X)
  ## Take intercept (column vector of ones)
  ones <- matrix(X[,1], ncol=1)
  colnames(ones) <- cnames[1]
  ## Remove intercept from data
  X <- matrix(X[,-1], ncol=(m-1))
  colnames(X) <- cnames[-1]

  ## Calculate means
  mns <- cmean(n, X, ones)

  ## Calculate variance
  vrn <- cvar(n, X, mns, ones)

  ## Minimums
  mins <- apply(X, 2, min)

  ## Maximums
  maxs <- apply(X, 2, max)

  # Return list
  res <- list(
    "means" = mns,
    "variance" = vrn,
    "min" = mins,
    "maxs" = maxs,
    "n" = n,
    "df" = n - m
  )

  return(res)

}

cmean <- function(n, X, ones) {

  ## Compute mean
  return(t(ones) %*% X * 1/n)

}

cvar <- function(n, X, means, ones) {

  ## Compute variance
  return(t(ones) %*% (X - ones %*% means)^2 * (1/(n-1)))

}

## Calculate the coefficients and intercept

linear_regression <- function(X, y) {

  ## Apply the linear regression formula
  res <- solve( t(X) %*% X ) %*% t(X) %*% y

}

## Inference

# Degrees of freedom
# Call
# Residuals
# MSRE
# MSE

# Calculate p-values, F-ratio, SE values, etc
compute_tests <- function(X, y, coeff, n, m) {

  # Get standard errors
  SE <- standard_errors(y, X, coeff, n, m)

  # Compute t-values for coefficients
  tvalues <- tvalue(coeff, SE)

  # Compute the p-values for the t-values of the coeffficients
  pvalues_tstats <- Pvalues(tvals, (n-m))

  # Predict y given the model
  yhat <- predict_y(X, coeff)

  # Compute the residuals
  resids <- compute_residuals(y, yhat)

  # Compute sums-of-squares, f-statistic and r^2
  statistics <- sums_of_squares(n, m, y, yhat, resids)

  # Put in a list & return
  res <- list(

    "coef" = list(
      "standard_errors" = SE,
      "t_values" = tvalues,
      "p" = pvalues_tstats
    ),
    "predicted" = yhat,
    "residuals" = resids,
    "sums_of_squares" = statistics$sums_of_squares,
    "means_of_squares" = statistics$means_of_squares,
    "f_test" = statistics$f_test,
    "R_squared" = statistics$R_squared

  )

}

# @details code adapted from: https://stats.stackexchange.com/questions/44838/how-are-the-standard-errors-of-coefficients-calculated-in-a-regression
# @seealso
#   b) https://stats.stackexchange.com/questions/85943/how-to-derive-the-standard-error-of-linear-regression-coefficient
#   c) https://stat.ethz.ch/pipermail/r-help/2006-September/113115.html
standard_errors <- function(y, X, coeff, n, m) {

  # Estimate of the variance
  ssquared <- sum( (y - X %*% coeff) ^ 2 ) / (n - m)

  # var-covar matrix
  vcv <- ssquared * chol2inv( chol( t(X) %*% X ) )

  # SE
  SE <- sqrt( diag( vcv ) )

  # Return
  return(SE)

}

# Compute t-values given estimate of standard errors and coefficients
tvalue <- function(coeff, SE) {

  coeff / SE

}

# Compute p-values of coefficients given t-values
Pvalues <- function(tvals, df) {

  return(2 * pt( tvals, df, lower.tail = FALSE ))

}

# Predict values of y given the design matrix and the coefficients
predict_y <- function(X, coeff) {

  return(as.vector(matrix(coeff, ncol = length(coeff)) %*% t(X)))

}

# Compute the residuals of the model
compute_residuals <- function(y, yhat) {

  return(y - yhat)

}

# Compute the sums of squares for the model, the F-statistic and the r-squared value
# @seealso: http://facweb.cs.depaul.edu/sjost/csc423/documents/f-test-reg.htm
sums_of_squares <- function(n, p, y, yhat, resids) {

  ## Total sums of squares
  TSS <- sum((y - mean(y))^2)

  ## Sums of squares for the error
  SSE <- sum((resids)^2)

  ## Sums of squares for the model
  SSM <- sum((yhat - mean(y))^2)

  ## Degrees of freedom for the sums of squares
  DFM <- p - 1
  DFE <- n - p
  DFT <- n - 1

  ## Mean of squares for model
  MSM <- SSM / DFM

  ## Mean of squares for error
  MSE <- SSE / DFE

  ## Mean of square for total
  MST <- TSS / DFT

  ## F-statistic
  Fstat <- MSM / MSE

  ## P-value
  pval_fstat <- 1 - pf(Fstat, DFM, DFE)

  ## R-squared
  rsquared <- round(SSM / TSS, digits=4)

  ## Return
  res <- list(

    "sums_of_squares" = list(
      "TSS" = TSS,
      "MSS" = SSM,
      "RSS" = SSE
    ),
    "means_of_squares" = list(
      "MST" = MST,
      "MSM" = MSM,
      "MSE" = MSE
    ),
    "f_test" = list(
      "f_statistic" = Fstat,
      "p" = pval_fstat
    ),
    "R_squared" = rsquared

  )

  return(res)

}


