## Functions for assignment 2, part II
#
# Functions:

# Main functions ----

#' Fit a linear regression model
#'
#' Use linear_model() to fit a linear regression model. The function also calculates standard errors, p-values, the F-statistic & R-squared
#'
#' @param formula an object of class 'formula' or a string that can be coerced to this class. The formula is a symbolic description of the data and should be passed as 'DV ~ IV1 + IV2 + ...' (where DV stands for 'dependent variable' and the IVs are the independent variables). You are also allowed to pass 'DV ~ .' indicating that you will use all all variables in the data as predictors.
#' @param data data frame containing the variables of interest
#'
#' @return list of class 'linear_model' containing:
#' \itemize{
#'   \item{inputs: }{user inputs: formula, DV and IV(s), data entered, number of observations (n), number of predictors (m)}
#'   \item{summary_statistics: }{mean, variance, minimum, maximum for each variable. Also contains the number of observations (n) and the degrees of freedom (df)}
#'   \item{coefficients: }{estimators for each predictor in the model obtained from the linear regression model}
#'   \item{tests: }{list containing the results of analysis of the coefficients. Specifically:
#'     \itemize{
#'       \item{coef: }{a list containing standard errors, t-values and p-values for the estimators.}
#'       \item{predicted: }{predicted values.}
#'       \item{residuals: }{residuals}
#'       \item{sums_of_squares: }{a list containg the sums of squares (total, model, residual sums of squares)}
#'       \item{means_of_squares: }{a list containing the mean sums of squares (total, model and residual mean of squares}
#'       \item{f_test: }{a list containing F-statistic and associated p-value.}
#'       \item{R_squared: }{unajusted R-squared value}
#'     }
#'   }
#' }
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
  ss <- summary_statistics(init$inputs$X,
                           init$inputs$n,
                           init$inputs$m)

  ## Linear model
  coeff <- linear_regression(init$inputs$X,
                             init$inputs$y)

  ## Tests
  tests <- compute_tests(init$inputs$X,
                         init$inputs$y,
                         coeff,
                         init$inputs$n,
                         init$inputs$m)

  ## Bring results together into one list
  res <- list(

    "inputs" = init$inputs,
    "summary_statistics" = ss,
    "coefficients" = coeff,
    "tests" = tests

  )

  ## Set class
  class(res) <- "linear_model"

  ## Return
  return(res)

}

## Methods for the 'linear_model' class

# Print a summary of the model statistics
#' @export
summary.linear_model <- function(x) {

  ## Formula --> character
  form <- as.character(x$inputs$formula)
  formchar <- paste0(form[2], " ", form[1], " ", form[3])

  ## Construct individual parts
  general <- paste0(
    "Formula: '", formchar, "'", "\n\n"
  )

  ## num. observations + predictors
  obs <- paste0("Obs.\tPredictors\n",
                x$inputs$n, "\t", x$inputs$m, "\n\n")

  ## Summary statistics
  df <- x$summary_statistics$df
  n <- x$summary_statistics$n

  ## Format summary stats
  ss <- x$summary_statistics
  ss$df <- NULL ; ss$n <- NULL

  ## To matrix
  ss_bind <- do.call(rbind, ss)
  rownames(ss_bind) <- c("mean", "variance", "min", "max")
  ss_bind <- round(ss_bind, digits=2)

  ## Model information

  # Retrieve information from object
  cf <- x$coefficients
  se <- x$tests$coef$standard_errors
  t <- x$tests$coef$t_values
  p <- x$tests$coef$p

  # Bind together
  modinfo <- do.call(cbind, list(cf, se, t, p))
  # Round to two digits
  modinfo <- round(modinfo, digits=4)

  # Message about degrees of freedom
  minfo <- paste0(
    "F-statistic: ", round(x$tests$f_test$f_statistic, digits=2),
    ", p-value: ", x$tests$f_test$p, "\n",
    "R-squared: ", x$tests$R_squared, "\n",
    "df: ", df, "\n\n"
  )

  ## Residuals
  resids <- summary(x$tests$residuals)

  ## Cat pieces to console
  cat(general)
  cat(obs)
  ## Print list containing the summary statistics matrix
  print.listof(list("Summary statistics" = ss_bind))
  # Print
  print.listof(list("Model information" = modinfo))
  cat(minfo)
  print.listof(list("Residuals"=resids))

}

# Print general model information to console
#' @export
print.linear_model <- function(x) {

  ## Formula --> character
  form <- as.character(x$inputs$formula)
  formchar <- paste0(form[2], " ", form[1], " ", form[3])

  ## Construct message
  msg <- paste0(

    "Linear regression model containing ", x$inputs$m, " predictors and ", x$inputs$n, " observations\n\n",
    "DV: ", x$inputs$variables$DV, "\n",
    "IV: ", ifelse(length(x$inputs$variables$IV) > 1,
                   paste0(x$inputs$variables$IV, collapse= ", "),
                   x$inputs$variables$IV), "\n\n",
    "Formula: '", formchar, "'"

  )

  ## Cat message + formatting
  cat(msg)

}

# Create a residual plot
#' @export
plot.linear_model <- function(x) {

  ## Plot predicted versus residuals
  plot(predict(x), resid(x),
       xlab = "predicted",
       ylab = "residuals",
       main = "Predicted versus residuals")
  ## Add a horizontal line at y=0
  abline(a = 0, b = 0, col = "red")

}

# Coef method
#' @export
coef.linear_model <- function(x) {

  x$coefficients

}

# Predict method
#' @export
predict.linear_model <- function(x) {

  x$tests$predicted

}

# Residuals & resid method
#' @export
residuals.linear_model <- function(x) {

  x$tests$residuals

}
#' @export
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

  ## Number of observations
  n <- nrow(X)
  ## Number of columns
  m <- ncol(X)

  ## Put in list & return
  res <- list(
    "inputs" = list(
      "formula" = formula,
      "variables" = vars,
      "y" = y,
      "X" = X,
      "n" = n,
      "m" = m
    )
  )

  return(res)

}

## Calculate summary statistics

summary_statistics <- function(X, n, m) {

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

# Compute the mean
cmean <- function(n, X, ones) {

  ## Compute mean
  return(t(ones) %*% X * 1/n)

}

# Compute variance
cvar <- function(n, X, means, ones) {

  ## Compute variance
  return(t(ones) %*% (X - ones %*% means)^2 * (1/(n-1)))

}

## Calculate the coefficients and intercept

linear_regression <- function(X, y) {

  ## Apply the linear regression formula
  res <- solve( t(X) %*% X ) %*% t(X) %*% y

  ## Give column names
  colnames(res) <- "Estimate"

  ## Return
  return(res)

}

## Inference

# Calculate p-values, F-ratio, SE values, etc
compute_tests <- function(X, y, coeff, n, m) {

  # Get standard errors
  SE <- standard_errors(y, X, coeff, n, m)

  # Compute t-values for coefficients
  tvalues <- tvalue(coeff, SE)

  # Compute the p-values for the t-values of the coeffficients
  pvalues_tstats <- Pvalues(tvalues, (n-m))

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

  # Variable names
  vns <- colnames(X)

  # Estimate of the variance
  ssquared <- sum( (y - X %*% coeff) ^ 2 ) / (n - m)

  # var-covar matrix
  vcv <- ssquared * chol2inv( chol( t(X) %*% X ) )

  # SE
  SE <- sqrt( diag( vcv ) )

  # To matrix
  SE <- matrix(SE, ncol = 1)

  # Rownames
  rownames(SE) <- vns
  colnames(SE) <- "SE"

  # Return
  return(SE)

}

# Compute t-values given estimate of standard errors and coefficients
tvalue <- function(coeff, SE) {

  tval <- coeff / SE
  ## Col names
  colnames(tval) <- "t"

  # Return
  return(tval)

}

# Compute p-values of coefficients given t-values
Pvalues <- function(tvals, df) {

  pvals <- 2 * pt( tvals, df, lower.tail = FALSE )
  ## Colnames
  colnames(pvals) <- "p"

  # Return
  return(pvals)

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


