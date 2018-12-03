## Functions for assignment 3, part I

#' Empirical power of a t-test
#'
#' This function takes a reference mean value, sample size, mean, standard deviation, significance level and computes the empirical power of R randomly drawn normally distributed samples with mean mu, standard deviation sd and sample size n. It then calculates R t.tests with reference to the
#'
#' @param n numeric. sample size
#' @param mu mean value used to draw random normal samples. See ?rnorm() for more information
#' @param sd standard deviation used to draw random normal samples
#' @param alpha significance level
#' @param R number of replications
#' @param type string. if 'one_sample', then a one-sample t-test, else a two-sample t-test
#' @param ... optional arguments. You may pass selected parameters from the t.test() function. You may also pass the following parameters:
#' \itemize{
#'   \item{mu0: numeric. mean of control group if running a two-sample t-test or, if type = "one_sample", a value indicating the true value of the mean (or difference in means if you are performing a one sample test). See ?t.test() for more information},
#'   \item{n0: numeric. sample size of control group if running a two-sample t-test. If not supplied then the function will use the value of n defined above.}
#'   \item{sd0: numeric. standard deviation of control group if running a two-sample t-test. If not supplied then the function will use the value of sd defined above.}
#' }
#' Note that you are required to either pass all parameters related to the control group (n0, mu0, sd0) or a parameter related to the reference mean (ref_mu).
#'
#' @return list containing:
#' \itemize{
#'   \item{inputs: }{list. user input values}
#'   \item{data: }{matrix. values of R samples of size n with mean mu and standard deviation sd drawn from a normal distribution using rnorm()}
#'   \item{p_values: }{vector. resulting p-values of R tests comparing the samples against the reference mean ref_mu}
#'   \item{power: vector. power calculated by taking the proportion of p-values for which the value falls below or is equal to the significance level alpha}
#' }
#'
#' @seealso Rizzo, Maria L. 'Statistical Computing with R. Chapman and Hall/CRC, 2007'. (pp. 167-169)
#' @export
emp_power <- function(n, mu, sd, alpha=0.05, R = 1000, type=c("one_sample", "two_sample"), ...) {

  type <- match.arg(type)

  ## Get optional params
  # (for t-test etc.)
  opts <- list(...)

  # Check inputs
  inputs <- perform_checks_ep(n, mu, sd, alpha, R, type, opts)

  ## Generate data --> if two-sample, need two samples
  data <- generate_data(inputs$n, inputs$mu, inputs$sd, inputs$R)
  if(inputs$type == "two_sample") {
    ## Control sample
    data_control <- generate_data(inputs$n0, inputs$mu0, inputs$sd0, inputs$R)
    ## Perform test R times
    test_out <- rep_t_test(data, inputs$R, inputs$type, inputs$alternative,
                           data_control = data_control)
    ## Save data in data list
    data <- list(
      "data" = data,
      "control" = data_control
    )
  } else {
    ## One-sample
    test_out <- rep_t_test(data, inputs$R, inputs$type, inputs$alternative,
                           mu0 = inputs$mu0)
  }

  # Check % of p-values <= confidence level
  power <- mean(test_out <= alpha)

  # Return
  list(
    "inputs" = inputs,
    "data" = data,
    "p_values" = test_out,
    "power" = power,
    "se" = sqrt((power * (1-power))/1000)
  )

}

# HELPER FUNCTIONS ----

# Perform checks
perform_checks_ep <- function(n, mu, sd, alpha, R, type, opts) {

  ## Save inputs
  inputs <- list(
    "n" = n,
    "mu" = mu,
    "sd" = sd,
    "R" = R,
    "alpha" = alpha
  )

  # Retrieve optional arguments from options list
  alternative <- ifelse("alternative" %in% names(opts), opts$alternative, "two.sided")
  mu0 <- ifelse("mu0" %in% names(opts), opts$mu0, NA)
  n0 <- ifelse("n0" %in% names(opts), opts$n0, NA)
  sd0 <- ifelse("sd0" %in% names(opts), opts$sd0, NA)

  # Check if mu0 supplied
  if(is.null(mu0)) stop("'mu0' not supplied but is required")

  # Based on inputs, determine if two sample or one sample
  if(type == "one_sample") {
    inputs$mu0 <- mu0
    inputs$type <- type
  } else {
    # Set values of control to that of experiment group
    n0 <- ifelse(!is.na(n0), n0, n)
    sd0 <- ifelse(!is.na(sd0), sd0, sd)
    inputs$mu0 <- mu0
    inputs$n0 <- n0
    inputs$sd0 <- sd0
    inputs$type <- type
  }

  # Checks
  if(!alternative %in% c('two.sided', 'less', 'greater')) {
    stop("'alternative' must be one of 'one.sided', 'less' or 'greater'. See '?t.test' for more information")
  }

  # Construct vector with variables to check
  if(type == "one_sample") {
    check_me <- c(mu0, n, mu, sd, R, alpha)
  } else {
    check_me <- c(n, mu, sd, R, mu0, sd0, n0, alpha)
  }

  # Type checks
  if(!all(vapply(check_me, function(x) is(x)[1], "char") == "numeric")) {

    stop("Parameters 'n', 'mu0', 'mu', 'sd', 'R', 'alpha' must be numeric")

  }

  # Add to inputs
  inputs$alternative <- alternative

  # Return inputs list
  return(inputs)

}

# Generate a matrix of n rows and R columns. The matrix is populated with n * R values drawn from a random distribution using rnorm()
#
# @param n see emp_power()
# @param mu see emp_power()
# @param sd see emp_power()
# @param R see emp_power()
#
# @return matrix of size n x R containing (n x R) randomly drawn normally distributed values
generate_data <- function(n, mu, sd, R) {

  exp <- replicate(R, rnorm(n, mu, sd))

  # return
  return(exp)

}

# Perform R t-tests on randomly sampled data
#
# @param data matrix. generated by generate_data()
# @param R see emp_power()
# @param mu0 see ref_mu parameter under emp_power()
# @param alternative string. indicate whether you want two-sided or one-sided test
#
# @return vector containing p-values
rep_t_test <- function(data, R, type = c("one_sample", "two_sample"),
                       alternative, ...) {

  type <- match.arg(type)
  opts <- list(...)

  # Based on type, retrieve params and perform test
  if(type == "one_sample") {

    mu0 <- opts$mu0
    # Apply over data
    apply(data,2, function(x) {

      # Perform test
      tst <- t.test(x,
                    alternative=alternative,
                    mu = mu0)

      # Return results
      tst$p.value

    })

  } else {

    ## Two samples
    data_control <- opts$data_control

    out <- numeric(R)
    for(i in seq_along(1:R)){

      # Perform test
      tst <- t.test(data[,i],
                    data_control[,i],
                    alternative=alternative,
                    var.equal = TRUE)
      # Return results
      out[i] <- tst$p.value

    }

    return(out)

  }

}
