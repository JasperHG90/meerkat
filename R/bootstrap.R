## Bootstrap S3 class
## Calculate 95% CI given a function

# Use bootstrapping to calculate 95% confidence interval
#
# @param x a numeric vector
# @param y a numeric vector
# @param FUN function used to bootstrap
# @param R number of boostrap samples. defaults to 1.000
# @param ssize sample size of boostrap samples for x and y as a percentage of the total dataset
# @param ... optional arguments passed to FUN
#
# @return upper and lower confidence interval.
#
# @seealso <ARTICLE NAME>
bootstrap <- function(x, y, FUN, R = 1000, ssize = 0.5, ...) {

  ## ssize must be less than 1 & larger than 0
  if(!(ssize < 1 & ssize > 0)) {

    stop("ssize must be between 0 and 1")

  }

  ## Open output vector
  tstat_out <- vector(length = R)

  ## Loop through iterations
  for(it in 1:R) {

    ## Sample sizes --> round up
    ss_x <- ceiling(ssize * length(x))
    ss_y <- ceiling(ssize * length(y))

    s_x <- sample(x, ss_x, replace = TRUE)
    s_y <- sample(y, ss_y, replace = TRUE)

    ## Calculate t_statistic
    test <- FUN(s_x, s_y, ...)
    tstat_out[it] <- test$test$tstat

  }

  ## Sort the values
  tstat_out_sorted <- sort(tstat_out)

  ## Alpha value
  a <- 0.05

  ## Get lower and upper values of the 95% CI
  lower_ci_ind <- (1/2) * a * R
  upper_ci_ind <- 1 + (1 - (1/2) * a) * R

  lower_ci <- tstat_out_sorted[lower_ci_ind]
  upper_ci <- tstat_out_sorted[upper_ci_ind]

  ## Return
  return(
    list(
      "lower" = lower_ci,
      "upper" = upper_ci
    )
  )

}
