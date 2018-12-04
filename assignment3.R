## Assignment 3 dev

## PART II ----

# Create variables for flight simulation data
CSFI <- c(2,5,5,6,6,7,8,9)
TFI <- c(1,1,2,3,3,4,5,7,7,8)

CSFI[order(runif(length(CSFI)))]

# Run
pt <- resample_test(CSFI, TFI, 2000, TRUE)
pt
plot(pt)
t.test(CSFI, TFI)

# a)

# Formula for permutation/bootstrap test
resample_test <- function(x, y, R, replace = FALSE) {

  # Have to do this else get warning
  #replace <- match.arg(replace)

  # Preparation ----

  if(replace) {
    method <- "bootstrap"
  } else {
    method <- "permutation"

    # R must be between 99 and 999
    if(R < 99 | R > 999) {

      stop("Number of permutations R must be between 99 and 999")

    }

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
  # Pool the sample
  z <- c(x,y)

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

  # Compute the t-statistic between x and y
  t_xy <- t.test(x, y)$statistic

  # Permutation test ----

  # Vector to store the results
  res <- numeric(R)

  # Matrix to store the indices used for each random subsample
  # We do this so we can reproduce the results
  ind <- matrix(0L, ncol = max(n_x, n_y), nrow = R)

  # Permute
  for(i in 1:R) {

    # Sample k indices
    k <- sample(K, size = max(n_x, n_y), replace = replace)
    # Save indices
    ind[i,] <- k
    # Subset
    xt <- z[k]
    yt <- z[-k]

    # Compute two-tailed t statistic
    res[i] <- t.test(xt, yt)$statistic

  }

  # Calculate the empirical p-value of the original sample (this counts as one permutation) and the other permutations
  p <- mean(c(t_xy, res) >= t_xy)
  # Times two if p <=0.5, otherwise times 2(1-p)
  p <- ifelse(p <= 0.5, 2*(p), 2*(1-p))

  # TODO: Make one and two-tailed test

  # Store data
  test_results <- list(
    "t_xy" = t_xy,
    "t_rep" = res,
    "indices" = ind,
    "p" = p
  )

  # Pull list together
  ret <- list(
    "method" = method,
    "data" = data,
    "results" = test_results
  )

  # Add class
  class(ret) <- "resample_test"

  # Return values
  return(
    ret
  )

}

# Helper function that capitalizes first letter of a word
# See ?toupper
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Plot method
plot.resample_test <- function(object) {

  # Plot a histogram of distribution
  hist(object$results$t_rep, main =
       paste0(capwords(object$method), " distribution of replicates"),
       freq = FALSE,
       xlab = paste0("T (p = ", object$results$p,")"),
       breaks = "scott")
  # Add sample t-statistic as a point
  points(object$results$t_xy, 0, cex = 1, pch=16, col="red")

}

# Print method
print.resample_test <- function(object) {

  method <- object$method
  p <- object$results$p
  t0 <- object$results$t_xy
  K <- object$data$K
  R <- object$data$R

  msg <- paste0("Two-tailed ", method, " test from ", K, " observations", "\n\n",
                "K\t", "R\t", "t\t", "p\n",
                "-----------------------------------\n",
                K, "\t", R, "\t", round(t0, digits=2), "\t", round(p, digits=3))

  cat(msg)

}



# b)

# Rwrite the function such that it can be used by the boot() function
resample_test_boot <- function(x, y, indices) {

  t.test(x[indices], y[indices])$statistic

}

# c)

# Check out chapter 7 paragraphs 7.1-7.5
# Use jackknife / or bootstrap to create confidence intervals

