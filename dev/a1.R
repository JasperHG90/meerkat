# Assignment 3

# Part 1

rm(list=ls())

# Two groups
# (1) Experiment
#   - mu: 160
#   - sd: 15
# (2) Control
#   - mu: 150
#   - sd: 15
# outcome: test scores
# 50 participants each
# Randomly assigned to each of two conditions

# a) We assume that the data is approx. normal and sample from the normal distribution
n <- 50
set.seed(600)
exp <- rnorm(n, 160, 15)

# We perform a t-test on the data
t.test(exp, alternative="greater", mu = 10)
# The one-tailed test is statistically significant (p<0.0001). At a significance level of a=0.05 we would therefore reject the null that there is no difference in the test scores of these groups. The exiperimental group , it would seem, performed better than the control and the special training has had the desired effect.



# Define a grid of possible mean values from 140 to 160
grid <- seq(140, 160, 1)

# For each value, calculate the power while holding the other parameters constant
grid_out <- lapply(grid, function(x) emp_power(150, 50, x, 15, alpha=0.05, alternative="greater"))

# Get values
power <- sapply(pow, function(x) x$power)

# Calculate SE
library(Hmisc)
se <- sqrt(vals * (1-vals) / 1000)
# Plot error bars
errbar(grid, power, yplus=power + se, yminus = power - se, xlab = bquote(theta))
lines(grid, power, lty=3)

# When testing a hypothesis with H0 and H1, a type II error occurrs when H1 is true but H0 is not rejected. The power of test is given by the power function which gives us the probability pi(theta) of rejecting H0 given that the true value of the parameter is theta.

# Part II

# b) we can use the 'runif()' function to draw values from a uniform distribution.
set.seed(100)
# This draws 20 values from a uniform with a=0 and b=1
runif(20)
# If we wanted 80% of our rows randomly sampled, then we subset the probabilities generated by the uniform distribution s.t. they are below 0.8 --> this works because the uniform distribution is linear. Also, we get approximately 80%, not exactly.

# We can demonstrate this as follows: sample 80% of the values of a sequence of numbers from 1 to 20 and take the length of this sequence
nums <- vapply(1:2000, function(x) length(seq(1,20,1)[runif(20) < 0.8]), 0)
hist(nums)
mean(nums) # += 16
sqrt(var(nums)) # +- 1.76
# So on average, we draw 16 observations with an sd of 1.76 or +- 11% error

# We can do the same for 100 observations
nums <- vapply(1:2000, function(x) length(seq(1,100,1)[runif(100) < 0.8]), 0)
hist(nums)
mean(nums) # +- 80
sqrt(var(nums)) # +- 4.1 or +- 5% error

# On average, this gives us the right number of observations. We also note that, as the sample size we're sampling increases, the histogram looks more and more normal. This we can express in terms of the mean over the standard deviation, which tells us that the sample with 100 observations has smaller tails.

# This works for the same reason as that, for large samples, the Bessel correction to the variance is less important.

x <- c(5, 10, 50, 100, 500, 1000, 5000, 10000)
nums <- sapply(x, function(y) vapply(1:2000, function(z) length(seq(1,y,1)[runif(y) < 0.8]), 0))

# Plot error relative to the mean
plot(x, apply(nums, 2, function(x) sd(x) / mean(x)))

# Either central limit theorem or law of big numbers --> we're essentially going for a proportion so it makes sense for this to become better as n --> infinity (look up central limit theorem)

