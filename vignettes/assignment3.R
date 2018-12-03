## ------------------------------------------------------------------------
rm(list=ls())
library(meerkat)

# Set values and draw a sample
n <- 50
mu <- 160
sd <- 15
set.seed(600)
exp <- rnorm(n, mu, sd)
# We perform a t-test on the data
t.test(exp, alternative="greater", mu = 150)

