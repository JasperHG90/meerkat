## ------------------------------------------------------------------------
rm(list=ls())
library(meerkat)

# Set values and draw a sample
n <- 50
mu <- 160
mu0 <- 150
sd <- 15
set.seed(600)
x <- rnorm(n, mu, sd)
set.seed(500)
y <- rnorm(n, mu0, sd)
# We perform a t-test on the data
t.test(x, y, alternative="greater", var.equal = TRUE)

## ------------------------------------------------------------------------
library(ggplot2)
library(grid)
library(gridExtra)
# Define a grid of possible mean values from 140 to 160
grids <- list(
  grid_mu = seq(140, 160, 1),
  grid_sd = seq(5, 15, 0.5),
  grid_alpha = seq(0.01, 0.11, 0.005),
  grid_n = seq(50, 150, 5)
)

grids_out <- vector("list", length = 4)
# For each value, calculate the power of the t-test while holding the other parameters constant
for( i in seq_along(grids) ) {
  
  # Get name
  grids_in_name <- names(grids)[i]
  grid <- grids[[i]]
  
  # This switch is a wrapper for a bunch of if/else statements
  power_values <- switch(grids_in_name,
                          grid_mu = lapply(grid, function(x) emp_power(50, 
                                                                       x, 
                                                                       15, 
                                                                       R=1000, 
                                                                       type="two_sample",
                                                                       alpha=0.05, 
                                                                       alternative="greater", 
                                                                       mu0=150)),
                          grid_n = lapply(grid, function(x) emp_power(x, 155, 15, R=1000, 
                                                                      type="two_sample", alpha=0.05, 
                                                                      alternative="greater", mu0=150)),
                          grid_sd = lapply(grid, function(x) emp_power(50, 155, x, R=1000, 
                                                                       type="two_sample",alpha=0.05, 
                                                                       alternative="greater", mu0=150)),
                          grid_alpha = lapply(grid, function(x) emp_power(50, 155, 15, R=1000, 
                                                                          type="two_sample",alpha=x, 
                                                                          alternative="greater", mu0=150))
  )
  
  # Get values
  pv <- sapply(power_values, function(x) x$power)
  
  # In data frame
  df <- data.frame("grid" = grid,
                   "power" = pv,
                   "se" = sapply(power_values, function(x) x$se))
  
  # Plot
  grids_out[[i]] <- ggplot(df, aes(x=grid, y=power)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin=power - se, ymax=power + se)) + 
    theme_bw() +
    scale_x_continuous(name = strsplit(grids_in_name, "_")[[1]][2]) +
    scale_y_continuous(name = "Power")
  
}

# Plot
grid.arrange(grids_out[[1]], grids_out[[2]],
             grids_out[[3]], grids_out[[4]], 
             ncol=2, nrow=2)


## ------------------------------------------------------------------------
power.t.test(n=50, delta=10, sd=15, type="two.sample", 
             sig.level = 0.05, alternative="one.sided")

## ------------------------------------------------------------------------
set.seed(700)
emp <- emp_power(n=50, mu=160, mu0=150, sd=15, 
                 alpha=0.05, type="two_sample",
                 alternative="greater", R=1000)
emp$power

## ------------------------------------------------------------------------
# Create variables for flight simulation data
CSFI <- c(2,5,5,6,6,7,8,9)
TFI <- c(1,1,2,3,3,4,5,7,7,8)

## ------------------------------------------------------------------------
# Run
set.seed(1400)
pt <- permutation_test(CSFI, TFI, 999)
pt

## ------------------------------------------------------------------------
plot(pt)

## ------------------------------------------------------------------------
t.test(CSFI, TFI)

## ------------------------------------------------------------------------
set.seed(100)
# This draws 5 values from a uniform with a=0 and b=1
runif(5)

## ------------------------------------------------------------------------
# Sample from the density from 0 to 1
library(ggplot2)
library(gridExtra)
x <- seq(0, 1, 0.1)
df <- data.frame(x=x, dens=dunif(x, 0, 1), dist=punif(x,0,1), rand = runif(length(x), 0, 1))
p1 <- ggplot(df, aes(x=x, y=dens)) + geom_line() + ggtitle("uniform density")
p2 <- ggplot(df, aes(x=x, y=dist)) + geom_line() + ggtitle("uniform distribution")
p3 <- ggplot(df, aes(x=x, y=rand)) + geom_point() + ggtitle("random uniform distribution values") + 
  geom_hline(yintercept = 0.25, col = "red", linetype = 3) +
  geom_hline(yintercept = 0.5, col = "red", linetype = 3) +
  geom_hline(yintercept = 0.75, col = "red", linetype = 3)
grid.arrange(p1, p2,p3, ncol=3)

## ------------------------------------------------------------------------
set.seed(20)
obs_drawn <- vapply(1:2000, function(x) length(seq(1,20,1)[runif(20) < 0.8]), 0)
hist(obs_drawn)
mu20 <- mean(obs_drawn) # += 16
sd20<- sqrt(var(obs_drawn)) # +- 1.76
err <- sqrt(var(obs_drawn)) / 16

## ------------------------------------------------------------------------
set.seed(40)
obs_drawn <- vapply(1:2000, function(x) length(seq(1,100,1)[runif(100) < 0.8]), 0)
hist(obs_drawn)
mu100 <- mean(obs_drawn) # += 80
sd100<- sqrt(var(obs_drawn)) # +- 3.96
err100 <- sqrt(var(obs_drawn)) / 80 # 5%

## ------------------------------------------------------------------------
set.seed(888)
pt <- permutation_test(CSFI, TFI, 999, use_sample = FALSE)
pt

## ------------------------------------------------------------------------
set.seed(888)
pt <- permutation_test(CSFI, TFI, 999, use_sample = TRUE)
pt

## ------------------------------------------------------------------------
set.seed(1000)
permutation_test(CSFI, TFI, 999, use_sample = FALSE)

## ------------------------------------------------------------------------
set.seed(1000)
permutation_test(CSFI, TFI, 999, use_sample = TRUE)

## ------------------------------------------------------------------------
set.seed(1000)
permutation_test(CSFI, TFI, 999, use_sample = FALSE, tolerance = 0.1)

## ------------------------------------------------------------------------
set.seed(4500)
res <- boot_ttest(CSFI, TFI, R=2000)
res

## ------------------------------------------------------------------------
plot(res)

## ------------------------------------------------------------------------
t.test(CSFI, TFI, var.equal = TRUE)

