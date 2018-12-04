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
         grid_mu = lapply(grid, function(x) emp_power(50, x, 15, R=1000, type="two_sample",
                                                      alpha=0.05, alternative="greater", mu0=150)),
         grid_n = lapply(grid, function(x) emp_power(x, 155, 15, R=1000, type="two_sample", 
                                                     alpha=0.05, alternative="greater", mu0=150)),
         grid_sd = lapply(grid, function(x) emp_power(50, 155, x, R=1000, type="two_sample",
                                                      alpha=0.05, alternative="greater", mu0=150)),
         grid_alpha = lapply(grid, function(x) emp_power(50, 155, 15, R=1000, type="two_sample",
                                                         alpha=x, alternative="greater", mu0=150))
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
emp <- emp_power(n=50, mu=160, sd=15, alpha=0.05, type="two_sample",
                 alternative="greater", R=1000, mu0=150)
emp$power

