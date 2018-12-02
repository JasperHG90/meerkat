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
# For each value, calculate the power od the t-test while holding the other parameters constant
for( i in seq_along(grids) ) {
  
  # Get name
  grids_in_name <- names(grids)[i]
  grid <- grids[[i]]
  
  # This switch is a wrapper for a bunch of if/else statements
  power_values <- switch(grids_in_name,
         grid_mu = lapply(grid, function(x) emp_power(150, 50, x, 15, 
                                                      alpha=0.05, alternative="greater")),
         grid_n = lapply(grid, function(x) emp_power(150, x, 155, 15, 
                                                     alpha=0.05, alternative="greater")),
         grid_sd = lapply(grid, function(x) emp_power(150, 50, 155, x, 
                                                      alpha=0.05, alternative="greater")),
         grid_alpha = lapply(grid, function(x) emp_power(150, 50, 155, 15, 
                                                         alpha=x, alternative="greater"))
  )
  
  # Get values
  pv <- sapply(power_values, function(x) x$power)
  
  # In data frame
  df <- data.frame("grid" = grid,
                   "power" = pv)
  
  # Plot
  grids_out[[i]] <- ggplot(df, aes(x=grid, y=power)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    scale_x_continuous(name = strsplit(grids_in_name, "_")[[1]][2]) +
    scale_y_continuous(name = "Power")
  
}

# Plot
grid.arrange(grids_out[[1]], grids_out[[2]],
             grids_out[[3]], grids_out[[4]], ncol=2, nrow=2)


## ------------------------------------------------------------------------
power.t.test(n=40, delta=5, sd=15, sig.level = 0.05, alternative="one.sided")

## ------------------------------------------------------------------------
emp <- emp_power(ref_mu=150, n=30, mu=155, sd=15, alternative="greater", R=1000)
emp$power

