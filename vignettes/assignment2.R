## ------------------------------------------------------------------------
library(meerkat)
# Create two variables
CSFI <- c(2,5,5,6,6,7,8,9)
TFI <- c(1,1,2,3,3,4,5,7,7,8)

# Calculate t-test
set.seed(500)
tt1 <- meerkat::t_test(CSFI, TFI, variance_equal = TRUE, 
                       bootstrap_ssize = 0.7, 
                       R=1000)
tt1

## ------------------------------------------------------------------------
t.test(CSFI, TFI, var.equal = TRUE)

## ------------------------------------------------------------------------
t1 <- t_test(CSFI, TFI)
names(t1)

## ------------------------------------------------------------------------
# Load gala data
data("gala")

## ------------------------------------------------------------------------
# Run a linear regression model
mod1 <- lm("Species ~ Area + Elevation + Endemics", data = gala)
summary(mod1)

## ------------------------------------------------------------------------
# Retrieve residuals and predicted values
residuals_mod1 <- resid(mod1)
predicted_mod1 <- predict(mod1)

# Plot
plot(predicted_mod1, residuals_mod1)

## ------------------------------------------------------------------------
# Make subset of the variables
y <- as.matrix(gala[, "Species"])
# Create a formula
form <- formula("Species ~ Area + Elevation + Endemics")
# Create a model matrix
X <- model.matrix(form, gala)
# Apply the formula for the linear model
linmod <- solve( t(X) %*% X ) %*% t(X) %*% y
# Create the predicted values
yhat <- X %*% linmod
# Residuals
resid <- (y - yhat)
# Plot
plot(yhat, resid)

## ------------------------------------------------------------------------
print.listof(list("Coefficients" = cbind(linmod, coef(mod1))))

## ------------------------------------------------------------------------
# Load the meerkat library
library(meerkat)
# Create the model using the formula and the gala data
gala_mod <- linear_model(form, gala)
# Print the model
gala_mod

## ------------------------------------------------------------------------
# Call the summary function
summary(gala_mod)

## ------------------------------------------------------------------------
plot(gala_mod)

