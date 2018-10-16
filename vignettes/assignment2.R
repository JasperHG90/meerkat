## ------------------------------------------------------------------------
# Create two variables
CSFI <- c(2,5,5,6,6,7,8,9)
TFI <- c(1,1,2,3,3,4,5,7,7,8)

# Calculate t-test
meerkat::t_test(CSFI, TFI)

## ------------------------------------------------------------------------
t.test(CSFI, TFI, var.equal = TRUE)

## ------------------------------------------------------------------------
t1 <- t_test(CSFI, TFI)
names(t1)

## ------------------------------------------------------------------------
data("gala")

## ------------------------------------------------------------------------
mod1 <- lm("Species ~ Area + Elevation + Endemics", data = gala)
summary(mod1)

## ------------------------------------------------------------------------
residuals_mod1 <- resid(mod1)
predicted_mod1 <- predict(mod1)

# Plot
plot(predicted_mod1, residuals_mod1)

## ------------------------------------------------------------------------
# Make subset of the variables
gala_ss <- as.matrix(gala[,c("Area", "Elevation", "Endemics")])
y <- as.matrix(gala[, "Species"])
# Make a column vector of length nrow(gala_ss) containing only values of 1
onevect <- matrix(rep(1, nrow(gala_ss)), ncol=1)
# Bind the data together
X <- cbind(onevect, gala_ss)
# Apply the formula for the linear model
linmod <- solve( t(X) %*% X ) %*% t(X) %*% y
# Create the predicted values
yhat <- X %*% linmod
# Residuals
resid <- (y - yhat)
# Plot
plot(yhat, resid)

