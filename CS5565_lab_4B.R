# Load ISLR2 library and attach the Wage dataset
library(ISLR2)
attach(Wage)

# Fit a degree-4 polynomial regression model
fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

# Fit a degree-4 polynomial regression model with raw = T
fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))

# Fit a degree-4 polynomial regression model using I() function
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4),
            data = Wage)
coef(fit2a)

# Fit a degree-4 polynomial regression model using cbind()
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4),
            data = Wage)

# Create a sequence of age values for predictions
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])

# Predict and calculate standard errors for the degree-4 polynomial model
preds <- predict(fit, newdata = list(age = age.grid),
                 se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit,
                  preds$fit - 2 * preds$se.fit)

# Plot the degree-4 polynomial model with confidence bands
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1),
    oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Predict and calculate standard errors for the raw degree-4 polynomial model
preds2 <- predict(fit2, newdata = list(age = age.grid),
                  se = TRUE)
max(abs(preds$fit - preds2$fit))

# Fit polynomial models of different degrees and compare using ANOVA
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5))

# Fit polynomial models with education and compare using ANOVA
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

# Fit a logistic regression model with a degree-4 polynomial for age
fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage,
           family = binomial)

# Predict and calculate standard errors for the logistic regression model
preds <- predict(fit, newdata = list(age = age.grid), se = T)

# Calculate the probability of wage > 250 using the logistic model
pfit <- exp(preds$fit) / (1 + exp(preds$fit))

# Calculate the confidence bands for the logistic model
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit,
                        preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# Predict probabilities using the logistic model with type = "response"
preds <- predict(fit, newdata = list(age = age.grid),
                 type = "response", se = T)

# Plot the logistic regression model with confidence bands
plot(age, I(wage > 250), xlim = agelims, type = "n",
     ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Create age bins and count observations in each bin
table(cut(age, 4))

# Fit linear regression model with age bins as predictors
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

# Load ISLR2 library and attach the Wage dataset
library(ISLR2)
attach(Wage)

# Load the splines library 
library(splines)

# Create a sequence of age values for predictions
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])

# fit a basis spline regression model
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)

# Predict and plot basis spline regression model with confidence bands
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

# Explore dimensions of the basis matrix
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))

# Get the knots from the basis matrix
attr(bs(age, df = 6), "knots")

# Fit a natural spline regression model and predict
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)

# Plot the natural spline regression model
plot(age, wage, col = "gray")
lines(age.grid, pred2$fit, col = "red", lwd = 2)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)

# Compare the degrees of freedom for the two spline models
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Plot the local regression models with different span values
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)),
      col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)),
      col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Load ISLR2 library and attach the Wage dataset
library(ISLR2)
attach(Wage)

# Load the splines library
library(splines)

# Create a sequence of age values for predictions
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])

# Plot the age vs. wage data
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")

# Fit smoothing splines with different degrees of freedom
fit9 <- smooth.spline(age, wage, df = 9)
fit16 <- smooth.spline(age, wage, df = 16)
fit22 <- smooth.spline(age, wage, df = 22)

# Plot the smoothing splines with different degrees of freedom
lines(fit9, col = "red", lwd = 2)
lines(fit16, col = "blue", lwd = 2)
lines(fit22, col = "green", lwd = 2)

# Add a legend for the different smoothing splines
legend("topright", legend = c("9 DF", "16 DF", "22 DF"),
       col = c("red", "blue", "green"), lty = 1, lwd = 2, cex = .8)
