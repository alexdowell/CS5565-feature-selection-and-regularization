##Linear Models and Regularization Methods

##Subset Selection Methods

##Forward and Backward Stepwise Selection

# Load the leaps library
library(leaps)

# Perform forward stepwise selection
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
                         nvmax = 12, method = "forward")
summary(regfit.fwd)

# Perform backward stepwise selection
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
                         nvmax = 12, method = "backward")
summary(regfit.bwd)

##Choosing Among Models Using the Validation-Set Approach and Cross-Validation
# Create a train-test split with a random seed
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters),
                replace = TRUE)
test <- (!train)

# Fit a best subset selection model to the training data
regfit.best <- regsubsets(Salary ~ .,
                          data = Hitters[train, ], nvmax = 19)

# Create a model matrix for the test data
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])

# Calculate validation errors for models with different numbers of predictors
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

val.errors

# Find the model with the minimum validation error
which.min(val.errors)

# Show coefficients for the best model (with 7 predictors)
coef(regfit.best, 7)

# Define a custom predict function for regsubsets objects
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# Fit a best subset selection model to the entire dataset
regfit.best <- regsubsets(Salary ~ ., data = Hitters,
                          nvmax = 19)
coef(regfit.best, 7)

# Perform k-fold cross-validation
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19,
                    dimnames = list(NULL, paste(1:19)))

# Calculate cross-validated errors for models with different numbers of predictors
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
                         data = Hitters[folds != j, ],
                         nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

# Calculate the mean cross-validated error for each model
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# Plot the mean cross-validated errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")

# Fit a best subset selection model to the entire dataset
reg.best <- regsubsets(Salary ~ ., data = Hitters,
                       nvmax = 19)
coef(reg.best, 10)

# Ridge Regression and the Lasso

# Load the leaps library
library(leaps)

# Remove rows with missing values
Hitters <- na.omit(Hitters)

# Prepare the data
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

# Ridge Regression
# Load the glmnet library
library(glmnet)

# Create a grid for lambda values
grid <- 10^seq(10, -2, length = 100)

# Fit the ridge regression model
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

# Display dimensions of coefficients
dim(coef(ridge.mod))

# Get the 50th lambda value
ridge.mod$lambda[50]

# Get the coefficients for the 50th lambda value
coef(ridge.mod)[, 50]

# Calculate the L2-norm of coefficients for the 50th lambda value
sqrt(sum(coef(ridge.mod)[-1, 50]^2))

# Get the 60th lambda value
ridge.mod$lambda[60]

# Get the coefficients for the 60th lambda value
coef(ridge.mod)[, 60]

# Calculate the L2-norm of coefficients for the 60th lambda value
sqrt(sum(coef(ridge.mod)[-1, 60]^2))

# Get the ridge coefficients for s = 50
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]

# Get the ridge coefficients for s = 379
predict(ridge.mod, s = 379, type = "coefficients")[1:20, ]

# Set seed for reproducibility
set.seed(1)

# Split the data into training and testing sets
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

# Fit the ridge regression model on the training set
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)

# Make predictions on the test set with s = 4
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])

# Calculate mean squared error for predictions
mean((ridge.pred - y.test)^2)

# Calculate mean squared error for the null model
mean((mean(y[train]) - y.test)^2)

# Make predictions on the test set with s = 1e10
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])

# Calculate mean squared error for predictions
mean((ridge.pred - y.test)^2)

# Make predictions on the test set with s = 0 and exact = T
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact = T, x = x[train, ], y = y[train])

# Calculate mean squared error for predictions
mean((ridge.pred - y.test)^2)

# Fit a linear model on the training set
lm(y ~ x, subset = train)

# Get the ridge coefficients for s = 0 and exact = T
predict(ridge.mod, s = 0, exact = T, type = "coefficients",
        x = x[train, ], y = y[train])[1:20, ]

# Set seed for reproducibility
set.seed(1)

# Perform cross-validation for ridge regression
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)

# Plot the cross-validation results
plot(cv.out)

# Get the best lambda value
bestlam <- cv.out$lambda.min

# Make predictions on the test set using the best lambda value
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])

# Calculate mean squared error for predictions
mean((ridge.pred - y.test)^2)

# Fit the ridge regression model with alpha = 0
out <- glmnet(x, y, alpha = 0)

# Get the ridge coefficients with the best lambda value
predict(out, type = "coefficients", s = bestlam)[1:20, ]

# The Lasso
# Fit the Lasso model on the training set
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

# Plot the Lasso model
plot(lasso.mod)

# Set seed for reproducibility
set.seed(1)

# Perform cross-validation for the Lasso
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)

# Plot the cross-validation results
plot(cv.out)

# Get the best lambda value for the Lasso
bestlam <- cv.out$lambda.min

# Make predictions on the test set using the best lambda value
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])

# Calculate mean squared error for predictions
mean((lasso.pred - y.test)^2)

# Fit the Lasso model with the entire dataset
out <- glmnet(x, y, alpha = 1, lambda = grid)

# Get the Lasso coefficients with the best lambda value
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef

# Display the non-zero Lasso coefficients
lasso.coef[lasso.coef != 0]

# PCR Regression

# Principal Components Regression
# Load the pls library
library(pls)

# Set seed for reproducibility
set.seed(2)

# Fit the PCR model with cross-validation
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")

# Display the PCR model summary
summary(pcr.fit)

# Plot the MSEP validation results
validationplot(pcr.fit, val.type = "MSEP")

# Set seed for reproducibility
set.seed(1)

# Fit the PCR model on the training set with cross-validation
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")

# Plot the MSEP validation results
validationplot(pcr.fit, val.type = "MSEP")

# Make predictions on the test set using ncomp = 5
pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 5)

# Calculate mean squared error for predictions
mean((pcr.pred - y.test)^2)

# Fit the PCR model with ncomp = 5
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 5)

# Display the PCR model summary
summary(pcr.fit)

# PLS Regression

# Load the pls library
library(pls)

# Set seed for reproducibility
set.seed(379)

# Fit the partial least squares regression model using training data with cross-validation
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")

# Display summary of the PLS regression model
summary(pls.fit)

# Plot validation plot with Mean Squared Error of Prediction (MSEP) for different components
validationplot(pls.fit, val.type = "MSEP")

# Make predictions using the test data with 1 component in the PLS model
pls.pred <- predict(pls.fit, x[test, ], ncomp = 1)

# Calculate mean squared error for the predictions
mean((pls.pred - y.test)^2)

# Fit the partial least squares regression model on the entire dataset with 1 component
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 1)

# Display summary of the final PLS regression model
summary(pls.fit)