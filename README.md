# CS5565 Feature Selection and Regularization  

## Description  
This repository contains R scripts for **CS5565**, focusing on feature selection methods, model selection techniques, and regularization methods such as ridge regression, lasso, and principal component regression (PCR). The labs explore how different feature selection and shrinkage techniques affect model performance and interpretability.  

## Files Included  

### **Lab 4A: Feature Selection and Model Selection**  
- **File:** CS5565_lab_4A.R  
- **Topics Covered:**
  - Forward and backward stepwise selection using regsubsets().
  - Best subset selection and validation-set approach.
  - Cross-validation for selecting the best model.
  - Ridge regression and the lasso using glmnet().
  - Principal component regression (PCR) and partial least squares (PLS).
  - Evaluating model performance using cross-validation.  

### **Lab 4B: Polynomial and Spline Regression**  
- **File:** CS5565_lab_4B.R  
- **Topics Covered:**
  - Polynomial regression using lm() with different transformations.
  - ANOVA for comparing polynomial models of different degrees.
  - Logistic regression with polynomial features.
  - Regression splines using bs() and ns().
  - Smoothing splines and local regression.
  - Evaluating flexibility and overfitting using different degrees of freedom.  

### **Lab Questions and Analysis**  
- **File:** CS5565_lab_4_questions.pdf  
- **Contents:**
  - Analysis of feature selection results with different nvmax values.
  - Impact of changing lambda in ridge regression.
  - Effect of different seeds in partial least squares regression.
  - Interpretation of smoothing splines with varying degrees of freedom.  

## Installation  
Ensure R and the required libraries are installed before running the scripts.  

### Required R Packages  
- leaps  
- glmnet  
- pls  
- ISLR2  
- splines  

To install the necessary packages, run:  
install.packages(c("leaps", "glmnet", "pls", "ISLR2", "splines"))  

## Usage  
1. Open RStudio or an R console.  
2. Load the necessary dataset using library(ISLR2) or other relevant packages.  
3. Run the script using:  
   source("CS5565_lab_4A.R")  
   source("CS5565_lab_4B.R")  
4. View model summaries and results in the R console or through plots generated in RStudio.  

## Example Output  
- **Forward Stepwise Selection (Lab 4A)**  
  summary(regsubsets(Salary ~ ., data = Hitters, nvmax = 12, method = "forward"))  
  - Displays selected features at each step.  
- **Ridge Regression Coefficients (Lab 4A)**  
  predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]  
  - Shows how coefficients shrink with regularization.  
- **Polynomial Regression (Lab 4B)**  
  fit <- lm(wage ~ poly(age, 4), data = Wage)  
  summary(fit)  
  - Evaluates polynomial regression model coefficients.  
- **Spline Regression (Lab 4B)**  
  fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)  
  - Fits a regression spline with three knots.  

## Contributions  
This repository is designed for educational purposes. Feel free to fork and modify the scripts.  

## License  
This project is open for educational and research use.  

---
**Author:** Alexander Dowell  
