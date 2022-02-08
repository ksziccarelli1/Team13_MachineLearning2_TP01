# Exercise 6-11 

# Objective: Predict per-capita crime rate in the Boston dataset

# Dataset
library(MASS)
data(Boston)
dim(Boston) # 506 rows, 14 columns
sum(is.na(Boston$medv)) # No NAs

str(Boston)
# Real dataset
# Y-variable: medv (median home value in Boston suburbs)
# X-variables: 
  # crim (crime rate in town)
  # zn (residential land zoned for lots > 25000 sq ft) 
  # industry (non-retail business acreage)
  # chas (Charles River dumvar, 1 if along river, 0 if not)
  # nox (Nitrogen Oxide concentration in air, ppm)
  # rm (avg # of rooms in housing unit)
  # age (of housing unit if built prior to 1940)
  # dis (distance to employment sites in Boston)
  # rad (accessibility to highways)
  # tax (property tax rate)
  # ptratio (teacher-student ratio)
  # black (proportion of African-Americans)
  # lstat (lower status of pop %)

# a) Regression methods: Best Subsets, Lasso, PCR

# Best Subsets

library(leaps)
regfit.full.model <- regsubsets(medv~., Boston, nvmax=13)
summary(regfit.full.model)
# predictors with most asterisks: lstat and rm
reg.summary <- summary(regfit.full.model)
reg.summary$rsq # R-squared for 12th and 13th models is basically the same at 74.06% (so choose the 12th for parsimony)

# Plot 
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l") # RSS

plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R-Squared", type="l") # Adjusted R-squared
which.max((reg.summary$adjr2)) # 11th model has highest Adjusted R-squared
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=8)

plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l") # Cp
which.min((reg.summary$cp)) # 11th model has lowest Cp
points(11, reg.summary$cp[11], col="red", cex=2, pch=8)

plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min((reg.summary$bic)) # 11th model has lowest BIC
points(11, reg.summary$bic[11], col="red", cex=2, pch=8)

par(mfrow=c(1,1))

plot(regfit.full.model, scale="r2")
plot(regfit.full.model, scale="adjr2") 
plot(regfit.full.model, scale="Cp")
plot(regfit.full.model, scale="bic")
# Adjusted R-squared, Cp, and BIC indicate 11th model, eliminating indus and age

# Choose 11th model
coef(regfit.full.model,11)
# R-squared = 71.30%

# --- Split data into Test and Train ---

set.seed(100)
train <- sample(c(TRUE,FALSE), nrow(Boston), replace=TRUE) # random vector in which elements that are true will be in train, otherwise they won't be
test <- (!train) # another vector in which true elements are in test, false if not
x <- model.matrix(medv~., Boston)[, -1] 
y <- Boston$medv
y.test <- y[test]

# Lasso Model

library(glmnet)
grid <- 10^seq(10, -2, length=100)
lasso.model <- glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.model)
# some coefficients (around half) equal zero regardless of the tuning parameter  

# cross validation and Test MSE:
set.seed(100)
cv.lasso <- cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.lasso) 
# MSE increases as log(lambda) increases and the number of x variables decreases (highest MSE at 1 variable)
# lowest MSE with all 13 variables included
bestlambda <- cv.lasso$lambda.min # 0.011699
lasso.pred <- predict(lasso.model, s=bestlambda, newx= x[test, ], y=y[test])
mean((lasso.pred - y.test)^2) 
# Test MSE = 24.6927

lasso.model2 <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.model2, type="coefficients", s=bestlambda)
lasso.coef
# of the 13 variables, indus = 0.004 is the lowest, followed by black=0.009, zn=0.04, tax = -0.01, 
# age doesn't have a coefficient
# final lasso model can eliminate age and indus, still have relatively low MSE with 11 variables

# PCR Model (Principal Components Regression)

library(pls)
set.seed(100)
pcrmodel1 <- pcr(medv~., data=Boston, scale=TRUE, validation="CV")
summary(pcrmodel1)
# CV score provided for 13 components
# CV score is the root mean square error for each component
# % of variance explained - increases as more components are added, up to 74.06% of medv explained by 13 components
validationplot(pcrmodel1, val.type="MSEP") # lowest MSEP at M=13

# PCR on train data
set.seed(100)
pcrmodel2 <- pcr(medv~., data=Boston, subset=train, scale=TRUE, validation="CV")
validationplot(pcrmodel2, val.type="MSEP") # still shows lowest MSEP at M=13

# Test MSE
pcr.pred <- predict(pcrmodel2, x[test,], ncomp=13)
mean((pcr.pred-y.test)^2)
# Test MSE = 24.724 -- very similar to the lasso

# Fit PCR on the full data set
pcr.fit <- pcr(y~x, scale=TRUE, ncomp=13)
summary(pcr.fit)
# same % of variance explained (74.06%) as found above in Best Subsets with all 13 variables

# Justification for this set of models:

# b) Propose Model and evaluate model performance using Validation Set Error / Cross-Validation
  # Validation Set DEFINE
  # Cross-Validation DEFINE

# Already done cross-validation on Lasso and PCR above

# Validation Set - Best Subsets

regfit.best <- regsubsets(medv~.-indus-age, data=Boston[train,], nvmax=11)
# computing validation set error for the best model of each model size:
test.matrix <- model.matrix(medv~.-indus-age, data=Boston[test, ])
# list to hold test error values:
val.errors <- rep(NA, 11)
# Loop - for each model size i, in this model, sizes 1-11 bc 11 features 
for (i in 1:11){
  coefi <- coef(regfit.best, id=i) # this extracts the coefficients from regfit.best for the best model of each size
  pred <- test.matrix[, names(coefi)] %*% coefi # this takes the coefficients from above and multiplies them into the test matrix, becoming the predictions
  val.errors[i] <- mean((Boston$medv[test]-pred)^2) # this computes the test MSE for the best model of each size, using pred
}
val.errors
# we confirm our selection of 11 variables, as this gives us the lowest test error: 24.67

# predict function for CV (basically a function incorporating the for loop used for the validation set above)
  # need bc there is no predict method for regsubsets() 
predict.regsubsets <- function(object, newdata, id){
  formula <- as.formula(object$call[[2]])
  matrix <- model.matrix(formula, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  matrix[ ,xvars] %*% coefi
}

# Cross-Validation - Best Subsets

k <- 10 # folds
n <- nrow(Boston)
set.seed(100)
folds <- sample(rep(1:k, length=n))
# list to hold CV error values:
cv.errors <- matrix(NA, k, 11, dimnames=list(NULL, paste(1:11)))
# Nested For Loop
for (j in i:k){
  best.fit <- regsubsets(medv~.-indus-age, data=Boston[folds != j, ], nvmax=11) 
  # in the jth fold, the elements of folds which equal j are in the test set, and the folds that don't equal j are in train
  for (i in 1:11){
    pred <- predict.regsubsets(best.fit, Boston[folds == j, ], id=i) # object is best.fit regression from above, new data is Boston where folds equal j, and id is 1
    # predictions for the best model of each model size, this time where folds do equal j, so we're using test set
    cv.errors[j,i] <- mean((Boston$medv[folds == j] - pred)^2) # computing test MSE on each subset and storing in cv.errors matrix
  }
} # yields 10x11 matrix of CV errors = test MSE for the jth fold for the best i variable model
cv.errors

# average the columns of the matrix to obtain a vector 
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
# vector of 11 elements, each representing CV error for the best subset of that number of features
# result corroborates the Validation Set results, as the lowest error is in the 11th model at 23.44
plot(mean.cv.errors, type="b", col="blue")

#c) Does the chosen model include all features in the dataset?
    # Best Subsets does not, we eliminated indus and age earlier. The Adjusted R-squared, RSS, Cp, and BIC all indicated that
    # the 11th model was the most optimal, and all measures pointed towards removing these two features.
