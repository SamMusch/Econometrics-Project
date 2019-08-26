# Overview ----------------------------------------------------------------

# https://github.com/SamMusch/R-Projects/tree/master/Econometrics

# Regression

# Steps in multiple regression:
# 1) Specify the model
# 2) Fit the model
# 3) Inferences
# 4) Assumptions
# 5) Use the model


# For both multiple regression and logistic regress, there must be
# as least 1 interval IV. Otherwise, we would use anova.

# In simple linear model, we are looking for the coefficient of B1.
# When B1 = 0, no relationship.



# 1) Specify the model ----------------------------------------------------

### Selecting the best model
# 1 Simple is best - reduce overfitting
# 2 Maximize R^2
# 3 Minimize SER (residual st error) = max adj R^2
# 4 Significant predictors
# 5 Logical relationships
# 6 Residuals assumptions okay


# Process model (hypothesized)
# Y = B0 + B1*X1 + B2X2 + ... error
# B0 = regressand when predictor = 0 (only has interpretation when there is data near this point)
# B1 = mean increase in regressand for each increase in the predictor


# The error is a random variable
   # Described as a probability distribution
   # Y is also uncertain because of this error



# Log-looking pattern
# Convex (Exponential)
# Concave (manual laborer)





# 2) Fit the model --------------------------------------------------------

# Process model (hypothesized) describes what relationships look like in real world
# Y = B0 + B1*X1 + B2X2 + ... error
# The coefficients (parameters) are unknown

# Fitted Model (from sample) describes how model operates with our sample info
# Yhat = B0hat + B1hat*X1 + B2hat*X2
# The coefficients (statistics) are known

# We are looking to minimize sum of errors squared
# There is only one line that minimizes SSE
# This line always passing through the point (Xbar, Ybar)


# 3) Inferences (testing) -----------------------------------------------------------

# This is where we connect our known coef and extrap to unknown coef

# Hypothesis testing
# Ho: No relationship between Y and ANY of the predictors (betas)
# Ha: At least one beta != 0

# Explained variation = our beta hat line
# Unexplained variation = residuals

# Summary() - order doesnt matter - controlling for all other variables, what is the beta?
# Anova() - order matters - controlling for previous betas, what is this beta?



# Understanding the table, notable statistics -----------------------------

###### ANOVA within regression

### Looking at DF column first
# Regression: number of predictors
# Error: observations - predictors - 1
# Total: observation - 1

### Looking at SS column next
# Regression: explained variation
# Error: unexplained variation
# Total: regression + total


### Looking at MS column 
# MS Regression: explained variation / predictors
# MS Error: unexplained variation / (observations - predictors - 1)
# MS Total: regression + total


### Looking at F column next
# F = ms regression / ms error


## Understanding standard error of regression
# SSE = sum of all the errors ^ 2
# MSE = SSE / (# of observations - # of predictors - 1)
# s = sqrt(MSE)


###### Notable statistics

# SER = sqrt(MS error) = stdev of our errors (this CAN go up if we increase predictors)
# R^2 = proportion of variation in y explained by the model
# R = measure of relationship, also shows direction




#### Deriving standard error of regression
# SSE         = unexplained variation = sum of all the errors ^ 2
# df Error    = observations - predictors - 1
# MSE         = SSE / df Error
# SER         = sqrt(MS error) 
# SER         = stdev of unexplained variation (this CAN go up if we increase predictors)



# 4) Assumptions ----------------------------------------------------------

# Random sampling
# Stability over time
# Errors normally distributed
  # with mean = 0 and sd fixed across values of predictor values

# Standardized residuals residuals = sum of --> each sample residual / st error of the residuals

# Multico
# Doesnt impact f stat or R^2, only interp of individual beta
# corr()
# .2 = no isse
# .2 to .7 = be cautious
# .7 and above = issue



# 5) Using the model ------------------------------------------------------

### Description - whats related to what?
# This is what the fitted model is


### Estimation - given some predictors at an x value, what is the mean of y?
### Estimate mean of y given our x values --> will be tightest at xbar
# Mean of process y 
# = point est (yhat)
# +- test stat 
# * MSE (avg unexplained variation)


### Prediction - In a situation, what will happen?
### Predict interval for a particular y given our x values
# Mean of process y 
# = point est (yhat)
# +- test stat 
# * sqrt (SSE^2 + MSE^2)



# 5b) Beta Interp ---------------------------------------------------------------
### For each predictor in our multi model:
# T stat = coef / st error of coef
# T dist has df = rows - predictors - 1

### Nominal factors
# Same slopes
# coefficients refer to difference in the y intercept of regressand vs base while holding others constant


### Interaction
# different slopes
# coefficients refer to difference in the slopes relative to base


# Multico plots -----------------------------------------------------------

data <- c()

row_sums = rowSums(data)
good_mask = row_sums > 0
good_data <- data[good_mask, 0]

cormat = round(cor(good_data), 2)
melted_cormat = melt(cormat)
melted_cormat

ggplot(data = melted_cormat, aes(x = var1, y = var2, fill = value)) + 
  geom_tile(color = 'white') + scale_fill_grdient2(low = 'blue', high = 'red', mid = 'white', midpoint = 0, limit = c(-1,1))+
  coord_fixed()
