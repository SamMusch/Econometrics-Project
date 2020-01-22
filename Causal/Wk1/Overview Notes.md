# Week 1

## Overview

Causal inference is about understanding why, machine learning is about making accurate predictions

To evaluate in causal inference, we have to look at the results of changes that we make. What happened? What was expected to happen?

What levers do we pull? What outcome can we expect from this? Why did the algo make this prediction? If we change this feature, what will happen?



## Stats Overview - [Mochen Yang Notes](https://github.com/SamMusch/Private-Repo/blob/master/Causal/Wk1/L1%20Stats%20Overview.pdf)

### Random variable

Takes a random value with some probability - discrete or continuous

Described by a distribution

Moments

- Expectation
- Variance
- Skew
- Kurtosis



### Linear Regression

Estimating the population with the sample that we have - min SSE

Models relationship between DV and linear combo of IV + error

- Controlling for other variables, on average a 1-unit change in X1 corresponds to __ unit change in Y
- P-value = prob our result was a false-positive
- Confidence interval = range that includes the true value with __% probability
- R^2 = % of change in DV accounted for by our IV

OLS properties

- Unbiased - the average estimate of the coef = the true value
- Each coef estimate distribution is normal

OLS assumptions

- Exogeneity - error is not correlated with any of our IV's. If this does not hold, OLS properties don't hold.



