

[toc]

## Causal Overview

1. What is the causal relationship of interest?
2. What is the ideal experiment that can be run to capture this relationship?
3. What is the strategy to use sample data to approximate population data?
4. What are you studying, what's the sample, what are the assumptions?

Machine learning is about using our features to make predictions that are as accurate as possible. Model revisions are done with the intent to improve the model's accuracy (RMSE, F1, etc). Causal inference is about taking the results of what has happened and then looking at variation to understand the relationships that exist among the "result" and the features that lead to the result. 

What levers do we pull? What outcome can we expect from this? Why did the algo make this prediction? If we change this feature, what will happen? To evaluate in causal inference, we have to look at the results of changes that we make. What happened? What was expected to happen?



## Wk 1 - Stats Overview 

[Mochen Yang Notes](https://github.com/SamMusch/Private-Repo/blob/master/Causal/Wk1/L1%20Stats%20Overview.pdf)

### Random variable

A random variable takes a random value from some distribution

- Discrete - probability mass function - described by prob that the variable takes each of the numbers
- Continuous - probability density function - described by prob that variables takes value within interval 

Moments - Expectation, Variance, Skew, Kurtosis



### Linear Regression

Estimating the population with the sample that we have - min SSE

Models relationship between DV and linear combo of IV + error

- Confidence interval = range that includes the true mean with __% probability
- R^2 = % of change in DV accounted for by our IV

OLS properties

- Unbiased - the exact coef value that we find probably won't be exactly the same as the "true" value, but on average our estimate of the coef should equal the true value
- Each coef estimate's distribution is normal

OLS assumptions

- Exogeneity - error is not correlated with any of our IV's. If this does not hold, OLS properties don't hold



| Log Action         | Change in X | Change in Y |
| ------------------ | ----------- | ----------- |
| Normal             | unit        | unit        |
| Log IV (level log) | %           | unit        |
| Log DV (log level) | unit        | %           |
| Log Both           | %           | %           |

