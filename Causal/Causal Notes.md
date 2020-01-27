

[toc]

[Textbook Files](https://drive.google.com/drive/u/0/folders/1FRVhnqQwFjrQoCf0VrLwfSvRNgJF44H0)

[Github](https://github.com/SamMusch/R/tree/master/Causal)

[Private Github](https://github.com/SamMusch/Private-Repo/tree/master/Causal)

[Canvas](https://canvas.umn.edu/courses/161887/wiki)



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
  - Correct model spec is not necessarily the one with highest R^2, its the one which most closely reflects the relationship among the variables

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





## Week 2 - Correlation and Causation

[Mochen Yang Notes](https://github.com/SamMusch/Private-Repo/blob/master/Causal/Wk2 - Correlation/L2 Technical Notes.pdf)

### Threats to causal inference

1. Sample selection bias - sample doesn't represent the population we care about, can't generalize
2. Endogeneity - the error is related to one of our variables
   1. Omitted variable bias - we haven't accounted for everything
   2. Simultaneity bias - X and Y cause each other, there's something else "behind" them
   3. Measurement error - could be random or systematic - if random error is related to one of the variables, it doesn't just "go away". Our model will not actually reflect reality.



### Requirements for Causality

1. Correlation - the "prereq" for causal inference, but is not enough on its own
2. Temporal precendence - X has to lead to Y
3. Free from the threats above



### Randomized Experiments

Objective is to get rid of any kind of confounding explanations for why Y is changing. We are looking to control for all variables to isolate the impact of 1 variable in our response. 

Should be used when we have comparable subjects, we can randomize among them, and we have an outcome of interest that we can measure reliably. We can check how good our randomization was by comparing the *other* features between the groups and seeing if there are statistical differences.

- Simple
- Block - sort into groups, and then randomize





















