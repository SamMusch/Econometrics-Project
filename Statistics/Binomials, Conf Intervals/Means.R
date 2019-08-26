# Finding true mean ----------------------------------------------------------

library(readxl)
wait <- read_excel('gpa.xlsx')

# Given a sample mean, stdev, etc, what is the true mean?
t.test(wait$CollegeGPA, conf.level = .95)

# Interpret --> 94% of the time that we calculate CI, true mean will fall in the interval



# Finding true mean by hand -------------------------------------------------
# Extrapolating a sample mean to the population mean.
# We are given a sample mean, a stdev, and a sample size. 
# We are looking to determine the lower and upper bound of the true mean.

n <- 36              # Sample size --> if n is less than 30, we need to assume that xbar normal
xbar <- -34.8        # sample avg
s <- .2              # sample stdev
con <- .9            # confidence level given
con <- con / 2       # left and right side split even
prob <- qt(con, n-1) # if we have the conf interval


# Find a 90% confidence interval for the true mean
upper <- xbar + prob * (s / sqrt(n)) 
lower <- xbar - prob * (s / sqrt(n))
upper
lower

# Interpret --> 90% of the time that we calculate CI, true mean will fall in the interval





# Testing mean vs alternate ---------------------------------------------------------

#t.test(Congress$years, alternative = "two.sided", mu = 10) 
#t.test(Congress$money_pro, alternative = "greater", mu = 25000) 
#t.test(Congress$money_con, alternative = "less", mu = 25000) 

library(readxl)
sup <- read_excel('gpa.xlsx')
# Our question was -- is the new sample of 250k greater than the old sample of 210k?
testing_against <- 5.25
t.test(sup$CollegeGPA, alternative = "less", mu = testing_against, conf.level = .9) 

