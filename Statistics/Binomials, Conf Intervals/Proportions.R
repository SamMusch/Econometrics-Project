# Loading Data ------------------------------------------------------------
library(readxl)
# get data - see excel file for data description information
Congress <- read_excel("Piracy.xlsx", col_names = TRUE)

# 1) One Sample T Test -------------------------------------------------------

# 90% confidence interval - lower and upper bounds
# Looking for the average number of years served
t.test(Congress$years, conf.level = .90)

# RESULTS 
  # p-value < 2.2e-16
  # lower = 11.06 years
  # expect= 11.76 years 
  # upper = 12.45 years

# Alt hypth --> true mean != 0
# Conclude  --> there is a relationship

# 2) Proportion Confidence Interval ------------------------------------------

# 90% confidence interval for proportion with stance = "yes"
table(Congress$stance)
# Results
# 44 leaning no
# 122 no
# 11 undecided
# 294 unknown
# 63 yes
yes = 63 # answer of interest, number of successes
total = 534 # number of trials
p_hat <- yes / total



# 3) Binomial estimation method -----------------------------------------------------------------------

# Alt hypoth is that true prob of success != 63/534 (11.8%)
binom.test(yes, total, p_hat, conf.level = .9)
# Results
# Lower = 9%
# Mean = 11.8%  
# Upper = 14%




# 4) Wilson score method -----------------------------------------------------------------------

# Alt hypoth is that true prob of success != 63/534 (11.8%)
# This test is an alt to the binomial estimation method)
prop.test(yes, total, conf.level = .90)
# Results
  # Lower = 9%
  # Mean = 11.8%  
  # Upper = 14%




# P-values ----------------------------------------------------------------

library(readxl)
real <- read_excel('RealEstate.xlsx')
View(real)

table(real$Bathrooms) 

yes <- 226 + 33 + 8 + 2 + 2 + 1             # answer of interest, number of successes
total <- yes + 65 + 444  # number of trials
p_hat <- 1/3           # whatever we are comparing against

# Can we claim that less than 25% of women smoke?
binom.test(yes, total, p_hat, alternative = "greater")
prop.test(yes, total, p_hat, alternative = "greater")

### Assumptions?

