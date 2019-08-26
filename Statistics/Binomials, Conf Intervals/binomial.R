
# Binomial distribution

values <- c(0, 1, 2)
probs <- c(.05, .1, .25, .5)
probability_of_success <- .25
number_of_trials <- 15


# 1)
# Given our prob of success and number of trials,
# whats the prob that our number of successes is 1, 2, or 3?
dbinom(values, number_of_trials, probability_of_success)
# .0133 means 1.3% chance we only get one success
# This should make sense. With 15 trials and .25% chance each time,
# we'd expect to get at least one


# 2)
# Cumulative- same as above
# whats the prob that our number of successes is 1, 2, or 3?
pbinom(values, number_of_trials, probability_of_success)
# Everything is the same, we are just accumulating
# pbinom of 4 means LESS THAN OR EQUAL TO



# 3)
# Given we had x probability, how many occurences would we expect?
qbinom(probs, number_of_trials, probability_of_success)



bags_per_min <- 170
average <- 5.5
s <- .2
n <- 10
lower <- 5.29
upper <- 5.71


# 1) ----------------------------------------------------------------------
#probability of 1 having weight < 5.29 grams > 5.71 grams?
# .14 + .15 = .29

# cum below
pnorm(lower, mean = average, sd = s)
# .14

# cum above
pnorm(upper, mean = average, sd = s)
# 1 - .85 = .15


# 2) ----------------------------------------------------------------------
# Prob that there will be at least 1 error in 10 bags?
error <- s / sqrt(n)
pnorm(lower, average, error)

(1 - pnorm(upper, average, error))


