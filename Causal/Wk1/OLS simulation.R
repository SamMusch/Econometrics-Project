library(dplyr)
BMI = read.csv('BMI.csv') %>% filter(weight > 0)

mean(BMI$height)
hist(BMI$height)

# average height of men and women
BMI %>% group_by(gender) %>% summarize(avg_height = mean(height))

# OLS
m2 = lm(bmi ~ height + weight + gender, data = BMI)
summary(m2)


# OLS practice 2, model non-linear relationship
BMI = BMI %>% mutate(lnbmi = log(bmi),
                     lnweight = log(weight),
                     lnheight = log(height))


m3 = lm(lnbmi ~ lnheight + lnweight, data = BMI)
summary(m3)



# Running an experiment ----------------------------------------------------------------------

# Let's create a "population" of 100000 data points
set.seed(202020)
N = 100000

X1 = rnorm(n = N, mean = 1, sd = 2)  # random distribution
X2 = runif(n = N, min = 0, max = 5)  # uniform distribution
eps = rnorm(N)


# Create dataframe
Y = 1 + 2 * X1 + .5 * X2 + eps
population = data.frame(X1, X2, Y)


# take a sample of 500 data points
n = 500
sample_idx = sample(1:N, n)
sample_ = population[sample_idx, ]


model = lm(Y ~ X1 + X2, data = sample_)
summary(model)


# repeat sampling and estimating for multiple (100, 1000, 2000) times
nrep = 100
results = data.frame(b0 = rep(NA, nrep), b1 = rep(NA, nrep), b2 = rep(NA, nrep))


for (i in 1:nrep){
  n = 500
  sample_idx = sample(1:N, n)
  sample_ = population[sample_idx, ]
  model = lm(Y ~ X1 + X2, data = sample_)
  results[i, ] = coef(model)
}


colMeans(results)
hist(results$b1)

