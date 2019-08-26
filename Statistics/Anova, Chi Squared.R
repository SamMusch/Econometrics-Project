# Testing -----------------------------------------------------------------
# 1) Chi, multinomial
# nominal ~ nominal

# 2) ANOVA (one way)
# interval ~ nominal

# 3) Simple lin regression
# interval ~ interval

# 4) Logistic
# nominal ~ interval


# 1) Anova - How do the averages and st dev differ among groups? -----------------------------------------------------------------------

### Assumptions
# Random Sampling
# Stability over time
# Sample OF EACH GROUP > 30 --> else we need to assume that each of the groups are from a normal distribution
# The standard deviations of each must be equal

fit <- aov(a$CholReduction ~ a$Drug)
summary(fit)

# Averages of each group
print(model.tables(fit, "means"))

# Normality test -- assumption is normal
shapiro.test(fit$residuals)

# Checking for equal variances -- assumption is equal
library(car)
leveneTest(a$CholReduction, a$Drug)

# Check pairwise contrasts
TukeyHSD(fit, conf.level = .90)



# 2) Chi - Are observed results diff than expected? ---------------------------------------------------------------------
### Assumptions
# Random sampling
# Stability over time
# Sample size must be big enough that expected count for each category is at least 5
# Get more data
# Create bigger groups

library(readxl)
a <- read_excel("Gasoline.xlsx")

# Creating our contingency table
# Gives us tabular relationship between our 'x' and 'y'
cont_table <- table(a$`Second-last`, a$Last)
cont_table

# Running the test
# Ho: no relationship between last and second to last
# Ha: relationship
chisq.test(cont_table)

### Nature of the relationship
# Actual results - bigger number is bigger relationship
(chisq.test(cont_table)$residuals)^2

# If there was no relationship
chisq.test(cont_table)$expected

