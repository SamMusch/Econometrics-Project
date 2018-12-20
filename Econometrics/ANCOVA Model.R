### This lecture is where we are looking to see what impact the IV has on the DV, when we control for the covariate



# ANCOVA : Are the slopes significantly different? Does the drug actually help?

#IV: Dose
#DV: Effort resistance
#Covariate: Employee age (correlates to the DV, not the focus of the study though)
  #Essentially, we are controlling for the omitted variable

setwd("~/Desktop/csv-data-frames")
options(digits = 4, show.signif.stars=TRUE)
vit <- read.csv(file="~/Desktop/csv-data-frames/vitamin-a.csv",header=TRUE, sep=",", na.strings="?", stringsAsFactors = TRUE)
str(vit)
nrow(vit)
# View(vit)




##############################################################
### Running our ANCOVA model. Determining differences between placebo, low dose, high dose.
library(car)
model <- aov(effort~dose+age,data=vit)
ancova <- Anova(model, type="III")
print(ancova)
# Result: When controlling for age, the vitamin dose does have statistical significance on effort resistance

# Now, how big are the differences?
library(effects)
effect("dose",model)

# Are these differences statistically significant? 
library(multcomp)
mcomp <- glht(model, linfct=mcp(dose="Tukey"))
summary(mcomp)
confint(mcomp)
##############################################################





##############################################################
### Assumptions that we are going to test for.

#1 No outliers
#2 Relationship between IV an DV is linear
#3 No relationship between covariate and the IV
#4 Normal
#5 Homo variance
#6 Homoskedastic errors

# 3 We are testing to make sure that the covariate is not correlated with IV.
#This relationship between covar and IV should not be stat signif
#The difference between this Anova and the previous one is that this is multiplication
model <- aov(effort~age*dose,data=vit)
av <- Anova(model,type="III")
print(av)
# Result: P-value is above 5%, no evidence to support that IV and covar are correlated


# 4 Are residuals normally distributed? 
# zres stands for standardized residuals
res <- residuals(model)
zres <- scale(res)
shapiro.test(zres)
# Result: We reject that resids are normal

# 5 Homogenity of variance
# Variance of effort (DV) = Variance of dose (IV)
# Result: No evidence to reject that variance is different
leveneTest(vit$effort,vit$dose)

# 6 Homo errors
# Scatter plot of expected values vs fitted
library(ggplot2)
pred <- predict(model)
ggplot()+geom_point(aes(x=pred,y=res))
# Result: Errors appear to be random, accepting assumption of homo
##############################################################










