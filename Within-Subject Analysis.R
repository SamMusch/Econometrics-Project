#Lecture 4 (within)
# The within test is where we run the experient n times (t1, t2, etc) and analyze whether the results differ over time
#Are there significant differences between the begin, mid, and end time periods of the weight loss experiment?



#IV = time
#DV = weight
#3 time periods

diet <- read.csv("~/Desktop/csv-data-frames/diet1.csv")

##############################################################
#1 Build a matrix with the time periods (aka factor levels)
moments_mat <- c("Beginning","Middle","End")
print(moments_mat)

#2 Turn into dataframe
moments_frm <- data.frame(moments_mat)

#3 Build a matrix using our columns from  ^ 1&2, combining with values we were given
weight_mat <- cbind(diet$weight_beg,  diet$weight_mid,  diet$weight_end)
print(weight_mat)

#Columns = beg, mid, end
#Rows = each of the 82 people studied

#4 Get the means of each of the 3 time periods
# This code will produce three "intercepts", but they are really just the means 
# Results: 86.2,  82.1,  81.1
model <- lm(weight_mat~1)
summary(model)

##############################################################
##############################################################
### Assumptions that we are going to test for when seeing if there was change over time

#1 Normal
#2 No outliers
#3 * There is sphericity (differences of variance equal for the DV?)
# 3 Sphericity ^


#5 Within-subject analysis: significantly different?
#idata and idesign define the factor levels (3 time periods)
#moments_frm is the data frame we created with the matrix below
#moments_mat is the matrix we made of just the 3 column labels
model2 <- Anova(model,idata=moments_frm,idesign=~moments_mat,type="III")
summary(model2,multivariate=FALSE)
#Result: Mauchly test is <5%, so we do not have sphericity
#IF the test ^ is <5%, read the GG p-value. If p-value is <5%, means are different
#IF the test Mauchly is >5%, read the top Repeated-Measures test under the "Intercept". If p<5%, means dif

## So this ^ told us that we did not meet sphericity assumption and told us that our differences were statistically significant.
# However, we still need to calc the lower, mid and upper values to see how different we expect the actual populations to be.




##########################################################################################

#### This takes the original beg, mid, and end and melts them into one column
dietm <- melt(diet)
colnames(dietm) <- c("group","weight")
View(dietm)
View(diet)

#### Running ANOVA model: Are weight differences between the time periods stat significant?
model <- aov(weight~group, data=dietm)
TukeyHSD(model)






