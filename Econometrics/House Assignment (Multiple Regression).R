setwd("/Volumes/FLASHDRIVE")
options(digits = 4, show.signif.stars=TRUE)
housedata <- read.csv(file="/Volumes/FLASHDRIVE/HouseAssignment/houses.csv",header=TRUE, sep=",", na.strings="?")
str(housedata)
nrow(housedata)

install.packages("xts")
install.packages("ggplot2")
install.packages("moments")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")
install.packages("stargazer")

####Mean and st dev of price
summary(housedata$price)
pricevariance <- var(housedata$price)
pricestandarddeviation <- sqrt(pricevariance)
pricevariance
pricestandarddeviation


####Mean and st dev of sq ft
summary(housedata$sqft_living)
sqft_living_variance <- var(housedata$sqft_living)
sqft_living_standarddeviation <- sqrt(sqft_living_variance)
sqft_living_variance
sqft_living_standarddeviation

######test relationship between price and square feet
cor.test(housedata$sqft_living, housedata$price, use="complete.obs")




##### Scatter plot with price and sqft_living
dev.new(1)
par(mfrow=c(1,2))
plot(housedata$sqft_living,
     housedata$price,
     type="p",
     main="Sq Ft vs Price of House",
     xlab="Square Feet",
     ylab="Price")

####linear model
model1 <- lm(price~sqft_living, data=housedata)
abline(model1,lwd=1)
summary(model1)


#######coefficients, fitted values, residuals
betahats <- coef(model1)
betahats
residuals <- residuals(model1)
fittedvalues <- fitted(model1)


#####Calculating the predicted house price given the median square footage (1873)
pricefit <- betahats[1] + (betahats[2]*1873)
pricefit

######Confidence interval of slope with 95% interval
cislope <- confint(model1)
cislope




#####Calculating the predicted house price given 3200 sq ft
secondassignmentprice <- betahats[1] + (betahats[2]*3200)
secondassignmentprice


######Confidence interval of slope with 99% interval
cislope99 <- confint(model1, level=0.99)
cislope99

#######confidence interval for mean y
xvals <- data.frame(sqft_living=c(390,8000))
cimeany <- predict(model1, data.frame(sqft_living=3800), interval="confidence", level=0.95)
cimeany

#####prediction interval for the mean of a home w living area 3800 sq ft
pi <- predict(model1, data.frame(sqft_living=3800), interval="prediction", level=0.95)
pi

##dev.new(width=8,height=4)
##par(mfrow=c(1,2))
##plot(housedata$sqft_living,residuals,type="p",main="Residuals against Sq ft Living Area",xlab="Sq ft",ylab="residuals")
##plot(model1,which=1)




###calculating age of homes
housedata$age = 2015 - housedata$yr_built
summary(housedata$age)
sd(housedata$age)
var(housedata$age)

summary(housedata$waterfront)

pairwise <- data.frame(housedata$bedrooms, 
                       housedata$bathrooms, 
                       housedata$sqft_living, 
                       housedata$sqft_lot,
                       housedata$floors,
                       housedata$waterfront,
                       housedata$condition,
                       housedata$yr_built)
cor(pairwise, use="complete.obs")

multimodel <- lm(price~
                housedata$bedrooms+ 
                 housedata$bathrooms+ 
                 housedata$sqft_living+ 
                 housedata$sqft_lot+
                 housedata$floors+
                 housedata$waterfront+
                 housedata$condition+
                 housedata$yr_built,
                 data=housedata)
summary(multimodel)




####scatter plot of residuals
####qq plot of residuals
####cooks plot of residuals
dev.new(width=8, height=4)
par(mfrow=c(1,3))
plot(multimodel,which=1, main="Linear model: Residuals vs Fitted Values") 
plot(multimodel,which=2, main="Linear model: Q‐Q Plot") # residual Q‐Q plot 
plot(multimodel,which=4, main="Linear model: Cook's Distance") # Cook's distance


##sink("output.txt")
##print(names(housedata))
##print(summary(housedata))
##sink()

model2 <- lm(log(housedata$price)
   ~log(housedata$sqft_living)
   +housedata$bedrooms
   +housedata$bathrooms
   +log(housedata$sqft_lot)
   +housedata$floors
   +housedata$waterfront
   +housedata$condition
   +housedata$age
   , data = housedata)
model2

coef <- coef(model2)
res <- residuals(model2)
fitted <- fitted(model2)

library(car)
vif(model2)

multico <- data.frame(log(housedata$price)
                      ,log(housedata$sqft_living)
                      ,housedata$bedrooms
                      ,housedata$bathrooms
                      ,log(housedata$sqft_lot)
                      ,housedata$floors
                      ,housedata$waterfront
                      ,housedata$condition
                      ,housedata$age)
cor(multico, use="complete.obs")


##linear and log residual plots
dev.new(width=8, height=4)
par(mfrow=c(1,3))
plot(model2,which=1, main="Log model: Residuals vs Fitted Values") 
plot(model2,which=2, main="Log model: Q‐Q Plot") # residual Q‐Q plot 
plot(model2,which=4, main="Log model: Cook's Distance") # Cook's distance
plot(multimodel,which=1, main="Linear model: Residuals vs Fitted Values") 

modeltest <- confint(model2,level=0.95)
modeltest





lm(formula = log(price) 
                 ~log(sqft_living)
                 +bedrooms
                 +bathrooms
                 +log(sqft_lot)
                 +floors
                 +waterfront
                 +condition
                 +age
                 ,data = housedata)


model2ftest <- lm(log(price)
             ~log(sqft_living)
             +bedrooms
             +bathrooms
             +log(sqft_lot)
             +floors
             +waterfront
             +condition
             +age
             ,data=housedata)
abline(model2ftest,lwd=1)
summary(model2ftest)





###Joint hypothesis test
model3ftest <- lm(log(price)
                  ~log(sqft_living)
                  +log(sqft_lot)
                  +floors
                  +waterfront
                  +condition
                  +age
                  ,data=housedata)
summary(model3ftest)

anova(model3ftest,model2ftest)


###Ramsay RESET test
lincoefs <- coef(multimodel)
linresids <- residuals(multimodel)
linfitted <- fitted(multimodel)

aux1.multimodel <- lm(price~
                        housedata$bedrooms+ 
                        housedata$bathrooms+ 
                        housedata$sqft_living+ 
                        housedata$sqft_lot+
                        housedata$floors+
                        housedata$waterfront+
                        housedata$condition+
                        housedata$yr_built+
                        I(linfitted^2)+I(linfitted^3),
                      data=housedata)
summary(aux1.multimodel)
anova(multimodel,aux1.multimodel)


####Breusch Pagan Test
aux2.multimodel <- lm(I(linfitted^2)~
                        housedata$bedrooms+ 
                        housedata$bathrooms+ 
                        housedata$sqft_living+ 
                        housedata$sqft_lot+
                        housedata$floors+
                        housedata$waterfront+
                        housedata$condition+
                        housedata$yr_built,
                      data=housedata)
summary(aux2.multimodel)




