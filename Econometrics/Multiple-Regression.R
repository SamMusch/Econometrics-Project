# This whole file covers multiple regression. 


# Here we are just installing the packages we will need
install.packages("xts")
install.packages("ggplot2")
install.packages("moments")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")
install.packages("stargazer")
install.packages("ggcorrplot")




# Now we are reading in our csv file into Rstudio
getwd()
setwd("C:/Users/Sam/Documents")
options(digits = 4, show.signif.stars=TRUE)
housedata <- read.csv(file="C:/Users/Sam/Documents/GitHub/Python-Projects/Regression/Vscode/houses.csv",header=TRUE, sep=",", na.strings="?")
str(housedata)
nrow(housedata)



# You are provided a dataset on homes sold in 2015. The variables are:
  # id:         home identifier
  # price:      price sold (2015USD) 
  # bedrooms:   number of bedrooms 
  # bathrooms:  number of bathrooms 
  # sqft_living: size of living area (square feet)
  # sqft_lot:   size of lot (square feet) 
  # floors:     number of floors 
  # waterfront: 1 = waterfront, 0 = not 
  # condition:  condition of the house (1 = poor, 5 = excellent)
  # yr_built:   year the home was built 



# Now we are establishing our variables as being the columns from our csv file 
id <- housedata$id # 
price <- housedata$price
bedrooms <- housedata$bedrooms
bathrooms <- housedata$bathrooms
sqft_living <- housedata$sqft_living
sqft_lot <- housedata$sqft_lot
floors <- housedata$floors
waterfront <- housedata$waterfront
condition <- housedata$condition
yr_built <- housedata$yr_built

# Lets take a look at the age of our homes
age = 2015 - yr_built
summary(age)
sd(age)
var(age)

# Lets also take a quick look at our waterfront homes. This variable is binary, so it looks a bit different than the others.
summary(waterfront)
# The mean of .007 represents that very few people have waterfront homes (about 7 people out of every 1000)




# Lets get a feel for the price of the homes we are looking at
# (1) Creating variance and stdev
pricevariance <- var(price)
pricestandarddeviation <- sqrt(pricevariance)
# (2) Viewing the results
summary(price)
pricevariance
pricestandarddeviation
# So, our median home price is 450k, the mean price is 542k, and the stdev is 363k.



# Lets do the same thing for the size of the home
# (1) Creating variance and stdev
sqft_living_variance <- var(sqft_living)
sqft_living_standarddeviation <- sqrt(sqft_living_variance)
# (2) Viewing the results
summary(housedata$sqft_living)
sqft_living_variance
sqft_living_standarddeviation
# So, our median home size is 1873 ft, the mean price is 2041 ft, and the stdev is 893 ft.






# Lets see if there is a relationship between the size of the home and the price of the home.
cor.test(sqft_living, price, use="complete.obs")
# Our p-value is <2e-16 (extremely small), so it looks like there is a relationship.
# The 95% confidence intervall is between .6676 to .6928, demonstrating a strong positive correlation.



# Scatter plot with price and sqft_living
dev.new(1)
par(mfrow=c(1,2)) # 1 row, 2 columns
plot(sqft_living, # This is our x axis
     price, # This is our y axis
     type="p", # This means we will see "points" not lines
     main="Sq Ft vs Price of House", xlab="Square Feet", ylab="Price")



# Simple regression analysis - price and sqft_living
model1 <- lm(price~sqft_living, data=housedata)
abline(model1,lwd=1) # This plots points of sqft_living vs price, but with a line of best fit
summary(model1) 
# So our equation for predicting price is...
#   price = -22325 + 276 * sqft_living (plus or minus 3.5 for the sqft_living coefficient)
# The Rsquared shows how much of the price variance is due to the sqft variance
# Multiple R squared = .463
# Adjusted R squared = .463   # this takes into account extra regressors and their impact on correlation
# The 7944 st error from the "intercept" section represents the absolute difference of each house from the regression line



# Coefficients, residuals, fitted values
betahats <- coef(model1) # this shows us the expected y intercept and slope
betahats
residuals <- residuals(model1) # this shows us how "off" our estimate is from each point 
fittedvalues <- fitted(model1) 



# What is the true slope? 95%
cislope <- confint(model1)
cislope
# Our x variable was sqft_living. There is a 95% chance our true slope falls between the lower and upper bound (269.5 to 283.5)



# What is the true slope? 99%
cislope99 <- confint(model1, level=0.99)
cislope99



# Lets try to predict the price of a home with median sqft (1873)
pricefit <- betahats[1] + (betahats[2]*1873)  # betahats[1] is our y intercept. betahats[2] is our sqft coef
pricefit
# We would expect a house with median sqft to cost 496k. (Median home price is 450k for reference)



# Lets try to predict the price of a home with 3200 sqft
secondassignmentprice <- betahats[1] + (betahats[2]*3200)
secondassignmentprice
# We would expect this house to cost 862k.



# What is the confidence interval for the house of 3200?
xvals <- data.frame(sqft_living=c(390,8000))
cimeany <- predict(model1, data.frame(sqft_living=3800), interval="confidence", level=0.95)
cimeany
# lower = 1.01M 
# fit = 1.03M
# upper = 1.05M
# The prediction interval says that, given that we already know the sqft of the home is 3200, 
#   we can say with 95% certainty that the price will fall between these intervals. 



# What is the prediction interval for the house of 3200?
pi <- predict(model1, data.frame(sqft_living=3800), interval="prediction", level=0.95)
pi
# lower = 506k 
# fit = 1.03M
# upper = 1.55M
# The confidence interval tells us what we would expect the AVERAGE price to be 
#   of a home given that they have an x axis value of 3800.
# This will always be more spread out than the confidence interval.



# Lets take a look at our residuals.
dev.new(width=8,height=4)
par(mfrow=c(1,2))
plot(sqft_living,
     residuals,
     type="p",
     main="Residuals against Sq ft Living Area",
     xlab="Sq ft",
     ylab="residuals")
plot(model1,which=1)


























# We want to see how related our variables are to one another. 
# We have to watch out for multicollinearity. 
# If we have multicollinearity, we will be more likely to conclude that there is no relationship when there actually is.
# (1) We'll first get them into a dataframe
pairwise <- data.frame(bedrooms, bathrooms, sqft_living, sqft_lot,
                       floors,waterfront,condition, yr_built)



# (2) Next lets see the relationships visually:
library(ggplot2)
library(ggcorrplot)
data(pairwise)
corr <- round(cor(pairwise), 1)
head(corr[, 1:8])
ggcorrplot(corr, hc.order = TRUE, type = "lower",method="circle",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))



# (3) Now lets see the actual numbers to check what we have just seen
cor(pairwise, use="complete.obs")







# Lets build a multiple regression model with our variables.
multimodel <- lm(price~bedrooms+ bathrooms+sqft_living+ sqft_lot+floors+waterfront+condition+age,data=housedata)
summary(multimodel)
# The smaller that values on the far right (Under the PR(>|t|) section below), the greater relationship between itself and the house price.



# Residual analysis: Scatterplot, qq plot,cooks plot of residuals
# Explanations: https://docs.google.com/document/d/18B3oDKBd5geNst8goxwHTYtzQIHrp9gakv8WBFv9Llg/edit?usp=sharing
dev.new(width=8, height=4)
par(mfrow=c(1,3))
plot(multimodel,which=1, main="Linear model: Residuals vs Fitted Values") 
plot(multimodel,which=2, main="Linear model: QQ Plot") # residual QQ plot 
plot(multimodel,which=4, main="Linear model: Cook's Distance") # Cook's distance










######## THIRD MODEL ---- log log ############

# This is a new model with improved functional form.
model2 <- lm(log(price) ~ log(sqft_living) + log(sqft_lot)
             + bedrooms + bathrooms + floors + waterfront + condition + age, 
             data = housedata)
model2



# Saving coefficients, residuals, fitted values
coef <- coef(model2)
res <- residuals(model2)
fitted <- fitted(model2)



# Lets test for multicollinearity 
# (1) first way is with variance inflation factors
library(car)
vif(model2)
# If the value is >4, we need a closer look
# If the value is >10, we have a serious problem


# Still multicollinearity
# (2) Second way is visually with pairwise table
multico <- data.frame(log(price), log(sqft_living), log(housedata$sqft_lot),
                      bedrooms, bathrooms, floors, waterfront, condition, age)


library(ggplot2)
library(ggcorrplot)
data(multico)
corr <- round(cor(multico), 1)
head(corr[, 1:8])
ggcorrplot(corr, hc.order = TRUE, type = "lower",method="circle",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))



# Still multicollinearity
# (3) third way is using the pure numbers to check
cor(multico, use="complete.obs")



##linear and log residual plots
dev.new(width=8, height=4)
par(mfrow=c(1,3))
plot(model2,which=1, main="Log model: Residuals vs Fitted Values") 
plot(model2,which=2, main="Log model: QQ Plot") # residual QQ plot 
plot(model2,which=4, main="Log model: Cook's Distance") # Cook's distance
plot(multimodel,which=1, main="Linear model: Residuals vs Fitted Values") 



modeltest <- confint(model2,level=0.95)
modeltest



lm(formula = log(price) ~ log(sqft_living) + log(sqft_lot)
                 + bedrooms + bathrooms + floors + waterfront + condition + age, data = housedata)



model2ftest <- lm(log(price) ~ log(sqft_living) + log(sqft_lot)
                  + bedrooms + bathrooms + floors + waterfront + condition + age, data = housedata)
abline(model2ftest,lwd=1)
summary(model2ftest)
# Multiple Rsquared = 51.3% of variance in the log(price) is explained by the regressors.



# level level = unit change relative to unit change
# level log = unit change relative to % change
# log level = % change relative to unit change
# log log = % change relative to % change



# Model omitting bedrooms and bathromms
model3ftest <- lm(log(price) ~ log(sqft_living) + log(sqft_lot)
                  + floors + waterfront + condition + age, data=housedata)
summary(model3ftest)



# Joint hypothesis test --> am i able to drop the variables that were highly related?
anova(model3ftest,model2ftest)
# The p-value is tiny, so we cannot omit the variables



# Ramsay RESET test --> am i using the correct model specification?
lincoefs <- coef(multimodel)
linresids <- residuals(multimodel)
linfitted <- fitted(multimodel)

aux1.multimodel <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot +
                        floors + waterfront + condition + yr_built +
                        I(linfitted^2)+I(linfitted^3),data=housedata)
summary(aux1.multimodel)
anova(multimodel,aux1.multimodel)
# Our p-value is tiny, which shows that linear - linear is not the correct model



# Breusch Pagan Test --> are my errors homoskedastic? (are the residuals correlated with any of our regressors?)
# If no correlation --> homoskedastic and Shapiro
# If correlation --> hetero and Wilcoxon
aux2.multimodel <- lm(I(linresids^2)~
                        bedrooms + bathrooms + sqft_living + 
                        sqft_lot + floors + waterfront + condition +
                        yr_built, data=housedata)
summary(aux2.multimodel)
# Our p-value is tiny, so we can use homoskedastic test




