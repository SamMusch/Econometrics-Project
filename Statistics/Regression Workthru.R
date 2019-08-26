# Overview ----------------------------------------------------------------
options(digits = 3, show.signif.stars=TRUE)
housedata <- read.csv(file="C:/Users/Sam/Documents/GitHub/Python-Projects/Regression/Vscode/houses.csv",header=TRUE, sep=",", na.strings="?")

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



# Colinearity and VIF ------------------------------------
data <- c()

row_sums = rowSums(data)
good_mask = row_sums > 0
good_data <- data[good_mask, 0]

cormat = round(cor(good_data), 2)
melted_cormat = melt(cormat)
melted_cormat

ggplot(data = melted_cormat, aes(x = var1, y = var2, fill = value)) + 
  geom_tile(color = 'white') + scale_fill_grdient2(low = 'blue', high = 'red', mid = 'white', midpoint = 0, limit = c(-1,1))+
  coord_fixed()



library(car)
vif(model2)
# If the value is >4, we need a closer look
# If the value is >10, we have a serious problem


# Plotting ----------------------------------------------------------------

# Plots vs dependent
plot(iv, dv, pch = 16, xlab = "iv", ylab = "dv") 
abline(lm(dv ~ iv), lty=2, col='red')

# Multi Model -------------------------------------------------------------------

multimodel <- lm(price ~ bedrooms+ bathrooms+sqft_living+ sqft_lot+floors+waterfront+condition+age,data=housedata)
summary(multimodel)


# What is the true slope? 95%
cislope <- confint(model1, level=0.95)


# What is the confidence interval for the house of 3200?
cimeany <- predict(model1, data.frame(sqft_living=3800), interval="confidence", level=0.95)


# What is the prediction interval for the house of 3200?
pi <- predict(model1, data.frame(sqft_living=3800), interval="prediction", level=0.95)

# level level = unit change relative to unit change
# level log = unit change relative to % change
# log level = % change relative to unit change
# log log = % change relative to % change



# Residuals ---------------------------------------------------------------
# Explanations: https://docs.google.com/document/d/18B3oDKBd5geNst8goxwHTYtzQIHrp9gakv8WBFv9Llg/edit?usp=sharing
dev.new(width=8, height=4)
par(mfrow=c(1,3))
plot(multimodel,which=1, main="Linear model: Residuals vs Fitted Values") 
plot(multimodel,which=2, main="Linear model: QQ Plot") # residual QQ plot 
plot(multimodel,which=4, main="Linear model: Cook's Distance") # Cook's distance




# Ramsay RESET test --> am i using the correct model specification?
lincoefs <- coef(multimodel)
linresids <- residuals(multimodel)
linfitted <- fitted(multimodel)

new <- lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + condition + yr_built + I(linfitted^2)+I(linfitted^3),data=housedata)
anova(multimodel, new)
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