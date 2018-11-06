########################### TIMES SERIES ################################

library(stargazer) library(lmtest) library(sandwich) library(xts) library(dplyr) library(urca)

## T-bill example
setwd("c:/examples/")
options(digits=4, show.signif.stars=TRUE)
t <- read.csv(file="c:/examples/tbill.csv", header=TRUE, sep=",") 
nrow(t)

##### The two regressors are the inflation rate and the federal deficit. 
##### The regressand is the 3-month treasury bill rate.
dev.new(width=8, height=4)
par(mfrow=c(1,3))
plot(t$year, t$tbill, type="l", main="3-month Treasury Bill Rate",
     xlab="year", ylab="percent")
plot(t$year, t$inf, type="l", main="Inflation Rate",
     xlab="year", ylab="percent")
plot(t$year, t$def, type="l", main="Federal Deficit as Percent of GDP",
     xlab="year", ylab="percent")


####### Scatter diagrams from ^ 
dev.new(width=8, height=4)
par(mfrow=c(1,2))
plot(t$inf, t$tbill, type="p", main="Treasury Bill Rate Against Inflation Rate",
     xlab="Inflation (%)", ylab="T Bill Rate (%)")
plot(t$def, t$tbill, type="p", main="Treasury Bill Rate Against Federal Deficit-to-GDP",
     xlab="Federal Deficit-to-GDP (%)", ylab="T Bill Rate (%)")

############# END OF THE 1ST PAGE ##################










######### BEGINNING OF THE SECOND PAGE #########
stargazer(t, type="text", title="Table 1. Descriptive Statistics", digits=3, out="table1_T.txt")

cor.mat <- cor(t, use="complete.obs")

stargazer(cor.mat, type="text", title="Table 2. Correlation Matrix", digits=3, align = TRUE,
          out="table2_T.txt")

## contemporaneous Model 
model.tbill <- lm(tbill~inf+def, data=t) 
summary(model.tbill)



## plot the residuals over time
dev.new(1)
plot(t$year, model.tbill$residuals, type="l", main="Static Model: Residuals Over Time",
     xlab="Year", ylab="Residuals")


## DURBIN WATSON: first-order serial correlation 
dwtest(model.tbill, alternative="two.sided")
## Breusch-Godfrey Serial Correlation: can handle higher order serial correlation 
bgtest(model.tbill, order = 1)

## correcting for serial correlation 
## differencing
dtbill <- t$tbill-lag(t$tbill, 1)
dinf <- t$inf-lag(t$inf, 1)
ddef <- t$inf-lag(t$def, 1)
model.dtbill <- lm(dtbill~dinf+ddef, data=t) summary(model.dtbill)


## Heteroskedasticity Test: Breusch-Pagan Test 
bptest(model.dtbill)

## Errors are heteroskedastic.
## So we use obtain the heteroskedastic-consistent SEs. 
vcv <-vcovHC(model.dtbill, type="HC") coeftest(model.dtbill,vcv)

## plot the residuals over time
dev.new(width=8, height=4)
par(mfrow=c(1,2))
plot(t$year, model.tbill$residuals, type="l", main="Static Model: Residuals Over Time",
     xlab="Year", ylab="Residuals")
plot(t$year[2:nrow(t)], model.dtbill$residuals, type="l", main="Static Model (First Differences): Residuals Over Time",
     xlab="Year", ylab="Residuals")

############# END OF THE 2ND PAGE ##################












######## THIS IS THE BEGINNING OF THE 3RD PAGE THAT STARTS WITH DICKEY FULLER.
######## WE ARE USING DICKEY FULLER TO SEE IF WE HAVE STATIONARY DATA.
######## WE THEN GO TO DURBIN WATSON AND DIFFERENCING

#Augmented Dickey Fuller tests
summary(ur.df(t$tbill, type = c("none"), lags = 1)) ## without drift 
summary(ur.df(t$tbill, type = c("drift"), lags = 1)) ## with drift 
summary(ur.df(t$tbill, type = c("trend"), lags = 1)) ## with trend

summary(ur.df(t$inf, type = c("none"), lags = 1)) 
summary(ur.df(t$inf, type = c("drift"), lags = 1)) 
summary(ur.df(t$inf, type = c("trend"), lags = 1))

summary(ur.df(t$def, type = c("none"), lags = 1)) 
summary(ur.df(t$def, type = c("drift"), lags = 1)) 
summary(ur.df(t$def, type = c("trend"), lags = 1))

## Distributed Lag Models Example
setwd("c:/examples/")
options(digits=4, show.signif.stars=TRUE)
t <- read.csv(file="c:/examples/tbill.csv", header=TRUE, sep=",")

## Finite Distributed Lag Models: tbill and inf
## FDL Model
model.tbillFDL <- lm(tbill~inf+lag(t$inf,1)+lag(t$inf,2), data=t) 
summary(model.tbillFDL)
## plot the residuals over time
dev.new(1)
plot(t$year[3:nrow(t)], model.tbillFDL$residuals, type="l", main="FDL Model: Residuals Over Time",
     xlab="Year", ylab="Residuals")


## Durbin Watson: first-order serial correlation dwtest(model.tbillFDL, alternative="two.sided")
## Breusch-Godfrey Serial Correlation: can handle higher order serial correlation bgtest(model.tbillFDL, order = 1)
## correcting for serial correlation ## differencing the FDL Model dtbill <- t$tbill-lag(t$tbill, 1)
dinf <- t$inf-lag(t$inf, 1)
d1inf <- lag(t$inf,1)-lag(t$inf, 2) 
d2inf <- lag(t$inf,2)-lag(t$inf, 3)
model.tbillFDLd <- lm(dtbill~dinf+d1inf+d2inf) 
summary(model.tbillFDLd)

## plot the residuals over time 
dev.new(width=8, height=4) 
par(mfrow=c(1,2))

############# END OF THE 3RD PAGE ##################







############# BEGINNING OF THE 4TH PAGE ##################
plot(t$year[3:nrow(t)], model.tbillFDL$residuals, type="l", main="FDL Model: Residuals Over Time", xlab="Year", ylab="Residuals")
plot(t$year[4:nrow(t)], model.tbillFDLd$residuals, type="l", main="Differenced FDL Model: Residuals Over Time", xlab="Year", ylab="Residuals")

## Heteroskedasticity Test: Breusch-Pagan Test 
bptest(model.tbillFDLd)

## Errors are heteroskedastic.
## So we use obtain the heteroskedastic-consistent SEs. 
vcv <-vcovHC(model.tbillFDLd, type="HC") 
coeftest(model.tbillFDLd,vcv)


## Times Series: trend
setwd("c:/examples/")
options(digits=4, show.signif.stars=TRUE)
g <- read.csv(file="c:/examples/lfp1.csv", header=TRUE, sep=",") nrow(g)


## recall: nrow is the number of observations in your time series ## creates trend = 1,2,...
## works if your data are regularly spaced
trend <- 1:nrow(g)
dev.new(width=8, height=4)
par(mfrow=c(1,2))
plot(trend, g$lfpmen, type="l", main="Labor Force Participation, Men, Seasonally Adjusted",
     xlab="trend", ylab="percent")
plot(trend, g$lfpwomen, type="l", main="Labor Force Participation, Women, Seasonally Adjusted",
     xlab="trend", ylab="percent")


# Augmented DF tests
# series show a clear trend, so use this 
summary(ur.df(g$lfpmen, type = c("trend"), lags = 1)) 
summary(ur.df(g$lfpwomen, type = c("trend"), lags = 1))
model.lfpm <- lm(lfpmen ~ trend, data=g) 
abline(model.lfpm, lwd=1) 
summary(model.lfpm)
model.lfpw <- lm(lfpwomen ~ trend + I(trend^2), data=g) 
summary(model.lfpw)

## adding a quadratic model to the scatter diagram
## create a sequence of trend values (xvalues) & calculate the predicted yvalues for these xvalues 
xvalues <- seq(1, 850, 1)
yvalues <- predict(model.lfpw, list(trend=xvalues))


############### END OF THE 4TH PAGE #####################







############### BEGINNING OF THE 5TH PAGE #####################
plot(trend, g$lfpwomen, type="l", 
     main="Labor Force Particpation, Women, Seasonally Adjusted", 
     xlab="trend", ylab="percent")
lines(xvalues, yvalues, lwd=1)

## Exponential trend series
setwd("c:/examples/")
options(digits=4, show.signif.stars=TRUE)
q <- read.csv(file="c:/examples/tradeq.csv", header=TRUE, sep=",") nrow(q)

trend <- 1:nrow(q)

dev.new(width=8, height=4)
par(mfrow=c(1,2))
plot(trend, q$rimpgs, type="l", main="Real Imports of Goods and Services, Quarterly, SA Annual Rate",
     xlab="trend", ylab="Billion 2009 Dollars")
plot(trend, q$rexpgs, type="l", main="Real Exports of Goods and Services, Quarterly, SA Annual Rate",
     xlab="trend", ylab="Billion 2009 Dollars")


# Augmented DF tests
# series show a clear trend, so use this 
summary(ur.df(log(q$rimpgs), type = c("trend"), lags = 1)) 
summary(ur.df(log(q$rexpgs), type = c("trend"), lags = 1))
dev.new(width=8, height=4)
par(mfrow=c(1,2))
plot(trend, log(q$rimpgs), type="l", main="Nat. Log of Real Imports, Quarterly, SA Annual Rate",
     xlab="trend", ylab="in nat. log")
model.rimp <- lm(log(rimpgs) ~ trend, data=q) 
abline(model.rimp, lwd=1) 
summary(model.rimp)
plot(trend, log(q$rexpgs), type="l", main="Nat. Log of Real Exports, Quarterly, SA Annual Rate", xlab="trend", ylab="in nat. log")
model.rexp <- lm(log(rexpgs) ~ trend, data=q) 
abline(model.rexp, lwd=1) 
summary(model.rexp)

## Breusch-Godfrey Serial Correlation: can handle higher order serial correlation 
bgtest(model.rimp, order = 1)
bgtest(model.rexp, order = 1)

## Heteroskedasticity Test: Breusch-Pagan Test 
bptest(model.rimp)
bptest(model.rexp)

############### END OF THE 5TH PAGE #####################





