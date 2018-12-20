install.packages("xts")
install.packages("ggplot2")
install.packages("moments")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")
install.packages("stargazer")
install.packages("margins")
install.packages("dplyr")
install.packages("urca")


setwd("~/Desktop")
options(digits = 4, show.signif.stars=TRUE)
a <- read.csv(file="~/Desktop/attrition.csv",header=TRUE, sep=",", na.strings="?", stringsAsFactors = TRUE)
str(a)
nrow(a)
summary(a)

###### Creating dummys
attrition.fac <- ifelse(a$attrition=="Yes",1,0)
female <- ifelse(a$gender=="Female",1,0)
married <- ifelse(a$mstatus=="Married",1,0)
educ2 <- ifelse(a$educ==2,1,0)
educ3 <- ifelse(a$educ==3,1,0)
educ4 <- ifelse(a$educ==4,1,0)
educ5 <- ifelse(a$educ==5,1,0)

freqtravel1 <- ifelse(a$travel=="frequently",1,0)
raretravel2 <- ifelse(a$travel=="rarely",1,0)
nonetravel3 <- ifelse(a$travel=="none",1,0)

age<- a$age
experience<- a$experience
yrsatcom<- a$yrsatcom
yrsatpos  <- a$yrsatpos
dailypay  <- a$dailypay
satisfaction <- a$satisfaction
distance <- a$distance

library("stargazer")
#### TABLE 1 SUMMARY STATS
stargazer(a[,], type="text", 
          title="Table 1. Descriptive Statistics", 
          digits=2, align = TRUE,
          out="table1_N.txt")

a1 <- data.frame(educ2, educ3, educ4, educ5, freqtravel1,raretravel2,attrition.fac,female,married,age,experience,yrsatcom,yrsatpos,dailypay,satisfaction,distance)
nrow(a1)
a2 <- na.omit(a1)
nrow(a2)
ncol(a2)

cor.mat <- cor(a2[, c(1:16)], use="complete.obs")
stargazer(cor.mat, type="text", 
          title="Table 2. Correlation Matrix", 
          digits=3, align = TRUE,out="table2_N.txt")




######################################### PROBIT ####################################
####### RUNNING THE PROBIT MODEL
probit <- glm(formula = attrition.fac ~ educ2+ educ3+ educ4+educ5+freqtravel1+raretravel2+female+married+ age+ experience+ yrsatcom+yrsatpos+dailypay+satisfaction+distance,
              data=a2,family=binomial(link="probit"))
summary(probit)


###### MODEL SIGNIFICANCE
null.vs.probit <- probit$null.deviance - probit$deviance 
null.vs.probit
pvalue.probit <- pchisq(null.vs.probit,df=15,lower.tail=FALSE)
pvalue.probit


##### PROP CORRECTLY PREDICTED
fitted.probit <- fitted(probit,type="response")
max(fitted.probit)
min(fitted.probit)
sum(ifelse(probit$fitted.values<.5,0,1) == attrition.fac) / length(attrition.fac)


##### Marginal Effects
library("margins")
library("dplyr")
summary(margins(probit,type="response"))
######################################### END PROBIT ####################################


######################################### LOGIT ####################################
####### RUNNING THE logit MODEL
logit <- glm(formula = attrition.fac~educ2+educ3+educ4+educ5+freqtravel1+raretravel2+female+married+age+experience+yrsatcom+yrsatpos+dailypay+satisfaction+distance,
              data=a2,family=binomial(link="logit"))
summary(logit)


###### MODEL SIGNIFICANCE
null.vs.logit <- logit$null.deviance - logit$deviance 
null.vs.logit
pvalue.logit <- pchisq(null.vs.logit,df=15,lower.tail=FALSE)
pvalue.logit


##### PROP CORRECTLY PREDICTED
fitted.logit <- fitted(logit,type="response")
max(fitted.logit)
min(fitted.logit)
sum(ifelse(logit$fitted.values<.5,0,1) == attrition.fac) / length(attrition.fac)


##### Marginal Effects
library("margins")
library("dplyr")
summary(margins(logit,type="response"))


###### COMBO
stargazer(probit,logit,type="text",title="Table 3. Regression Results", 
          digits=2,align=TRUE,out="table_P.txt")






































