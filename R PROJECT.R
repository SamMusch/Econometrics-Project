install.packages("xts")
install.packages("ggplot2")
install.packages("moments")
install.packages("sandwich")
install.packages("lmtest")
install.packages("car")
install.packages("stargazer")
install.packages("urca")
install.packages("effects")
install.packages("multcomp")

library("stargazer") 
library("lmtest") 
library("sandwich") 
library("xts") 
library("dplyr") 
library("urca")
library("ggplot2")

setwd("~/Desktop/Project/Help")
options(digits = 4, show.signif.stars=TRUE)
a <- read.csv(file="~/Desktop/Metrics Project/Help/data.csv",header=TRUE, sep=",", na.strings="?", stringsAsFactors = TRUE)
str(a)
nrow(a)


##########  COMBINING CPI AND FY
b <- data.frame(FY=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014),
      CPI = c(177.1,179.9,184,188.9,195.3,201.6,207.342,215.303,214.537,218.056,224.939,229.594,232.957,236.736,237.017),
      TFP = c(2.26,2.32,2.29,2.39,2.53,2.47,2.49,2.41,2.5,2.59,2.56,2.56,2.55,2.7,2.54),
      inputs = c(1.05,1.03,1.02,1,.99,1.01,1,1.06,1.03,1,1,1,.99,1.01,1.06))

c <- merge(a,b,by="FY")
nrow(c)


#There is a strong correlation between TFP and inputs
cor.test(c$inputs, c$TFP, use="complete.obs")





c1 <- subset(c, select=c(FY,WAGET1,CPI, GENDER,AGE, B01, FOREIGNB, currstat, B07,A09,B11, D27, CROP, REGION6,BLWAGE,TASK,TFP,inputs))
summary(c1)
nrow(c1)
ncol(c1)


#################################

########## ALL OF THE VARIABLES I AM USING #############
TFP <- c1$TFP

region <- c1$REGION6
english <- c1$B07

CPI <- c1$CPI
FY <- c1$FY

TREND <-c1$FY-2000
gdpgrow <- c1$gdpgrow
AGRI <- c1$AGRI
wages <- 100*c1$WAGET1/c1$CPI
nom.avg.wage <- c1$WAGET1
belowmin <- ifelse(c1$BLWAGE==1,1,0)

AGE <- c1$AGE
AGESQ <- AGE*AGE

FARMEXP <-c1$B11
FARMEXPSQ <- FARMEXP*FARMEXP
TENURE <- c1$D27


UNDOCUMENTED <-ifelse(c1$currstat==4,1,0)
HISPANIC <- ifelse(c1$B01==7,0,1)
USBORN <- ifelse(c1$FOREIGNB==0,1,0)

NOENGL<-ifelse(c1$B07==1,1,0)
BADENGL<-ifelse(c1$B07==2,1,0)
SOMEENGL<-ifelse(c1$B07==3,1,0)
ENGL<-ifelse(c1$B07==4,1,0)

EDUC <- c1$A09
EDUC[EDUC==95|EDUC==96] <- 0


FRUITNUT <- ifelse(c1$CROP==2,1,0)
HORT <-ifelse(c1$CROP==3|c1$CROP==2|c1$CROP==1,1,0)
VEGE <-ifelse(c1$CROP==4,1,0)
FIELD <- ifelse(c1$CROP==1,1,0)
misc <- ifelse(c1$CROP==1,1,0)
CROP <- c1$CROP

TASK <- factor(c1$TASK)
COMPLEXTASK <- ifelse(TASK==4|TASK==5,1,0)
unskilled <- ifelse(TASK==1|TASK==2|TASK==3|TASK==4,1,0)
supervisor <- ifelse(TASK==5,1,0) 


SE <-ifelse(c1$REGION6==2,1,0)
MIDW <-ifelse(c1$REGION6==3,1,0)
SW <-ifelse(c1$REGION6==4,1,0)
NW <-ifelse(c1$REGION6==5,1,0)

FEMALE <- ifelse(c1$GENDER==1,1,0)


ENGL1 <- ifelse(c1$B07==1,1,0)
ENGL4 <- ifelse(c1$B07==4,1,0)

######################################
a1 <- data.frame(FY,wages,TREND, FEMALE,AGE,AGESQ,TASK,
                 HISPANIC, USBORN, UNDOCUMENTED,EDUC, FARMEXP, FARMEXPSQ,
                 BADENGL,SOMEENGL, ENGL,TENURE,belowmin,
                 FRUITNUT, HORT, VEGE, FIELD,SE,MIDW, SW,NW,COMPLEXTASK,NOENGL,region,english,TFP,ENGL1,ENGL4,unskilled,supervisor,misc,CROP)
nrow(a1)
agwage <- na.omit(a1)
nrow(agwage)
ncol(agwage)














mean.wage <- tapply(agwage$wages, agwage$FY, mean)
mean.wage
change <- mean.wage/lag(mean.wage,1) 
frame.change <- data.frame(change)
wage.use <- frame.change[-c(1),]
wage.use

TFP.change <- b$TFP/lag(b$TFP,1) 
TFP.frame.change <- data.frame(TFP.change)
TFP.use <- TFP.frame.change[-c(1:2),]
TFP.use

compare <- rbind(wage.use,TFP.use)


#Only the tests
t.test(agwage$CROP, alternative="two.sided",u=0, paired=F,conf.level =.95)
t.test(task.avg, alternative="two.sided",u=0, paired=F,conf.level =.95)
t.test(eng.avg, alternative="two.sided",u=0, paired=F,conf.level =.95)
t.test(agwage$region, alternative="two.sided",u=0, paired=F,conf.level =.95)

  


####Wages by crop
misc.avg <- tapply(agwage$wages, agwage$misc, FUN = mean)
misc.avg
vege.avg <- tapply(agwage$wages, agwage$VEGE, FUN = mean)
vege.avg
t.test(misc.avg,vege.avg, alternative="two.sided",u=0, paired=F,conf.level =.95)
crop.avg <- tapply(agwage$wages, agwage$CROP, FUN = mean)


####Wages by region
wagesByRegion <- tapply(agwage$wages, agwage$region, FUN = mean)
t.test(agwage$region, alternative="two.sided",u=0, paired=F,conf.level =.95)


####Wages by task
super.avg <- tapply(agwage$wages, agwage$supervisor, FUN = mean)
super.avg
unskilled.avg <- tapply(agwage$wages, agwage$unskilled, FUN = mean)
unskilled.avg
t.test(super.avg,unskilled.avg, alternative="two.sided",u=0, paired=F,conf.level =.95)
task.avg <- tapply(agwage$wages, agwage$TASK, FUN = mean)


####Wages by engl
wagesByEngl <- tapply(agwage$wages, agwage$ENGL, FUN = mean)
wagesByEngl1 <- tapply(agwage$wages, agwage$ENGL1, FUN = mean)
wagesByEngl1
wagesByEngl4 <- tapply(agwage$wages, agwage$ENGL4, FUN = mean)
wagesByEngl4
t.test(wagesByEngl1,wagesByEngl4, alternative="two.sided",u=0, paired=F,conf.level =.95)
task.avg <- tapply(agwage$wages, agwage$TASK, FUN = mean)



#### Correlation testing
cor.test(agwage$ENGL, agwage$USBORN, use="complete.obs")
cor.test(agwage$TFP, agwage$wages, use="complete.obs")

cor.test(agwage$COMPLEXTASK, agwage$wages, use="complete.obs")
cor.test(agwage$BASICTASK, agwage$wages, use="complete.obs")

cor.test(agwage$AGRI, agwage$gdpgrow, use="complete.obs")
cor.test(agwage$USBORN, agwage$wages, use="complete.obs")












######### Breusch Godfrey
bgtest(modelloglin, order=1)
########################################################
library("stargazer")
#### TABLE 1 SUMMARY STATS
###stargazer(agwage[,-1], type="text", title="Table 1. Descriptive Statistics", 
##digits=2, align = TRUE,out="table1_N.txt")


#### TABLE 2 CORRELATION
###cor.mat <- cor(agwage[, c(2,3,4,7,8,12,13,14,18)], use="complete.obs")
###stargazer(cor.mat, type="text", title="Table 2. Correlation Matrix", 
          ##digits=3, align = TRUE,out="table2_N.txt")

multi <- data.frame(TREND,FY,wages,FEMALE,AGE,AGESQ,HISPANIC, USBORN, 
  UNDOCUMENTED,BADENGL,SOMEENGL,ENGL,EDUC, 
  FARMEXP, FARMEXPSQ, TENURE,FRUITNUT, HORT, VEGE, FIELD,
  SE,MIDW, SW,NW,COMPLEXTASK)


############# TABLE 3 REGRESSION RESULTS #############
#### multi regression model with log - lin
modelloglin <- lm(log(wages)~ 
 TREND+FEMALE+AGESQ+ HISPANIC+ USBORN+ UNDOCUMENTED+ 
 BADENGL+SOMEENGL+ENGL+EDUC+FARMEXPSQ+ TENURE+ 
 FRUITNUT+HORT+ VEGE+ FIELD+ 
 SE +MIDW+ SW+NW+COMPLEXTASK,data = agwage)
summary(modelloglin)

coef <- coef(modelloglin)
res <- residuals(modelloglin)
fitted <- fitted(modelloglin)

##stargazer(modelloglin, type="text",title="Table 3. Regression Results", 
  ##        digits=2, align = TRUE,
    ##      dep.var.lables=c("ln(wages)"),
      ##    covariate.lables=c(AGRI,gdpgrow,TREND,FEMALE,AGESQ, HISPANIC, USBORN, UNDOCUMENTED, BADENGL, SOMEENGL, ENGL,EDUC, FARMEXPSQ, TENURE, FRUITNUT, 
        ##                       HORT, VEGE, FIELD, SE, MIDW, SW,NW),out="table4_N.txt")



############# TABLE 4 VIF #############
library(car)
vif(modelloglin)


############# TABLE 5 RAMSET #############
###Ramset RESET test
###Am I using the correct functional form?
aux1.multimodel <- lm(log(wages)
    ~AGRI+gdpgrow+TREND+
 FEMALE+AGESQ+ HISPANIC+ USBORN+ UNDOCUMENTED+ 
 BADENGL+SOMEENGL+ENGL+EDUC+FARMEXPSQ+ TENURE+ 
 FRUITNUT+HORT+ VEGE+ FIELD+SE +MIDW+ SW+NW+
 I(fitted^2)+I(fitted^3),data=agwage)
summary(aux1.multimodel)



############# TABLE 6 BREUSCH #############
####Breusch Pagan Test
aux2.multimodel <- lm(I(res^2)~AGRI+gdpgrow+TREND+
  FEMALE+AGESQ+ HISPANIC+ USBORN+ UNDOCUMENTED+ 
  BADENGL+SOMEENGL+ENGL+EDUC+
  FARMEXPSQ+ TENURE+ 
  FRUITNUT+HORT+ VEGE+ FIELD+
  SE +MIDW+ SW+NW,data=agwage)
summary(aux2.multimodel)
bp.longhand <- nrow(agwage)*summary(aux2.multimodel)$r.squared 
bp.longhand

bptest(modelloglin)

######HETERO STANDARD ERRORS
vcv <- vcovHC(modelloglin, type="HC1") 
coeftest(modelloglin, vcv)


# mean by UNDOC by FY
meanbY <- tapply(agwage$wages, list(agwage$FY, agwage$COMPLEXTASK), mean)  

# see the structure of the matrix you have created, save as data frame.
t3 <- data.frame(meanbytask) 
t3





##### Dr Co Email, mean wage by FY
## define FY

FY = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)
lagFY= c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

mean.wage <- tapply(agwage$wages, agwage$FY, mean)
mean.wage

meanbydoc <- tapply(agwage$wages, list(agwage$FY, agwage$UNDOCUMENTED), mean)  # mean by UNDOC by FY
t2 <- data.frame(meanbydoc)  # see the structure of the matrix you have created, save as data frame.
t2

meandocumented <- meanbydoc[,-2]
meandocumented
meanundocumented <- meanbydoc[,-1]
meanundocumented

meanbytask <- tapply(agwage$wages, list(agwage$FY, agwage$COMPLEXTASK), mean)  # mean by UNDOC by FY
t3 <- data.frame(meanbytask)  # see the structure of the matrix you have created, save as data frame.
t3

basictasks <- meanbytask[,-2]
basictasks
complextasks <- meanbytask[,-1]
complextasks









###### 
counts <- c(3.746,4.2,4.639)
barplot(counts, main="Undocumented & No English vs Documented & Speak English Well", 
        xlab="",ylim=c(0,5),ylab="$USD (1984)")
legend(-.25, 5.5, legend=c("Undoc and No Engl ($3.746)", "Average ($4.20)","Doc and Speak English Well ($4.639)"),
       lty=c(2014,4.6),bty="n",
       title="Line types", text.font=6, bg='white')


plot(lagFY,meanENGL,type="l",col="blue",ylim=c(3.5, 5.5),
     main="Wages of English Speakers vs No English", 
     ylab="Wages (in 1984 USD)",xlab="FY")
legend(2000, 5.8, legend=c("English Speakers", "Average Ag Worker","No English"),
       col=c("blue", "black","orange"), lty=c(2002,4.6),bty="n",
       title="", text.font=4, bg='white')
lines(lagFY,mean.wage,col="black")
lines(lagFY,meanNOENGL,col="orange")


plot(FY,documented,type="l",col="red",ylim=c(3.5, 4.75),
     main="Wages of Undocumented and Documented Workers", 
     ylab="Wages (in 1984 USD)")
legend(2000, 4.92, legend=c("Documented Workers", "Average","Undocumented Workers"),
       col=c("red", "black","green"), lty=c(2014,4.6),bty="n",
       title="Line types", text.font=4, bg='white')
lines(FY,mean.wage,col="black")
lines(FY,undocumented,col="green")


plot(FY,complextasks,type="l",col="blue",ylim=c(3.5, 4.75),
     main="Wages of Complex Tasks (Supervisor, Irrigation, Machine Operator) vs Basic Tasks (Pre-harvest, Harvest, Post-harvest)", 
     ylab="Wages (in 1984 USD)")
legend(2000, 4.92, legend=c("Complex Tasks", "Average Ag Worker","Basic Tasks"),
       col=c("blue", "black","orange"), lty=c(2014,4.6),bty="n",
       title="Line types", text.font=4, bg='white')
lines(FY,mean.wage,col="black")
lines(FY,basictasks,col="orange")



plot(lagFY,mean.gdp,type="l",col="green",ylim=c(-2.5,4),xlim=c(2002,2014),
     main="GDP Growth, Agriculture Percent Share Added to GDP, Agriculture Wage Growth",xlab="FY", 
     ylab="Percent")
lines(lagFY,mean.agri,col="black")
lines(lagFY,change,col="red")
legend(2009, 0, legend=c("GDP Growth", "Agriculture % Added to GDP","Agriculture Wage Growth"),
       col=c("green", "black","red"), lty=c(2014,4.6),bty="n",
       title="Line types", text.font=4, bg='white')




#### All continuous variables
dev.new(width=24, height=4)
par(mfrow=c(1,2))
plot(agwage$AGE,
     agwage$wages,
     type="p",
     main="Wages Earned by Age",
     xlab="Age",
     ylab="Wages")
plot(agwage$AGESQ,
     agwage$wages,
     type="p",
     main="Wages Earned by Age Squared",
     xlab="Age Squared",
     ylab="Wages")

dev.new(width=24, height=4)
par(mfrow=c(1,2))
plot(agwage$EDUC,
     agwage$wages,
     type="p",
     main="Wages by Education (1=1st grade complete)",
     xlab="Years of Education Completed",
     ylab="Wages")
plot(agwage$TENURE,
     agwage$wages,
     type="p",
     main="Wages earned by Tenure (Years)",
     xlab="Tenure",
     ylab="Wages")


dev.new(width=24, height=4)
par(mfrow=c(1,2))
plot(agwage$FARMEXP,
     agwage$wages,
     type="p",
     main="Wages earned by farm experience (years)",
     xlab="Age",
     ylab="Wages")
plot(agwage$FARMEXPSQ,
     agwage$wages,
     type="p",
     main="Wages Earned by farm experience squared",
     xlab="Age Squared",
     ylab="Wages")












#################### Residuals 
dev.new(width=24, height=4)
par(mfrow=c(1,1))
plot(modelloglin,which=1, main="Natural Log Model: Residuals vs Fitted Values") 

dev.new(width=24, height=4)
par(mfrow=c(1,1))
plot(modelloglin,which=2, main="Natural Log Model: Q‐Q Plot")

dev.new(width=24, height=4)
par(mfrow=c(1,1))
plot(modelloglin,which=4, main="Natural Log Model: Cook's Distance")














betahats <- coef(modelloglin)
res <- residuals(modelloglin)
fitted <- fitted(modelloglin)

## calculate predicted mpg for a US‐branded car with mean characteristics.
## note y and some x's are in logs. origin.fac2=0 and origin.fac3=0 for US‐branded cars. ## after getting yhat (in log), take its inverse which is exp.

betahats[1]
betahats[2]
mean(agwage$AGRI)
betahats[2]*mean(agwage$AGRI)


fit2b <- betahats[1]
+betahats[2]*mean(agwage$AGRI)
+betahats[3]*mean(agwage$gdpgrow)
+betahats[4]*mean(agwage$TREND)
+betahats[5]*mean(agwage$FEMALE)+
  betahats[6]*mean(agwage$AGESQ)+ 
  betahats[7]*mean(agwage$HISPANIC)+
  betahats[8]*mean(agwage$USBORN)+ 
  betahats[9]*mean(agwage$UNDOCUMENTED)*1+
  betahats[10]*mean(agwage$BADENGL)+ 
  betahats[11]*mean(agwage$SOMEENGL)+
  betahats[12]*mean(agwage$ENGL)*0+ 
  betahats[13]*mean(agwage$EDUC)+
  betahats[14]*mean(agwage$FARMEXPSQ)+ 
  betahats[15]*mean(agwage$TENURE)+
  betahats[16]*mean(agwage$FRUITNUT)+ 
  betahats[17]*mean(agwage$HORT)+
  betahats[18]*mean(agwage$VEGE)+ 
  betahats[19]*mean(agwage$FIELD)+
  betahats[20]*mean(agwage$SE)+ 
  betahats[21]*mean(agwage$MIDW)+
  betahats[22]*mean(agwage$SW)+
  betahats[23]*mean(agwage$NW)+ 
  betahats[24]*mean(agwage$COMPLEXTASK)
fit2b
exp(mpgfit2b)




workerdata <- data.frame(FY=c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014),
                meanBADENGL = c(4.107, 3.968, 4.070, 3.958, 3.967, 4.086, 4.102, 4.332, 4.280, 4.223, 4.110, 4.017, 4.113, 4.381),
                meanENGL = c(4.502, 4.648, 4.487, 4.489, 4.349, 4.757, 4.619, 4.910, 4.868, 4.552, 4.595, 4.601, 4.702, 4.717),
                meandocumented = c(4.345, 4.397, 4.367, 4.330, 4.209, 4.512, 4.498, 4.703, 4.714, 4.447, 4.497, 4.494, 4.539, 4.580),
                meanundocumented = c(3.834, 3.731, 3.756, 3.650, 3.753, 3.717, 3.773, 3.941, 3.987, 4.049, 3.909, 3.845, 4.051, 4.215))
c <- merge(agwage,workerdata,by="FY")
nrow(c)

lowpower <- lm(mean.wage~meanBADENGL+meanundocumented, data=c)

vcv <-vcovHC(modelloglin, type="HC1")
coeftest(modelloglin,vcv)





############## prediction stuff
mean.wage <- tapply(agwage$wages, agwage$FY, mean)
mean.wage


meanbyENGL <- tapply(agwage$wages, agwage$ENGL, mean)
meanbyENGL

meanbyNOENGL <- tapply(agwage$wages, agwage$NOENGL, mean)
meanbyNOENGL

meanbyUNDOC <- tapply(agwage$wages, agwage$UNDOCUMENTED, mean)
meanbyUNDOC


low <- tapply(agwage$wages, list(agwage$NOENGL, agwage$UNDOCUMENTED), mean)
low
low1 <- low[-1,-1]
low1


high <- tapply(agwage$wages, list(agwage$ENGL, agwage$UNDOCUMENTED), mean)
high
high1 <- high[-1,-2]
high1


meanwage <- mean(mean.wage)
meanwage

model <- lm(meanwage~high1)
summary(model)





workerdata2 <- data.frame(meanBADENGL = c(),
                         meanENGL = c(),
                         meandocumented = c(),
                         meanundocumented = c())
workerdata2

model <- lm()

cimeany <- predict(model, data.frame(c), interval="confidence", level=0.95) 
cimeany



