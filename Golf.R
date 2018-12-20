setwd("~/Desktop")
options(digits = 4, show.signif.stars=TRUE)
b <- read.csv(file="~/Desktop/bank3.csv",header=TRUE, sep=",", na.strings="?", stringsAsFactors = TRUE)
str(b)
nrow(b)
summary(b)

tourney <- a$tourney
par3<- ifelse(b$par==3,1,0)
par4<- ifelse(b$par==4,1,0)
par5<- ifelse(b$par==5,1,0)
fairhit <- ifelse(b$fair=="-",0,1)
fairmiss <- ifelse(b$fair=="-",1,0)
girhit <- ifelse(b$gir=="-",0,1)
girmiss <- ifelse(b$gir=="-",1,0)
putts0 <- ifelse(b$putts=="-",1,0)
putts1 <- ifelse(b$putts==1,1,0)
putts2 <- ifelse(b$putts==2,1,0)
putts3 <- ifelse(b$putts==3,1,0)
strokes <- b$strokes
upanddownyes <- ifelse(b$upanddown=="-",0,1) 
upanddownno <-ifelse(b$upanddown=="-",1,0)

library("stargazer")
#### TABLE 1 SUMMARY STATS
stargazer(b[,-1], type="text", title="Table 1. Descriptive Statistics", digits=2, align = TRUE,
          out="table1_N.txt")

