install.packages("tidyquant")
library(tidyquant)

SQ <- as.data.frame(tq_get("SQ", get = "stock.prices", complete_cases = TRUE, from="2018-01-01"))
SQ <- SQ[,-2:-4]
price <- SQ[,4]
price
volume <- SQ[,3]
volume <- volume/100000
volume



SQ <- lm(log(price)~volume)
summary(SQ)

dev.new()
dev.new(width=22, height=4)
par(mfrow=c(1,2))
plot(volume,
     price,
     type="p",
     main="Price Distribution by Volume",
     xlab="Volume",
     ylab="Price")
count(price)






