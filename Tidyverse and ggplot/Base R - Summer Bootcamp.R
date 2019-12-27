# Bootcamp 1----------------------------------------------------------------
library(tidyverse)
s <- read_csv('scores.csv')

# How many student did the optional paper?
sum(is.na(s$reflection))  # did not do the paper
sum(!is.na(s$reflection))  # did the paper

# Add a column to represent course total
s$total <- rowSums(s[2:27], na.rm = TRUE)

# 5. Do we comply with the grading policy?
median(s$total)


# Bootcamp 2 --------------------------------------------------------------

# Basics
p <- read.csv('pizza.csv')
nrow(p)        # Number of rows in data set?
names(p[1])    # Name of first column in data set
tail(p, n = 3) # Last 3 rows in the data set
p$Rating       # Vector of data from Rating column

# Rating and CostPerSlice of the first 10 rows
p[1:10, c('Rating', 'CostPerSlice')]

# Average rating of shops with a brick oven
p[p$BrickOven]               # viewing only with brick ovens
p[p$BrickOven,]$Rating       # ratings of these observations
mean(p[p$BrickOven,]$Rating) # average rating

# Average rating of shops where CostPerSlice 2 dollars or less
mean(p$Rating[p$CostPerSlice <= 2])

# Load and describe -------------------------------------------------------

library(tidyverse)
### CSV files
# Step 1) Session -> Set Working Directory -> Choose Working Directory
# Step 2) s <- read.csv('scores.csv', header = T)    # or header = F

### Calculating frequencies
a                       # runs each response and number of times
round(prop.table(a),2)  # percentage of total

### Descriptive stats
library("psych")
describe(mpg)


# Quick graphs ------------------------------------------------------------

### Bar chart with categorical variable
# Step 1) Turn the column into a table, largest goes first
a <- table(mpg$manufacturer)[order(a, decreasing = T)]
a

# Step 2) Frequency in decending order
barplot(a,
        horiz = T,
        col = "blue",
        border = NA,
        xlim = c(0,40),
        main = "main title",
        xlab = "x lab")



### Historgram with quant variable
hist(mpg$hwy,
     col = 3,
     main = "main title",
     xlab = "shjk")

### Adding a normal curve
x <- mpg$hwy
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(mpg$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue") 



### Boxplot
boxplot(mpg$hwy, 
        col = "beige",
        notch = T,       # finds the medians of the boxes
        horizontal = T,
        main = "Name of Boxplot",
        xlab = "x axis")


# Correlation -------------------------------------------------------------


### correlation
google <- read.csv('google_correlate.csv')
names(google)

g.quant <- google[c("data_viz", "degree", "facebook", "nba")] # selecting only the quant variables
g.quant %>% cor()  # correlation matrix
cor.test(g.quant$data_viz, g.quant$degree) # statistical test between two variables



### correlations better - matrix and p values
library("Hmisc")
rcorr(as.matrix(g.quant)) # we had to turn out prior df into a matrix



# Cross tables ------------------------------------------------------------

### Creating cross tabs
cross_table <- table(google$has_nba, google$region)
cross_table


# getting marginal freq (eg how many men vs women?)
margin.table(cross_table, 1)
margin.table(cross_table, 2)


# getting proportions of these tables
round(prop.table(cross_table),2)


### Creating cross tabs
cross_table <- table(google$has_nba, google$region)
round(prop.table(cross_table,1),2)
round(prop.table(cross_table,2),2)  # percentages of columns


chisq.test(cross_table)
t.test(google$nba ~ google$has_nba)


# One way anova - compare several groups on only 1 factor
anova1 <- aov(data_viz ~ region, data = google)
summary(anova1)

# Two way anova
anova2 <- aov(data_viz ~ region + stats_ed + region:stats_ed, data = google)
summary(anova2)







# Stats Class Tutorial xlsx ----------------------------------------------------
library(tidyverse)
library(readxl)
library(psych)
smoking <- read_excel("Smoking.xlsx", na = "NA", col_names = TRUE) # we have a header row
View(smoking)
summary(smoking)
describe(smoking)

mean(smoking$amtWeekends, na.rm = T)

# Creating a histogram with Normal Curve of smokers by age
h <- hist(smoking$age)
x <- smoking$age # renaming age column as x
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue") 


### Boxplot
boxplot(smoking$age)


### Scatterplot
plot(smoking$amtWeekdays, smoking$amtWeekends, pch = 16, main = "Smoking", xlab = "Weekdays", ylab = "Weekends") 
abline(lm(smoking$amtWeekends~smoking$amtWeekdays), lty=2, col="red") 



### Bar chart for ordinal and nominal values
sex_count <- table(smoking$sex)
barplot(sex_count, ylim = c(0,1000), main = "Sex")


### Same but for marital status
marital_sort <- factor(smoking$maritalStatus, levels = c("Single", "Married", "Widowed", "Divorced", "Separated"))
marital_count <- table(marital_sort)
barplot(marital_count, main = "Marital Status", las = 2)



### Income levels
incomeSort<-factor(smoking$grossIncome, levels = c("Under 2,600","2,600 to 5,200","5,200 to 10,400","10,400 to 15,600","15,600 to 20,800","20,800 to 28,600","28,600 to 36,400","Above 36,400","Refused","Unknown")) 
incomeCount <- table(incomeSort) 
barplot(incomeCount, las = 2) 



### Do you smoke?
smokers <- table(smoking$smoke)
smokers
barplot(smokers, main = "Smoke?")


# Stats Class Tutorial csv ----------------------------------------------------------------------
marathon <- read.table("marathon.csv", header = TRUE, sep = ",", strip.white = TRUE)
hist(marathon$Time, breaks = 10)

library('pastecs')
stat.desc(smoking[, c('age', 'amtWeekends', 'amtWeekdays')])

library(ggplot2)
Gender <- marathon$Gender 
ggplot(marathon, aes(x=marathon$Time, fill = Gender)) + 
  geom_histogram(position = "dodge", bins = 10) + 
  xlab("Time (hours)") + 
  ggtitle("New York Marathon Winners, 1970-1999") 
