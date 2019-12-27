library(tidyverse)
# Setup -------------------------------------------------------------------
file_name = "mobileApps.csv"
mobile_apps <- tbl_df(read.csv(file_name))
View(mobile_apps)

library(nycflights13)
library(tidyverse)
flights


# Viewing to begin
library(dplyr)
glimpse(flights)
summary(flights)
describe(flights)



# Missing -----------------------------------------------------------------

gg_miss_var(riskfactors)
miss_var_summary()

# How many of these in each column?
miss_scan_count(data = pacman, search = list("N/A", "missing","na", " "))

#Convert them to na (specific cols)
replace_with_na(pacman, 
                replace = list(
                  col1 = c("N/A", "na", "missing"), 
                  col2 = c("N/A", "na", "missing")))


# Convert to na (all cols)
replace_with_na_all(pacman, 
                    ~.x %in% c("N/A", "missing", "na", " "))



# Missing values
sum(is.na(df)) # count of all na
na.omit(df) # removes rows with an NA
complete.cases() # rows with no NA
prop_miss()


#detecting missing values
library(mice)
md.pattern(mobile_apps)


#visually detecting missing values
library(VIM)
aggr(mobile_apps, prop=FALSE, numbers=TRUE)
matrixplot(mobile_apps)


# Finding implicit missing
library(tidyr)
frogger %>% 
  fill(name) %>%
  complete(name, time)


# How do groups change when another col is na?
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarise(wind_ns_mean = mean(wind_ns),
            wind_ns_sd = sd(wind_ns),
            n_obs = n())



# Replacing na with linear model
ocean_imp_lm_wind <- oceanbuoys %>% 
  bind_shadow() %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns) %>% 
  impute_lm(humidity ~ wind_ew + wind_ns) %>%
  add_label_shadow()


# Quick adjust ------------------------------------------------------------

hist(df$col, breaks = 20) # bucket size


miss <- which(is.na(df$col)) # helps locate
df[miss, ] # view only these rows
df$col[miss] <- 100 # replace with 100



# Gather, spread, separate, unite -------------------------------------------------------

library(tidyr)
# Gather - when 1 variable is currently split into multiple columns
# https://tidyr.tidyverse.org/reference/spread.html
gather(wide, new_col_name, new_val_name, -cols_to_ignore) # na.rm = TRUE
spread(tall, old_col_name, old_val_name)


# Separate - splitting apart into 2 columns
# https://tidyr.tidyverse.org/reference/separate.html
separate(df, col = ___, into = c("___", "___"), sep = "___")
unite(df, yr_month, yr, month) # sep = '-'


# Factors, Strings and dates -------------------------------------------------------

# Convert columns to numeric
mutate_at(weather5, vars(CloudCover:WindDirDegrees), funs(as.numeric))

# Factors
library(forcats)
data %>% fct_reorder() # reorder categorical


# Converting strings to date
library(lubridate)
mdy('07/30/96')
ymd('2015 August 25')
a2$dob <- ymd(a$dob)






# Working with strings
library(stringr)
str_trim('    abc    ') #removes begin and end whitespace
str_pad('233', width = 5, side = 'left', pad = '0') # pad until 5 digits
str_detect(df, 'what we look for')
str_replace(df, 'old', 'new')
tolower('LOWER')

as.logical(str_detect(students3$dob, '1997')) # Born in 1997?




# Data Types --------------------------------------------------------------
# int = integer
# dbl = real numbers
# chr = strings
# dttm = date + time
# lgl = true or false
# fctr = factors
# date = dates
# Use class to see each column
class(flights$dep_time)


# Select, Filter, mutate, arrange, transmutate ------------------------------------------------------------------

## Select
flights %>% select(year:month, day, starts_with("abc"))
ends_with("xyz")
contains("ijk")
last_col()


## Filter
flights %>% filter(word %in% c('ebay','apple'))


## Arrange
flights %>% arrange(desc(dep_delay))


# Mutate
# https://dplyr.tidyverse.org/reference/mutate.html
subset %>% 
  mutate (gain = dep_delay - arr_delay,      ## determing if flight was longer than expected
          speed = distance / air_time * 60,  ## determining speed
          ratio = fraction / lag(fraction))  ## percent change (ratio)


# Transmutate
counties %>%
  transmute(state, county, population, density = population / land_area) %>%
  filter(population > 1000000) %>%
  arrange(density)


# Normalizing with grouped mutates
babynames %>%
  group_by(name) %>%
  mutate(name_total = sum(number),   # Total number of babies named x
         name_max = max(number)) %>% # Total number of babies named x in baby's max year
  ungroup() %>%                      # Pulled out of the grouping, left the new cols though
  mutate(fraction_max = number / name_max)


# Summarise ---------------------------------------------------------------
flights %>% count(month, sort = T)

# Mean of one column
flights %>% summarise(delay = mean(dep_delay, na.rm = TRUE))

# Group by and then mean

# Average delay by year - returns one row
flights %>% 
  group_by(year) %>%  # These are the columns 
  summarise(delay = mean(dep_delay, na.rm = TRUE)) # remove rows with missing values

# Average delay by month - returns 31 rows
flights %>% 
  group_by(year, day) %>%  # These are the columns 
  summarise(delay = mean(dep_delay, na.rm = TRUE))

# Average delay by day - returns 365 rows
flights %>%
  group_by(year, month, day) %>%  # These are the columns 
  summarise(delay = mean(dep_delay, na.rm = TRUE))

# Count the states with more people in Metro or Nonmetro areas
counties_selected %>%
  group_by(state, metro) %>%
  summarize(total_pop = sum(population)) %>%
  top_n(1, total_pop) %>%
  ungroup() %>%
  count(metro) %>%
  rename(counting = n)


# Useful Creation / Math Functions -----------------------------------------------
# https://r4ds.had.co.nz/transform.html#mutate-funs

# Integer division
5 %/% 1

# Remainder division
5 %% 1

# Logs
log(3)
log2(3)
log10(3)

# Leading and lag (time series)
x <- 1:10
lag(x)
lead(x)

# Cumulative aggregates - rolling measures
cumsum(x)
cumprod(x) 
cummin(x) 
cummax(x)
cummean(x)


# Really good example with visualization ----------------------------------

# Can we see a summary of delays by each individual city?

# Summary figures by a variable of interest
delays <- flights %>% 
  group_by(dest) %>%                        ## establishes dest as the variable we are aggregating
  summarise(
    count = n(),                            ## counts how many times each city was the dest
    dist = mean(distance, na.rm = TRUE),    ## finds the average number of miles traveled to get there
    delay = mean(arr_delay, na.rm = TRUE)   ## finds the average delay in arrival time to the city
  ) %>% 
  filter(count > 20, dest != "HNL")         ## only counting cities that were visited more than 20 times and not Honolulu
delays

ggplot(data = delays,                             ## data is the code above
       mapping = aes(x = dist, y = delay)) +           ## x and y axis are the summaries from above
  geom_point(aes(size = count), alpha = 1/3) +    ## dot plot and size signals frequency, alpha makes them more transparent
  geom_smooth(se = TRUE)                          ## se = FALSE means no band around the line



# Another really good example ---------------------------------------------

# How often are flights delayed? Are they usually delayed by quite a bit?

# 1st lets look at only flights that havent been cancelled
not_cancelled <- flights %>%                         ## using flights as data again
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%   ## removing rows where there is no data in these 2 columns
  group_by(year, month, day) %>%            ## grouping by our 3 date columns
  summarise(
    mean = mean(dep_delay),                          ## finding the average departure delay
    delay = mean(arr_delay, na.rm = TRUE),           ## average arrival delay
    n = n()
  )


## establishing n as the count of all
ggplot(data = not_cancelled, mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)





