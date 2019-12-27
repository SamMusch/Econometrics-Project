# General -----------------------------------------------------------------
# https://ggplot2.tidyverse.org/reference/

library(tidyverse)

# Break down ggplot https://i.imgur.com/DwTBcZ9.png

# ggplot(data = <DATA>) +   
#   <GEOM_FUNCTION>(                  ### 3.6 Geometric Objects
#       mapping = aes(<MAPPINGS>)),   ### 3.3 Aesthetics
#       stat = <stat>,                ### 3.7 Statistical Transformations
#       position = <position>         ### 3.8 Positional Adjustments
#   ) + 
#   <Coordinate_function> +           ### 3.9 Coordinate systems
#   <Facet_function>                  ### 3.5 Facets



# Multico plots -----------------------------------------------------------

GGally::ggpairs(mtcars2)


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


# Aesthetic -----------------------------------------------------------

# Typical aes --> https://i.imgur.com/gU0i5uh.png



# Mapping inside of aes
# Scatter + smooth
head(mpg)
ggplot(data = mpg) +
  geom_point(aes(x=displ, y=hwy, alpha = class, color = class)) + 
  geom_smooth(se = T, aes(x=displ, y=hwy))

# Size - changes size of dots based on char
# alpha - transparency of points

# shape = 1 is a hollow circle
# shape = 21 is hollow circle as well, can be filled with other color though


# Facets --------------------------------------------------------------

# One variable (should be a discrete variable)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)


# Two variables
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)


# Scatter -----------------------------------------------------------

# Scatter + smooth
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# Scatter + smooth + mulitple lines
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))


# Muti group Scatter + smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()



# Split group (suvived vs died)
# Split by class
# Split by gender
ggplot(titanic, aes(x = Pclass, y = Age, color = Sex)) +
  geom_point(position = position_jitterdodge(0.5, 0, 0.6), size = 3, alpha = .5) + 
  facet_grid(. ~ Survived)


# Bars and Hist -----------------------------------------

# Bar chart
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# Stacked
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = 'stack')

# Up to 100%
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = 'fill')


# Next to each other
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = 'dodge')


# Next to each other, overlapping
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = position_dodge(width = .2), alpha = .6)



# Histogram as proportion for different groups
ggplot(adult, aes(x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .)




# Mosaic - Chi squared ----------------------------------------------------

# The initial contingency table
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))

# Create groupSum, xmax and xmin columns
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum
# The groupSum column needs to be removed; don't remove this line
DF$groupSum <- NULL

# Copy row names to variable X
DF$X <- row.names(DF)

# Melt the dataset
library(reshape2)
DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

# dplyr call to calculate ymin and ymax - don't change
library(dplyr)
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

# Plot rectangles - don't change
library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = FILL)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()



# Lines and area -------------------------------------------------------------------

# Area proportion over time
ggplot(mtcars, aes(x = mpg, y = mpg, fill = cyl)) +
  geom_area(position = 'fill')


# Line plots with shaded areas
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_rect(data = recess,
            aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) + geom_line()


# Groups + members changing over time
ggplot(ChickWeight, aes(x = Time, y = weight, color = Diet)) +
  geom_line(aes(group = Chick, alpha = .3)) + 
  geom_smooth(lwd = 2, se = F)


# Change over time
ggplot(barley, aes(x = year, y = yield, col = site, group = site, fill = site)) + 
  stat_summary(fun.y = mean, geom = 'line') + 
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = 'ribbon', col = NA, alpha = .1)


# Pie ---------------------------------------------------------------------

# Convert bar chart to pie chart
variable_in_our_df <- 1
ggplot(mtcars, aes(x = factor(variable_in_our_df), fill = am)) +
  geom_bar(position = "fill", width = 1) +
  facet_grid(. ~ cyl) + 
  coord_polar(theta = "y") +
  theme_void()

# Labels ------------------------------------------------------------------
# Define your own theme function below

library(ggthemes)
graph + theme_tufte()

theme_ilo <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Bookman", color = "gray25"),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(color = "gray30"),
      plot.background = element_rect(fill = "gray95"),
      plot.margin = unit(c(5, 10, 5, 10), units = "mm")
    )
}



# Create the plot
ggplot(plot_data) +
  geom_point(aes(x = working_hours, y = hourly_compensation)) +
  labs(
    x = "Working hours per week",
    y = "Hourly compensation",
    title = "The more people work, the less compensation they seem to receive",
    subtitle = "Working hours and hourly compensation in European countries, 2006",
    caption = "Data source: ILO, 2017",
    legend.position = c(.85,.85)
  )



# Geom text - labeling connecting lines
geom_text(
  aes(x = working_hours,
      y = country,
      label = round(working_hours, 1),
      hjust = ifelse(year == "2006", 1.4, -0.4)
  ),
  # Change the appearance of the text
  size = 3,
  family = "Bookman",
  color = "gray25"
)






# Linear regression -----------------------------------------------------------------------

# Linear regression, color by another variables
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = T, linetype = 2)


# Linear regression line for each group + all as an average
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) + 
  stat_smooth(method = "lm", se = FALSE, aes(group = 1))


# Multiple regression
# Continuous on x axis
# Multiple categorical on y axis
fit <- lm(Income ~ Frost + Illiteracy + Murder, data = states)
plot_summs(fit, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)

# Might be useful ---------------------------------------------------------

stat_quantile(quantiles = .5) # to only plot median

# Multiple "box plot"
compare = am
ggplot(mtcars, aes(x = cyl, y = wt, color = compare, fill = compare, group = compare)) +
  stat_summary(fun.data = mean_cl_normal, position = position_dodge(width = .1))



