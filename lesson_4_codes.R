
# Udacity
# Data Analysis with R
# Problem Set4
# William W. Chiang, 1/5/2016

# Load ggplot2 package and diamonds dataset
library(ggplot2)
data(diamonds)

# Your first task is to create a
# scatterplot of price vs x.
# using the ggplot syntax.
ggplot(aes(x = price, y = x), data = diamonds) +
  geom_point()

# Determining correlation coefficients will require the alr3 package
library(alr3)

# What is the correlation between price and x?
cor.test(x = diamonds$price, y = diamonds$x)
# What is the correlation between price and y?
cor.test(x = diamonds$price, y = diamonds$y)
# What is the correlation between price and z?
cor.test(x = diamonds$price, y = diamonds$z)

# Create a simple scatter plot of price vs depth
ggplot(aes(x = price, y = depth), data = diamonds) +
  geom_point()

# Change the code to make the transparency of the points to be 1/100 of
# what they are now and mark the x-axis every 2 units
ggplot(aes(x = depth, y = price), data = diamonds) +
  +     geom_point(alpha = 1/100) +
  +     scale_x_continuous(breaks = seq(0, 100, 2)) +
  +     labs(x = "Depth (%)", y = "Price (USD)", title = "Diamond Price vs. Depth") +
  +     coord_cartesian(xlim = c(55,65))

# The correlation betwen depth and price can be determine from the r code below
cor.test(x = diamonds$depth, y = diamonds$price)

# Create a scatterplot of price vs carat and omit the top 1% of price and carat values
ggplot(aes(x = carat, y = price), data = diamonds) +
  geom_point() +
  scale_x_continuous(limits = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(limits = c(0, quantile(diamonds$price, 0.99))) +
  labs(title = "Diamond Price vs. Mass", x = "Mass (carats)", y = "Price (USD)")  

# Create a new variable for volume in the diamonds data frame
diamonds$volume <- with(diamonds, x * y * z)

# Create a scatterplot of price vs. volume (x * y * z)
ggplot(aes(x = volume, y = price), data = diamonds) +
  geom_point()

# Find correlation of price and volume for diamonds where 0 < volume < 800 
with(diamonds[diamonds$volume > 0 & diamonds$volume < 800, ], cor(volume, price))

# Create price vs volume scatterplot with best fit line with subset diamonds data
diamonds_subset <- subset(diamonds, volume > 0 & volume <= 800)

ggplot(aes(x = volume, y = price), data = diamonds_subset) +
  geom_point(alpha = 1/100) +
  geom_smooth(method = "lm", colour = "red") +
  labs(title = "Price vs. Volume", x = "Volume", y = "Price (USD)")

# Use the function dplyr package to create a new data frame containing
# info on diamonds by clarity, and name the data fram diamondsByClarity
library(dplyr)

diamondsByClarity <- summarise(group_by(diamonds, clarity),
                               mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n()
)

diamondsByClarity

# Bar charts of mean prices
library(gridExtra)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity")

p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")

grid.arrange(p1,p2, ncol = 1)