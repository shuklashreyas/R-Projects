# Load the built-in dataset
data("mtcars")
# Preview the data
head(mtcars)

# Structure of the dataset
str(mtcars)

# Summary statistics
summary(mtcars)

# Check for missing values
sum(is.na(mtcars))

# Install ggplot2 if necessary
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Histogram of miles per gallon (mpg)
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Distribution of Miles per Gallon", x = "Miles per Gallon (mpg)")

# Scatter plot for mpg vs hp
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +
  labs(title = "Horsepower vs. Miles per Gallon", x = "Horsepower (hp)", y = "Miles per Gallon (mpg)")

# Boxplot of mpg by number of cylinders
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Miles per Gallon by Number of Cylinders", x = "Number of Cylinders", y = "Miles per Gallon (mpg)")

# Correlation matrix and heatmap
cor_matrix <- cor(mtcars)
heatmap(cor_matrix, main = "Correlation Matrix", col = heat.colors(10), symm = TRUE)
