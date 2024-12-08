# Load the mtcars dataset
data("mtcars")

### Univariate Analysis

# 1. Data Overview
# Display the structure of the dataset
str(mtcars)

# Display the number of observations and variables
dim(mtcars)

# 2. Summary Statistics
# Choose a numerical variable, e.g., mpg
mpg <- mtcars$mpg

# Calculate the mean, median, standard deviation, min, and max
mean_mpg <- mean(mpg)
median_mpg <- median(mpg)
sd_mpg <- sd(mpg)
min_mpg <- min(mpg)
max_mpg <- max(mpg)

# Display the results
cat("Mean of mpg:", mean_mpg, "\n")
cat("Median of mpg:", median_mpg, "\n")
cat("Standard Deviation of mpg:", sd_mpg, "\n")
cat("Min of mpg:", min_mpg, "\n")
cat("Max of mpg:", max_mpg, "\n")

# 3. Distribution Visualization
# Create a histogram and a boxplot for mpg
hist(mpg, main = "Histogram of mpg", xlab = "Miles Per Gallon", col = "lightblue", border = "black")
boxplot(mpg, main = "Boxplot of mpg", ylab = "Miles Per Gallon", col = "lightgreen")

# 4. Categorical Variable Analysis
# Choose the variable `cyl` (number of cylinders)
cyl <- as.factor(mtcars$cyl)

# Bar plot for cylinders
barplot(table(cyl), main = "Distribution of Cylinders", xlab = "Number of Cylinders", ylab = "Frequency", col = "lightcoral")

### Multivariate Analysis

# 5. Correlation Analysis
# Select two numerical variables: mpg and hp
mpg_hp <- mtcars[, c("mpg", "hp")]

# Calculate the Pearson correlation coefficient
cor_mpg_hp <- cor(mpg_hp$mpg, mpg_hp$hp)

# Display the correlation coefficient
cat("Pearson Correlation between mpg and hp:", cor_mpg_hp, "\n")

# 6. Scatter Plot Visualization
# Create a scatter plot to visualize the relationship between mpg and hp
plot(mpg_hp$mpg, mpg_hp$hp, main = "Scatter Plot of mpg vs hp", xlab = "Miles Per Gallon", ylab = "Horsepower", pch = 19)

# Add a trend line (linear regression line)
abline(lm(hp ~ mpg, data = mtcars), col = "red", lwd = 2)

# 7. Multiple Regression
# Fit a linear regression model predicting mpg using hp and wt as predictors
lm_model <- lm(mpg ~ hp + wt, data = mtcars)

# Display the summary of the model
summary(lm_model)

# 8. Model Diagnostics
# Plot the residuals of the regression model
par(mfrow = c(2, 2))  # Set up a 2x2 plot window
plot(lm_model)

# 9. Principal Component Analysis (PCA)

# Perform PCA on the numerical variables (all variables except 'Species')
num_data_iris <- iris[, -which(names(iris) == "Species")]

# Perform PCA
pca_iris <- prcomp(num_data_iris, center = TRUE, scale. = TRUE)

# Plot the explained variance
summary(pca_iris)

# Get the proportion of variance explained by each principal component
explained_variance_iris <- summary(pca_iris)$importance[2,]  # Proportion of Variance

# Create a data frame for plotting
pca_df_iris <- data.frame(
  PC = factor(1:length(explained_variance_iris), levels = 1:length(explained_variance_iris)),
  Variance = explained_variance_iris
)

# Create a ggplot for the scree plot
library(ggplot2)
ggplot(pca_df_iris, aes(x = PC, y = Variance)) +
  geom_line(group = 1, color = "blue", size = 1.2) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points on the line
  labs(
    title = "Scree Plot of PCA (iris)",
    x = "Principal Component",
    y = "Proportion of Variance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

# 10. PCA Interpretation
# Biplot of PCA
biplot(pca3, main = "Biplot of PCA (mtcars)", cex = 0.5)

