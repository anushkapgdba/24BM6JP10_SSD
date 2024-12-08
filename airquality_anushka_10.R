# Load the airquality dataset
data("airquality")
### Univariate Analysis

# 1. Data Overview
# Display the structure of the dataset
str(airquality)
# Display the number of observations and variables
dim(airquality)

# 2. Summary Statistics
# Choose a numerical variable, e.g., Ozone
ozone <- airquality$Ozone
# Calculate the mean, median, standard deviation, min, and max
mean_ozone <- mean(ozone, na.rm = TRUE)
median_ozone <- median(ozone, na.rm = TRUE)
sd_ozone <- sd(ozone, na.rm = TRUE)
min_ozone <- min(ozone, na.rm = TRUE)
max_ozone <- max(ozone, na.rm = TRUE)

# Display the results
cat("Mean of Ozone:", mean_ozone, "\n")
cat("Median of Ozone:", median_ozone, "\n")
cat("Standard Deviation of Ozone:", sd_ozone, "\n")
cat("Min of Ozone:", min_ozone, "\n")
cat("Max of Ozone:", max_ozone, "\n")

# 3. Distribution Visualization
# Create a histogram and a boxplot for Ozone
hist(ozone, main = "Histogram of Ozone", xlab = "Ozone Concentration (ppb)", col = "lightblue", border = "black")
boxplot(ozone, main = "Boxplot of Ozone", ylab = "Ozone Concentration (ppb)", col = "lightgreen")

# 4. Categorical Variable Analysis
# Choose the variable `Month`
month <- as.factor(airquality$Month)

# Bar plot for Month
barplot(table(month), main = "Distribution of Month", xlab = "Month", ylab = "Frequency", col = "lightcoral")

### Multivariate Analysis

# 5. Correlation Analysis
# Select two numerical variables: Ozone and Temp
ozone_temp <- airquality[, c("Ozone", "Temp")]

# Calculate the Pearson correlation coefficient
cor_ozone_temp <- cor(ozone_temp$Ozone, ozone_temp$Temp, use = "complete.obs")

# Display the correlation coefficient
cat("Pearson Correlation between Ozone and Temp:", cor_ozone_temp, "\n")

# 6. Scatter Plot Visualization
# Create a scatter plot to visualize the relationship between Ozone and Temp
plot(ozone_temp$Ozone, ozone_temp$Temp, 
     main = "Scatter Plot of Ozone vs Temp", 
     xlab = "Ozone Concentration (ppb)", 
     ylab = "Temperature (°F)", 
     pch = 19, 
     col = "blue")

# Add a trend line (linear regression line)
abline(lm(Temp ~ Ozone, data = ozone_temp), col = "red", lwd = 2)


# 7. Multiple Regression
# Fit a linear regression model predicting Ozone using Temp and Wind as predictors
lm_model <- lm(Ozone ~ Temp + Wind, data = airquality)

# Display the summary of the model
summary(lm_model)

# 8. Model Diagnostics
# Plot the residuals of the regression model
par(mfrow = c(2, 2))  # Set up a 2x2 plot window
plot(lm_model)

# 9. Principal Component Analysis (PCA)

# Select only numerical columns from the airquality dataset
num_data <- na.omit(airquality[, sapply(airquality, is.numeric)])

# Perform PCA
pca_airquality <- prcomp(num_data, center = TRUE, scale. = TRUE)

# Get the summary of PCA
summary(pca_airquality)

# Get the proportion of variance explained by each principal component
explained_variance <- summary(pca_airquality)$importance[2,]  # Proportion of Variance

# Create a data frame for plotting
pca_df <- data.frame(
  PC = factor(1:length(explained_variance), levels = 1:length(explained_variance)),
  Variance = explained_variance
)

# Create a ggplot for the scree plot
library(ggplot2)
ggplot(pca_df, aes(x = PC, y = Variance)) +
  geom_line(group = 1, color = "blue", size = 1.2) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points on the line
  labs(
    title = "Scree Plot of PCA (airquality)",
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
# Biplot of the first two principal components
biplot(pca_airquality, main = "Biplot of PCA (airquality)", cex = 0.5)
