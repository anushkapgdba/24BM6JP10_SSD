# Load the iris dataset
data(iris)

# 1. Data Overview
# Display the structure of the dataset
cat("Data Overview: \n")
str(iris)

# Display the number of observations and variables
cat("\nNumber of observations and variables: \n")
dim(iris)

# 2. Summary Statistics
# Choose a numerical variable (e.g., Sepal.Length)
sepal_length <- iris$Sepal.Length

# Calculate summary statistics
mean_sepal_length <- mean(sepal_length)
median_sepal_length <- median(sepal_length)
sd_sepal_length <- sd(sepal_length)
min_sepal_length <- min(sepal_length)
max_sepal_length <- max(sepal_length)

# Display the results
cat("\nSummary Statistics for Sepal Length:\n")
cat("Mean of Sepal Length:", mean_sepal_length, "\n")
cat("Median of Sepal Length:", median_sepal_length, "\n")
cat("Standard Deviation of Sepal Length:", sd_sepal_length, "\n")
cat("Min of Sepal Length:", min_sepal_length, "\n")
cat("Max of Sepal Length:", max_sepal_length, "\n")

# 3. Distribution Visualization
# Histogram of Sepal Length
cat("\nHistogram and Boxplot for Sepal Length: \n")
hist(sepal_length, main = "Histogram of Sepal Length", xlab = "Sepal Length (cm)", col = "lightblue", border = "black")

# Boxplot of Sepal Length
boxplot(sepal_length, main = "Boxplot of Sepal Length", ylab = "Sepal Length (cm)", col = "lightgreen")

# 4. Categorical Variable Analysis
# Choose a categorical variable (e.g., Species)
species <- as.factor(iris$Species)

# Bar plot for 'Species'
cat("\nBar Plot for Species: \n")
barplot(table(species), main = "Distribution of Species", xlab = "Species", ylab = "Frequency", col = "lightcoral")

# 5. Correlation Analysis
# Choose two numerical variables (e.g., Sepal.Length and Petal.Length)
sepal_petal <- iris[, c("Sepal.Length", "Petal.Length")]

# Calculate the Pearson correlation coefficient
cor_sepal_petal <- cor(sepal_petal$Sepal.Length, sepal_petal$Petal.Length)

# Display the correlation coefficient
cat("\nPearson Correlation between Sepal Length and Petal Length:", cor_sepal_petal, "\n")

# 6. Scatter Plot Visualization
# Create a scatter plot to visualize the relationship between Sepal Length and Petal Length
plot(iris$Petal.Length, iris$Sepal.Length, 
     main = "Scatter Plot of Petal Length vs Sepal Length", 
     xlab = "Petal Length (cm)", 
     ylab = "Sepal Length (cm)", 
     pch = 19)

# Add a trend line (linear regression line)
abline(lm(Sepal.Length ~ Petal.Length, data = iris), col = "red", lwd = 2)


# 7. Multiple Regression
# Fit a linear regression model predicting Sepal.Length using Petal.Length and Sepal.Width as predictors
cat("\nMultiple Regression: \n")
lm_model_iris <- lm(Sepal.Length ~ Petal.Length + Sepal.Width, data = iris)

# Display the summary of the model
summary(lm_model_iris)

# 8. Model Diagnostics
# Plot the residuals of the regression model
cat("\nModel Diagnostics (Residual Plots): \n")
par(mfrow = c(2, 2))  # Set up a 2x2 plot window
plot(lm_model_iris)

# 9. Principal Component Analysis (PCA)
cat("\nPrincipal Component Analysis (PCA): \n")
# Perform PCA on the numerical variables (all variables except 'Species')
num_data_iris <- iris[, -which(names(iris) == "Species")]

# Perform PCA
pca_iris <- prcomp(num_data_iris, center = TRUE, scale. = TRUE)

# Plot the explained variance (scree plot)
screeplot(pca_iris, main = "Scree Plot of PCA")

# Alternatively, use the summary function to see the variance explained by each component
summary(pca_iris)

# 10. PCA Interpretation

cat("\nPCA Interpretation (Biplot): \n")
# Visualize the PCA results (Biplot)
biplot(pca_iris, main = "Biplot of PCA (iris)", cex = 0.5)
