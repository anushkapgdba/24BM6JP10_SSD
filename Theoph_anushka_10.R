# Load the Theoph dataset
data(Theoph)

# 1. Data Overview
cat("Data Overview: \n")
str(Theoph)

# Display the number of observations and variables
cat("\nNumber of observations and variables: \n")
dim(Theoph)

# 2. Summary Statistics
# Choose a numerical variable (e.g., conc)
conc <- Theoph$conc

# Calculate summary statistics
mean_conc <- mean(conc, na.rm = TRUE)
median_conc <- median(conc, na.rm = TRUE)
sd_conc <- sd(conc, na.rm = TRUE)
min_conc <- min(conc, na.rm = TRUE)
max_conc <- max(conc, na.rm = TRUE)

# Display the results
cat("\nSummary Statistics for Concentration (conc):\n")
cat("Mean of conc:", mean_conc, "\n")
cat("Median of conc:", median_conc, "\n")
cat("Standard Deviation of conc:", sd_conc, "\n")
cat("Min of conc:", min_conc, "\n")
cat("Max of conc:", max_conc, "\n")

# 3. Distribution Visualization
# Histogram of conc
cat("\nHistogram and Boxplot for Concentration (conc): \n")
hist(conc, main = "Histogram of Concentration", xlab = "Concentration (mg/L)", col = "lightblue", border = "black")

# Boxplot of conc
boxplot(conc, main = "Boxplot of Concentration", ylab = "Concentration (mg/L)", col = "lightgreen")

# 4. Categorical Variable Analysis
# Choose a categorical variable (e.g., Subject)
subject <- as.factor(Theoph$Subject)

# Bar plot for 'Subject'
cat("\nBar Plot for Subject: \n")
barplot(table(subject), main = "Distribution of Subjects", xlab = "Subject", ylab = "Frequency", col = "lightcoral")

# 5. Correlation Analysis
# Choose two numerical variables (e.g., Time and conc)
time_conc <- Theoph[, c("Time", "conc")]

# Calculate the Pearson correlation coefficient
cor_time_conc <- cor(time_conc$Time, time_conc$conc, use = "complete.obs")

# Display the correlation coefficient
cat("\nPearson Correlation between Time and Concentration:", cor_time_conc, "\n")

# 6. Scatter Plot Visualization
# Create a scatter plot to visualize the relationship between Time and Concentration
cat("\nScatter Plot for Time vs Concentration: \n")
plot(time_conc$Time, time_conc$conc, main = "Scatter Plot of Time vs Concentration", 
     xlab = "Time (hours)", ylab = "Concentration (mg/L)", pch = 19)

# Add a trend line (linear regression line)
abline(lm(conc ~ Time, data = Theoph), col = "red")

# 7. Multiple Regression
# Fit a linear regression model predicting conc using Time and Dose as predictors
cat("\nMultiple Regression: \n")
lm_model_theoph <- lm(conc ~ Time + Dose, data = Theoph)

# Display the summary of the model
summary(lm_model_theoph)

# 8. Model Diagnostics
# Plot the residuals of the regression model
cat("\nModel Diagnostics (Residual Plots): \n")
par(mfrow = c(2, 2))  # Set up a 2x2 plot window
plot(lm_model_theoph)

# 9. Principal Component Analysis (PCA)

# Perform PCA on the numerical variables (Time, conc, Dose, and Weight)
num_data_theoph <- Theoph[, c("Time", "conc", "Dose", "Wt")]

# Perform PCA
pca_theoph <- prcomp(num_data_theoph, center = TRUE, scale. = TRUE)

# Get the proportion of variance explained by each principal component
explained_variance_theoph <- summary(pca_theoph)$importance[2,]  # Proportion of Variance

# Create a data frame for plotting
pca_df_theoph <- data.frame(
  PC = factor(1:length(explained_variance_theoph), levels = 1:length(explained_variance_theoph)),
  Variance = explained_variance_theoph
)

# Create a ggplot for the scree plot
library(ggplot2)
ggplot(pca_df_theoph, aes(x = PC, y = Variance)) +
  geom_line(group = 1, color = "blue", size = 1.2) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points on the line
  labs(
    title = "Scree Plot of PCA (Theoph)",
    x = "Principal Component",
    y = "Proportion of Variance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

# Alternatively, you can display the summary of the PCA
summary(pca_theoph)

# 10. PCA Interpretation
biplot(pca_theoph, main = "Biplot of PCA (theoph)", cex = 0.5)

