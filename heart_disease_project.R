
#List of required packages
packages <- c("ggplot2", "dplyr", "scales", "tidyr","GGally", "corrplot", "moments", "fastDummies")

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load the packages
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(GGally)
library(corrplot)
library(moments)
library(fastDummies)
library(caret)


#For image folder
if(!dir.exists("images")) {
  dir.create("images")
}
cat("Created 'images' folder for saving plots\n")





# ------------A.Data Understanding----------------- 

# 1.Load the dataset into R. 
# 2.Display the first few rows of the dataset.

url <- "https://drive.google.com/uc?export=download&id=1BO1CfdN6z-ooSBsV9ZmX6ZxKFTnghwUT"
data <- read.csv(url)
cat("First few rows of the dataset:\n")
head(data)



# 3.Show shape (rows × columns). 
cat("Rows:", nrow(data), "Columns:", ncol(data) )


# 4.Display data types of each column
cat("\nData types of each column:\n")
str(data)

# 5.Generate basic descriptive statistics (mean, median, mode, std, min, max, count, etc.).
numerical_cols <- c("age", "trestbps", "chol", "thalch", "oldpeak")
cat("\nNumerical variables statistics:\n")
for(col in numerical_cols) {
  if(col %in% names(data)) {
    cat("\n---", col, "---\n")
    cat("Mean:", mean(data[[col]], na.rm = TRUE), "\n")
    cat("Median:", median(data[[col]], na.rm = TRUE), "\n")
    cat("Standard Deviation:", sd(data[[col]], na.rm = TRUE), "\n")
    cat("Minimum:", min(data[[col]], na.rm = TRUE), "\n")
    cat("Maximum:", max(data[[col]], na.rm = TRUE), "\n")
    cat("Count:", sum(!is.na(data[[col]])), "\n")
    cat("NA Count:", sum(is.na(data[[col]])), "\n")
  }
}

cat("\n5. Descriptive statistics:\n")
summary(data)


# 6. Identify categorical and numerical features

# Identify numerical features
numerical_features <- c()
for(col in names(data)) {
  if(is.numeric(data[[col]]) || 
     (is.integer(data[[col]]) && length(unique(na.omit(data[[col]]))) > 10)) {
    numerical_features <- c(numerical_features, col)
  }
}

cat("Numerical features (", length(numerical_features), "):\n")
print(numerical_features)

# Identify categorical features
categorical_features <- c()
for(col in names(data)) {
  if(is.character(data[[col]]) || 
     is.factor(data[[col]]) ||
     (is.numeric(data[[col]]) && length(unique(na.omit(data[[col]]))) <= 10)) {
    categorical_features <- c(categorical_features, col)
  }
}

# Remove numerical features from categorical list
categorical_features <- setdiff(categorical_features, numerical_features)

cat("\nCategorical features (", length(categorical_features), "):\n")
print(categorical_features)


cat("\n=== DATASET SUMMARY ===\n")
cat("Total features:", ncol(data), "\n")
cat("- Numerical features:", length(numerical_features), "\n")
cat("- Categorical features:", length(categorical_features), "\n")
cat("Total records:", nrow(data), "\n")
cat("Missing values in dataset:", sum(is.na(data)), "\n")





# --------------- B.Data Exploration & Visualization-------------

#### 1. UNIVARIATE ANALYSIS

numerical_vars <- c("age", "trestbps", "chol", "thalch", "oldpeak")

# Histograms 

cat("Histograms for Numerical Variables:\n")
for(var in numerical_vars) {
  p <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(fill = "chocolate", color = "black", bins = 20) +
    labs(title = paste("Distribution of", var), x = var, y = "Frequency") 
  print(p)
  
  ggsave(paste0("images/histogram_", var, ".png"), plot = p, width = 8, height = 6, dpi = 300)
}



# Boxplot

cat("Boxplots for Numerical Variables:\n")
for(var in numerical_vars) {
  p <- ggplot(data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    labs(title = paste("Boxplot of", var), y = var) 
  print(p)
}



# Bar charts 

p <- ggplot(data, aes(x = .data[[var]])) +
  geom_bar(fill = "tomato", color = "black") +
  labs(title = paste("Distribution of", var), x = var, y = "Count") 
print(p)
ggsave(paste0("images/bar_", var, ".png"), plot = p, width = 8, height = 6, dpi = 300)


# Frequency for categorical variables
categorical_vars <- c("sex", "cp", "fbs", "restecg", "exang", "num")

for(var in categorical_vars) {
  
  freq_table <- table(data[[var]])
  
  cat(paste("\nFrequency of", var, ":\n"))
  print(freq_table)
}




#### 2. BIVARIATE ANALYSIS


num_data <- data[, numerical_vars]
correlation_matrix <- cor(num_data, use = "complete.obs")

# Create heatmap

png("images/heatmap.png", width = 1000, height = 800, res = 120)
corrplot(correlation_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black",
         title = "Correlation Matrix Heatmap", mar = c(0,0,1,0))
dev.off()

# Print correlation values
cat("Correlation Matrix Values:\n")
print(round(correlation_matrix, 2))

# Scatter plots for numeric pairs
cat("\nScatter Plots for Numerical Pairs:\n")

# Age vs Cholesterol
p1 <- ggplot(data, aes(x = age, y = chol)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Age vs Cholesterol", x = "Age", y = "Cholesterol") +
  theme_minimal()
print(p1)
ggsave("images/scatter_age_vs_chol.png", plot = p1, width = 8, height = 6, dpi = 300)


# Blood Pressure vs Cholesterol
p2 <- ggplot(data, aes(x = trestbps, y = chol)) +
  geom_point(alpha = 0.6, color = "red") +
  labs(title = "Blood Pressure vs Cholesterol", x = "Blood Pressure", y = "Cholesterol") +
  theme_minimal()
print(p2)
ggsave("images/scatter_bp_vs_chol.png", plot = p2, width = 8, height = 6, dpi = 300)


# Max Heart Rate vs Age
p3 <- ggplot(data, aes(x = age, y = thalch)) +
  geom_point(alpha = 0.6, color = "darkorchid") +
  labs(title = "Age vs Max Heart Rate", x = "Age", y = "Max Heart Rate") +
  theme_minimal()
print(p3)
ggsave("images/scatter_age_vs_heartrate.png", plot = p3, width = 8, height = 6, dpi = 300)





#### 3. Identify Patterns, Skewness, and Possible Outliers

# Select numerical columns
numerical_vars <- c("age", "trestbps", "chol", "thalch", "oldpeak")

# Function to detect outliers using IQR
detect_outliers_iqr <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  which(x < lower | x > upper)
}

# Loop through numerical variables
for (var in numerical_vars) {
  x <- data[[var]]
  if (!is.numeric(x)) next
  
  # Calculate skewness
  sk <- skewness(x, na.rm = TRUE)
  
  # Detect outliers
  outliers <- detect_outliers_iqr(x)
  
  cat("\n-----------------------------\n")
  cat("Variable:", var, "\n")
  cat("Skewness:", round(sk, 3), "\n")
  cat("Number of Outliers (IQR method):", length(outliers), "\n")
  
  # Histogram with density curve
  p1 <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
    geom_density(color = "red", size = 1) +
    labs(title = paste("Distribution & Skewness of", var),
         subtitle = paste("Skewness =", round(sk, 3)),
         x = var, y = "Density") +
    theme_minimal()
  print(p1)
  ggsave(paste0("images/histogram_density_", var, ".png"), plot = p1, width = 8, height = 6, dpi = 300)
}


  # Boxplot to show outliers
  p2 <- ggplot(data, aes(y = .data[[var]])) +
    geom_boxplot(fill = "pink", color = "black") +
    labs(title = paste("Boxplot for", var),
         subtitle = paste("Outliers detected:", length(outliers)),
         y = var) +
    theme_minimal()
  
  print(p2)
  ggsave(paste0("images/boxplot_outliers_", var, ".png"), plot = p2, width = 8, height = 6, dpi = 300)


 
   

# ------------ C. Data Preprocessing--------- 
 

### 1)Handling Missing Values ------------

#  Detect Missing Values
cat("Total Missing Values in Dataset:", sum(is.na(data)), "\n")

cat("\nMissing Values per Column:\n")
print(colSums(is.na(data)))

#  Replace Missing Values

# Function to replace NA with Mean (numeric) or Mode (categorical)
replace_na <- function(x) {
  
  if (is.numeric(x)) {
    # Replace numeric NA with mean
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    # Replace categorical NA with mode
    mode_val <- names(sort(table(x), decreasing = TRUE))[1]
    x[is.na(x)] <- mode_val
  }
  return(x)
}

data <- as.data.frame(lapply(data, replace_na))

cat("\nAfter Handling Missing Values:\n")
print(colSums(is.na(data)))



#### 2) Handling Outliers ------------

# Identify numerical columns

numerical_vars <- c("age", "trestbps", "chol", "thalch", "oldpeak")

# Detect outliers using IQR method

for (var in numerical_vars) {
  
  if (!var %in% names(data)) {
    
    cat("Skipping:", var, "- not found in data\n")
    
    next
    
  }
  
  x <- data[[var]]
  
  if (!is.numeric(x)) {
    
    cat("Skipping:", var, "- not numeric\n")
    
    next
    
  }
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  iqr_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * iqr_val
  
  upper <- Q3 + 1.5 * iqr_val
  
  # Identify outliers
  
  outliers <- which(x < lower | x > upper)
  
  cat("\nVariable:", var, "\n")
  
  cat("Outliers detected:", length(outliers), "\n")
  
  # Boxplot before handling
  
  png(paste0("images/boxplot_before_", var, ".png"), width = 800, height = 600, res = 120)
  
  boxplot(x, main = paste("Before Handling Outliers -", var), col = "gold")
  dev.off()
  
  # --- Option 1: Remove Outliers ---
  

  
  # data <- data[!(data[[var]] < lower | data[[var]] > upper), ]
  
  # --- Option 2: Cap Outliers (Winsorization) ---
  
  x[x < lower] <- lower
  
  x[x > upper] <- upper
  
  # Update dataset
  
  data[[var]] <- x
  
  # Boxplot after handling
  
  png(paste0("images/boxplot_after_", var, ".png"), width = 800, height = 600, res = 120)
  
  boxplot(data[[var]], main = paste("After Handling Outliers -", var), col = "rosybrown")
  
  dev.off()
}




#### 3) Data Conversion ------------

# Check structure of dataset
str(data)

# Identify categorical variables
cat_vars <- c("sex", "cp", "fbs", "restecg", "exang", "num")

# Option 1: Label Encoding (convert factors to numeric codes) -----
for (var in cat_vars) {
  data[[var]] <- as.numeric(as.factor(data[[var]]))
}
cat("\nAfter Label Encoding:\n")
str(data)

# Option 2: One-Hot Encoding (create dummy variables) -----


# Create dummy variables for categorical features
data <- fastDummies::dummy_cols(data, 
                              select_columns = cat_vars, 
                              remove_first_dummy = TRUE, 
                              remove_selected_columns = TRUE)

cat("\nAfter One-Hot Encoding:\n")
str(data)




#### 4) Data Transformation ------------


# Identify numerical variables
numerical_vars <- c("age", "trestbps", "chol", "thalch", "oldpeak")

# Option 1: Min-Max Normalization -----
for (var in numerical_vars) {
  data[[paste0(var, "_minmax")]] <- (data[[var]] - min(data[[var]], na.rm = TRUE)) /
    (max(data[[var]], na.rm = TRUE) - min(data[[var]], na.rm = TRUE))
}

# Option 2: Z-Score Standardization -----
for (var in numerical_vars) {
  data[[paste0(var, "_zscore")]] <- as.numeric(scale(data[[var]]))
}

# Option 3: Log / Sqrt Transformation (to fix skewness) -----
for (var in numerical_vars) {
  data[[paste0(var, "_log")]] <- log(data[[var]] + 1)   # +1 to avoid log(0)
  data[[paste0(var, "_sqrt")]] <- sqrt(data[[var]])
}

str(data)




#### 5) Feature Selection ------------

# Identify numeric variables
numeric_vars <- sapply(data, is.numeric)
num_data <- data[, numeric_vars]

# ----- Option 1: Correlation Analysis -----

cor_matrix <- cor(num_data, use = "pairwise.complete.obs")

# Replace NA correlations with 0 
cor_matrix[is.na(cor_matrix)] <- 0

print(round(cor_matrix, 2))


png("images/correlation_heatmap.png", width = 1000, height = 800, res = 150)

corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8, tl.col = "black",
         title = "Correlation Heatmap")
dev.off()


# Remove highly correlated features
high_cor <- findCorrelation(cor_matrix, cutoff = 0.85, names = TRUE)

cat("\nHighly correlated features to remove:\n")
print(high_cor)

selected_data <- num_data[, !(names(num_data) %in% high_cor)]



# ----- Option 2: Variance Thresholding -----
nzv <- nearZeroVar(selected_data)

cat("\nNear-zero variance features to remove:\n")
print(names(selected_data)[nzv])

selected_data <- selected_data[, -nzv]


cat("\nFinal Selected Features:\n")
print(names(selected_data))






