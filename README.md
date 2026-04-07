# Heart Disease Prediction Analysis

## Project Overview
This project performs comprehensive data analysis on the UCI Heart Disease dataset to identify patterns and risk factors for cardiovascular disease.

## Objectives
- Apply complete data science pipeline (data collection to preprocessing)
- Explore relationships between medical parameters
- Prepare data for machine learning applications

## Dataset Information
- **Source**: Kaggle - UCI Heart Disease Dataset
- **Records**: 920 patients
- **Sources**: 4 medical centers (Cleveland, Hungary, Switzerland, VA Long Beach)

## Technologies Used
- **Language**: R Programming
- **Libraries**: ggplot2, dplyr, corrplot, caret, fastDummies, moments

## Analysis Performed

### Data Understanding
- Dataset loading and structure analysis
- Statistical summary (mean, median, mode, etc.)
- Data type identification

### Exploratory Data Analysis (EDA)
- Univariate analysis (histograms, boxplots)
- Bivariate analysis (scatter plots, correlation)
- Pattern and outlier detection

### Data Preprocessing
- **Missing Values**: Mean/Mode imputation
- **Outliers**: IQR method with capping (Winsorization)
- **Data Conversion**: Label encoding & One-hot encoding
- **Transformation**: Min-Max, Z-score, Log, Sqrt
- **Feature Selection**: Correlation analysis & variance thresholding

## Key Findings
| Finding | Insight |
|---------|---------|
| Age vs Heart Rate | Negative correlation (-0.40) |
| Cholesterol Distribution | Right-skewed with outliers |
| Asymptomatic Patients | 54% show no symptoms |
| Gender Distribution | 78.8% Male, 21.2% Female |


