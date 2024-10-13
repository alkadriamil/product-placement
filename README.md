# Product Placement Analysis Shiny App

## Overview

This Shiny app is designed to analyze the effectiveness of product placements based on Click-Through Rate (CTR) data. It provides a user-friendly interface for uploading data, visualizing CTR across different placements, and performing various statistical analyses to gain insights into the most effective placement strategies.

## Features

1. **Data Upload**: 
   - Support for CSV and XLSX file formats
   - Data preview and summary statistics

2. **Data Visualization**:
   - Bar plot of mean CTR by placement

3. **Statistical Analyses**:
   - Analysis of Variance (ANOVA)
   - Tukey's Honest Significant Difference (HSD) test
   - Duncan's Multiple Range Test
   - Durbin-Watson test for autocorrelation
   - Bartlett's test for homogeneity of variances
   - Shapiro-Wilk test for normality

4. **Recommendation**:
   - Data-driven suggestion for the most effective placement

## Installation

To run this app locally, you'll need R and the following packages:

```R
install.packages(c("shiny", "shinydashboard", "tidyverse", "readxl", "agricolae", "car", "DT"))
```

## Usage

1. Clone this repository or download the app.R file.
2. Open R or RStudio and set your working directory to the folder containing the app.R file.
3. Run the following command:

```R
shiny::runApp()
```

4. The app will open in your default web browser.

## Data Format

Your input data should be in CSV or XLSX format with at least two columns:

- `placement`: The different placement categories
- `x`: The Click-Through Rate (CTR) values

## How to Use

1. **Upload Data**: 
   - Go to the "Upload Data" tab
   - Click "Choose CSV or XLSX file" and select your data file
   - Click "Load Data" to upload and preview your data

2. **Analyze Data**:
   - Navigate to the "Data Analysis" tab
   - Select the analyses you want to perform
   - Click "Run Analysis" to generate results

3. **Interpret Results**:
   - Review the CTR plot to visualize differences between placements
   - Examine the statistical test results to understand significant differences and data characteristics
   - Read the recommendation for the suggested best placement strategy

## Contributing

Feel free to fork this repository and submit pull requests with any enhancements. For major changes, please open an issue first to discuss what you would like to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)
