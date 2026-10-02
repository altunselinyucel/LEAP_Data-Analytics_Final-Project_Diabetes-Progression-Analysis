# diabetes-progression-final-project
Data Analytics and Statistics Final Project - Diabetes Progression Analysis
Data Analytics and Statistics with Applications

## Overview
This project investigates clinical predictors of one-year diabetes disease progression using an observational dataset of 442 patients. The analysis explores baseline physiological and serum measurements, assesses correlations, and fits both simple and multiple linear regression models to evaluate predictive performance and model limitations.

## Repository Contents
- `diabetes_analysis.R`: Complete and reproducible R analysis script (data preparation, EDA, correlations, regression models, diagnostics).
- `Diabetes_Progression_Analytical_Report.pdf`: 4-6 page analytical report answering all six project questions.
- `Decision_Maker_Presentation.pdf`: Executive presentation designed for health-service decision-makers.
- `diabetes.tab.txt`: Observational dataset containing 442 patient records and 11 variables.

## Key Statistical Findings
- Body mass index (BMI), blood pressure (BP), and serum marker S5 are statistically significant positive predictors of diabetes progression (all p < 0.001).
- The multiple regression model explains 48.01% of the variation in progression (R² = 0.4801, Adjusted R² = 0.4765), substantially improving upon the simple BMI model (R² = 0.3439).
- Multicollinearity among predictors is low (all VIF values < 1.35).
- Because the study design is observational, findings reflect statistical associations rather than proven causal effects.
