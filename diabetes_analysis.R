################################################################################
# FINAL PROJECT SCENARIO 1: UNDERSTANDING DIABETES PROGRESSION
# Starter R script
#
# This script is designed to run from top to bottom.
# It gives you working code for the required analysis, together with many
# comments explaining what each section does.
#
# IMPORTANT
# 1. The code produces results, but your task is to interpret them.
# 2. Do not copy raw R output into your report without explanation.
# 3. Do not treat an association as evidence of causation.
# 4. You may adapt the code and choose different predictors when you can
#    justify your choices.
################################################################################

# ---------------------------------------------------------------------------- #
# 0. SETUP
# ---------------------------------------------------------------------------- #

# The project uses a small number of common packages.
# We use requireNamespace() to check whether each package is available.
# If a package is missing, R will install it before loading it.
needed_packages <- c("ggplot2", "dplyr", "broom")

for (pkg in needed_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(ggplot2)
library(dplyr)
library(broom)

# Make printed numbers a little easier to read.
options(digits = 4)


# ============================================================================ #
# QUESTION 1. UNDERSTAND AND PREPARE THE DATA
# ============================================================================ #

# A local copy of the dataset is supplied with the project.
# Keep diabetes.tab.txt in the same folder as this R file.
# The original public source is:
# https://www4.stat.ncsu.edu/~boos/var.select/diabetes.tab.txt
data_file <- "diabetes.tab.txt"

# If R cannot find the file in the current working directory, choose it manually.
# This avoids making the graded project depend on an external website being online.
if (!file.exists(data_file)) {
  message("diabetes.tab.txt was not found in the current working directory.")
  message("Please choose the supplied diabetes.tab.txt file.")
  data_file <- file.choose()
}

# Read the supplied tab-separated file.
# header = TRUE means that the first row contains the column names.
diabetes <- read.table(data_file, header = TRUE, sep = "\t")

# Convert column names to lower case so they are easier to type consistently.
names(diabetes) <- tolower(names(diabetes))

# The source dataset uses 'y' as the disease-progression outcome.
# Rename it to something clearer for the rest of the analysis.
if ("y" %in% names(diabetes)) {
  diabetes <- diabetes %>% rename(progression = y)
}

# Look at the first few rows.
head(diabetes)

# Number of rows and columns.
dim(diabetes)

# Variable names.
names(diabetes)

# Structure and data types.
str(diabetes)

# The variable 'sex' is numerically coded in the source data, but it represents
# categories rather than a continuous measurement. Check the values present,
# then convert it to a factor so R treats it as categorical in later analyses.
unique(diabetes$sex)
diabetes$sex <- factor(diabetes$sex)

# Count missing values in each variable.
colSums(is.na(diabetes))

# Total number of missing values.
sum(is.na(diabetes))

# Check for duplicated rows.
sum(duplicated(diabetes))

# Basic summary of every variable.
summary(diabetes)

# FOR YOUR REPORT
# - What does one row represent?
# - Which variable is the outcome?
# - Which variables are predictors?
# - Which variables are numerical, and which are categorical in meaning?
# - Are there missing values, duplicated rows or obviously problematic values?
# - Why do health-related data require careful and responsible interpretation?


# ============================================================================ #
# QUESTION 2. EXPLORE THE DATA
# ============================================================================ #

# A helper function for common descriptive statistics.
describe_numeric <- function(x) {
  c(
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE)
  )
}

# Descriptive statistics for the outcome.
describe_numeric(diabetes$progression)

# Descriptive statistics for BMI.
describe_numeric(diabetes$bmi)

# Descriptive statistics for age.
describe_numeric(diabetes$age)

# A compact descriptive table for all numeric variables.
# You do not need to put the whole table in your report.
descriptive_table <- diabetes %>%
  summarise(across(
    where(is.numeric),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      median = ~median(.x, na.rm = TRUE),
      sd = ~sd(.x, na.rm = TRUE),
      min = ~min(.x, na.rm = TRUE),
      max = ~max(.x, na.rm = TRUE)
    )
  ))

print(descriptive_table)

# Distribution of diabetes progression.
ggplot(diabetes, aes(x = progression)) +
  geom_histogram(bins = 25, colour = "white") +
  labs(
    title = "Distribution of diabetes progression",
    x = "Progression measure",
    y = "Count"
  ) +
  theme_minimal()

# Distribution of BMI.
ggplot(diabetes, aes(x = bmi)) +
  geom_histogram(bins = 25, colour = "white") +
  labs(
    title = "Distribution of BMI",
    x = "BMI",
    y = "Count"
  ) +
  theme_minimal()

# One additional useful visualisation: boxplot of progression.
ggplot(diabetes, aes(y = progression)) +
  geom_boxplot() +
  labs(
    title = "Diabetes progression: boxplot",
    y = "Progression measure",
    x = NULL
  ) +
  theme_minimal()

# You may replace or supplement the boxplot with another plot that you find
# more informative. For example, you could examine age, blood pressure or s5.

# FOR YOUR REPORT
# - What do the centre and spread of the main variables look like?
# - Are the distributions approximately symmetric or clearly skewed?
# - Are there unusual observations?
# - Which features of the data matter before modelling begins?


# ============================================================================ #
# QUESTION 3. INVESTIGATE RELATIONSHIPS
# ============================================================================ #

# Keep only genuinely numerical measurement variables for Pearson correlations.
# 'sex' has already been converted to a factor, so it is excluded automatically.
numeric_data <- diabetes %>% select(where(is.numeric))

# Correlation matrix.
cor_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")
round(cor_matrix, 2)

# Correlations of all numeric variables with progression.
# The outcome itself will appear with correlation 1, so focus on the predictors.
progression_correlations <- sort(
  cor_matrix[, "progression"],
  decreasing = TRUE
)
print(progression_correlations)

# Simple heatmap using base R.
# Rowv = NA and Colv = NA keep the variables in their original order instead
# of reordering them with clustering dendrograms.
heatmap(
  cor_matrix,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  symm = TRUE,
  margins = c(7, 7),
  main = "Correlation heatmap"
)

# Scatterplot 1: BMI and progression.
ggplot(diabetes, aes(x = bmi, y = progression)) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Progression versus BMI",
    x = "BMI",
    y = "Progression measure"
  ) +
  theme_minimal()

# Scatterplot 2: blood pressure and progression.
ggplot(diabetes, aes(x = bp, y = progression)) +
  geom_point(alpha = 0.65) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Progression versus blood pressure",
    x = "Blood pressure",
    y = "Progression measure"
  ) +
  theme_minimal()

# You may replace the second scatterplot with another relationship that looks
# more informative after examining the correlation matrix.

# FOR YOUR REPORT
# - Which predictors are most strongly associated with progression?
# - Which predictors are strongly related to each other?
# - Could some predictors carry overlapping information?
# - Which variables look most promising for regression analysis, and why?


# ============================================================================ #
# QUESTION 4. BUILD AND INTERPRET REGRESSION MODELS
# ============================================================================ #

# ---- Simple linear regression ------------------------------------------------

# BMI is used as the starting predictor because it is meaningful and commonly
# examined in this dataset. You may choose another predictor if you justify it.
simple_model <- lm(progression ~ bmi, data = diabetes)

# Standard model summary.
summary(simple_model)

# Tidy coefficient table, including confidence intervals.
tidy(simple_model, conf.int = TRUE)

# Model-level statistics such as R-squared and adjusted R-squared.
glance(simple_model)

# Coefficients for writing the fitted equation.
coef(simple_model)

# FOR YOUR REPORT
# - Write the fitted equation.
# - Interpret the slope in context.
# - Does the intercept have a useful real-world interpretation?
# - What does R-squared mean here?


# ---- Multiple regression -----------------------------------------------------

# This is a ready-to-run starting model using three predictors.
# You may change the predictor set after examining your EDA and correlations.
multiple_model <- lm(
  progression ~ bmi + bp + s5,
  data = diabetes
)

summary(multiple_model)

tidy(multiple_model, conf.int = TRUE)

glance(multiple_model)

# Compare the simple and multiple models using measures taught in the course.
# R-squared shows explained variation, while adjusted R-squared takes model
# complexity into account when several predictors are used.
model_comparison <- bind_rows(
  glance(simple_model) %>% mutate(model = "Simple: BMI"),
  glance(multiple_model) %>% mutate(model = "Multiple: BMI + BP + S5")
) %>%
  select(model, r.squared, adj.r.squared, sigma)

print(model_comparison)

# FOR YOUR REPORT
# - Interpret at least two coefficients from the multiple model.
# - Use the phrase 'holding the other predictors constant' correctly.
# - Compare the simple and multiple models.
# - Do not assume that the model with the larger R-squared is automatically
#   better in every respect.


# ============================================================================ #
# QUESTION 5. EVALUATE THE ANALYSIS
# ============================================================================ #

# ---- Residuals versus fitted values -----------------------------------------

# augment() creates fitted values and residuals in a convenient data frame.
diagnostic_data <- augment(multiple_model)

ggplot(diagnostic_data, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.65) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Residuals versus fitted values",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

# Look for obvious structure rather than trying to prove that every assumption
# is perfectly satisfied. A strong curve or a clear change in spread would be
# warning signs for a simple linear model.


# ---- Multicollinearity using VIF --------------------------------------------

# VIF can be calculated without installing another package.
manual_vif <- function(model) {
  X <- model.matrix(model)[, -1, drop = FALSE]  # remove intercept

  vif_values <- sapply(seq_len(ncol(X)), function(i) {
    target <- X[, i]
    others <- X[, -i, drop = FALSE]

    if (ncol(others) == 0) {
      return(1)
    }

    r2 <- summary(lm(target ~ others))$r.squared
    1 / (1 - r2)
  })

  setNames(vif_values, colnames(X))
}

manual_vif(multiple_model)

# There is no single universal VIF cut-off. Interpret the values together with
# the correlation matrix and with the purpose of the model.


# ---- Statistical uncertainty ------------------------------------------------

# A tidy table with estimates, standard errors, t-values, p-values and 95%
# confidence intervals.
coef_table <- tidy(multiple_model, conf.int = TRUE)
print(coef_table)

# You can also obtain the confidence intervals directly.
confint(multiple_model, level = 0.95)

# FOR YOUR REPORT
# - Comment on the residuals-versus-fitted plot.
# - Use VIF and the correlation matrix to discuss multicollinearity.
# - For at least two important coefficients, discuss the estimate and either
#   the confidence interval, t-value or p-value.
# - Distinguish statistical significance from practical importance.
# - State the main statistical weaknesses or uncertainties in the analysis.


# ============================================================================ #
# QUESTION 6. DRAW APPROPRIATE CONCLUSIONS
# ============================================================================ #

# No new code is required for this question.
# Use the results you have already produced and think critically about what they
# do and do not support.

# FOR YOUR REPORT
# Consider the following points:
# - Is this dataset experimental or observational?
# - Can the regression coefficients be interpreted causally?
# - Could there be confounding variables that are not included in the dataset?
# - Could measurement issues affect the results?
# - Would the results necessarily generalise to another population?
# - Which conclusions are supported by the evidence?
# - Which claims would go beyond the evidence?


# ============================================================================ #
# SEPARATE DELIVERABLE: DECISION-MAKER PRESENTATION
# ============================================================================ #

# The presentation is NOT another analysis section and does not require new R
# code. It should communicate the most important results from the report to a
# health-service decision-maker who is not a statistician.
#
# Suggested 4 to 6 slide structure:
# 1. The question and the data
# 2. The most important patterns in the data
# 3. The main regression findings
# 4. What the results mean in practical terms
# 5. The main uncertainty or limitation
# 6. What should and should not be concluded
#
# Use only a small number of well-chosen figures or statistics. Avoid raw R
# output and unnecessary technical notation.

################################################################################
# END OF STARTER SCRIPT
################################################################################
