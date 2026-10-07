# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 6: Multiple regression
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/6.1-multiple-regression.html
# The video walkthrough of this script is linked at the top of that chapter.
#
# How to use this script
# - Save it in your BIO144 RStudio project folder and open it in RStudio.
# - Run it one line (or one command) at a time, from the top, with Ctrl+Enter
#   (Cmd+Enter on a Mac). Look at what each line does before moving on.
# - Lines starting with # are comments: R ignores them. Read them!
# - The datasets are read directly from the internet, so you need to be online.
# - Experiment: change things and re-run them. That is how you learn.
# - Use the document outline (top right of the script pane) to jump between
#   sections.
# - When you reach the end: Session > Restart R, then run the whole script
#   again from the top, to check that it runs without errors.
#
# Code that only draws the teaching figures in the chapter, and code in the
# Extras sections (not examinable), is not included.
# =============================================================================


# 1. Load the packages ----
library(tidyverse)  # read_csv(), dplyr functions, ggplot2
library(ggfortify)  # autoplot() for model diagnostic plots
library(car)        # vif() for collinearity


# 2. An example dataset: read and look at the data ----
# Blood pressure (bp), age (years) and minutes of exercise per week, for 100 people.
bp_data_multreg <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_data_multreg.csv")
head(bp_data_multreg)

# With three variables we can make three scatter plots.
# Response against the first explanatory variable:
ggplot(bp_data_multreg, aes(x = age, y = bp)) +
  geom_point() +
  labs(x = "Age", y = "Blood pressure")
# Response against the second explanatory variable:
ggplot(bp_data_multreg, aes(x = mins_exercise, y = bp)) +
  geom_point() +
  labs(x = "Minutes of exercise", y = "Blood pressure")
# The two explanatory variables against each other:
ggplot(bp_data_multreg, aes(x = age, y = mins_exercise)) +
  geom_point() +
  labs(x = "Age", y = "Minutes of exercise")
# Look at: bp goes up with age, and down with exercise. Age and exercise are
# NOT related to each other. This is important (see section 11, Collinearity).


# 3. Fitting the model ----
# To have two explanatory variables, add the second one with a "+".
m_bp_age_exercise <- lm(bp ~ age + mins_exercise, data = bp_data_multreg)


# 4. Checking the assumptions ----
# The assumptions are the same as for simple regression, and we check them
# with the same four plots. (The chapter uses par() and plot(); in the course
# we use autoplot(). smooth.colour = NA removes the wiggly smooth lines.)
autoplot(m_bp_age_exercise, smooth.colour = NA)
# (A warning "Removed 100 rows containing missing values" comes from
# smooth.colour = NA. You can ignore it.)
# Check: no pattern in Residuals vs Fitted; points close to the line in the
# Normal Q-Q plot; no trend in Scale-Location; no points with high leverage
# AND a large residual. Here all look good.


# 5. Question 1: As an ensemble, are the explanatory variables useful? ----
summary(m_bp_age_exercise)
# Look at the LAST line: "F-statistic: 76.35 on 2 and 97 DF, p-value: < 2.2e-16".
# This F-test tests H0: all slopes are zero (beta_1 = beta_2 = 0).
# Model df = 2 (two slopes). Residual df = 100 - 1 - 2 = 97.


# 6. Question 2: Which variables are associated with the response? ----
# The coefficients table has one t-test for each slope (H0: slope = 0).
summary(m_bp_age_exercise)$coef
# Look at: the Estimate column. age = 0.55 (bp increases by 0.55 mmHg per
# year of age, with exercise held constant). mins_exercise = -0.097
# (bp decreases by about 0.1 mmHg per minute of exercise, with age held constant).

# 95% confidence intervals of the coefficients:
confint(m_bp_age_exercise)
# Check: neither interval includes zero.


# 7. Question 3: What proportion of variability is explained? ----

## Multiple R-squared ----
# R-squared is in summary(); here is how to get just that number:
summary(m_bp_age_exercise)$r.squared
# Look at: about 0.61, so the model explains 61% of the variation in bp.

# Two other ways to get the same number.
# (a) The squared correlation between the observed and the fitted values:
r_squared <- cor(m_bp_age_exercise$fitted.values, bp_data_multreg$bp)^2
r_squared

# (b) SSM / SST, using the sums of squares in the ANOVA table:
anova_bp_age_exercise <- anova(m_bp_age_exercise)
anova_bp_age_exercise
SSM <- anova_bp_age_exercise$`Sum Sq`[1] + anova_bp_age_exercise$`Sum Sq`[2]
SST <- sum(anova_bp_age_exercise$`Sum Sq`)
R_squared <- SSM / SST
R_squared

## Adjusted R-squared ----
# R-squared always increases when we add a variable, even a useless one.
# Let's add a variable of pure random numbers and see.
# (Your numbers will be different each time you run this, because they are random.)
bp_data_multreg$random_variable <- rnorm(nrow(bp_data_multreg))
m_bp_age_exercise_random <- lm(bp ~ age + mins_exercise + random_variable,
                               data = bp_data_multreg)
summary(m_bp_age_exercise)$r.squared
summary(m_bp_age_exercise_random)$r.squared
# Check: the second R-squared is (a little) higher, though the variable is noise.

# Adjusted R-squared corrects for the number of explanatory variables.
# It is also on the second to last line of summary().
summary(m_bp_age_exercise)$adj.r.squared
summary(m_bp_age_exercise_random)$adj.r.squared
# Check: adjusted R-squared usually does not increase (often it decreases)
# when we add the random variable. Use it to compare models with different
# numbers of explanatory variables.


# 8. Question 4: Are some explanatory variables more important than others? ----
# The two slopes have different units (mmHg per year, mmHg per minute), so we
# cannot compare them directly. One way to relate them: how many extra minutes
# of exercise compensate for the bp increase from one extra year of age?
extra_mins_exercise <- coef(m_bp_age_exercise)["age"] / -coef(m_bp_age_exercise)["mins_exercise"]
extra_mins_exercise
# Look at: about 5.7 extra minutes of exercise per week, per year of age.
# coef() gives the estimates; ["age"] picks one by its name.


# 9. Question 5: How do we make predictions? ----
# Predictions for the people in the dataset (the fitted values):
predictions <- predict(m_bp_age_exercise)
head(predictions)

# Predictions for new values. The new data frame must contain ALL the
# explanatory variables, with exactly the same names as in the model.
new_data <- data.frame(age = c(30, 40, 50),
                       mins_exercise = c(50, 100, 150))
new_predictions <- predict(m_bp_age_exercise, newdata = new_data)
new_predictions

## Conditional effects plot ----
# Predicted bp across ages, with exercise held constant at 100 minutes.
new_data_effects <- data.frame(age = seq(20, 80, by = 1),
                               mins_exercise = 100)
new_data_effects <- new_data_effects |>
  mutate(new_predictions_effects = predict(m_bp_age_exercise, newdata = new_data_effects))
ggplot(data = new_data_effects, aes(x = age, y = new_predictions_effects)) +
  geom_line() +
  labs(x = "Age", y = "Predicted blood pressure") +
  ggtitle("Conditional effects plot for\nage (mins_exercise = 100)")

# Now at three levels of exercise. expand.grid() makes a data frame with
# ALL combinations of the values we give it (61 ages x 3 exercise levels = 183 rows).
new_data_effects_3 <- expand.grid(age = seq(20, 80, by = 1),
                                  mins_exercise = c(0, 150, 300))
new_data_effects_3 <- new_data_effects_3 |>
  mutate(prediction = predict(m_bp_age_exercise, newdata = new_data_effects_3),
         mins_exercise_fac = as.factor(mins_exercise))
# We made mins_exercise into a factor so that ggplot uses separate colours
# (not a colour gradient) for the three levels.
ggplot(data = new_data_effects_3, aes(x = age, y = prediction,
                                      col = mins_exercise_fac)) +
  geom_line() +
  labs(x = "Age", y = "Predicted blood pressure",
       color = "Minutes\nof exercise") +
  ggtitle("Conditional effects plot for age\nat three levels of minutes of exercise")
# Look at: the lines are parallel. In this model the effect of age is the
# same at every amount of exercise (Chapter 7 shows models where it is not).


# 10. Reporting ----
# Give effect sizes in meaningful units, with confidence intervals.
# Effect of ten years of age, and of one hour (60 min) of exercise per week:
10 * coef(m_bp_age_exercise)["age"]
10 * confint(m_bp_age_exercise)["age", ]
60 * coef(m_bp_age_exercise)["mins_exercise"]
60 * confint(m_bp_age_exercise)["mins_exercise", ]
nobs(m_bp_age_exercise)   # number of observations used in the model
# Look at: about 5.5 mmHg higher per ten years of age (95% CI 4.5 to 6.6), and
# about 5.8 mmHg lower per extra hour of exercise per week (95% CI 4.5 to 7.1).


# 11. Collinearity ----

## Perfect collinearity ----
# In these data mins_exercise = 100 - age exactly.
bp_data_perfect <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_data_perfect.csv")
m1_age <- lm(bp ~ age, data = bp_data_perfect)
summary(m1_age)
m1_both <- lm(bp ~ mins_exercise + age, data = bp_data_perfect)
summary(m1_both)
# Look at: the estimate for age is now NA. R cannot separate the effects of
# two variables that carry exactly the same information. R-squared is unchanged.

## Collinearity among three explanatory variables ----
# Here x3 is (partly) predictable from x1 and x2.
data_collinear <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/data_collinear.csv")
cor(data_collinear)
m_collinear_123 <- lm(y ~ x1 + x2 + x3, data = data_collinear)
summary(m_collinear_123)$coefficients
m_collinear_12 <- lm(y ~ x1 + x2, data = data_collinear)
summary(m_collinear_12)$coefficients
# Look at: the estimates for x1 and x2 change a lot when x3 is left out
# (x1: 1.47 -> 2.12; x2: -1.92 -> -0.40). This is the instability caused by collinearity.

## Collinearity and interpretation of R-squared ----
data_collinear_strong <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/data_collinear_strong.csv")
cor(data_collinear_strong)
pairs(data_collinear_strong)   # a quick scatter plot of every pair of variables
m2_age <- lm(bp ~ age, data = data_collinear_strong)
m2_mins_exercise <- lm(bp ~ mins_exercise, data = data_collinear_strong)
m2_both <- lm(bp ~ age + mins_exercise, data = data_collinear_strong)
summary(m2_age)$r.squared
summary(m2_mins_exercise)$r.squared
summary(m2_both)$r.squared
# Look at: about 0.25, 0.25 and 0.26. The two variables share the explained
# variance, so the model with both is hardly better than either alone.

## Do I have a problem (with collinearity)? ----
# The variance inflation factor (VIF), from the car package.
# VIF = 1: no collinearity. Above 5 or 10: a problem (rule of thumb).
vif(m_bp_age_exercise)   # about 1.05: no problem
vif(m_collinear_123)     # 1.07, 1.41, 1.46: some, but not severe

# An example with more severe collinearity.
# This makes example data like those in the chapter. You do not need to understand this code.
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- 2 * x1 + 3 * x2 + rnorm(n, 0, 1)  # less noise, more collinearity
y <- 5 + 1.5 * x1 - 2 * x2 + 0.5 * x3 + rnorm(n, 0, 1)
data_collinear_severe <- data.frame(x1 = x1, x2 = x2, x3 = x3, y = y)
m_collinear_severe <- lm(y ~ x1 + x2 + x3, data = data_collinear_severe)
vif(m_collinear_severe)
# Check: the VIFs of x2 and x3 are above 10.


# 12. Assessing the importance of an explanatory variable with collinearity ----
# Compare a model with the focal variable (x1) to a model without it.
# Both models contain all the other explanatory variables.
m_full <- lm(y ~ x1 + x2 + x3, data = data_collinear)
m_reduced <- lm(y ~ x2 + x3, data = data_collinear)
anova(m_reduced, m_full)
# Look at: Res.Df and RSS of each model; "Sum of Sq" = the RSS explained by
# adding x1; the F value (about 150) and its p-value test whether x1 explains
# extra variation once x2 and x3 are already in the model.

# Partial R-squared: share of the variation NOT explained by the other
# variables that x1 explains.
rss1 <- anova(m_reduced, m_full)$"RSS"[1]
rss2 <- anova(m_reduced, m_full)$"RSS"[2]
partial_r2 <- (rss1 - rss2) / rss1
partial_r2   # about 0.61

# Semi-partial R-squared: share of the TOTAL variation explained uniquely by x1.
tss <- sum((data_collinear$y - mean(data_collinear$y))^2)
semi_partial_r2 <- (rss1 - rss2) / tss
semi_partial_r2


# 13. Confounding: why we include other explanatory variables ----
# Survey of gardens: plant height, fertiliser (g/week) and water (litres/week).
# Gardeners who use more fertiliser also water more.
garden_survey <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/garden_survey.csv")
m_fertiliser_only <- lm(height ~ fertiliser, data = garden_survey)
coef(m_fertiliser_only)
m_fertiliser_water <- lm(height ~ fertiliser + water, data = garden_survey)
coef(m_fertiliser_water)
confint(m_fertiliser_water)
# Look at: fertiliser slope 3.5 cm per g without water in the model (biased:
# fertiliser "takes the credit" for water), and 0.89 with water in the model
# (the true value in these simulated data is 1).

# The cost of adjusting: the standard error of the fertiliser slope increases.
summary(m_fertiliser_only)$coefficients
summary(m_fertiliser_water)$coefficients
# Look at: the "Std. Error" of fertiliser, 0.21 -> 0.26.


# Practical toolbox: code in the Unit 6 practical not covered above ----
# The Unit 6 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

## select(): keep only the variables you want ----
bp_three_vars <- bp_data_multreg |>
  select(bp, age, mins_exercise)
head(bp_three_vars)

## Histograms to look at distributions, and log10 transformation ----
# bins = sets the number of bars (with few data, use few bins).
ggplot(bp_data_multreg, aes(x = mins_exercise)) +
  geom_histogram(bins = 10)
# mutate() adds a new, transformed variable (log10() needs values > 0):
bp_data_multreg <- bp_data_multreg |>
  mutate(log10_bp = log10(bp))
ggplot(bp_data_multreg, aes(x = log10_bp)) +
  geom_histogram(bins = 10)

## Residual degrees of freedom ----
# n - (number of explanatory variables + 1 for the intercept) = 100 - 3 = 97.
df.residual(m_bp_age_exercise)

## A 95% confidence interval "by hand" with the t-distribution ----
# estimate +/- t-critical value * standard error. Use qt(0.975, df = residual df),
# not 1.96: with few residual df the t-value is clearly larger than 1.96.
est_age <- summary(m_bp_age_exercise)$coefficients["age", "Estimate"]
se_age <- summary(m_bp_age_exercise)$coefficients["age", "Std. Error"]
t_crit <- qt(0.975, df = df.residual(m_bp_age_exercise))
t_crit
c(est_age - t_crit * se_age, est_age + t_crit * se_age)
# Check: the same as confint(m_bp_age_exercise)["age", ].

## Conditional effects plot with a 95% confidence band ----
# predict(..., interval = "confidence") gives three columns: fit, lwr, upr.
# (interval = "prediction" would give the wider prediction band.)
new_data_band <- data.frame(age = seq(20, 80, by = 1),
                            mins_exercise = mean(bp_data_multreg$mins_exercise))
band <- predict(m_bp_age_exercise, newdata = new_data_band, interval = "confidence")
new_data_band <- cbind(new_data_band, band)
head(new_data_band)
ggplot(new_data_band, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line() +
  labs(x = "Age", y = "Predicted blood pressure\n(exercise at its mean)")

## Standardised coefficients with scale() ----
# scale() subtracts the mean and divides by the standard deviation. The slopes
# are then "SDs of bp per SD of the explanatory variable", so they can be compared.
m_bp_standardised <- lm(scale(bp) ~ scale(age) + scale(mins_exercise),
                        data = bp_data_multreg)
summary(m_bp_standardised)
# Look at: about 0.67 for age and -0.58 for exercise. The t-values and
# p-values are the same as in the unstandardised model.
