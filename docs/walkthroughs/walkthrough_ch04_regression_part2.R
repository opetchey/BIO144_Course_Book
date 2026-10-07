# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 4: Regression Part 2
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/4.1-regression-part2.html
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
library(tidyverse)  # readr, dplyr, ggplot2 and more
library(ggfortify)  # autoplot() for model checking plots


# 2. Read the data and fit the model (from Chapter 3) ----
bp_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/Simulated_Blood_Pressure_and_Age_Data.csv")
bp_data

bp_age_model <- lm(Systolic_BP ~ Age, data = bp_data)

# We interpret a model only AFTER checking its assumptions (Chapter 3).
autoplot(bp_age_model)
# Check: no worrying patterns, so we can go on and interpret the model.


# 3. How good is the regression model (R squared)? ----
# First with a small example dataset, like the one in the chapter.
# This makes example data. You do not need to understand the rnorm() part.
set.seed(1233)
x <- 1:10
y1 <- 2 * x + rnorm(10, 0, 1)

# SST: total sum of squares. Squared distances of the data from their mean.
SST <- sum((y1 - mean(y1))^2)
SST

# SSM: model sum of squares. Squared distances of the predicted values
# (on the regression line) from the mean.
m_sd1 <- lm(y1 ~ x)
y1_predicted <- predict(m_sd1)
SSM <- sum((y1_predicted - mean(y1))^2)
SSM

# SSE: error sum of squares. Squared residuals (data minus predicted values).
SSE <- sum((y1 - y1_predicted)^2)
SSE
# Or, because SST = SSM + SSE:
SST - SSM

# R squared: the proportion of the variation in y explained by the model.
R.squared <- SSM / SST
R.squared
# Check: the same as "Multiple R-squared" in the summary() output.
summary(m_sd1)$r.squared

# Where is R squared in the regression output? Now with the blood pressure model:
summary(bp_age_model)
# Look at: "Multiple R-squared: 0.9002". The model explains about 90% of the
# variation in systolic blood pressure.


# 4. Is the relationship statistically significant? ----
# Null hypothesis H0: slope = 0. Alternative H1: slope is not 0.

# The estimate and its standard error (columns 1 and 2 of the coefficients table):
summary(m_sd1)$coefficients[1:2, 1:2]

# The t-statistic = estimate / standard error.
bp_age_coefs <- summary(bp_age_model)$coefficients
bp_age_coefs
t_slope <- bp_age_coefs[2, 1] / bp_age_coefs[2, 2]
t_slope
# Check: the same as the "t value" in the Age row (about 29.7).

# The p-value: the probability of a t-value at least this far from zero, if
# H0 were true. It uses a t-distribution with n - 2 degrees of freedom.
# pt() gives the area in one tail; times 2 for both tails (two-tailed test).
n <- 100
t_value <- 1.5
p_value <- 2 * pt(-abs(t_value), df = n - 2)
p_value
# Look at: about 0.14. A t-value of 1.5 is quite likely by chance.

t_value <- 2.5
p_value <- 2 * pt(-abs(t_value), df = n - 2)
p_value
# Look at: about 0.014. A bigger t-value gives a smaller p-value.

# For the blood pressure model:
2 * pt(-abs(t_slope), df = n - 2)
# Look at: a tiny number. summary() prints it as "<2e-16" in the "Pr(>|t|)"
# column. There is very strong evidence that blood pressure is associated with age.

# The residual degrees of freedom can also be taken from the model:
df.residual(bp_age_model)


# 5. Confidence interval of the slope ----
# 95% CI = estimate +/- critical t-value * standard error.
# The critical t-value is the 0.975 quantile of the t-distribution.
t_0975 <- qt(0.975, df = n - 2)
t_0975
# Look at: about 1.98. For very many degrees of freedom it gets close to 1.96.
half_interval <- t_0975 * summary(bp_age_model)$coef[2, 2]
lower_limit <- coef(bp_age_model)[2] - half_interval
upper_limit <- coef(bp_age_model)[2] + half_interval
ci_slope <- c(lower_limit, upper_limit)
slope <- coef(bp_age_model)[2]
slope
ci_slope

# Easier and safer: confint() does the whole calculation.
ci_slope_2 <- confint(bp_age_model, level = 0.95)[2, ]
ci_slope_2

# confint() without [2, ] gives the CIs of all coefficients:
confint(bp_age_model)

# The same, using values from the coefficients table:
beta <- bp_age_coefs[2, 1]
sdbeta <- bp_age_coefs[2, 2]
beta + c(-1, 1) * qt(0.975, df = df.residual(bp_age_model)) * sdbeta
# Look at: slope 0.82 mmHg per year, 95% CI 0.77 to 0.88. Zero is far
# outside the interval.


# 6. Confidence band ----
# Make a data frame with new values of the explanatory variable. The column
# name must be the same as in the model (Age).
new_data <- tibble(Age = seq(20, 80, by = 1))

# predict() with interval = "confidence" gives the fitted value (fit) and the
# lower (lwr) and upper (upr) limits of the 95% confidence band.
conf_band <- cbind(new_data,
                   predict(bp_age_model, newdata = new_data,
                           interval = "confidence", level = 0.95))
head(conf_band)

ggplot(bp_data, aes(x = Age, y = Systolic_BP)) +
  geom_point() +
  geom_line(aes(y = fit), data = conf_band, color = "purple", linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, y = fit), data = conf_band, alpha = 0.3) +
  labs(x = "Age (years)", y = "Systolic BP (mmHg)")
# Look at: the band is narrow, and a little wider at the ends of the data.


# 7. Prediction band ----
# interval = "prediction" also includes the scatter of single observations
# around the line, so the band is much wider.
prediction_band <- cbind(new_data,
                         predict(bp_age_model, newdata = new_data,
                                 interval = "prediction", level = 0.95))

ggplot(bp_data, aes(x = Age, y = Systolic_BP)) +
  geom_point() +
  geom_line(aes(y = fit), data = prediction_band, color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, y = fit), data = prediction_band,
              alpha = 0.3, fill = "green") +
  geom_ribbon(aes(ymin = lwr, ymax = upr, y = fit), data = conf_band,
              alpha = 0.3, fill = "purple") +
  labs(x = "Age (years)", y = "Systolic BP (mmHg)")

# Predictions for a few chosen ages:
some_ages <- tibble(Age = c(30, 50, 70))
predict(bp_age_model, newdata = some_ages, interval = "confidence")
predict(bp_age_model, newdata = some_ages, interval = "prediction")
# Look at: for age 50 the expected (mean) blood pressure is about 140 mmHg,
# 95% CI 139 to 141. But a single new 50-year-old could be anywhere from
# about 130 to 150 mmHg (the prediction interval).


# 8. Extrapolation: predicting beyond the data ----
# The model only describes the range of ages in the data:
range(bp_data$Age)
# Look at: 21 to 79 years.

# The intercept is the predicted blood pressure at age 0 (a newborn baby!).
# That is far outside the data, so it has no biological meaning.
coef(bp_age_model)[1]

# Centring the explanatory variable makes the intercept meaningful.
# I() tells R to calculate Age - 50 inside the formula.
m_bp_age_centred <- lm(Systolic_BP ~ I(Age - 50), data = bp_data)
coef(m_bp_age_centred)
# Look at: the slope is unchanged. The intercept (about 140 mmHg) is now the
# expected blood pressure of a 50-year-old.


# 9. Reporting ----
# A figure for a report: data, regression line and 95% confidence band.
# geom_smooth(method = "lm") fits the same model and draws the band for you.
ggplot(bp_data, aes(x = Age, y = Systolic_BP)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE, level = 0.95) +
  labs(x = "Age (years)", y = "Systolic BP (mmHg)")
# In a report, use a figure caption instead of a title.

# The numbers for the Results sentence:
coef(bp_age_model)[2]             # slope (effect size)
confint(bp_age_model)[2, ]        # its 95% confidence interval
df.residual(bp_age_model)         # degrees of freedom for error
bp_age_coefs[2, "t value"]        # t-value
summary(bp_age_model)$r.squared   # R squared
# Example sentence: "Systolic blood pressure was positively associated with
# age (slope = 0.82 mmHg per year, 95% CI [0.77, 0.88], degrees of freedom =
# 98, t = 29.7, p < 0.001). The model explained 90% of the variability."

# How big is the effect? Express it over a meaningful change, e.g. 10 years:
10 * coef(bp_age_model)[2]
10 * confint(bp_age_model)[2, ]
# Look at: about 8.2 mmHg per 10 years (95% CI 7.7 to 8.8).
# Remember: these are observational data, so say "associated with", not "caused".


# 10. Relationship between R squared and the p-value of the slope ----
# With many data points, even a very weak relationship can have a small p-value.
# This makes example data. You do not need to understand the rnorm() part.
set.seed(123)
n_big <- 2000
weak_data <- tibble(x = 1:n_big) |>
  mutate(y = 0.01 * x + rnorm(n_big, 0, 90))
ggplot(weak_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
m_weak <- lm(y ~ x, data = weak_data)
summary(m_weak)
# Look at: Multiple R-squared is about 0.006 (less than 1% explained), but
# the p-value of the slope is about 0.0004. Small p does not mean strong!


# Practical toolbox: code in the Unit 4 practical not covered above ----
# The Unit 4 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

# (1) The correlation coefficient r, and how it relates to R squared.
r_bp_age <- cor(bp_data$Age, bp_data$Systolic_BP)
r_bp_age
r_bp_age^2   # in simple regression, r squared = R squared (0.90 here)
# r has a sign (direction of the relationship). R squared does not.

# (2) Critical t-values for different degrees of freedom.
qt(0.975, df = 8)     # few degrees of freedom: about 2.31
qt(0.975, df = 1000)  # many degrees of freedom: close to 1.96

# (3) A model with log10-transformed variables: predict, then back-transform.
# Tiny made-up example: body mass (g) and metabolic rate of 12 animals in
# two groups. You do not need to understand the simulation code.
set.seed(4)
animal_data <- tibble(group = rep(c("bird", "mammal"), each = 6),
                      species = paste0("sp", 1:12),
                      body_mass = round(10^runif(12, 0, 4), 1)) |>
  mutate(metabolic_rate = round(0.5 * body_mass^0.7 * 10^rnorm(12, 0, 0.1), 2),
         log10_body_mass = log10(body_mass),
         log10_metabolic_rate = log10(metabolic_rate))
m_animal <- lm(log10_metabolic_rate ~ log10_body_mass, data = animal_data)

# New values must be given on the log10 scale, because the model uses log10.
new_mass <- tibble(log10_body_mass = log10(c(10, 1000)))
pred_conf <- predict(m_animal, newdata = new_mass, interval = "confidence")
pred_pred <- predict(m_animal, newdata = new_mass, interval = "prediction")
10^pred_conf   # back-transform to the original scale with 10^
10^pred_pred

# (4) Back-transformed regression line and band on the raw (untransformed) axes.
# Predict over a sequence of values from the smallest to the largest mass.
line_data <- tibble(body_mass = seq(min(animal_data$body_mass),
                                    max(animal_data$body_mass),
                                    length.out = 100)) |>
  mutate(log10_body_mass = log10(body_mass))
line_preds <- predict(m_animal, newdata = line_data, interval = "confidence")
line_data <- line_data |>
  mutate(fit = 10^line_preds[, "fit"],   # [, "fit"] takes one column
         lwr = 10^line_preds[, "lwr"],
         upr = 10^line_preds[, "upr"])
ggplot() +
  geom_point(data = animal_data, aes(x = body_mass, y = metabolic_rate)) +
  geom_line(data = line_data, aes(x = body_mass, y = fit), color = "blue") +
  geom_ribbon(data = line_data, aes(x = body_mass, ymin = lwr, ymax = upr), alpha = 0.2) +
  labs(x = "Body mass (g)", y = "Metabolic rate")
# Look at: a straight line on log-log axes is a curve on the raw axes.

# (5) A publication-quality graph: log axes, colour by group, point labels.
# scale_x_log10() / scale_y_log10() show raw values on log-spaced axes.
ggplot(animal_data, aes(x = body_mass, y = metabolic_rate)) +
  geom_smooth(formula = y ~ x, method = "lm", color = "black") +
  geom_point(aes(colour = group), size = 2.5) +    # colour gives a legend (key)
  geom_text(aes(label = species), size = 3, vjust = -0.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Body mass (g, log scale)", y = "Metabolic rate (log scale)",
       colour = "Group") +
  theme_bw(base_size = 14)
# Note: with scale_x_log10() the line is fitted to log10 values, as in m_animal.

# (6) Find the largest value of a variable (to know the range of the data):
max(animal_data$body_mass)
