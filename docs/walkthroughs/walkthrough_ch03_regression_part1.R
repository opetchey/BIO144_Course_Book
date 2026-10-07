# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 3: Regression Part 1
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/3.1-regression-part1.html
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
# Load all packages at the top of a script, so it is clear what is needed.
library(tidyverse)  # readr, dplyr, ggplot2 and more
library(ggfortify)  # autoplot() for model checking plots
library(patchwork)  # put several ggplots side by side with + and /


# 2. An example: blood pressure and age ----
# Read the data directly from the course book website.
bp_age_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/Simulated_Blood_Pressure_and_Age_Data.csv")

# Always look at the data first.
bp_age_data
glimpse(bp_age_data)
# Check: 100 rows (people) and 2 columns, Age and Systolic_BP.

# Remove any rows with missing values (NA). Here there are none, so the
# number of rows stays at 100. It is still a good habit to check.
bp_age_data <- na.omit(bp_age_data)
nrow(bp_age_data)

# Graph the data, with the question in mind: how does blood pressure change
# with age? The response (Systolic_BP) goes on the y-axis.
ggplot(bp_age_data, aes(x = Age, y = Systolic_BP)) +
  geom_point() +
  labs(x = "Age (years)", y = "Systolic Blood Pressure (mmHg)")
# Before fitting a model, guess the intercept and the slope from this graph.


# 3. Calculating the intercept and slope: let's do it in R ----
# Fit the linear regression with lm() ("linear model").
# Formula: response ~ explanatory variable. data = the data frame.
bp_age_model <- lm(Systolic_BP ~ Age, data = bp_age_data)

# Look at the results.
summary(bp_age_model)
# Look at: the "Coefficients" table.
#  - "(Intercept)" row, "Estimate" column: the intercept (about 99 mmHg).
#  - "Age" row, "Estimate" column: the slope (about 0.82 mmHg per year).
#  - "Residual standard error: ... on 98 degrees of freedom": 100 data points
#    minus 2 estimated parameters (intercept and slope) = 98.
# How close were your guesses? (We interpret the rest of this output in Chapter 4.)

# Optional check of the least squares formulas (see "Least squares estimates"):
# slope = cov(x, y) / var(x), intercept = mean(y) - slope * mean(x)
slope_by_hand <- cov(bp_age_data$Age, bp_age_data$Systolic_BP) / var(bp_age_data$Age)
intercept_by_hand <- mean(bp_age_data$Systolic_BP) - slope_by_hand * mean(bp_age_data$Age)
c(intercept_by_hand, slope_by_hand)
# Check: the same numbers as in summary().


# 4. Dealing with the error: observed, expected and residuals ----
# The observed values of the response are in the data:
head(bp_age_data$Systolic_BP)

# coef() gives the intercept [1] and the slope [2].
coef(bp_age_model)

# Expected (fitted) value = intercept + slope * Age. Add it as a new column.
bp_age_data <- bp_age_data |>
  mutate(Expected_BP = coef(bp_age_model)[1] + coef(bp_age_model)[2] * Age)

# Residual = observed - expected.
bp_age_data <- bp_age_data |>
  mutate(Residuals = Systolic_BP - Expected_BP)

# Easier: fitted() and residuals() get the same numbers from the model.
bp_age_data <- bp_age_data |>
  mutate(Expected_BP = fitted(bp_age_model),
         Residuals = residuals(bp_age_model))
head(bp_age_data)
# Check: Expected_BP + Residuals = Systolic_BP for every row.


# 5. Is the model good enough to use? (a) Normally distributed residuals ----
# The assumptions are about the RESIDUALS, not about the raw data.
# First, a histogram of the residuals: is it roughly symmetric and bell-shaped?
ggplot(bp_age_data, aes(x = Residuals)) +
  geom_histogram(bins = 10)

# Better: the QQ-plot. Sample quantiles of the residuals against theoretical
# quantiles of a normal distribution.
qqnorm(residuals(bp_age_model))
qqline(residuals(bp_age_model))
# Look at: the points should lie close to the line. Small wiggles at the
# ends are normal. Systematic curves (S-shape, banana shape) are a warning.

# What is a quantile? The 0.5 quantile is the median: half the values are
# below it. Here, 21 made-up blood pressure values:
set.seed(1)
bp_21 <- tibble(Blood_Pressure = round(rnorm(21, 120, 20), 1))
quantile(bp_21$Blood_Pressure, 0.5)
median(bp_21$Blood_Pressure)  # the same value

# How does a "good" QQ-plot look? Make some from truly normal random numbers.
# Run this several times: every plot is from a normal distribution, so this
# amount of wiggle is not a problem.
sim_values <- rnorm(100)
qqnorm(sim_values)
qqline(sim_values)


# 6. (b) Constant error variance (homoscedasticity) ----
# Calculate the square root of the absolute standardised residuals, by hand.
# sigma_hat is the residual standard error (divide by n - 2, see chapter).
bp_age_data <- bp_age_data |>
  mutate(fitted = predict(bp_age_model),
         residuals = residuals(bp_age_model),
         sigma_hat = sqrt(sum(residuals^2) / (n() - 2)),
         R_i = residuals / sigma_hat,
         sqrt_abs_R_i = sqrt(abs(R_i)))

# The scale-location plot: size of residuals against fitted values.
ggplot(bp_age_data, aes(x = fitted, y = sqrt_abs_R_i)) +
  geom_point() +
  labs(x = "Fitted values", y = expression(sqrt(abs(R[i]))))
# Check: there should be no trend. Here there is none, so variance looks constant.

# The same plot, made by autoplot() (plot number 3 of the model checking plots):
autoplot(bp_age_model, which = 3, smooth.colour = NA)
# (smooth.colour = NA hides the smoothed line. Ignore the warning it gives.)

# How it looks when the variance increases with the fitted values.
# This makes example data like those in the chapter. You do not need to
# understand the simulation code.
set.seed(2)
hetero_data <- tibble(x = runif(100, 1, 10)) |>
  mutate(y = 100 + 5 * x + 2 * x * rnorm(100, 0, 1))
ggplot(hetero_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
m_hetero <- lm(y ~ x, data = hetero_data)
autoplot(m_hetero, which = 3)
# Look at: the points (and the blue line) go up from left to right. The
# residuals get bigger as the fitted values get bigger: not constant variance.


# 7. (c) Independence ----
# There is no code for this. Think about the study design: is each row a
# different, independent unit (here, a different person)? Compare the number
# of rows with the number of units of observation.


# 8. (d) Linearity ----
# The blood pressure data look linear:
ggplot(bp_age_data, aes(x = Age, y = Systolic_BP)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)

# Example data with a curved relationship (made by simulation).
set.seed(1)
curved_data <- tibble(x = runif(100, 1, 10)) |>
  mutate(y = 2 * x + rnorm(100, 0, 2) + 0.5 * x^2)
m_curved <- lm(y ~ x, data = curved_data)

# Show the residuals as red vertical lines from the points to the line.
ggplot(curved_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  geom_segment(aes(xend = x, yend = fitted(m_curved)), color = "red")

# Residuals against fitted values, made by hand with ggplot:
ggplot(mapping = aes(x = fitted(m_curved), y = residuals(m_curved))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
# Look at: a U-shape. The straight line is too high in the middle and too
# low at the ends. This means the relationship is not linear.

# The same plot with autoplot() (plot 1, "Residuals vs Fitted"):
autoplot(m_curved, which = 1)
# And for the blood pressure model, without the smoothed line:
autoplot(bp_age_model, which = 1, smooth.colour = NA)
# Check: no pattern, points scattered evenly around zero. Good.


# 9. (e) No outliers: leverage and influence ----
# Add one extreme, made-up data point (age 80, blood pressure 120).
bp_age_data_outlier <- bp_age_data |>
  select(Age, Systolic_BP) |>
  bind_rows(tibble(Age = 80, Systolic_BP = 120))
m_bp_age_outlier <- lm(Systolic_BP ~ Age, data = bp_age_data_outlier)

# Compare the line with the outlier (blue) and without it (red).
ggplot(bp_age_data_outlier, aes(x = Age, y = Systolic_BP)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  geom_smooth(formula = y ~ x, data = bp_age_data, method = "lm",
              se = FALSE, color = "red") +
  xlim(10, 85) +
  ylim(100, 180)
coef(bp_age_model)
coef(m_bp_age_outlier)
# Look at: the slope changes from 0.82 to 0.78 mmHg per year. The point has
# high leverage (far from the mean age) and a big residual, but the 100 other
# points hold the line in place, so its influence is moderate.

# Residuals vs leverage plot (plot 5). Check the top and bottom right corners.
autoplot(m_bp_age_outlier, which = 5)
# Look at: observation 101 (the added point) is far to the right (high
# leverage) and far from zero (big residual).

# The same plot for the original model:
autoplot(bp_age_model, which = 5, smooth.colour = NA)
# Look at: observations 71, 85 and 87 are labelled, but none is far out.


# 10. The autoplot() function from the ggfortify package ----
# All four model checking plots at once. This is what you will usually do.
autoplot(bp_age_model, smooth.colour = NA)
# smooth.colour = NA removes the smoothed lines. It causes a warning message
# "Removed 100 rows containing missing values". You can ignore it.


# 11. Dealing with non-linearity: transformations ----
# This makes example data like those in the chapter (a power law with noise).
# You do not need to understand this code.
set.seed(3)
nonlinear_data <- tibble(x = runif(50)) |>
  mutate(y = exp(log(0.1) + 0.5 * log(x) + rnorm(50, 0, 0.1)))

# Make transformed versions of the variables with mutate().
# log() is the natural logarithm. sqrt() is the square root.
nonlinear_data <- nonlinear_data |>
  mutate(log_y = log(y),
         log_x = log(x),
         sqrt_y = sqrt(y))

# Untransformed: data with line (p1) and residuals vs fitted (p2).
p1 <- ggplot(nonlinear_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
m_untransformed <- lm(y ~ x, data = nonlinear_data)
p2 <- ggplot(mapping = aes(x = fitted(m_untransformed), y = residuals(m_untransformed))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess")
p1 + p2   # patchwork: + puts plots side by side
# Look at: the curve in the residuals. Not linear.

# Square root of y:
p3 <- ggplot(nonlinear_data, aes(x = x, y = sqrt_y)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
m_sqrt_y <- lm(sqrt_y ~ x, data = nonlinear_data)
p4 <- ggplot(mapping = aes(x = fitted(m_sqrt_y), y = residuals(m_sqrt_y))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess")

# Log of y:
p5 <- ggplot(nonlinear_data, aes(x = x, y = log_y)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
m_log_y <- lm(log_y ~ x, data = nonlinear_data)
p6 <- ggplot(mapping = aes(x = fitted(m_log_y), y = residuals(m_log_y))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess")
(p1 + p2) / (p3 + p4) / (p5 + p6)   # patchwork: / puts plots on top of each other
# Look at: still a pattern in the residuals after both transformations of y.

# Log of y AND log of x:
p7 <- ggplot(nonlinear_data, aes(x = log_x, y = log_y)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)
m_log_log <- lm(log_y ~ log_x, data = nonlinear_data)
p8 <- ggplot(mapping = aes(x = fitted(m_log_log), y = residuals(m_log_log))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(formula = y ~ x, method = "loess")
p7 + p8
# Look at: now the relationship is linear and the residuals show no pattern.

# Always check all the model checking plots of the model you choose:
autoplot(m_log_log)
# Important: choose the transformation BEFORE you look at p-values (no p-hacking).


# 12. Interpreting a model with log-transformed variables ----
# In a log-log model the slope is a "power": doubling x multiplies y by 2^slope.
coef(m_log_log)
2^coef(m_log_log)[2]
# Look at: the slope is close to 0.5, the value used to simulate the data. So
# doubling x multiplies y by about 2^0.5 = 1.41, i.e. about 41% larger.

# To get predictions back on the original scale, back-transform with exp().
exp(predict(m_log_log, newdata = tibble(log_x = log(c(0.25, 0.5)))))


# Practical toolbox: code in the Unit 3 practical not covered above ----
# The Unit 3 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

# (1) Wrangling with filter(), select() and drop_na(), then counting rows.
# A tiny made-up dataset with some missing values (NA):
tiny_data <- tibble(site = c("A", "B", "C", "D", "E"),
                    year = c(2012, 2013, 2013, 2013, 2013),
                    mass = c(10, 25, NA, 400, 12),
                    length = c(3, 6, 7, 30, NA),
                    colour = c("red", NA, "blue", "red", "blue"))
tiny_2013 <- tiny_data |>
  filter(year == 2013) |>             # keep only rows from 2013
  select(site, mass, length) |>       # keep only the columns we need
  drop_na()                           # remove rows with NA in these columns
tiny_2013
nrow(tiny_2013)  # number of data points available for the analysis
# Note: select() BEFORE drop_na(), so that NAs in columns you do not need
# (here colour) do not remove rows.

# (2) Look at the distribution of a variable, and a log10 transformation.
# Right-skewed data (many small values, a few large) often look better on a
# log scale. log10() is the logarithm to base 10.
ggplot(tiny_2013, aes(x = mass)) +
  geom_histogram(bins = 5)
tiny_2013 <- tiny_2013 |>
  mutate(log10_mass = log10(mass),
         log10_length = log10(length))
tiny_2013
10^2  # back-transform: a log10 value of 2 is a raw value of 100

# (3) Extend the axes to zero with xlim() and ylim(), to help guess the intercept.
ggplot(bp_age_data, aes(x = Age, y = Systolic_BP)) +
  geom_point() +
  xlim(0, 80) +
  ylim(0, 180)

# (4) Cook's distances: how much each point changes the fitted line.
# sort(..., decreasing = TRUE)[1:5] shows the five largest values.
sort(cooks.distance(bp_age_model), decreasing = TRUE)[1:5]
# Look at: all values are far below the common warning thresholds of 0.5 or 1.

# (5) QQ-plots made with ggplot, and putting plots side by side.
p_qq <- ggplot(bp_age_data, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line()
p_hist <- ggplot(bp_age_data, aes(x = Residuals)) +
  geom_histogram(bins = 10)
p_hist + p_qq   # or patchwork::wrap_plots(p_hist, p_qq)

# (6) Random numbers from different distributions, to explore QQ-plots.
# The first number is how many values to draw. The others are parameters.
example_values <- tibble(
  normal = rnorm(100, mean = 0, sd = 1),
  uniform = runif(100, min = 0, max = 1),
  beta = rbeta(100, 2, 2),            # change the two numbers to change the shape
  lognormal = rlnorm(100, meanlog = 0, sdlog = 1),
  poisson = rpois(100, lambda = 5),   # counts
  binomial = rbinom(100, size = 10, prob = 0.3)  # successes out of 10
)
ggplot(example_values, aes(x = lognormal)) +
  geom_histogram(bins = 20)
qqnorm(example_values$lognormal)
qqline(example_values$lognormal)
# Try the other columns too, and compare their histograms and QQ-plots.
