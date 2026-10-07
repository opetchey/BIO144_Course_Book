# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 8: Count data (GLM part 1)
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/8.1-GLM1-count-data.html
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

library(tidyverse)  # read_csv(), ggplot2, dplyr, and more
library(ggfortify)  # autoplot() for model diagnostic plots
library(car)        # vif() (only used in the Practical toolbox section)


# 2. Example: Soay sheep - read and look at the data ----

# Does the body mass of female Soay sheep (body.size, kg) affect their
# lifetime reproductive success (fitness = number of offspring)?
soay <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/soay_sheep.csv")

# Look at the first rows. Check: fitness is a count (0, 1, 2, ...).
head(soay)

# As always, start with a graph of the data.
ggplot(soay, aes(x = body.size, y = fitness)) +
  geom_point(alpha = 0.4) +
  labs(x = "Body mass (kg)", y = "Lifetime fitness")
# Look at: the spread of fitness gets larger as body mass increases,
# and the relationship looks curved. Both are typical of count data.


# 3. The wrong analysis: a linear model ----

# First we (wrongly) treat the counts as if they were continuous and normal.
mod_soay_lm <- lm(fitness ~ body.size, data = soay)

# Model checking plots (the first four: residuals vs fitted, QQ-plot,
# scale-location, Cook's distance).
autoplot(mod_soay_lm, which = 1:4, add.smooth = TRUE)
# Look at: a curved pattern in residuals vs fitted (non-linearity), and
# increasing spread in the scale-location plot (variance not constant).

# The fitted straight line. Look at: at small body mass the line goes below
# zero, i.e. it predicts negative numbers of offspring, which is impossible.
ggplot(soay, aes(x = body.size, y = fitness)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Body mass (kg)", y = "Lifetime fitness")


# 4. From LM to GLM ----

# A linear model is a GLM with a normal (gaussian) family and identity link.
# glm() needs the family argument.
mod_soay_glm <- glm(fitness ~ body.size, data = soay,
                    family = gaussian(link = "identity"))
summary(mod_soay_glm)

# Compare with the lm() summary.
summary(mod_soay_lm)
# Check: the Coefficients tables are identical. The bottom parts differ,
# because glm() uses maximum likelihood (and reports deviance, AIC).


# 5. Poisson GLM: the Poisson distribution ----

# dpois(y, lambda) gives the probability of observing count y when the
# mean (expected count) is lambda.
dpois(3, lambda = 5)   # probability of a count of 3
dpois(5, lambda = 5)   # a count of 5 is more likely when the mean is 5
dpois(3.5, lambda = 5) # warning and 0: counts must be whole numbers
dpois(-1, lambda = 5)  # 0: counts cannot be negative


# 6. R - Poisson GLM ----

# family = poisson with a log link. (The log link is the default, so
# family = poisson alone does the same.)
soay_glm <- glm(fitness ~ body.size, data = soay,
                family = poisson(link = "log"))


## 6.1 Checking model assumptions (the GLM checklist) ----

# The checklist in the chapter has 7 points. Here they are in code.

# Point 1 (family) and 2 (independence) come from thinking about the data
# and study design, not from R: fitness is a count, so Poisson is a good
# start; each row is a different sheep, so we assume independence.

# Points 3, 5 and 6: diagnostic plots.
autoplot(soay_glm, which = 1:4, add.smooth = TRUE)
# Look at: residuals vs fitted now shows no strong pattern, and the
# scale-location plot is flatter than for the linear model. The QQ-plot of
# the deviance residuals is only a rough guide for a GLM.

# Point 5: the residuals vs leverage plot (which = 5) for influential points.
autoplot(soay_glm, which = 5)

# Point 4: dispersion = residual deviance / residual degrees of freedom.
# This should be roughly 1 (more about this in section 7).
deviance(soay_glm) / df.residual(soay_glm)

# Point 7: zeros. Compare the observed number of zeros with the number the
# model expects. dpois(0, fitted values) is the probability of a zero for
# each sheep; adding these up gives the expected number of zeros.
sum(soay$fitness == 0)
sum(dpois(0, lambda = fitted(soay_glm)))
# Check: here the two numbers are similar (10 observed, about 12 expected),
# so there is no sign of zero inflation.


## 6.2 Interpreting coefficients ----

summary(soay_glm)
# Look at: the Estimate column. The intercept (about -1.25) and the slope
# for body.size (about 0.054) are on the log scale (the link scale).

# Back-transform with exp(). exp(slope) is the factor by which the expected
# count is multiplied for each extra kg of body mass (about 1.055 here,
# i.e. a 5.5% increase per kg).
exp(coef(soay_glm))

# Expected fitness of a 40 kg sheep, step by step:
body_size_example <- 40
linear_predictor <- coef(soay_glm)[1] + coef(soay_glm)[2] * body_size_example
expected_count <- exp(linear_predictor)  # back-transform from the log scale
expected_count
# Check: about 2.46 offspring. (The chapter's hand calculation gives 2.48,
# because it uses rounded coefficients.)


## 6.3 Analysis of deviance ----

# anova() with test = "Chisq" compares the deviance with and without
# body.size (a likelihood ratio test).
anova(soay_glm, test = "Chisq")
# Look at: the "Deviance" in the body.size row (the reduction in deviance,
# about 235.3) on 1 Df, and the p-value Pr(>Chi).


## 6.4 Reporting ----

# Predictions for a sequence of body sizes. We get the fit and its standard
# error on the log (link) scale, which is the default of predict().
new_data <- data.frame(body.size = seq(min(soay$body.size), max(soay$body.size),
                                       length.out = 100))
soay_predictions <- predict(soay_glm, newdata = new_data, se.fit = TRUE)

# Calculate the 95% confidence interval on the log scale FIRST, then
# back-transform the fit and both limits with exp().
new_data$fit <- exp(soay_predictions$fit)
new_data$lower <- exp(soay_predictions$fit - 1.96 * soay_predictions$se.fit)
new_data$upper <- exp(soay_predictions$fit + 1.96 * soay_predictions$se.fit)

# Graph of the data with the fitted curve and its confidence band.
# Each layer gets its own data and aes(), because the layers use different
# data frames (soay for the points, new_data for the line and band).
ggplot() +
  geom_point(data = soay, aes(x = body.size, y = fitness), alpha = 0.4) +
  geom_line(data = new_data, aes(x = body.size, y = fit), color = "blue") +
  geom_ribbon(data = new_data, aes(x = body.size, ymin = lower, ymax = upper),
              alpha = 0.2, fill = "blue") +
  labs(x = "Body mass (kg)", y = "Lifetime fitness")

# The numbers for the reporting sentence in the chapter:
round(exp(coef(soay_glm)[2]), 2)          # multiplicative effect per kg
round(exp(confint(soay_glm)[2, ]), 2)     # its 95% confidence interval
anova(soay_glm, test = "Chisq")$Deviance[2]   # chi-squared (change in deviance)
anova(soay_glm, test = "Chisq")[2, 5]         # p-value

# How big is the effect? Expected fitness of the lightest and heaviest sheep.
new_data$fit[1]                # lightest (about 0.8 offspring)
new_data$fit[nrow(new_data)]   # heaviest (about 12.2 offspring)


# 7. Overdispersion ----

# Dispersion = residual deviance / residual degrees of freedom.
# Values above about 1.5 to 2 suggest overdispersion.
dispersion_soay <- deviance(soay_glm) / df.residual(soay_glm)
dispersion_soay
# Check: about 1.00, so no overdispersion in these data.


## 7.1 Quasi-Poisson and negative binomial models ----

# The quasi-Poisson model estimates the dispersion from the data.
soay_quasi <- glm(fitness ~ body.size, data = soay, family = quasipoisson)
summary(soay_quasi)
# Look at: the estimates are the same as in the Poisson model. The standard
# errors are multiplied by the square root of the "Dispersion parameter"
# (near 1 here, so they hardly change). Note: t values, not z values.


# 8. Zero inflation ----

# No code in the chapter. See the zeros check in section 6.1.


# 9. Multiple explanatory variables ----

# A new version of the data, with the parasite load of each sheep.
soay_parasites <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/soay_sheep_with_parasites.csv")
head(soay_parasites)

# Graph: fitness against parasite load.
ggplot(soay_parasites, aes(x = parasite.load, y = fitness)) +
  geom_point(alpha = 0.4) +
  labs(x = "Parasite load", y = "Lifetime fitness")

# Poisson GLM with two explanatory variables (log link is the default).
soay_glm_parasites <- glm(fitness ~ body.size + parasite.load,
                          data = soay_parasites, family = poisson)

# Check the model, as always.
autoplot(soay_glm_parasites, which = 1:4, add.smooth = TRUE)
deviance(soay_glm_parasites) / df.residual(soay_glm_parasites)  # dispersion

# Analysis of deviance. Terms are added in order (body.size first, then
# parasite.load), like anova() for a linear model.
anova(soay_glm_parasites, test = "Chisq")
summary(soay_glm_parasites)
# Look at: body.size has a positive estimate, parasite.load a negative one.
# Each effect is for the other variable held constant.


# Practical toolbox: code in the Unit 8 practical not covered above ----
# The Unit 8 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

# Count the rows (observations) of a dataset.
nrow(soay_parasites)

# Make a numeric variable into a factor with mutate(), so that a model
# estimates a separate mean for each value (not a linear trend).
# Tiny made-up example:
example_data <- tibble(year = c(2001, 2001, 2002, 2003), count = c(3, 5, 0, 2))
example_data <- example_data |>
  mutate(year = factor(year))
example_data$year     # Check: Levels: 2001 2002 2003
levels(example_data$year)

# A histogram of a count response variable.
ggplot(soay_parasites, aes(x = fitness)) +
  geom_histogram(binwidth = 1)

# A log scale on the y-axis. Add 1 to the counts first, because log(0) is
# not defined.
ggplot(soay_parasites, aes(x = body.size, y = fitness + 1)) +
  geom_point() +
  scale_y_log10()

# Colour by a third variable, or one panel per group with facet_wrap().
ggplot(soay_parasites, aes(x = body.size, y = fitness, colour = parasite.load)) +
  geom_point()
ggplot(soay_parasites, aes(x = body.size, y = fitness)) +
  geom_point() +
  facet_wrap(~ parasite.load > 5)

# Test one variable by comparing two nested models (with and without it).
# The test has 1 df here: the larger model has one more parameter.
soay_glm_body_only <- glm(fitness ~ body.size, data = soay_parasites, family = poisson)
anova(soay_glm_body_only, soay_glm_parasites, test = "Chisq")

# With quasi-Poisson models, use an F-test instead of a chi-squared test.
soay_quasi_body_only <- glm(fitness ~ body.size, data = soay_parasites,
                            family = quasipoisson)
soay_quasi_parasites <- glm(fitness ~ body.size + parasite.load,
                            data = soay_parasites, family = quasipoisson)
anova(soay_quasi_body_only, soay_quasi_parasites, test = "F")

# Effect of a change of more than 1 unit: multiply the coefficient first,
# then exp(). Here, the effect of 10 kg more body mass:
exp(10 * coef(soay_glm)["body.size"])
# And its 95% confidence interval. confint.default() gives a simple
# (Wald) interval on the log scale, which we multiply and back-transform.
exp(10 * confint.default(soay_glm)["body.size", ])

# Predictions on the count (response) scale directly, with type = "response".
# (type = "link", the default, gives the log scale.)
new_sheep <- data.frame(body.size = c(40, 50))
predict(soay_glm, newdata = new_sheep, type = "response")
predict(soay_glm, newdata = new_sheep, type = "link")

# Keep only some variables, and remove rows with missing values (NA).
soay_small <- soay_parasites |>
  select(fitness, body.size) |>
  na.omit()
sum(is.na(soay_parasites))   # number of missing values in the dataset

# All pairwise scatterplots of the variables in a dataset.
pairs(soay_parasites)

# Variance inflation factors (collinearity, Chapter 6) work for GLMs too.
vif(soay_glm_parasites)

# Diagnostic plots without the smoothed (blue) line.
autoplot(soay_glm_parasites, smooth.colour = NA)
# (You can ignore the warnings "Removed 100 rows containing missing values":
# they come from the line we asked not to draw.)

# Predictions for one variable while the other is held at its mean.
# expand.grid() makes all combinations of the values given.
new_data_parasites <- expand.grid(
  parasite.load = seq(min(soay_parasites$parasite.load),
                      max(soay_parasites$parasite.load), length.out = 100),
  body.size = mean(soay_parasites$body.size)
)
new_data_parasites <- new_data_parasites |>
  mutate(predicted_fitness = predict(soay_glm_parasites,
                                     newdata = new_data_parasites,
                                     type = "response"))
ggplot(new_data_parasites, aes(x = parasite.load, y = predicted_fitness)) +
  geom_line() +
  labs(x = "Parasite load", y = "Predicted fitness (body size at its mean)")

# Observed against predicted values, with a regression line. If the model
# predicts well, the intercept is near 0 and the slope near 1.
soay_parasites <- soay_parasites |>
  mutate(predicted_fitness = predict(soay_glm_parasites, type = "response"))
ggplot(soay_parasites, aes(x = predicted_fitness, y = fitness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
m_obs_pred <- lm(fitness ~ predicted_fitness, data = soay_parasites)
summary(m_obs_pred)
confint(m_obs_pred)   # does the interval include 0 (intercept) and 1 (slope)?
