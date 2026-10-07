# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 11: Mixed models
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/11.1-mixed-models.html
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
library(tidyverse)  # read_csv(), dplyr, ggplot2
library(patchwork)  # put two ggplots side by side with +
library(lme4)       # lmer(): linear mixed models
library(lmerTest)   # adds degrees of freedom and p-values to lmer() output
# Note: once lmerTest is loaded, lmer() is the lmerTest version. In sections
# 6 and 8 we write lme4::lmer() to use the original lme4 version, so you see
# what the chapter shows first. Section 10 then uses the lmerTest version.


# 2. Random intercept models: the illustrative data ----
# 10 individuals, each measured 5 times: growth at different temperatures.
example_repeated <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/mixed_model_example.csv")
head(example_repeated)
# Look at: the same individual appears in several rows. These rows are not
# independent: they are grouped by individual.

# Random intercept syntax in R (just read these; they are not run):
#   y ~ x + (1 | group)            random intercept for each group
#   y ~ x + (1 + x | group)        random intercept AND random slope of x
#   y ~ treatment + (1 | plot/plant)            nested: plants within plots
#   y ~ x + (1 | individual) + (1 | observer)   crossed random effects
# Inside the brackets: what varies (left of |) among which groups (right of |).


# 3. Hands-on example: plant growth with repeated measures ----
# 30 plants at 10, 15 or 20 degrees C, each measured every week (weeks 0-7).
# read_csv() reads temp as a number (10, 15, 20). Temperature is a treatment
# with three levels, so we make it a factor with factor().
plant_growth <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/plant_growth_repeated.csv") |>
  mutate(temp = factor(temp, levels = c("10", "15", "20")))
head(plant_growth)
# Check: 240 rows (30 plants x 8 weeks), and temp is shown as <fct> (factor).


# 4. Explore the data ----
# group = plant_id draws one line per plant, so we can follow each plant.
ggplot(plant_growth, aes(x = week, y = height, group = plant_id, color = temp)) +
  geom_line(alpha = 0.25) +
  geom_point(alpha = 0.5, size = 1) +
  labs(x = "Week", y = "Height (cm)", color = "Temp (°C)")
# Look at: plants grow over time; warmer plants tend to be taller; and plants
# differ in their height at week 0 (different baselines = random intercepts).


# 5. Wrong model: treat all rows as independent ----
m_lm <- lm(height ~ week * temp, data = plant_growth)
anova(m_lm)
# Look at: the Residuals row has 234 degrees of freedom, but there are only
# 30 plants. Many more residual df than independent units = pseudoreplication.
# The p-values of this model cannot be trusted.


# 6. Mixed model: random intercept for plant ----
# (1 | plant_id) gives each plant its own intercept (baseline height).
m_lmm1 <- lme4::lmer(height ~ week * temp + (1 | plant_id), data = plant_growth)
anova(m_lmm1)
# Look at: lme4 gives F values but NO degrees of freedom and NO p-values.
# Calculating them for mixed models is difficult (see section 8).


# 7. Model checking ----
# autoplot() does not work for mixed models, so we make the plots ourselves.
# fitted() gives the fitted values, resid() gives the residuals.

# 7a. Residuals vs fitted: look for no pattern around the dashed zero line
plot(fitted(m_lmm1), resid(m_lmm1),
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)

# 7b. Normal QQ plot of the residuals: points should be close to the line
qqnorm(resid(m_lmm1)); qqline(resid(m_lmm1))

# 7c. Scale-location: the spread should not change with the fitted values
plot(fitted(m_lmm1), sqrt(abs(resid(m_lmm1))),
     xlab = "Fitted values", ylab = "sqrt(|Residuals|)")

# 7d. Residuals vs the continuous explanatory variable (here week)
plot(plant_growth$week, resid(m_lmm1), xlab = "Week", ylab = "Residuals")
abline(h = 0, lty = 2)
# Check: in all four plots, nothing worrying. (A shortcut for 7a is
# plot(m_lmm1), see the toolbox at the end.)


# 8. Extension: random slopes for week ----
# (1 + week | plant_id): each plant has its own intercept AND its own growth
# rate (slope of week).
m_lmm2 <- lme4::lmer(height ~ week * temp + (1 + week | plant_id), data = plant_growth)
summary(m_lmm2)
# Look at the "Random effects" part: the Variance and Std.Dev. among plants in
# the intercept and in the week slope, and the Residual variance.
# The "Fixed effects" part: the average intercept and slopes.
# Note the message "boundary (singular) fit": the variance among plants in
# their week slope is almost zero (and Corr is 1.00). The data contain almost
# no evidence that plants differ in growth rate, so this model is more
# complex than the data support (see the caution box in the chapter).

# Compare the two mixed models with a likelihood ratio test.
# (R refits both models with ML instead of REML for this; that is fine.)
anova(m_lmm1, m_lmm2)
# Look at: Pr(>Chisq) is far above 0.05 (about 0.78 here), so there is little
# evidence that the random slopes improve the model.


# 9. Visualising fitted values ----
# predict() gives the fitted value for each row, including each plant's own
# random effects. We add them as new columns.
plant_growth_fitted <- plant_growth |>
  mutate(fit_lmm1 = predict(m_lmm1),
         fit_lmm2 = predict(m_lmm2))

p_fit1 <- ggplot(plant_growth_fitted, aes(week, height, group = plant_id, color = temp)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_line(aes(y = fit_lmm1), alpha = 0.35) +
  labs(title = "Random intercept", y = "Height (cm)")
p_fit2 <- ggplot(plant_growth_fitted, aes(week, height, group = plant_id, color = temp)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_line(aes(y = fit_lmm2), alpha = 0.35) +
  labs(title = "Random intercept + slope model", y = "Height (cm)")
p_fit1 + p_fit2
# Look at: left, parallel lines (same slope, different intercepts); right,
# slopes can differ a little among plants. Here the two look very similar.


# 10. Significance testing for fixed effects ----
# Now use lmer() from lmerTest (loaded at the top). Same model, but anova()
# now gives degrees of freedom (Satterthwaite's method) and p-values.
m_lmm1 <- lmer(height ~ week * temp + (1 | plant_id), data = plant_growth)
anova(m_lmm1)
# Look at: NumDF and DenDF columns (numerator and denominator df) and Pr(>F).
# Check: DenDF for temp is about 46, much smaller than the 234 residual df of
# the wrong model, because temperature varies among plants, not within them.
# All three terms have p < 0.05.
# The Sum Sq for temp differs from the lme4 anova() table in section 6: lme4
# gives sequential (Type I) sums of squares, lmerTest gives Type III by default
# (each term tested after all others). anova(m_lmm1, type = 1) gives the
# sequential version with p-values.

# Reporting: see the template in the chapter. Report the model structure
# (fixed effects, and plant as a random effect), the effects, and how the
# p-values were obtained (Satterthwaite df, lmerTest package).


# Practical toolbox: code in the Unit 11 practical not covered above ----
# The Unit 11 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

## T1. Group means: lm() with a factor, predict(), and group_by()/summarise() ----
# Mean growth of each individual, estimated separately (no pooling)
m_growth_lm <- lm(growth ~ individual, data = example_repeated)
individual_preds <- data.frame(individual = unique(example_repeated$individual))
individual_preds$predicted_lm <- predict(m_growth_lm, newdata = individual_preds)
individual_preds
# The same numbers, calculated directly with dplyr; n() counts the rows
example_repeated |>
  group_by(individual) |>
  summarise(mean_growth = mean(growth), n_obs = n())

## T2. An intercept-only mixed model and partial pooling ----
# growth ~ 1 + (1 | individual): an overall mean, plus a random deviation for
# each individual.
m_growth_lmm <- lmer(growth ~ 1 + (1 | individual), data = example_repeated)
# re.form = NULL means "include the random effects" in the predictions
individual_preds <- individual_preds |>
  mutate(predicted_lmm = predict(m_growth_lmm, newdata = individual_preds, re.form = NULL))
individual_preds
# fixef() gives the fixed effects: here just the overall mean
fixef(m_growth_lmm)
# Look at: the LMM predictions are pulled ("shrunk") towards the overall mean.

## T3. Plot two sets of predictions with a legend; geom_hline(); coord_flip() ----
# Putting a text label inside aes(color = "...") makes a legend entry.
ggplot(individual_preds, aes(x = individual)) +
  geom_point(aes(y = predicted_lm, color = "LM predictions"), size = 3, alpha = 0.5) +
  geom_point(aes(y = predicted_lmm, color = "LMM predictions"), size = 3, alpha = 0.5) +
  geom_hline(yintercept = mean(example_repeated$growth), linetype = "dashed") +
  labs(y = "Growth", color = "Legend") +
  coord_flip()   # swap the x and y axes

## T4. Variance components and the proportion of variance among groups ----
# summary() shows the Variance of the random intercept and of the Residual.
summary(m_lmm1)
# VarCorr() gives them as a table we can calculate with
m_lmm1_varcomp <- as.data.frame(VarCorr(m_lmm1))
m_lmm1_varcomp
# Proportion of variance among plants = among-plant variance / total variance.
# Use the VARIANCES (column vcov), not the standard deviations (sdcor).
m_lmm1_varcomp$vcov[1] / sum(m_lmm1_varcomp$vcov)
# Check: about 0.44, so 44% of the variance not explained by the fixed
# effects is due to differences among plants.

## T5. Short form of a random slope: (week | plant_id) ----
# (week | plant_id) means the same as (1 + week | plant_id): R adds the
# random intercept automatically.

## T6. One number per independent unit, then a t-test ----
# Fit a separate regression for each plant, keep each plant's slope,
# and test whether the mean slope differs from zero.
plant_slopes <- plant_growth |>
  group_by(plant_id) |>
  summarise(slope = coef(lm(height ~ week))[2])  # [2] is the slope
t.test(plant_slopes$slope)

## T7. Confidence intervals for the fixed effects of a mixed model ----
# parm = "beta_" means "only the fixed effects"; method = "Wald" is fast.
confint(m_lmm1, parm = "beta_", method = "Wald")

## T8. Quick residuals vs fitted plot for a mixed model: plot() ----
plot(m_lmm1)

## T9. One panel per group, with a regression line in each ----
# facet_wrap(~ group) and geom_smooth(method = "lm", se = FALSE)
ggplot(example_repeated, aes(x = temperature, y = growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ individual)
