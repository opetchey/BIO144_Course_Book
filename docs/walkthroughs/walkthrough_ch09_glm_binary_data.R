# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 9: Binomial data (GLM part 2)
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/9.1-GLM2-binary-data.html
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


# 2. Binary and binomial data ----

# Simulate whether each of 10 people passes a test (1) or not (0), when the
# probability of passing is 0.8. set.seed() makes the "random" numbers the
# same every time, so you get the same result as the book.
set.seed(123)
number_persons <- 10
y_binary <- rbinom(number_persons, size = 1, prob = 0.8)
y_binary          # binary data: one 0 or 1 per person

# The same, but recorded as the number of passes out of 10 (binomial data).
set.seed(123)
number_persons <- 10
y_binomial <- rbinom(1, size = number_persons, prob = 0.8)
y_binomial        # one number: how many of the 10 passed

# Binary data can be added up to give binomial data.
sum(y_binary)
y_binomial
# (These two numbers need not be equal: they come from different random draws.)

# dbinom(k, size, prob): probability of k successes out of size trials.
dbinom(8, size = 10, prob = 0.8)

# The probability of each possible number of successes (0 to 10).
p_k_successes <- dbinom(0:10, size = 10, prob = 0.8)
ggplot(mapping = aes(x = 0:10, y = p_k_successes)) +
  geom_point() +
  geom_line(col = "lightgrey") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  labs(x = "Number of successes", y = "Probability")


## 2.1 Probabilities, odds and log-odds ----

# Odds = probability / (1 - probability). Odds can be larger than 1.
prob_seq <- seq(0.01, 0.99, by = 0.01)
odds_seq <- prob_seq / (1 - prob_seq)
ggplot(mapping = aes(x = prob_seq, y = odds_seq)) +
  geom_line() +
  labs(x = "Probability", y = "Odds")

# Log-odds (logit) = log(odds). It goes from minus to plus infinity, so it
# can be modelled by a linear predictor. This is the logit link.
log_odds_seq <- log(odds_seq)
ggplot(mapping = aes(x = prob_seq, y = log_odds_seq)) +
  geom_line() +
  labs(x = "Probability", y = "Log-odds (logit)")

# Odds ratio example from the chapter: probability 0.4 vs 0.2.
(0.4 / (1 - 0.4)) / (0.2 / (1 - 0.2))   # about 2.67


# 3. A simple binomial GLM ----

# The chapter simulates pass/fail data for 50 children and 50 adults.
set.seed(8761)
n_children <- 50
n_adults <- 50
p_pass_children <- 0.8
p_pass_adults <- 0.4
y_children <- rbinom(n_children, size = 1, prob = p_pass_children)
y_adults <- rbinom(n_adults, size = 1, prob = p_pass_adults)

pass_data <- tibble(
  age_group = rep(c("child", "adult"), each = 50),
  pass = c(y_children, y_adults)
)
pass_data

# Summarise: number tested, number passed, and pass rate in each group.
pass_summary <- pass_data |>
  group_by(age_group) |>
  summarise(
    n_tested = n(),
    n_passed = sum(pass),
    pass_rate = mean(pass)
  )
pass_summary
# Check: pass rate 0.60 for adults and 0.78 for children.

# Binomial GLM with logit link. family = binomial alone does the same,
# because the logit link is the default for the binomial family.
m_pass_age_group <- glm(pass ~ age_group, data = pass_data,
                        family = binomial(link = "logit"))

# The coefficients table.
coef(summary(m_pass_age_group))
# Look at: the intercept (about 0.405) is the log-odds of passing for adults
# (the reference level, first in the alphabet). age_groupchild (about 0.860)
# is the DIFFERENCE in log-odds between children and adults.

# Back-transform log-odds to a probability: exp(eta) / (1 + exp(eta)).
eta_adults <- coef(m_pass_age_group)[["(Intercept)"]]
p_adults <- exp(eta_adults) / (1 + exp(eta_adults))
p_adults      # Check: the same as the observed pass rate of adults (0.60)

eta_children <- eta_adults + coef(m_pass_age_group)[["age_groupchild"]]
p_children <- exp(eta_children) / (1 + exp(eta_children))
p_children    # Check: the same as the observed pass rate of children (0.78)


## 3.1 The odds ratio ----

# exp() of a coefficient is an odds ratio. Here: the odds of passing for
# children are about 2.36 times the odds for adults.
exp(coef(m_pass_age_group)[["age_groupchild"]])


# 4. Aggregated binomial data: beetle mortality ----

# Eight groups of beetles, each exposed to a different insecticide dose.
# For each group: number tested and number killed.
beetle <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/beetle_mortality.csv")
head(beetle)

# As always, start with a graph.
ggplot(beetle, aes(x = Dose, y = Mortality_rate)) +
  geom_point() +
  labs(x = "Dose", y = "Mortality rate")


## 4.1 Wrong analysis 1: linear regression on mortality rate ----

mod_beetle_lm <- lm(Mortality_rate ~ Dose, data = beetle)
autoplot(mod_beetle_lm)
# Look at: curvature in residuals vs fitted, and a poor QQ-plot.
# (Also: a straight line will predict rates below 0 and above 1.)


## 4.2 Wrong analysis 2: Poisson regression on counts killed ----

mod_beetle_pois <- glm(Number_killed ~ Dose, data = beetle, family = poisson)
summary(mod_beetle_pois)$coefficients

# The "impossible" prediction: more beetles killed than were tested.
dose_example <- 76.54
pred_killed <- predict(mod_beetle_pois,
                       newdata = data.frame(Dose = dose_example),
                       type = "response")
pred_killed                       # about 76.5 predicted deaths ...
num_tested_example <- beetle$Number_tested[beetle$Dose == dose_example]
num_tested_example                # ... but only 60 beetles were tested!


## 4.3 Doing it right! ----

# For aggregated data the response has two columns: successes and failures.
# Here a "success" is a beetle killed, a "failure" a beetle that survived.
beetle <- beetle |>
  mutate(Number_survived = Number_tested - Number_killed)
head(beetle)

# cbind(successes, failures) on the left of ~, and family = binomial.
beetle_glm <- glm(cbind(Number_killed, Number_survived) ~ Dose,
                  data = beetle, family = binomial)

# Model checking plots (GLM checklist points 3, 5 and 6).
autoplot(beetle_glm)
# Look at: not perfect, but there are only 8 data points (doses).


## 4.4 Analysis of deviance (likelihood-based ANOVA) ----

anova(beetle_glm, test = "Chisq")
# Look at: the Dose row. The deviance falls by about 259.2 on 1 Df,
# so the p-value is tiny.


## 4.5 Plotting the fitted relationship ----

# Predict on the link (log-odds) scale with standard errors, then
# back-transform the fit and the limits (fit +/- 2 SE) to probabilities.
# The dose range is made wider than the data (by 20 on each side) to show
# the S-shape of the logistic curve.
dose_grid <- tibble(Dose = seq(min(beetle$Dose) - 20, max(beetle$Dose) + 20,
                               length.out = 400))
beetle_predictions <- predict(beetle_glm, newdata = dose_grid, se.fit = TRUE) |>
  as_tibble() |>
  mutate(p_hat = exp(fit) / (1 + exp(fit)),
         p_hat_upper_2se = exp(fit + 2 * se.fit) / (1 + exp(fit + 2 * se.fit)),
         p_hat_lower_2se = exp(fit - 2 * se.fit) / (1 + exp(fit - 2 * se.fit)))
dose_grid <- bind_cols(dose_grid, beetle_predictions)

ggplot() +
  geom_point(data = beetle, aes(x = Dose, y = Mortality_rate)) +
  geom_line(data = dose_grid, aes(x = Dose, y = p_hat), color = "blue") +
  geom_ribbon(data = dose_grid,
              aes(x = Dose, ymin = p_hat_lower_2se, ymax = p_hat_upper_2se),
              alpha = 0.2, fill = "blue") +
  labs(x = "Dose", y = "Predicted mortality probability")
# Look at: the predicted probability never goes below 0 or above 1.


## 4.6 Reporting ----

# Predicted probability of death at a dose of 50, with a 95% confidence
# interval. Calculate on the link scale, then back-transform with plogis()
# (plogis(x) is the same as exp(x) / (1 + exp(x))).
pred_50 <- predict(beetle_glm, newdata = data.frame(Dose = 50),
                   type = "link", se.fit = TRUE)
prob_50 <- plogis(pred_50$fit)
prob_50_ci <- plogis(pred_50$fit + c(-1.96, 1.96) * pred_50$se.fit)
prob_50       # about 0.09
prob_50_ci    # about 0.06 to 0.14

# The other numbers for the reporting sentence in the chapter:
summary(beetle_glm)
exp(coef(beetle_glm)[["Dose"]])     # odds ratio per unit of dose (about 1.28)
exp(confint(beetle_glm)["Dose", ])  # its 95% CI (about 1.23 to 1.34)
anova(beetle_glm, test = "Chisq")$Deviance[2]  # change in deviance, on 1 df
anova(beetle_glm, test = "Chisq")[2, 5]        # p-value


## 4.7 Overdispersion in aggregated binomial data ----

# GLM checklist point 4: residual deviance / residual df should be about 1.
deviance(beetle_glm)
df.residual(beetle_glm)
deviance(beetle_glm) / df.residual(beetle_glm)
# Check: about 1.4, at the limit of what is usually acceptable.


## 4.8 Quasibinomial as a pragmatic fix ----

beetle_glm_q <- glm(cbind(Number_killed, Number_survived) ~ Dose,
                    data = beetle, family = quasibinomial)
summary(beetle_glm_q)
# Look at: the same estimates as beetle_glm, but larger standard errors,
# because they are scaled by the estimated "Dispersion parameter".


# 5. Individual-level binary data: blood screening (ESR) ----

# One row per person. y = 1 means high ESR (possible disease), y = 0 low.
plasma <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/plasma_esr_original_HSAUR3.csv")
plasma


## 5.1 Complication 1: graphical description ----

# Scatter plots of 0/1 data show two bands and are not very informative.
# A conditional density plot shows the proportion of 1s and 0s along the
# explanatory variable. cdplot() needs the response as a factor.
# (cdplot() is base R graphics; par(mfrow = c(1, 2)) puts two plots side by
# side, and par(mfrow = c(1, 1)) resets this.)
par(mfrow = c(1, 2))
cdplot(factor(y) ~ fibrinogen, data = plasma, xlab = "Fibrinogen")
cdplot(factor(y) ~ globulin, data = plasma, xlab = "Globulin")
par(mfrow = c(1, 1))
# Look at: the proportion of high ESR (the light area, y = 1) increases
# with fibrinogen, and a little with globulin.


## 5.2 Fit the binomial regression model ----

# For 0/1 data the response is just the 0/1 variable.
plasma_glm <- glm(y ~ fibrinogen + globulin, data = plasma, family = binomial)

# (The chapter does not show the results of this model. Here they are.)
summary(plasma_glm)
exp(coef(plasma_glm))   # odds ratios per unit of each explanatory variable


## 5.3 Complication 2: model checking and dispersion ----

autoplot(plasma_glm)
# Look at: two bands of points in residuals vs fitted (one for the 0s, one
# for the 1s). This is normal for 0/1 data and hard to interpret.
# Do NOT use the deviance / df dispersion check for individual 0/1 data.


# 6. Two common practical issues ----

# Separation and reporting: see the chapter. Separation is shown in the
# Practical toolbox section below.


# Practical toolbox: code in the Unit 9 practical not covered above ----
# The Unit 9 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

# Total number of trials (beetles tested) in an aggregated dataset.
sum(beetle$Number_tested)

# The range of the fitted values of a model. fitted() gives the fitted value
# for each row of the data. The linear model fits impossible mortality rates.
range(fitted(mod_beetle_lm))   # Check: the maximum is above 1

# The failures can be calculated inside cbind(), without a new column.
beetle_glm_2 <- glm(cbind(Number_killed, Number_tested - Number_killed) ~ Dose,
                    data = beetle, family = binomial)
coef(beetle_glm_2)   # Check: the same as coef(beetle_glm)

# Odds ratios and simple (Wald) 95% confidence intervals for all
# coefficients: confint.default() on the log-odds scale, then exp().
exp(coef(beetle_glm))
exp(confint.default(beetle_glm))

# Predicted probabilities directly, with type = "response" (no CI).
predict(beetle_glm, newdata = data.frame(Dose = c(50, 60, 70)), type = "response")

# Testing an interaction by comparing two nested models.
plasma_glm_interaction <- glm(y ~ fibrinogen * globulin, data = plasma,
                              family = binomial)
anova(plasma_glm, plasma_glm_interaction, test = "Chisq")
# Look at: Df = 1 (the interaction adds one parameter) and Pr(>Chi).

# Correlation between explanatory variables (important when interpreting
# their separate effects, Chapter 6).
cor(plasma$fibrinogen, plasma$globulin)
pairs(select(plasma, fibrinogen, globulin, y))

# An interaction between a continuous and a categorical variable in a
# binomial GLM, and predictions with CIs for several groups.
# This makes example data (two made-up products). You do not need to
# understand this code.
set.seed(1)
tiny_tox <- tibble(product = rep(c("X", "Y"), each = 6),
                   dose = rep(1:6, times = 2),
                   n = 20) |>
  mutate(dead = rbinom(12, size = n,
                       prob = plogis(ifelse(product == "X", -3 + 1 * dose, -2 + 0.4 * dose))))

m_tox <- glm(cbind(dead, n - dead) ~ dose * product, data = tiny_tox,
             family = binomial)
coef(m_tox)
# The slope for product X (the reference level) is the "dose" coefficient.
# The slope for product Y is "dose" plus "dose:productY":
coef(m_tox)[["dose"]] + coef(m_tox)[["dose:productY"]]

# Predicted probabilities at one dose for each product, with 95% CIs
# (link scale first, then plogis()).
new_tox <- data.frame(dose = 3, product = c("X", "Y"))
pred_tox <- predict(m_tox, newdata = new_tox, type = "link", se.fit = TRUE)
new_tox <- new_tox |>
  mutate(prob = plogis(pred_tox$fit),
         lower = plogis(pred_tox$fit - 1.96 * pred_tox$se.fit),
         upper = plogis(pred_tox$fit + 1.96 * pred_tox$se.fit))
new_tox

# Fitted curves for each product: predict over a sequence of doses for every
# product (expand.grid() makes all combinations), then plot.
tox_grid <- expand.grid(dose = seq(1, 6, length.out = 50), product = c("X", "Y"))
tox_grid$prob <- predict(m_tox, newdata = tox_grid, type = "response")
ggplot(tiny_tox, aes(x = dose, y = dead / n, colour = product)) +
  geom_point() +
  geom_line(data = tox_grid, aes(y = prob)) +
  labs(x = "Dose", y = "Proportion dead")

# The effect of one variable with another held at its mean.
fib_grid <- expand.grid(fibrinogen = seq(min(plasma$fibrinogen),
                                         max(plasma$fibrinogen), length.out = 100),
                        globulin = mean(plasma$globulin))
fib_grid$prob <- predict(plasma_glm, newdata = fib_grid, type = "response")
ggplot(fib_grid, aes(x = fibrinogen, y = prob)) +
  geom_line() +
  labs(x = "Fibrinogen", y = "Probability of high ESR (globulin at its mean)")

# Separation: when x perfectly separates the 0s and 1s. Tiny made-up example.
sep_data <- tibble(x = 1:10, y = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1))
m_sep <- glm(y ~ x, data = sep_data, family = binomial)
summary(m_sep)
# Look at: the warnings "algorithm did not converge" and "fitted
# probabilities numerically 0 or 1 occurred", the huge estimates and
# standard errors, and the meaningless p-values. These are symptoms of
# separation. (Here the warnings are expected.)

# Reading a file that has no header row and uses "?" for missing values.
# col_names = FALSE: the first row is data, not names. na = "?": treat "?"
# as NA. Then give the columns names with names(). (I() lets us write a tiny
# file directly in the code; with a real file you give its path or URL.)
no_header_data <- read_csv(I("12,0.5,1\n?,0.7,0\n15,?,1"),
                           col_names = FALSE, na = "?")
names(no_header_data) <- c("age", "fraction", "alive")
no_header_data
