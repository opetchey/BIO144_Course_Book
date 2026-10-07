# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 12: Review: planning and implementing statistical analyses
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/12.1-review.html
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

# Part 1 of the chapter (planning) has no R code. Part 2 (implementing) works
# through three examples, each with the same steps:
#   read data -> planned figure -> fit the planned model -> check it ->
#   interpret the output -> make explicit predictions -> plot -> report.
# Notice how the same steps work for a linear model and for GLMs.


# 1. Load the packages ----
library(tidyverse)  # read_csv(), dplyr, tidyr, ggplot2
library(ggfortify)  # autoplot() for model diagnostic plots
library(lme4)       # lmer(), only used in the Practical toolbox at the end
library(lmerTest)   # p-values for lmer(), only used in the Practical toolbox


# 2. Example 1: Linear model with an interaction ----
# Question: does the relationship between body size and metabolic rate
# differ among species?

## 2a. An example dataset ----
metabolism <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/metabolism_species_body_size.csv")
metabolism |>
  slice_head(n = 6)   # the first 6 rows (like head())

# Planned figure: one colour per species
ggplot(metabolism, aes(body_size, metabolic_rate, colour = species)) +
  geom_point() +
  labs(x = "Body size", y = "Metabolic rate", colour = "Species") +
  theme_classic()
# Look at: do the three species seem to have different slopes?

## 2b. Fit the planned model ----
# species * body_size means species + body_size + species:body_size
# (both main effects AND their interaction).
m_metabolism <- lm(metabolic_rate ~ species * body_size, data = metabolism)

## 2c. Check the model before interpreting ----
autoplot(m_metabolism)
# Look at: residuals vs fitted (no pattern?), QQ plot (points near the line?),
# scale-location (even spread?), residuals vs leverage (no single point
# dominating?). We look for obvious problems, not perfect plots.

## 2d. Interpret the output ----
summary(m_metabolism)   # coefficient level: t tests, one per coefficient
anova(m_metabolism)     # term level: F tests, one per term
# Look at: in anova(), the row species:body_size tests whether the slopes
# differ among species (the question). In summary(), body_size is the slope of
# the reference species (species_A); species_B:body_size is how much the slope
# of species_B differs from that of species_A.

# Residual degrees of freedom: observations minus estimated parameters
df.residual(m_metabolism)
nrow(metabolism)                                  # number of observations
length(coef(m_metabolism))                        # number of parameters
nrow(metabolism) - length(coef(m_metabolism))     # the same as df.residual()
# Check: 135 observations - 6 parameters = 129 residual df.

## 2e. Make explicit predictions ----
# Do not use geom_smooth() as the model. Instead:
# 1) make new data with the explanatory variable values we want predictions for
# expand_grid() makes every combination of species and 100 body sizes.
new_metabolism <- expand_grid(
  species = c("species_A", "species_B", "species_C"),
  body_size = seq(5, 30, length.out = 100)
)
# 2) predict(); interval = "confidence" also gives the 95% CI (lwr, upr)
pred_metabolism <- predict(
  m_metabolism,
  newdata = new_metabolism,
  interval = "confidence"
)
# 3) put the new data and the predictions side by side in one tibble
plot_metabolism <- bind_cols(
  new_metabolism,
  as_tibble(pred_metabolism)
)
plot_metabolism |>
  slice_head(n = 6)
# Look at: columns fit (the prediction), lwr and upr (the confidence interval).

# 4) plot the data and the model predictions together
ggplot(metabolism, aes(body_size, metabolic_rate, colour = species)) +
  geom_point(alpha = 0.65) +
  geom_ribbon(
    data = plot_metabolism,
    aes(x = body_size, ymin = lwr, ymax = upr, fill = species),
    alpha = 0.18,
    colour = NA,
    inherit.aes = FALSE   # do not use the aes() from the first line here
  ) +
  geom_line(data = plot_metabolism, aes(y = fit), linewidth = 1) +
  labs(x = "Body size", y = "Metabolic rate",
       colour = "Species", fill = "Species") +
  theme_classic()
# The lines and ribbons come from m_metabolism: the figure shows the model
# we fitted and checked. See the chapter for an example report.


# 3. Example 2: Binomial GLM ----
# Question: does habitat type affect whether individuals are infected?

## 3a. An example dataset ----
infection <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/infection_habitat.csv")
infection |>
  slice_head(n = 6)
# infected is binary: 0 = not infected, 1 = infected.

# Planned figure: the proportion infected in each habitat.
# The mean of 0s and 1s is the proportion of 1s.
infection_summary <- infection |>
  group_by(habitat) |>
  summarise(
    infected = mean(infected),
    .groups = "drop"
  )
ggplot(infection_summary, aes(habitat, infected)) +
  geom_col(fill = "grey70", colour = "grey25") +
  labs(x = "Habitat", y = "Observed proportion infected") +
  theme_classic()

## 3b. Fit the planned model ----
m_infection <- glm(
  infected ~ habitat,
  family = binomial,
  data = infection
)

## 3c. Check before interpreting ----
# Overdispersion: the residual deviance / residual df check does NOT work for
# individual-level 0/1 data like these (one row per individual), so we do not
# use it here. It is useful for counts and grouped binomial data (see 4c).

## 3d. Interpret the output ----
summary(m_infection)
# Look at: the coefficients are on the log-odds (link) scale and use z tests.
# habitatgrassland is the difference in log-odds between grassland and forest.

## 3e. Make explicit predictions ----
# type = "response" gives predictions on the probability scale.
# pick(everything()) passes the columns of this tibble as newdata.
new_infection <- tibble(
  habitat = c("forest", "grassland")
) |>
  mutate(
    predicted_probability = predict(
      m_infection,
      newdata = pick(everything()),
      type = "response"
    )
  )
new_infection
# Check: these predicted probabilities equal the observed proportions in
# infection_summary, because the model has one probability per habitat.

# Plot the 0/1 data (jittered so the points do not overlap) and the predictions
ggplot(infection, aes(habitat, infected)) +
  geom_jitter(width = 0.08, height = 0.03, alpha = 0.25) +
  geom_point(
    data = new_infection,
    aes(y = predicted_probability),
    size = 4,
    colour = "firebrick"
  ) +
  labs(x = "Habitat", y = "Infection status / predicted probability") +
  theme_classic()


# 4. Example 3: Count GLM ----
# Question: does food availability affect the number of offspring produced?

## 4a. An example dataset ----
offspring <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/offspring_food.csv")
offspring |>
  slice_head(n = 6)

# Planned figure
ggplot(offspring, aes(food, n_offspring)) +
  geom_point(alpha = 0.7) +
  labs(x = "Food availability", y = "Number of offspring") +
  theme_classic()

## 4b. Fit the planned model ----
m_offspring <- glm(
  n_offspring ~ food,
  family = poisson,
  data = offspring
)

## 4c. Check before interpreting ----
deviance(m_offspring) / df.residual(m_offspring)
# Check: about 1.34. Somewhat above 1, but not "much larger", so the Poisson
# model is acceptable here.
# (If it were much larger than 1, we would consider family = quasipoisson.)

## 4d. Interpret the output ----
summary(m_offspring)
# Look at: the food coefficient is on the log scale. Positive = the expected
# number of offspring increases with food.

## 4e. Make explicit predictions ----
new_offspring <- tibble(
  food = seq(0, 10, length.out = 100)
) |>
  mutate(
    predicted_count = predict(
      m_offspring,
      newdata = pick(everything()),
      type = "response"   # counts, not log counts
    )
  )

ggplot(offspring, aes(food, n_offspring)) +
  geom_point(alpha = 0.6) +
  geom_line(
    data = new_offspring,
    aes(y = predicted_count),
    colour = "firebrick",
    linewidth = 1
  ) +
  labs(x = "Food availability", y = "Number of offspring") +
  theme_classic()
# Look at: the prediction line curves upwards, because the Poisson GLM uses a
# log link. See the chapter for the report and the final checklists.


# Practical toolbox: code in the Unit 12 practical not covered above ----
# The Unit 12 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.
# The mixed model code (T4 to T8) is from Chapter 11; here we use the plant
# growth dataset from Chapter 11 (30 plants, each measured in 8 weeks).

## T1. Choose the reference level of a factor: fct_relevel() ----
# By default R puts factor levels in alphabetical order, and the first level
# is the reference level in summary(). fct_relevel() moves a level to first.
metabolism_relevelled <- metabolism |>
  mutate(species = fct_relevel(species, "species_C"))
levels(metabolism_relevelled$species)
# Now coefficients would be differences from species_C.

## T2. Check the design: rows, groups, and replicates per combination ----
plant_growth <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/plant_growth_repeated.csv")
nrow(plant_growth)                  # number of rows
n_distinct(plant_growth$plant_id)   # number of different plants (the groups)
plant_growth |> count(temp, week)   # number of rows per combination

## T3. Mean of the response for each combination of two variables ----
plant_growth |>
  group_by(temp, week) |>
  summarise(mean_height = mean(height), .groups = "drop")

## T4. Effect at another level when there is an interaction: add coefficients ----
# With an interaction, body_size is the slope for the reference species
# (species_A). The slope for species_B is body_size + species_B:body_size.
coef(m_metabolism)
coef(m_metabolism)["body_size"] + coef(m_metabolism)["speciesspecies_B:body_size"]

## T5. A mixed model with lmerTest: summary() and anova() ----
# (1 | plant_id): a random intercept for each plant (see Chapter 11).
m_height_mixed <- lmer(height ~ week * temp + (1 | plant_id), data = plant_growth)
summary(m_height_mixed)   # Random effects (variances) and Fixed effects
anova(m_height_mixed)     # F tests with Satterthwaite degrees of freedom

## T6. Check a mixed model: plot(), qqnorm(), ranef() ----
plot(m_height_mixed)                 # residuals vs fitted values
qqnorm(resid(m_height_mixed)); qqline(resid(m_height_mixed))
ranef(m_height_mixed)                # the estimated effect of each plant
# Look at: are any plant effects extreme compared with the others?

## T7. Variance components and confidence intervals ----
m_height_varcomp <- as.data.frame(VarCorr(m_height_mixed))
m_height_varcomp
# Proportion of variance among plants (use variances, column vcov)
m_height_varcomp$vcov[1] / sum(m_height_varcomp$vcov)
# 95% confidence intervals of the fixed effects
confint(m_height_mixed, parm = "beta_", method = "Wald")

## T8. Predictions for an average group: re.form = NA ----
# re.form = NA means "ignore the random effects", so we get predictions for
# an average plant (not for one particular plant).
new_plant <- expand_grid(week = c(0, 7), temp = c(10, 20))
new_plant |>
  mutate(predicted_height = predict(m_height_mixed, newdata = new_plant, re.form = NA))
