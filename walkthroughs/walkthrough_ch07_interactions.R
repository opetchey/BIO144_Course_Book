# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 7: Interactions
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/7.1-interactions.html
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


# 2. Example 1 (1 categorical, 1 continuous) ----
# Blood pressure and minutes of exercise per week, for people with a
# meat heavy diet, and (separately) for people with a vegetarian diet.
bp_meatheavy <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_meatheavy.csv")
head(bp_meatheavy)
bp_vegetarian <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_vegetarian.csv")
head(bp_vegetarian)

# The file bp_1cont1cat.csv contains both datasets together (100 rows).
bp_1cont1cat <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_1cont1cat.csv")

# Both diets in one graph, with a regression line for each diet.
# Mapping diet to colour (col = diet) makes geom_smooth() fit one line per diet.
ggplot(bp_1cont1cat, aes(x = mins_per_week, y = bp, col = diet)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(x = "Minutes per week of exercise", y = "Blood pressure", col = "Diet")
# Look at: the lines are not parallel. The slope is steeper for the meat heavy
# diet. Non-parallel lines = evidence of an interaction.


# 3. Example 2 (2 categorical) ----
# Diet (meat heavy, vegetarian) and exercise (low, high), 10 people per combination.
bp_2cat <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_2cat.csv")
head(bp_2cat)
# R orders the levels of a categorical variable alphabetically, so "high"
# would come before "low". We make exercise a factor with the order we want.
bp_2cat <- bp_2cat |>
  mutate(exercise = factor(exercise, levels = c("low", "high")))

# Calculate the mean of each combination of diet and exercise...
grouped_data <- bp_2cat |>
  group_by(diet, exercise) |>
  summarise(mean_bp = mean(bp), sd_bp = sd(bp), .groups = "drop")
grouped_data
# ...and plot the data, with the means joined by lines (an "interaction plot").
ggplot(bp_2cat, aes(x = exercise, y = bp, col = diet)) +
  geom_point(alpha = 0.5) +
  geom_point(data = grouped_data, aes(y = mean_bp), size = 3) +
  geom_line(data = grouped_data, aes(y = mean_bp, group = diet)) +
  labs(x = "Exercise", y = "Blood pressure", col = "Diet")
# Look at: the lines are not parallel. High exercise lowers bp much more in
# vegetarians than in meat eaters.


# 4. Example 3 (two continuous) ----
bp_2cont <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_2cont.csv")
head(bp_2cont)

# The third variable (age) shown by a colour gradient:
ggplot(bp_2cont, aes(x = mins_per_week, y = bp, col = age)) +
  geom_point(size = 2) +
  scale_color_gradient(low = "darkblue", high = "yellow") +
  labs(x = "Minutes per week of exercise", y = "Blood pressure")

# Another way: cut() makes a categorical version (classes) of a continuous variable.
bp_2cont <- bp_2cont |>
  mutate(age_class = cut(age, breaks = seq(10, 100, 10)))
head(bp_2cont)
# Look at: the new column age_class. "(30,40]" means more than 30 and up to 40.
ggplot(bp_2cont, aes(x = mins_per_week, y = bp, col = age_class)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(x = "Minutes per week of exercise", y = "Blood pressure", col = "Age class")
# Look at: the slope of bp against exercise is shallower for older people.
# Try: make the complementary graph, with age on the x-axis and classes of
# minutes of exercise as colour (cut(mins_per_week, breaks = seq(0, 200, 20))).
# It shows the same, single interaction from the other perspective.


# 5. Implementing interactions: doing it in R ----
# Two equivalent ways to include both main effects AND the interaction:
m_bp_exercise_diet_explicit <- lm(bp ~ mins_per_week + diet + mins_per_week:diet,
                                  data = bp_1cont1cat)
m_bp_exercise_diet <- lm(bp ~ mins_per_week * diet, data = bp_1cont1cat)
# ":" = only the interaction term. "*" = main effects + interaction.
# Check: the two models have exactly the same coefficients.
coef(m_bp_exercise_diet_explicit)
coef(m_bp_exercise_diet)

# Check the model assumptions before interpreting the results, with autoplot().
# (A warning "Removed ... rows containing missing values" comes from
# smooth.colour = NA. You can ignore it.)
autoplot(m_bp_exercise_diet, smooth.colour = NA)

# F-test of the interaction (H0: the slope is the same for both diets, beta_3 = 0).
anova(m_bp_exercise_diet)
# Look at: the row mins_per_week:diet (the colon shows an interaction term).
# F = 4.05 on 1 and 96 df, p = 0.047. Some evidence that the effect of
# exercise depends on diet (p is just below 0.05, so the evidence is not strong).

# The coefficients:
summary(m_bp_exercise_diet)$coefficients
# Look at:
# (Intercept)                  = expected bp with 0 minutes of exercise, meat heavy diet
# mins_per_week                = slope for the meat heavy diet (the reference level)
# dietvegetarian               = difference in intercept, vegetarian minus meat heavy
# mins_per_week:dietvegetarian = difference in slope, vegetarian minus meat heavy


# 6. Reporting our findings ----
summary(m_bp_exercise_diet)$r.squared   # about 0.27

# The numbers for a reporting sentence about the interaction: F value, its df,
# the residual df, and the p-value. [3, ] is the row of the interaction term,
# [4, ] is the residuals row.
anova(m_bp_exercise_diet)[3, ]
anova(m_bp_exercise_diet)[4, "Df"]

# Effect sizes (see the tip box in this section of the chapter): the slope for
# each diet. Slope for vegetarians = slope for meat heavy + difference in slope.
coefs <- coef(m_bp_exercise_diet)
slope_meat <- coefs["mins_per_week"]
slope_veg <- coefs["mins_per_week"] + coefs["mins_per_week:dietvegetarian"]
60 * c(slope_meat, slope_veg)   # change in bp per extra hour of exercise per week
# Look at: about -5.8 mmHg per hour for meat heavy, -1.6 mmHg per hour for vegetarians.

# A confidence interval for the slope of each diet. confint() gives the CI of
# the slope for the reference level. To get the CI for vegetarians, we make
# "vegetarian" the reference level with relevel() and fit the model again.
60 * confint(m_bp_exercise_diet)["mins_per_week", ]
bp_1cont1cat_vegref <- bp_1cont1cat |>
  mutate(diet = relevel(factor(diet), ref = "vegetarian"))
m_bp_exercise_diet_vegref <- lm(bp ~ mins_per_week * diet, data = bp_1cont1cat_vegref)
60 * confint(m_bp_exercise_diet_vegref)["mins_per_week", ]
# Look at: meat heavy 95% CI -8.7 to -2.9; vegetarian -4.6 to 1.3 (includes zero).
anova(m_bp_exercise_diet_vegref)
# Check: changing the reference level does not change the F-test of the interaction.


# 7. Three models: two-way ANOVA ----
mod_2cat <- lm(bp ~ diet * exercise, data = bp_2cat)
# (the same as lm(bp ~ diet + exercise + diet:exercise, data = bp_2cat))
autoplot(mod_2cat, smooth.colour = NA)
anova(mod_2cat)
# Look at: four rows (diet, exercise, diet:exercise, Residuals). Each term has
# 1 df; residual df = 40 - 4 = 36. The interaction: F(1, 36) = 13.77, p < 0.001.

## More than two levels ----
bp_2cat_3levels <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_3cat.csv")
head(bp_2cat_3levels)
bp_2cat_3levels <- bp_2cat_3levels |>
  mutate(exercise = factor(exercise, levels = c("low", "high")))

grouped_data_3levels <- bp_2cat_3levels |>
  group_by(diet, exercise) |>
  summarise(mean_bp = mean(bp), .groups = "drop")
ggplot(bp_2cat_3levels, aes(x = exercise, y = bp, col = diet)) +
  geom_point(alpha = 0.5) +
  geom_point(data = grouped_data_3levels, aes(y = mean_bp), size = 3) +
  geom_line(data = grouped_data_3levels, aes(y = mean_bp, group = diet)) +
  labs(x = "Exercise", y = "Blood pressure", col = "Diet")

mod_2cat_3levels <- lm(bp ~ diet * exercise, data = bp_2cat_3levels)
autoplot(mod_2cat_3levels, smooth.colour = NA)
anova(mod_2cat_3levels)
# Look at the Df column: diet 2 (3 levels - 1), exercise 1, diet:exercise
# 2 x 1 = 2, Residuals 60 - 6 = 54. Still only four rows: the ANOVA table
# tests each term as a whole.


# 8. Three models: multiple regression with interaction term ----
mod_2cont <- lm(bp ~ mins_per_week * age, data = bp_2cont)
autoplot(mod_2cont, smooth.colour = NA)
anova(mod_2cont)
# Look at: the same structure as the two-way ANOVA table, with one row for
# each main effect, one for mins_per_week:age, and one for the residuals.
# The interaction: F(1, 96) = 30.3, p < 0.001.

# What the interaction means: the slope of exercise changes with age.
# Slope of exercise at a given age = coefficient of mins_per_week
#                                    + interaction coefficient x age
coefs_2cont <- coef(mod_2cont)
coefs_2cont
coefs_2cont["mins_per_week"] + coefs_2cont["mins_per_week:age"] * c(20, 80)
# Look at: about -0.45 mmHg per minute at age 20, but only -0.07 at age 80.
# The effect of exercise is stronger in younger people.


# Practical toolbox: code in the Unit 7 practical not covered above ----
# The Unit 7 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

## Checking a dataset: replicates, levels, missing values ----
# Number of observations in each combination of the two treatments:
bp_2cat |>
  group_by(diet, exercise) |>
  summarise(n = n(), .groups = "drop")
# distinct() lists the different values (levels) of a variable:
bp_2cat |>
  distinct(exercise)
# table() counts how many rows have each value:
table(bp_2cat$diet)
# Count missing values (NA) in a variable:
sum(is.na(bp_2cat$bp))
# na.omit() removes every row that has an NA in any column:
bp_2cat |>
  na.omit() |>
  nrow()

## Distribution of the response in each treatment combination ----
# facet_grid(rows ~ columns) makes one small graph for each combination.
ggplot(bp_2cat, aes(x = bp)) +
  geom_histogram(bins = 7) +
  facet_grid(diet ~ exercise)

## Transforming, and replacing codes by words ----
# sqrt() (square root) and log10() transformations are made with mutate().
# ifelse(condition, value if TRUE, value if FALSE) turns codes into words.
# This tiny made-up example has a 0/1 code for diet:
tiny_data <- tibble(area = c(4, 9, 16, 25), meat_code = c(1, 0, 1, 0))
tiny_data <- tiny_data |>
  mutate(area_sqrt = sqrt(area),
         diet = ifelse(meat_code == 1, "meat heavy", "vegetarian"))
tiny_data

## Sorting: a league table ----
# arrange() sorts rows from low to high; desc() sorts from high to low.
bp_2cont |>
  arrange(desc(mins_per_week)) |>
  head()

## Box plots and points, dodged ----
# position_dodge() puts the groups side by side; position_jitterdodge()
# also jitters the points so they do not lie on top of each other.
ggplot(bp_2cat, aes(x = diet, y = bp, col = exercise)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
             alpha = 0.6) +
  labs(x = "Diet", y = "Blood pressure", col = "Exercise")

## Total sum of squares, and R-squared, by hand ----
# The total sum of squares = the sum of all the Sum Sq in the ANOVA table.
sst <- sum((bp_2cat$bp - mean(bp_2cat$bp))^2)
sst
sum(anova(mod_2cat)$`Sum Sq`)
# R-squared = 1 - residual sum of squares / total sum of squares:
1 - anova(mod_2cat)["Residuals", "Sum Sq"] / sst
summary(mod_2cat)$r.squared

## Plotting an interaction model with predict() and a confidence band ----
# expand.grid() makes all combinations of exercise values and the two diets.
new_data <- expand.grid(mins_per_week = seq(0, 200, length.out = 100),
                        diet = c("meat heavy", "vegetarian"))
predictions <- predict(m_bp_exercise_diet, newdata = new_data, interval = "confidence")
new_data <- cbind(new_data, predictions)   # adds columns fit, lwr, upr
ggplot() +
  geom_point(data = bp_1cont1cat, aes(x = mins_per_week, y = bp, col = diet), alpha = 0.6) +
  geom_ribbon(data = new_data, aes(x = mins_per_week, ymin = lwr, ymax = upr, fill = diet),
              alpha = 0.2) +
  geom_line(data = new_data, aes(x = mins_per_week, y = fit, col = diet)) +
  labs(x = "Minutes per week of exercise", y = "Blood pressure",
       col = "Diet", fill = "Diet") +
  theme_classic() +
  theme(text = element_text(size = 14), legend.position = "top")
# This plots exactly the model we fitted (geom_smooth() fits its own models).
# theme_classic() and theme() change the look, e.g. for a publication.
