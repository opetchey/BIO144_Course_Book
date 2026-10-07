# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 5: Analysis of variance (ANOVA)
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/5.1-ANOVA.html
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
# Load multcomp BEFORE tidyverse. multcomp also loads the MASS package, which
# has its own select() function. Packages loaded later "mask" (hide) functions
# of the same name loaded earlier, so loading tidyverse last makes sure that
# select() is the dplyr one. (Otherwise: "unused arguments" error in select().)
library(multcomp)   # for Tukey HSD post-hoc tests with glht()
library(tidyverse)
library(ggfortify)  # for autoplot() of model checking plots
# Note: stat_summary(fun.data = mean_cl_normal) in section 8 needs the Hmisc
# package to be installed (install.packages("Hmisc")); you do not need to load it.


# 2. Introduction: read and look at the data ----
# Blood pressure (bp) of 50 people with one of four diets.
bp_data_diet <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_data_diet.csv")

# Keep the three variables we need (select() also puts them in this order).
bp_data_diet <- select(bp_data_diet, bp, diet, person_ID)
head(bp_data_diet)
# Look at: bp is numeric <dbl> (the response); diet is character <chr>, a
# categorical explanatory variable; person_ID is not used in the analysis.

# How many people are in each diet group?
bp_data_diet |>
  count(diet)
# Look at: the groups are not the same size (7 to 16 people).

# Always graph the raw data first.
# geom_jitter() moves the points a little sideways so they do not overlap.
# height = 0 means NO vertical jitter: the bp values stay exactly correct.
ggplot(bp_data_diet, aes(x = diet, y = bp)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.7) +
  labs(title = "Blood Pressure by Diet Type",
       x = "Diet Type",
       y = "Blood Pressure (mm Hg)")
# Look at: do the group means look different, compared with the spread of
# the points within each group? That is what ANOVA asks.


# 3. How does it look like in R? Fit the model ----
# ANOVA is a linear model, so we use lm(), exactly as for regression.
# The only difference: the explanatory variable (diet) is categorical.
m_bp_diet <- lm(bp ~ diet, data = bp_data_diet)


# 4. Doing ANOVA in R: check the assumptions ----
# Check the model assumptions BEFORE you look at the results.
autoplot(m_bp_diet, which = 1:4, add.smooth = FALSE)
# Look at:
# - Residuals vs Fitted (top left): one column of points per diet. The spread
#   should be similar in each column.
# - Normal Q-Q (top right): points close to the line = residuals are roughly
#   normally distributed. Here it looks good.
# - Scale-Location (bottom left): no clear trend = similar variance in each
#   group (no strong heteroscedasticity).
# - Cook's distance (bottom right): no single point should stand out a lot.
# A few residuals are a bit large; we ignore them here.


# 5. Doing ANOVA in R: the ANOVA table ----
anova(m_bp_diet)
# Look at, row by row:
# - diet:      Df = k - 1 = 4 - 1 = 3 (k = number of groups)
# - Residuals: Df = n - k = 50 - 4 = 46 (n = number of observations)
# - Mean Sq = Sum Sq / Df
# - F value = Mean Sq (diet) / Mean Sq (Residuals) = 1758.08 / 84.82 = 20.73
# - Pr(>F) = the p-value: very small here (1.2e-08).
# Report as: diet had a significant effect on blood pressure
# (F(3, 46) = 20.7, p < 0.0001).

# Optional: calculate the sums of squares yourself, to see where the numbers
# in the ANOVA table come from (chapter section "Calculating the sums of squares").
ss_data <- bp_data_diet |>
  mutate(overall_mean = mean(bp)) |>
  group_by(diet) |>
  mutate(group_mean = mean(bp)) |>
  ungroup()
ss_data |>
  summarise(SST = sum((bp - overall_mean)^2),          # total
            SSM = sum((group_mean - overall_mean)^2),  # model (between groups)
            SSE = sum((bp - group_mean)^2))            # residual (within groups)
# Check: SSM and SSE are the same as Sum Sq in the ANOVA table, and
# SST = SSM + SSE.


# 6. Difference between pairs of groups ----
# The F-test only says that NOT ALL group means are equal. It does not say
# which groups differ. If we do many tests, the chance of at least one false
# positive (Type I error) gets large, so we must correct for multiple tests.

# 6a. Bonferroni correction: pairwise t-tests, p-values multiplied by the
# number of tests (here 6 pairs from 4 groups).
pairwise.t.test(bp_data_diet$bp,
                bp_data_diet$diet,
                p.adjust.method = "bonferroni")
# Look at: a table of adjusted p-values, one for each pair of diets.
# All pairs differ (p < 0.05) except vegan vs vegetarian.

# The same without correction, to compare:
pairwise.t.test(bp_data_diet$bp,
                bp_data_diet$diet,
                p.adjust.method = "none")
# Check: Mediterranean vs vegetarian is 0.0040 here, and 6 x 0.0040 = 0.024,
# the Bonferroni-adjusted value above (0.0239).

# 6b. Tukey HSD with glht() from the multcomp package.
# mcp() needs diet to be a factor, so first convert it, then re-fit the model.
bp_data_diet <- bp_data_diet |>
  mutate(diet = as.factor(diet))
m_bp_diet <- lm(bp ~ diet, data = bp_data_diet)
tukey_test <- glht(m_bp_diet, linfct = mcp(diet = "Tukey"))
summary(tukey_test)
# Look at: one row per pair. Estimate = difference between the two group means.
# "Adjusted p values reported" means the multiple-test correction was done.
# Again, only vegan - vegetarian is not significant.
# Bonferroni is more conservative than Tukey HSD, which is more conservative
# than Fisher's LSD (see "Which method to use?").


# 7. Other contrasts ----
# Question: do diets with meat differ from diets without meat?
# Make a new explanatory variable with two groups. | means "or".
# The diet names must be written exactly as in the data ("meat heavy", with
# the space): diet == "meat" would match nothing, and give no error!
bp_data_diet <- mutate(bp_data_diet,
                       meat_or_no_meat = ifelse(diet == "meat heavy" |
                                                diet == "Mediterranean",
                                                "meat", "no meat"))
head(bp_data_diet)
# Check: always check a new variable! Each diet should be in only one group.
bp_data_diet |>
  count(diet, meat_or_no_meat)

m_bp_meat_or_no_meat <- lm(bp ~ meat_or_no_meat, data = bp_data_diet)

# Compare the two models with an F-test: does the four-diet model explain
# significantly more variation than the two-group model?
anova(m_bp_diet, m_bp_meat_or_no_meat)
# Look at: RSS (residual sum of squares) is smaller for the four-diet model
# (Model 1: 3901.5) than for the two-group model (Model 2: 5231.6), and the
# p-value is small (0.0012). So it is not only meat or no meat:
# the kind of diet matters.


# 8. Communicating the results of ANOVA ----
# Report the F-test AND the effect sizes (how big the differences are).
# The coefficients are differences from the reference group. The reference
# group is the first level of the factor (alphabetical order):
levels(bp_data_diet$diet)
summary(m_bp_diet)
confint(m_bp_diet)  # 95% confidence intervals of the differences
# Look at: (Intercept) is the mean of the reference group (the first level
# shown by levels() above). Each other row is the difference between that diet
# and the reference group. Note: alphabetical order depends on your computer's
# language settings, so the first level may be "meat heavy" or "Mediterranean".
# To be sure, set the order yourself with fct_relevel() (see the Practical
# toolbox at the end of this script).

# Group means, for the reporting sentence:
bp_data_diet |>
  group_by(diet) |>
  summarise(mean_bp = mean(bp))
# Example: the mean bp of the meat heavy group was about 123 mmHg, and of the
# vegan group about 27 mmHg lower (one-way ANOVA, F(3, 46) = 20.7, p < 0.0001).

# A graph of the raw data with the group means (red points):
ggplot(bp_data_diet, aes(x = diet, y = bp)) +
  geom_jitter(width = 0.1, height = 0) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(title = "Blood Pressure by Diet",
       x = "Diet",
       y = "Blood Pressure (mmHg)")

# The same with error bars showing the 95% confidence interval of each mean
# (mean_cl_normal needs the Hmisc package installed).
ggplot(bp_data_diet, aes(x = diet, y = bp)) +
  geom_jitter(width = 0.1, height = 0, col = "grey") +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, color = "black") +
  labs(title = "Blood Pressure by Diet\nBlack points and error bars show mean ± 95% CI",
       x = "Diet",
       y = "Blood Pressure (mmHg)")


# Practical toolbox: code in the Unit 5 practical not covered above ----
# The Unit 5 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

# (a) Is there one row per individual? Compare the number of rows with the
# number of unique IDs.
nrow(bp_data_diet)
length(unique(bp_data_diet$person_ID))

# (b) Missing values in the response. na.omit() removes the NAs from a vector;
# filter(!is.na(...)) removes the rows with NA from a data frame.
# A tiny example (our diet data have no missing values):
example_bp <- tibble(bp = c(120, NA, 105, 98), diet = c("vegan", "vegan", "meat heavy", "vegan"))
length(na.omit(example_bp$bp))   # number of non-missing values
example_bp_no_na <- example_bp |>
  filter(!is.na(bp))
example_bp_no_na

# (c) Put the groups in a sensible order with fct_relevel() (forcats package,
# loaded with tidyverse). The first level named becomes the reference group
# in summary(), so the coefficients become differences from that group.
bp_data_diet <- bp_data_diet |>
  mutate(diet = fct_relevel(diet, "meat heavy", "Mediterranean", "vegetarian", "vegan"))
levels(bp_data_diet$diet)
m_bp_diet_reordered <- lm(bp ~ diet, data = bp_data_diet)
summary(m_bp_diet_reordered)
# Look at: now (Intercept) is the mean of meat heavy, and dietvegan is the
# vegan minus meat heavy difference (about -27 mmHg).
# Also look at: Multiple R-squared = the proportion of the variation in bp
# explained by diet. The anova() table does not change when you reorder.

# (d) A box-and-whisker plot with the raw data on top.
ggplot(bp_data_diet, aes(x = diet, y = bp)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5) +
  labs(x = "Diet", y = "Blood pressure (mmHg)")
