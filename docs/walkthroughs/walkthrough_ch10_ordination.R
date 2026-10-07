# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 10: Ordination
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/10.1-ordination.html
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
library(tidyverse)  # read_csv(), dplyr, tidyr, ggplot2
library(vegan)      # vegdist(), metaMDS(), stressplot() and the multivariate tests


# 2. Working example: skull shape through time ----
# Four skull measurements (mm) for 150 skulls, and the age of each skull
# (thousand.years = thousands of years ago). Question: has skull shape changed
# through time?
skull <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/skull_shape_time.csv")
skull
# Check: 150 rows, one row per skull. Columns: id, the four measurements,
# and thousand.years. This is WIDE format: one column per measurement.


# 3. Wide vs long format ----
# Ordination functions want wide format (one column per variable).
# ggplot2 graphs with facets want LONG format: one row per skull per
# measurement. pivot_longer() makes the long version.
skull_long <- skull |>
  pivot_longer(
    cols = c(max.breadth, basi.height, basi.length, nasal.height),
    names_to = "dimension",
    values_to = "value"
  )
skull_long
# Look at: now 600 rows (150 skulls x 4 measurements). The column "dimension"
# says which measurement, the column "value" holds the number.


# 4. Explore first: four scatterplots (measurement vs time) ----
# One panel per measurement. scales = "free_y" lets each panel have its own
# y-axis range, because the measurements differ in size.
p_vars <- skull_long |>
  ggplot(aes(x = thousand.years, y = value)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ dimension, scales = "free_y", ncol = 2) +
  labs(x = "Thousand years ago", y = "Measurement (mm)")
p_vars
# Look at: do some measurements change with time? Four separate regressions
# would mean four tests (multiple testing) and would ignore that the
# measurements are correlated. Ordination looks at all four together.


# 5. Ordination I: PCA ----

## 5a. Centering and scaling, and running the PCA ----
# Select ONLY the four measurement columns (not id, not time), then run
# prcomp(). center = TRUE subtracts each variable's mean; scale. = TRUE
# divides by each variable's SD, so all four variables count equally.
# (Note the dot in "scale.": that is the argument name.)
skull_pca <- skull |>
  dplyr::select(max.breadth, basi.height, basi.length, nasal.height) |>
  prcomp(center = TRUE, scale. = TRUE)

# The new coordinates (scores) of each skull on the PC axes are in $x
head(skull_pca$x)
# Look at: four PC columns, because we started with four variables.

## 5b. Variance represented ----
summary(skull_pca)
# Look at the row "Proportion of Variance": PC1 represents about 56% of the
# variance and the row "Cumulative Proportion" shows PC1 + PC2 together
# represent about 76%. PC3 and PC4 (the rest) are ignored in a 2D plot.

## 5c. PCA scores plot (PC1 vs PC2) ----
# Put the scores in a tibble and add back the id and time of each skull
# (the rows are in the same order as in the skull data).
pca_scores <- as_tibble(skull_pca$x) |>
  mutate(id = skull$id, thousand.years = skull$thousand.years)
pca_scores |> select(id, PC1, PC2, thousand.years) |> slice_head(n = 5)

ggplot(pca_scores, aes(x = PC1, y = PC2, color = thousand.years)) +
  geom_point(alpha = 0.8) +
  labs(color = "Thousand\nyears ago")
# Look at: is there a colour gradient across the plot? If older and younger
# skulls are in different places, the multivariate shape is linked to time.

## 5d. What do the axes mean? Loadings and correlations ----
# Loadings ($rotation): the weight of each original variable in each PC.
loadings <- as_tibble(skull_pca$rotation, rownames = "variable")
loadings
# Look at: all four variables have PC1 loadings of the same sign, so PC1 is
# overall SIZE. PC2 has a mix of signs, so PC2 is SHAPE (some measurements
# go up while others go down). The overall sign of a PC is arbitrary.

# Another way: correlations between the original variables and the PC scores
skull_measurements <- skull |>
  dplyr::select(max.breadth, basi.height, basi.length, nasal.height)
round(cor(skull_measurements, skull_pca$x), 2)
# Check: the pattern of signs is the same as in the loadings.
# (The biplot in the chapter, with arrows for the loadings, is a teaching
# figure and is not included here.)


# 6. Ordination II: NMDS ----

## 6a. Step 1: Choose a distance measure ----
# Scale the measurements (mean 0, SD 1), then compute the Euclidean distance
# between every pair of skulls.
X_scaled <- scale(skull_measurements)
skull_distances <- dist(X_scaled, method = "euclidean")
# skull_distances has 150 x 149 / 2 = 11175 distances: too many to print.

## 6b. Distances for community (abundance) data: Bray-Curtis ----
# A tiny made-up community: 3 sites (rows), 4 species (columns).
communities <- data.frame(species_A = c(10, 8, 0),
                          species_B = c(5, 6, 0),
                          species_C = c(0, 0, 4),
                          species_D = c(0, 0, 3))
vegdist(communities, method = "bray")
dist(communities, method = "euclidean")
# Look at: Bray-Curtis between sites 1 and 2 is about 0.10 (similar species
# in similar numbers); sites 1 and 3 share no species, so Bray-Curtis is
# exactly 1. Euclidean distances have no upper limit and depend on the units.

## 6c. Step 2: Fit NMDS with multiple random starts ----
# set.seed() makes the random starts the same every time you run it.
# k = 2 dimensions; trymax = up to 50 random starts; autotransform = FALSE
# because we already scaled the data; trace = FALSE hides progress output.
set.seed(1)
skull_nmds <- metaMDS(skull_distances, k = 2, trymax = 50,
                      autotransform = FALSE, trace = FALSE)
skull_nmds
# Look at: the number of dimensions, how many random starts were tried,
# and the stress.

## 6d. Step 3: Assess NMDS fit with stress ----
# Stress plot (Shepard plot): original dissimilarity (x) against the distance
# in the NMDS plot (y). Points close to the red step line = good fit.
stressplot(skull_nmds)

skull_nmds$stress
# Rule of thumb: < 0.1 good, 0.1 to 0.2 acceptable, > 0.2 poor.
# Check: here the stress is about 0.15, so an acceptable fit in 2D.

# Does a 3D solution fit much better?
set.seed(1)
skull_nmds_3d <- metaMDS(skull_distances, k = 3, trymax = 50,
                         autotransform = FALSE, trace = FALSE)
skull_nmds_3d$stress
# Look at: lower stress (about 0.07), but 2D is easier to show and interpret.

## 6e. Plot the NMDS scores, coloured by time ----
# scores() extracts the coordinates of the skulls ("sites") from the NMDS.
nmds_scores <- as_tibble(scores(skull_nmds, display = "sites")) |>
  mutate(id = skull$id, thousand.years = skull$thousand.years)

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = thousand.years)) +
  geom_point(alpha = 0.8) +
  labs(color = "Thousand\nyears ago")
# Note: NMDS axis directions are arbitrary. Only relative distances matter.

# What do the NMDS axes mean? Correlate the original variables with them,
# just as we did for PCA.
cor_nmds <- round(cor(skull_measurements,
                      nmds_scores |> select(NMDS1, NMDS2)), 2)
cor_nmds
# Look at: the pattern is similar to PCA, because these data are fairly linear.


# 7. Hypothesis testing: avoid "four separate regressions" ----
# AWARENESS LEVEL: in BIO144 you do not need to run these tests yourself.
# You should know what question each one answers (see the table in this
# section of the chapter). The code is here so you can see what it looks like.

## 7a. MANOVA (parametric) ----
# The response is a matrix of the four measurements, made with cbind().
skull_manova <- manova(cbind(max.breadth, basi.height, basi.length, nasal.height) ~ thousand.years,
                       data = skull)
summary(skull_manova, test = "Pillai")
# Look at: the Pr(>F) column, the p-value for the effect of time on all four
# measurements together.

# Bin the years into 8 groups (a factor) to compare groups of skulls
skull <- skull |>
  mutate(year_class = cut(thousand.years, breaks = 8) |> fct_inorder())
skull_manova_class <- manova(cbind(max.breadth, basi.height, basi.length, nasal.height) ~ year_class,
                             data = skull)
summary(skull_manova_class, test = "Pillai")

## 7b. PERMANOVA (distance-based) ----
# Do the groups differ in their position (centroid)? Uses the same distance
# matrix as the NMDS, and a permutation test (no normality assumption).
set.seed(1)
adonis2(skull_distances ~ year_class, data = skull, permutations = 999)

## 7c. PERMDISP / betadisper: are groups equally variable? ----
# PERMANOVA can be significant because groups differ in SPREAD, not position.
# betadisper() checks whether the spread differs among groups.
skull_betadisper <- betadisper(skull_distances, skull$year_class)
anova(skull_betadisper)
set.seed(1)
permutest(skull_betadisper, permutations = 999)

## 7d. Fitting time onto an ordination: envfit and ordisurf ----
# envfit: is time related to the NMDS positions along a straight direction?
set.seed(1)
skull_envfit_years <- envfit(skull_nmds ~ thousand.years, data = skull,
                             permutations = 999)
skull_envfit_years
# Look at: r2 (how well time fits) and Pr(>r) (permutation p-value).

# These two graphs use base R plotting, because vegan draws them that way.
plot(skull_nmds, type = "n")                                  # empty plot
points(skull_nmds, display = "sites", pch = 16, cex = 0.8)   # the skulls
plot(skull_envfit_years, col = "black")                      # the time arrow

# ordisurf: a curved (non-linear) surface of time over the NMDS plot
plot(skull_nmds, type = "n")
points(skull_nmds, display = "sites", pch = 16, cex = 0.8)
ordisurf(skull_nmds ~ thousand.years, data = skull, add = TRUE)
# Look at: the contour lines show the fitted age at each place in the plot.


# Practical toolbox: code in the Unit 10 practical not covered above ----
# The Unit 10 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

## T1. Select columns by the start of their name: starts_with() ----
# Useful when there are many variables with similar names (e.g. species).
skull |> select(starts_with("basi"))

## T2. Scale the data yourself with scale(), then add other columns back ----
# scale() returns a matrix, so convert it back to a data frame.
skull_scaled <- skull |>
  select(max.breadth, basi.height, basi.length, nasal.height) |>
  scale() |>
  as.data.frame() |>
  mutate(id = skull$id, thousand.years = skull$thousand.years)

## T3. Check the scaling worked: summarise(across(...)) ----
# across() applies functions (here mean and sd) to several columns at once.
skull_scaled |>
  summarise(across(c(max.breadth, basi.height, basi.length, nasal.height),
                   list(mean = mean, sd = sd)))
# Check: every mean is (almost) 0 and every SD is 1. (Numbers like 1e-16 are
# zero, apart from tiny computer rounding.)

## T4. PCA on data that are already scaled, from a matrix ----
# as.matrix() makes a numeric matrix. Because the data are already centred
# and scaled, we do not need prcomp() to do it again.
skull_matrix <- skull_scaled |>
  select(max.breadth, basi.height, basi.length, nasal.height) |>
  as.matrix()
skull_pca_prescaled <- prcomp(skull_matrix, center = FALSE, scale. = FALSE)
summary(skull_pca_prescaled)
# Check: the same proportions of variance as skull_pca in section 5b.

## T5. Get one column of scores or loadings with [ , ] ----
# [, 1] means "all rows, column 1". So this is the PC1 score of every skull:
head(skull_pca$x[, 1])
skull_pca$rotation[, "PC1"]   # the PC1 loadings, chosen by column name

## T6. NMDS coordinates with $points ----
# Instead of scores(), you can get the coordinates from $points.
# Note the column names are then MDS1 and MDS2 (not NMDS1 and NMDS2).
head(as.data.frame(skull_nmds$points))

## T7. Principal components as explanatory variables in lm() ----
# The chapter section "PCA in multiple regression" describes this without
# code. Here, as a demonstration only, we predict the age of a skull from the
# four correlated measurements, or from PC1 alone.
skull_with_pc <- skull_scaled |>
  mutate(PC1 = skull_pca$x[, 1])
m_age_measurements <- lm(thousand.years ~ max.breadth + basi.height + basi.length + nasal.height,
                         data = skull_with_pc)
m_age_pc1 <- lm(thousand.years ~ PC1, data = skull_with_pc)
summary(m_age_measurements)
summary(m_age_pc1)
# Look at: the number of estimated slopes (4 vs 1) and the R-squared of each
# model. PC1 avoids collinearity, but its slope is harder to interpret,
# because PC1 is a mix of all four measurements (see the loadings).
