# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 1: Introduction
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/1.1-intro.html
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
# This is the analysis of the class reaction time data from the first lecture
# (steps 8 to 12 of the workflow in the chapter). You do not need to understand
# every line yet: the functions used here are explained in the coming weeks.
# Keep a note of what you do not understand, and look out for it later.
# =============================================================================


# 1. Load the packages ----
# Install a package only once (e.g. install.packages("tidyverse")), in the
# Console. Load it with library() every time you start R.
# tidyverse loads readr (import), dplyr and tidyr (wrangling) and ggplot2 (graphs).
library(tidyverse)


# 2. Data import ----
# Read the class data directly from the internet.
reaction_times <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/reaction_times_2027.csv")

# Look at the data. Check: how many rows (participants) and columns are there?
# Are the reaction times numbers (<dbl>)?
reaction_times
str(reaction_times)


# 3. Data wrangling ----
# The column names from the form are long and contain spaces. Give them short,
# simple names. (Order matters: the names are given to the columns in order.)
names(reaction_times) <- c("RT1", "RT2", "RT3", "RT4", "RT5",
                           "Random_number", "Sex_at_birth")
names(reaction_times)

# Give each participant an identifier.
reaction_times <- reaction_times |>
  mutate(ID = paste0("ID-", row_number()))

# Put each reaction time on its own row ("long" format: one row per
# participant per trial). This makes it easy to summarise per participant.
reaction_times_long <- reaction_times |>
  pivot_longer(cols = RT1:RT5,
               names_to = "Trial",
               values_to = "RT")
reaction_times_long

# Mean reaction time of each participant (the mean of their five trials).
mean_reaction_times <- reaction_times_long |>
  group_by(ID, Sex_at_birth) |>
  summarise(mean_RT = mean(RT), .groups = "drop")
mean_reaction_times

# How many participants of each sex at birth?
mean_reaction_times |>
  count(Sex_at_birth)


# 4. Visualisation ----
# The planned graph: box plots of reaction time for females and males, with
# reaction time on the horizontal axis. The points show the individual values.
ggplot(mean_reaction_times, aes(x = mean_RT, y = Sex_at_birth)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0.1, alpha = 0.5) +
  labs(x = "Mean reaction time (ms)", y = "Sex at birth")

# Check: are there implausible values (e.g. a mistake when typing in the data)?
# A mean reaction time below 50 ms or above 500 ms is probably a mistake.
# Remove such values, and record how many were removed (and why) in your report.
mean_reaction_times_clean <- mean_reaction_times |>
  filter(mean_RT > 50, mean_RT < 500)
nrow(mean_reaction_times) - nrow(mean_reaction_times_clean)

# Plot again, without the implausible values.
ggplot(mean_reaction_times_clean, aes(x = mean_RT, y = Sex_at_birth)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0.1, alpha = 0.5) +
  labs(x = "Mean reaction time (ms)", y = "Sex at birth")


# 5. Statistical test ----
# Two-sample t-test: is the mean reaction time different between the groups?
# var.equal = TRUE assumes the two groups have similar variances.
RT_ttest <- t.test(mean_RT ~ Sex_at_birth,
                   data = mean_reaction_times_clean,
                   var.equal = TRUE)
RT_ttest
# Look at: the t value, the degrees of freedom (df), the p-value, the two group
# means, and the 95% confidence interval of the difference between the means.


# 6. Critical thinking ----
# How big is the difference, and is it biologically relevant? Calculate the
# means, standard deviations and number of participants in each group.
mean_reaction_times_clean |>
  group_by(Sex_at_birth) |>
  summarise(mean = mean(mean_RT),
            sd = sd(mean_RT),
            n = n())
# Questions to ask: how precise is the estimate of the difference (look at the
# confidence interval)? Could something other than sex at birth explain a
# difference? To whom do the conclusions apply, given how the subjects were
# selected?


# 7. Reporting ----
# Take the numbers for a results sentence from the t-test object.
RT_ttest$estimate    # the two group means
RT_ttest$conf.int    # 95% confidence interval of the difference
RT_ttest$statistic   # t value
RT_ttest$parameter   # degrees of freedom
RT_ttest$p.value     # p-value
# Example structure of a results sentence (fill in the numbers):
# "Mean reaction time of females was X ms and of males Y ms (difference
#  Z ms, 95% CI A to B ms; t-test, t = ..., df = ..., p = ...)."


# Practical toolbox: code in the Unit 1 practical not covered above ----
# The Unit 1 practical and weekly quiz use some code that this chapter does not.
# Here it is, so you will recognise it in the practical. This section does not
# solve the practical.

# T1. Working in an RStudio project. In the practical you make a project
# (File > New Project...). A project sets the working directory to the project
# folder, so files are read from and saved to that folder. Check where you are:
getwd()
list.files()   # the files in the project folder

# T2. Installing packages. Run install.packages() once, in the Console, not in
# your script (it would re-install the package every time you run the script):
# install.packages("tidyverse")

# T3. Download a file into your project folder, then read it from there.
# (destfile is the name the file gets in your project folder.)
download.file("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/reaction_times_2027.csv",
              destfile = "reaction_times_2027.csv")
reaction_times_local <- read_csv("reaction_times_2027.csv")

# T4. Reading error messages. Each line below has a typical error. Remove the #
# at the start of a line and run it, read the error message, then fix it.
# read_cvs("reaction_times_2027.csv")      # could not find function: a typo in a function name
# read_csv("reaction_time_2027.csv")       # does not exist: a typo in the file name
# mean(reaction_times_local$rt1)           # Unknown or uninitialised column / NA: names are case-sensitive
# mean(c(1, 2, 3)                          # missing bracket: the Console shows + and waits. Press Esc.
# Tip: copy an error message into a search engine or an AI assistant, and ask
# it to EXPLAIN the error (not to rewrite your whole script).

# T5. Check your script is reproducible: Session > Restart R, then run the
# whole script from the top (Code > Run Region > Run All). Restarting R gives
# you a clean start; it is better than rm(list = ls()), which does not unload
# packages or reset options.
