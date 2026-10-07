# =============================================================================
# BIO144 Data Analysis in Biology
# Code walkthrough for Chapter 2: R and RStudio
# Course book chapter: https://opetchey.github.io/BIO144_Course_Book/2.1-R-and-RStudio.html
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
# Install a package only ONCE (e.g. install.packages("tidyverse")), in the
# Console, not in your script. Load it with library() EVERY time you start R.
# tidyverse loads readr (import), dplyr (wrangling), ggplot2 (graphs) and more.
library(tidyverse)
library(GGally)   # used only in the Practical toolbox section at the end
library(janitor)  # for clean_names() (install it once if needed: install.packages("janitor"))


# 2. Getting to know R ----

# R as a calculator. Functions have a name and brackets: exp(), sqrt().
1 + 1
exp(2)
sqrt(16)

# Assign values to objects with <- . Objects then appear in the Environment pane.
a <- 5
b <- 10
c <- a + b
c   # typing the name of an object shows its value

# A vector holds several values. c() "combines" them.
my_vector <- c(1, 2, 3, 4, 5)
my_vector
# Arithmetic on a vector is done on every element.
my_vector * 2
my_vector + 10
my_vector ^ 2

# Vectors can also hold text (character strings), always in quotes.
my_strings <- c("apple", "banana", "cherry")
my_strings
paste("I like", my_strings)
toupper(my_strings)

# A data frame is a table: each column is a variable, each row an observation.
my_data <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Height = c(165, 180, 175)
)
my_data


# 3. Getting help ----
# ? opens the help page of a function in the Help pane.
# Look at: the Arguments section and the Examples at the bottom.
?mean


# 4. Finding and fixing errors ----
# An ERROR stops R: nothing is produced. A WARNING means the code ran, but you
# should check something.
my_values <- c(3, 5, 8)
# The next line gives an error, so it is commented out. Remove the # and run it:
# sum(my_value)
# Error: object 'my_value' not found
# Read the message: R knows no object called my_value. It is a typo; the
# object is called my_values. The fixed code:
sum(my_values)

# Two more common errors (also commented out so the script still runs):
# Mean(my_values)
# Error: could not find function "Mean". R is case-sensitive: it is mean().
# sum(my_values
# A missing bracket: the Console shows + and waits. Press Esc to clear it.

# Strategy (see the chapter for the full list): read the message; run the
# script line by line from the top; look at your objects with str(), head(),
# names(), summary(); restart R and run everything again.
str(my_data)
names(my_data)
summary(my_data)


# 5. Add-on packages and R versions ----
# Install once (here commented out, because you do not need it every time):
# install.packages("ggplot2")
# Which version of R do you have? Keep R and packages up to date, but not
# just before an exam.
R.version.string


# 6. Importing and viewing data ----
# Work in an R Project and put data files in it. Then you can read a file with
# a short relative path, e.g. read_csv("datasets/my_data.csv").
# In this script we read the data straight from the course book web site.
my_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/my_data.csv")

# Ways to look at a data frame:
# View(my_data)   # opens it in a new tab (or click it in the Environment pane)
head(my_data)     # first few rows
glimpse(my_data)  # variable names, types and first values
# Check: the number of rows and columns, the variable names, and the types
# (<dbl> = numeric, <chr> = character/text). Always assume the import could
# have gone wrong until you have checked!

# Are there duplicated values where there should be none?
# $ picks one variable (column) from a data frame.
duplicated(my_data$Name)        # one TRUE/FALSE (logical) value per row
any(duplicated(my_data$Name))   # TRUE if at least one is duplicated
example_vector <- c("A", "B", "A", "C", "B")
duplicated(example_vector)      # Think first: what do you expect?

# How many missing values (NA) in each variable?
# across(everything(), ...) does sum(is.na(.)) for every column.
my_data |>
  summarise(across(everything(), ~ sum(is.na(.))))


# 7. Data wrangling: clean the variable names ----
# clean_names() from the janitor package makes all names lower case, with _
# instead of spaces and no special characters. Names like this are much easier
# to type in code.
my_data <- my_data |>
  clean_names()
names(my_data)


# 8. Data wrangling: manipulate the data frame ----
# dplyr functions: select() columns, filter() rows by a condition, slice()
# rows by position, arrange() rows, mutate() new variables.
# The pipe |> passes the result on the left into the function on the right.

# A small example dataset (tibble() is the tidyverse version of data.frame()).
my_data1 <- tibble(
  name = c("Alice", "Bob", "Charlie", "David", "Eva"),
  age = c(25, 30, 35, 40, 45),
  score = c(90, 85, 95, 80, 70))

# The same kind of data with 100 people. This makes random example data;
# set.seed() makes the random numbers the same every time.
set.seed(123)
my_data2 <- tibble(name = paste0("Person_", sprintf("%03d", 1:100)),
  age = sample(20:50, 100, replace = TRUE),
  score = rnorm(100, mean = 75, sd = 10))
my_data2

# Select columns: by name, by position, by type, or by part of the name.
my_data2 |> select(name, score)
my_data2 |> select(1, 3)
my_data2 |> select(where(is.numeric))
my_data2 |> select(contains("a"))

# Filter: keep the rows that meet a condition. & means "and".
my_data2 |> filter(age > 30)
my_data2 |> filter(age > 30 & score < 90)

# Slice: keep rows by their position.
my_data2 |> slice(1:2)

# Arrange: sort the rows (smallest first; desc() for largest first).
my_data2 |> arrange(age)
my_data2 |> arrange(desc(age))

# Mutate: add new variables (one or several at once).
my_data2 |>
  mutate(age_in_5_years = age + 5)
my_data2 |>
  mutate(
    age_in_5_years = age + 5,
    percentage_score = score / 100
  )
# Check: my_data2 itself has not changed! To keep a result, assign it with <-.
my_data2


# 9. Grouping and summarising data ----
# Simulated data: blood pressure of 60 people, each measured at 4 visits.
bp_multifactor <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Course_Book/refs/heads/main/datasets/bp_multifactor.csv")
glimpse(bp_multifactor)
# Look at: 240 rows (60 people x 4 visits) and 7 variables.

# summarise() alone gives one number for the whole dataset ...
bp_multifactor |>
  summarise(mean_bp = mean(systolic_bp))

# ... with group_by() first, one number per group (here per person).
bp_multifactor |>
  group_by(id) |>
  summarise(mean_bp = mean(systolic_bp))

# group_by() alone only adds the grouping: look for "Groups: id [60]".
bp_multifactor |>
  group_by(id)

# Group by two variables, and calculate several summaries.
bp_multifactor |>
  group_by(sex, treatment) |>
  summarise(
    mean_bp = mean(systolic_bp),
    sd_bp = sd(systolic_bp)
  )
# Look at: one row for each combination of sex and treatment (4 rows).
# The message "summarise() has grouped output by 'sex'" is information, not an error.


# 10. Working with categorical variables ----
# Categorical variables are usually imported as character <chr>. Sometimes we
# want a factor <fct>, for example to set the order of the categories.
my_data3 <- tibble(
  name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  education_level = c("Bachelor's", "Master's", "PhD", "High School", "Bachelor's"),
  age = c(19, 23, 25, 16, 20)
)
my_data3

my_data3 <- my_data3 |>
  mutate(education_level = factor(education_level))
my_data3
levels(my_data3$education_level)
ggplot(my_data3, aes(x = education_level, y = age)) +
  geom_point()
# Look at: the levels are in alphabetical order, which makes no sense here.

# Set the order of the levels ourselves:
my_data3 <- my_data3 |>
  mutate(education_level = factor(education_level,
                                  levels = c("High School", "Bachelor's", "Master's", "PhD")))
ggplot(my_data3, aes(x = education_level, y = age)) +
  geom_point()


# 11. Visualisation: three basic types ----
# Every ggplot: ggplot(data, aes(x = ..., y = ...)) + a geom_...() layer.
# Layers are ADDED with + (not with the pipe |>).

# Scatterplot: relationship between two continuous variables.
ggplot(my_data2, aes(x = age, y = score)) +
  geom_point()

# Histogram: distribution of one continuous variable.
ggplot(my_data2, aes(x = score)) +
  geom_histogram(bins = 10)

# Box and whisker plot: compare the distribution among groups.
# First make a grouping variable with case_when() (condition ~ value).
my_data2 <- my_data2 |>
  mutate(age_group = case_when(
    age < 30 ~ "20-29",
    age >= 30 & age < 40 ~ "30-39",
    age >= 40 ~ "40-49"
  ))
ggplot(my_data2, aes(x = factor(age_group), y = score)) +
  geom_boxplot()


# 12. Visualisation: labels and themes ----
# labs() sets the title and axis labels (always give units!).
# theme_...() changes the overall look.
ggplot(my_data2, aes(x = age, y = score)) +
  geom_point() +
  labs(
    title = "Scatterplot of Age vs Score",
    x = "Age (years)",
    y = "Score"
  ) +
  theme_minimal()


# 13. Saving ggplot visualisations ----
# Save the graph in an object, then save the object to a file with ggsave().
plot1 <- ggplot(my_data2, aes(x = age, y = score)) +
  geom_point() +
  labs(
    title = "Scatterplot of Age vs Score",
    x = "Age (years)",
    y = "Score"
  )
plot1   # show the graph

# The file extension (.pdf, .png) sets the file type. Width and height are
# in inches. The file goes in your project folder: look in the Files pane.
ggsave("scatterplot_age_vs_score.pdf", plot = plot1, width = 8, height = 6)

# To save into a sub-folder, first create it. showWarnings = FALSE stops
# a warning when the folder already exists (e.g. when you run this again).
dir.create("plots", showWarnings = FALSE)
ggsave("plots/scatterplot_age_vs_score.pdf", plot = plot1, width = 8, height = 6)


# Practical toolbox: code in the Unit 2 practical not covered above ----
# The Unit 2 practical and weekly quiz use some code that this chapter does not.
# Here it is, shown on this chapter's data (or a tiny example), so you will
# recognise it in the practical. This section does not solve the practical.

# (a) Reading a file that is not comma-separated, with read_delim().
# Here we first write my_data2 as a tab-separated .txt file, then read it back.
# delim = "\t" means tab; for spaces use delim = " ".
write_delim(my_data2, "my_data2_tab.txt", delim = "\t")
my_data2_from_txt <- read_delim("my_data2_tab.txt", delim = "\t")
glimpse(my_data2_from_txt)
# Check: if you get only ONE column, the delimiter is probably wrong.

# (b) Number of rows, and number of different values in a variable.
nrow(bp_multifactor)
bp_multifactor |>
  summarise(n_people = n_distinct(id),
            n_people_again = length(unique(id)))

# (c) Missing values: !is.na() keeps rows that are NOT missing; drop_na()
# removes every row with a missing value in any variable.
my_data_with_na <- tibble(x = c(1, NA, 3, 4), y = c(10, 20, NA, 40))
my_data_with_na |> filter(!is.na(x) & !is.na(y))
my_data_with_na |> drop_na()

# (d) Remove particular rows: %in% asks "is the value in this list?", and !
# means "not". Here we remove persons 1, 2 and 3.
my_data2 |>
  filter(!(name %in% c("Person_001", "Person_002", "Person_003")))

# (e) Colour points by a categorical variable (ggplot adds a key/legend).
ggplot(my_data2, aes(x = age, y = score, colour = age_group)) +
  geom_point()

# (f) Facets: one panel per group. scales = "free" lets each panel have its
# own axis limits (see ?facet_wrap).
ggplot(bp_multifactor, aes(x = age, y = systolic_bp)) +
  geom_point() +
  facet_wrap(~ treatment, scales = "free")

# (g) Histograms of many variables at once: make the data "long" with
# pivot_longer(), then facet by variable name.
bp_multifactor |>
  select(age, visit, systolic_bp) |>
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ variable, scales = "free")

# (h) All pairwise relationships at once with ggpairs() from GGally.
# Look at: scatterplots (lower), distributions (diagonal), correlations (upper).
bp_multifactor |>
  select(age, visit, systolic_bp) |>
  ggpairs()

# (i) Log-transforming variables inside aes(), e.g. for skewed data
# (only possible for values greater than zero).
ggplot(bp_multifactor, aes(x = log(age), y = log(systolic_bp))) +
  geom_point()
