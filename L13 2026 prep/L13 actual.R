## Load libraries
library(ggplot2)
library(ggfortify)

## Set random number seed, for reproducibility
set.seed(12026)

## Ex. 1 Linear model with interaction ----

## *** Planning ***
## 1. Question
## 
## 
## 
## 
## Simulated data ----
n_per_species <- 45
metabolism <- data.frame(
  species = rep(c("species_A", "species_B", "species_C"), each = n_per_species),
  body_size = c(
    runif(n_per_species, 5, 30),
    runif(n_per_species, 5, 30),
    runif(n_per_species, 5, 30)
  )
)
metabolism$metabolic_rate <- with(
  metabolism,
  3 +
    0.22 * body_size +
    ifelse(species == "species_B", 1.3, 0) +
    ifelse(species == "species_C", -0.7, 0) +
    ifelse(species == "species_B", 0.14 * body_size, 0) +
    ifelse(species == "species_C", -0.08 * body_size, 0) +
    rnorm(nrow(metabolism), mean = 0, sd = 1.4)
)





## Ex. 2 Binomial GLM ----
n_per_habitat <- 80
infection <- data.frame(
  habitat = rep(c("forest", "grassland"), each = n_per_habitat)
)
infection$p_infected <- ifelse(infection$habitat == "forest", 0.28, 0.55)
infection$infected <- rbinom(nrow(infection), size = 1, prob = infection$p_infected)



## Ex. 3 Count GLM ----
n_count <- 90
offspring <- data.frame(
  food = runif(n_count, 0, 10)
)
offspring$expected_count <- exp(0.6 + 0.18 * offspring$food)
offspring$n_offspring <- rpois(n_count, lambda = offspring$expected_count)
