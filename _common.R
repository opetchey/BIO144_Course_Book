
invisible(
  capture.output(
    suppressWarnings(
      suppressMessages({
        suppressPackageStartupMessages({
          library(ggplot2)
          library(tidyverse)
          library(patchwork)
          library(gtsummary)
          library(here)
          library(kableExtra)
          library(ggfortify)
          library(HSAUR3)
          library(vegan)
          library(car)
          library(broom)
          library(knitr)
          library(plotly)
          library(lme4)
          #library(lmerTest)
        })
      })
    )
  )
)

## BIO144 house style for figures (Spring Term). Source: BIO144_Style/R/theme_bio144.R
## (copied to style/ by BIO144_Style/quarto/sync.R). Used in the book only: students'
## own plots keep the ggplot2 defaults.
source("style/theme_bio144.R")
use_bio144_style()

## ggfortify (autoplot()) gives harmless ggplot2 deprecation warnings
## (fortify(<lm>), size aesthetic for lines); hide them in the book
options(lifecycle_verbosity = "quiet")
