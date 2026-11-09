## BIO144 house style ("Spring Term") for ggplot2 figures.
## Source: BIO144_Style/R/theme_bio144.R. Copy into the repos that use it; edit the source only.
##
## Use in a document's setup chunk (e.g. the book's _common.R):
##   source("style/theme_bio144.R")
##   use_bio144_style()          # sets the theme, the default colours and the geom colours
##
## Or per plot:  ggplot(...) + ... + theme_bio144() + scale_colour_bio144_d()

library(ggplot2)

## House colours
bio144_col <- c(
  ink        = "#1E3226",  # text
  muted      = "#566E5F",  # axis text, captions
  line       = "#D5E0CF",  # grid lines
  green      = "#2F6B45",  # leaf green: points, lines, single-colour bars
  green_soft = "#E3EEDB",  # facet strips
  band       = "#A9C99B",  # confidence / prediction bands
  pink       = "#B2445F",  # highlight (e.g. a model fit or a value to notice)
  paper      = "#FFFFFF"
)

## Categorical palette: the Okabe-Ito colours (safe for the common forms of colour
## blindness), ordered so the first ones suit the spring palette: green, pink, blue, orange ...
bio144_palette <- c("#009E73", "#CC79A7", "#0072B2", "#E69F00",
                    "#56B4E9", "#D55E00", "#F0E442", "#000000")

theme_bio144 <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text              = element_text(colour = bio144_col[["ink"]]),
      axis.text         = element_text(colour = bio144_col[["muted"]], size = rel(0.85)),
      axis.title        = element_text(colour = bio144_col[["ink"]]),
      axis.line         = element_line(colour = bio144_col[["muted"]], linewidth = 0.4),
      axis.ticks        = element_line(colour = bio144_col[["muted"]], linewidth = 0.4),
      axis.ticks.length = unit(3, "pt"),
      panel.grid.major  = element_line(colour = bio144_col[["line"]], linewidth = 0.35),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = rel(1.15), hjust = 0,
                                       margin = margin(b = 6)),
      plot.subtitle     = element_text(colour = bio144_col[["muted"]], hjust = 0,
                                       margin = margin(b = 8)),
      plot.caption      = element_text(colour = bio144_col[["muted"]], size = rel(0.8), hjust = 1),
      plot.title.position = "plot",
      strip.background  = element_rect(fill = bio144_col[["green_soft"]], colour = NA),
      strip.text        = element_text(colour = bio144_col[["ink"]], face = "bold",
                                       margin = margin(4, 4, 4, 4)),
      legend.title      = element_text(face = "bold", size = rel(0.9)),
      legend.key        = element_blank(),
      plot.background   = element_rect(fill = bio144_col[["paper"]], colour = NA),
      complete = TRUE
    )
}

scale_colour_bio144_d <- function(...) scale_colour_manual(values = bio144_palette, ...)
scale_fill_bio144_d   <- function(...) scale_fill_manual(values = bio144_palette, ...)
## Continuous: pale spring green to dark leaf green (one hue, so it also prints in grey)
scale_colour_bio144_c <- function(...) scale_colour_gradient(low = "#C9E3B6", high = "#173B26", ...)
scale_fill_bio144_c   <- function(...) scale_fill_gradient(low = "#E3EEDB", high = "#173B26", ...)
scale_color_bio144_d  <- scale_colour_bio144_d
scale_color_bio144_c  <- scale_colour_bio144_c

## Set everything as the default for the rest of the session/document
use_bio144_style <- function(base_size = 12) {
  theme_set(theme_bio144(base_size = base_size))
  options(ggplot2.discrete.colour   = bio144_palette,
          ggplot2.discrete.fill     = bio144_palette,
          ggplot2.continuous.colour = function(...) scale_colour_bio144_c(...),
          ggplot2.continuous.fill   = function(...) scale_fill_bio144_c(...))
  update_geom_defaults("point",   list(colour = bio144_col[["green"]], size = 2))
  update_geom_defaults("line",    list(colour = bio144_col[["green"]]))
  update_geom_defaults("smooth",  list(colour = bio144_col[["pink"]], fill = bio144_col[["band"]]))
  update_geom_defaults("bar",     list(fill = bio144_col[["green"]]))
  update_geom_defaults("col",     list(fill = bio144_col[["green"]]))
  update_geom_defaults("boxplot", list(fill = bio144_col[["green_soft"]], colour = bio144_col[["ink"]]))
  update_geom_defaults("density", list(fill = bio144_col[["green_soft"]], colour = bio144_col[["green"]]))
  invisible(TRUE)
}
