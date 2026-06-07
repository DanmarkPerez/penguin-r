# penguin_correlation.R
# Project 2: Correlation Analysis & Statistical Trends
# Examining the relationship between flipper length and bill length

# ── 0. Packages ───────────────────────────────────────────────────────────────
# install.packages("palmerpenguins", quiet = TRUE)
# install.packages("ggplot2",        quiet = TRUE)
# install.packages("dplyr",          quiet = TRUE)
# install.packages("tidyr",          quiet = TRUE)

library(palmerpenguins)
library(ggplot2)
library(dplyr)
library(tidyr)

# ── 1. Load and filter data ───────────────────────────────────────────────────
data("penguins")

penguin_clean <- penguins %>%
  drop_na(flipper_length_mm, bill_length_mm, species, island)

# ── 2. Define visual mappings ─────────────────────────────────────────────────
color_map <- c(
  "Adelie"    = "#B5446E",   # raspberry pink
  "Chinstrap" = "#3D7AB5",   # cobalt blue
  "Gentoo"    = "#2D936C"    # emerald green
)

shape_map <- c(
  "Biscoe"    = 19,   # filled circle
  "Dream"     = 18,   # filled diamond
  "Torgersen" = 17    # filled triangle
)

# ── 3. Build the scatterplot ──────────────────────────────────────────────────
scatter <- ggplot(penguin_clean, aes(
  x     = bill_length_mm,
  y     = flipper_length_mm,
  color = species,
  shape = island
)) +

  # Linear regression trendline with transparent SE band
  geom_smooth(
    method    = "lm",
    formula   = y ~ x,
    aes(group = 1),
    color     = "black",
    fill      = "grey70",
    alpha     = 0.20,
    linewidth = 0.8,
    se        = TRUE
  ) +

  # Data points
  geom_point(size = 3, alpha = 0.80) +

  # Apply color and shape scales
  scale_color_manual(values = color_map) +
  scale_shape_manual(values = shape_map) +

  # Labels
  labs(
    title    = "Bill Length vs. Flipper Length in Antarctic Penguins",
    subtitle = "Linear trend fitted across all species — island groupings shown by shape",
    x        = "Bill Length (mm)",
    y        = "Flipper Length (mm)",
    color    = "Species",
    shape    = "Island",
    caption  = "Data: palmerpenguins R package | Regression: OLS with 95% confidence band"
  ) +

  # Theme
  theme_light(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle   = element_text(size = 9, hjust = 0.5, color = "grey45"),
    plot.caption    = element_text(size = 7, color = "grey55"),
    legend.position = "right",
    legend.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ── 4. Save output ────────────────────────────────────────────────────────────
ggsave(
  filename = "penguin_correlation.png",
  plot     = scatter,
  width    = 11,
  height   = 7,
  dpi      = 300
)

message("Done! penguin_correlation.png has been saved.")

# ── TREND OBSERVATIONS ────────────────────────────────────────────────────────
# - Bill length and flipper length show a clear positive linear relationship
#   when looking at all three species combined.
# - Gentoo penguins stand out with notably longer flippers, clustering at the
#   top of the chart despite varying bill lengths.
# - Adelie penguins tend to have shorter bills and shorter flippers overall,
#   sitting in the lower-left region of the plot.
# - Chinstrap penguins display longer bills relative to their flipper length,
#   separating them from Adelie penguins even though both share similar flipper
#   size ranges.
# - Biscoe island is predominantly associated with Gentoo observations, while
#   Dream and Torgersen islands are more evenly distributed across species.
