# penguin_distribution.R
# Project 1: Categorical Distributions & Morphized Anatomy
# Visualizing bill depth variation across Antarctic penguin species

# ── 0. Packages ───────────────────────────────────────────────────────────────
install.packages("palmerpenguins", quiet = TRUE)
install.packages("ggplot2", quiet = TRUE)

library(palmerpenguins)
library(ggplot2)

# ── 1. Load and clean data ────────────────────────────────────────────────────
data("penguins")
clean_df <- penguins[complete.cases(penguins), ]   # remove rows with any NA

# ── 2. Define a custom color palette per species ──────────────────────────────
my_colors <- c(
  "Adelie"    = "#C1666B",   # muted red
  "Chinstrap" = "#4281A4",   # ocean blue
  "Gentoo"    = "#48A999"    # teal green
)

# ── 3. Construct the multi-panel figure ───────────────────────────────────────
fig <- ggplot(clean_df, aes(x = species, y = bill_depth_mm, fill = species)) +

  # Violin layer — shows the full density distribution
  geom_violin(
    trim      = TRUE,
    alpha     = 0.7,
    color     = "grey90",
    linewidth = 0.3
  ) +

  # Boxplot layer — marks exact quartiles inside the violin
  geom_boxplot(
    width         = 0.12,
    alpha         = 0.25,
    color         = "grey15",
    linewidth     = 0.5,
    outlier.shape = 23,
    outlier.fill  = "lightyellow",
    outlier.color = "grey30",
    outlier.size  = 1.8
  ) +

  # Separate panel per species
  facet_grid(. ~ species) +

  # Apply custom color mapping
  scale_fill_manual(values = my_colors) +

  # Axis and title labels
  labs(
    title    = "Bill Depth Distribution Across Penguin Species",
    subtitle = "Comparing morphological variation — Palmer Archipelago, Antarctica",
    x        = NULL,
    y        = "Bill Depth (mm)",
    fill     = "Species",
    caption  = "Data: palmerpenguins R package"
  ) +

  # Clean theme styling
  theme_bw(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle      = element_text(size = 9, hjust = 0.5, color = "grey45"),
    plot.caption       = element_text(size = 7, color = "grey55", hjust = 1),
    strip.text         = element_text(face = "bold", size = 11),
    strip.background   = element_rect(fill = "grey95", color = "grey70"),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ── 4. Export as high-resolution PNG ─────────────────────────────────────────
ggsave(
  filename = "penguin_distribution.png",
  plot     = fig,
  width    = 11,
  height   = 6.5,
  dpi      = 300
)

message("Done! penguin_distribution.png has been saved.")
