# Load required libraries
library(ggplot2)
library(showtext)

# Download libertine font
font_add_google("Libertinus Serif", family = "libertine")
showtext_auto()
showtext_opts(dpi = 300)

# Set global plot theme for the project
theme_set(
  theme_minimal(base_size = 12, base_family = "libertine") +
    theme(
      plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle     = element_text(size = 12, hjust = 0.5),
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y       = element_text(size = 10),
      axis.title.x      = element_text(margin = margin(t = 15), size = 11),
      axis.title.y      = element_text(margin = margin(r = 15), size = 11),
      panel.grid.major  = element_line(color = "grey95", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      legend.background = element_blank(),
      legend.text       = element_text(size = 10),
      legend.title      = element_text(size = 10)
    )
)

################################################################################
# GLOBAL COLOR PALETTES (Custom Blues)
################################################################################

# Set global palettes for continuous data
options(ggplot2.continuous.colour = function() scale_colour_distiller(palette = "Blues", direction = -1))
options(ggplot2.continuous.fill   = function() scale_fill_distiller(palette = "Blues", direction = -1))

# Define a custom vector of five blue colors
cb_blues <- c("#08306b", "#2171b5", "#4292c6", "#6baed6", "#9ecae1")

# Set global palettes for discrete data
options(ggplot2.discrete.colour = function() scale_colour_manual(values = cb_blues))
options(ggplot2.discrete.fill   = function() scale_fill_manual(values = cb_blues))