################################################################################
# GLOBAL PLOT THEME FOR MASTER'S THESIS
################################################################################

library(ggplot2)

# Define clean academic theme
theme_thesis <- function() {
  theme_minimal(base_size = 11) %+replace%
    theme(
      # Grid lines
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95", linewidth = 0.5),
      
      # Titles and labels
      plot.title    = element_text(face = "bold", size = 14, hjust = 0.5, 
                                   margin = margin(b = 6)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", 
                                   margin = margin(b = 10)),
      axis.title    = element_text(face = "bold", size = 11),
      axis.text     = element_text(color = "grey10", size = 10),
      axis.text.x  = element_text(angle = 45, hjust = 1),
      
      # Legend positioning
      legend.position = "bottom",
      legend.title    = element_text(face = "bold", size = 10),
      legend.text     = element_text(size = 9),
      
      # Margins
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
    )
}

# Apply the theme universally
theme_set(theme_thesis())

################################################################################
# GLOBAL COLOR PALETTES
################################################################################

# Set global palettes for continuous data (gradients)
options(ggplot2.continuous.colour = function() scale_colour_distiller(palette = "YlGnBu"))
options(ggplot2.continuous.fill   = function() scale_fill_distiller(palette = "YlGnBu"))

# Set global palettes for discrete data (categories)
options(ggplot2.discrete.colour   = function() scale_colour_brewer(palette = "YlGnBu"))
options(ggplot2.discrete.fill     = function() scale_fill_brewer(palette = "YlGnBu"))