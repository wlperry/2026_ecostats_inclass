# ==============================================================================
# CUSTOM THEMES FOR DATASCIENCE CLASS
# Source this file to load theme_small, theme_regular, and theme_large
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. THEME SMALL (Designed for ~ 3x3 inch PDF output)
# ------------------------------------------------------------------------------
theme_small <- function(base_size = 9, base_family = "sans") {
  theme(
    # --- GLOBAL TEXT ---
    text = element_text(family = base_family, size = base_size, colour = "black"),
    
    # --- PLOT ELEMENTS (Outer Canvas) ---
    plot.background = element_rect(fill = "white", colour = NA), 
    plot.title = element_text(face = "bold", size = rel(1.2)), 
    plot.subtitle = element_text(face = "plain", size = rel(1)), 
    plot.caption = element_text(face = "italic", size = rel(0.8)), 
    
    # --- AXIS LINES & TICKS (Thinner for small plots) ---
    axis.line = element_line(colour = "black", linewidth = 0.3), 
    axis.line.x = element_line(colour = "black"), 
    axis.line.y = element_line(colour = "black"), 
    
    axis.ticks = element_line(colour = "black", linewidth = 0.3), 
    axis.ticks.x = element_line(colour = "black"), 
    axis.ticks.y = element_line(colour = "black"), 
    
    # --- AXIS TITLES & TEXT ---
    axis.title = element_text(face = "bold", size = base_size), 
    axis.title.x = element_text(margin = margin(t = 5)),  # Smaller margin
    axis.title.y = element_text(margin = margin(r = 5), angle = 90), 
    
    axis.text = element_text(colour = "gray20"), 
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5), 
    axis.text.y = element_text(margin = margin(r = 3)), 
    
    # --- PANEL ELEMENTS ---
    panel.background = element_rect(fill = "white", colour = NA), 
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.3), 
    
    panel.grid.major =  element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    
    # --- LEGEND ---
    legend.position = "right", 
    legend.background = element_rect(fill = "white", colour = NA), 
    legend.key = element_rect(fill = NA, colour = NA), 
    legend.title = element_text(face = "bold", size = rel(0.9)), 
    legend.text = element_text(face = "plain", size = rel(0.7)), 
    
    # --- FACET STRIPS ---
    strip.background = element_rect(fill = "gray90", colour = "black", linewidth = 0.3), 
    strip.text = element_text(face = "bold", size = base_size), 
    strip.text.x = element_text(margin = margin(t = 3, b = 3)), 
    strip.text.y = element_text(angle = -90) 
  )
}


# ------------------------------------------------------------------------------
# 2. THEME REGULAR (Designed for ~ 7x7 inch PDF output)
# ------------------------------------------------------------------------------
theme_regular <- function(base_size = 16, base_family = "sans") {
  theme(
    # --- GLOBAL TEXT ---
    text = element_text(family = base_family, size = base_size, colour = "black"),
    
    # --- PLOT ELEMENTS (Outer Canvas) ---
    plot.background = element_rect(fill = "white", colour = NA), 
    plot.title = element_text(face = "bold", size = rel(1.2)), 
    plot.subtitle = element_text(face = "plain", size = rel(1)), 
    plot.caption = element_text(face = "italic", size = rel(0.8)), 
    
    # --- AXIS LINES & TICKS (Standard width) ---
    axis.line = element_line(colour = "black", linewidth = 0.5), 
    axis.line.x = element_line(colour = "black"), 
    axis.line.y = element_line(colour = "black"), 
    
    axis.ticks = element_line(colour = "black", linewidth = 0.5), 
    axis.ticks.x = element_line(colour = "black"), 
    axis.ticks.y = element_line(colour = "black"), 
    
    # --- AXIS TITLES & TEXT ---
    axis.title = element_text(face = "bold", size = base_size), 
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.title.y = element_text(margin = margin(r = 10), angle = 90), 
    
    axis.text = element_text(colour = "gray20"), 
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5), 
    axis.text.y = element_text(margin = margin(r = 5)), 
    
    # --- PANEL ELEMENTS ---
    panel.background = element_rect(fill = "white", colour = NA), 
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5), 
    
    panel.grid.major =  element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    
    # --- LEGEND ---
    legend.position = "right", 
    legend.background = element_rect(fill = "white", colour = NA), 
    legend.key = element_rect(fill = NA, colour = NA), 
    legend.title = element_text(face = "bold", size = rel(1.1)), 
    legend.text = element_text(face = "plain", size = rel(0.9)), 
    
    # --- FACET STRIPS ---
    strip.background = element_rect(fill = "gray90", colour = "black", linewidth = 0.5), 
    strip.text = element_text(face = "bold", size = base_size), 
    strip.text.x = element_text(margin = margin(t = 5, b = 5)), 
    strip.text.y = element_text(angle = -90) 
  )
}


# ------------------------------------------------------------------------------
# 3. THEME LARGE (Designed for ~ 16x16 inch PDF output)
# ------------------------------------------------------------------------------
theme_large <- function(base_size = 36, base_family = "sans") {
  theme(
    # --- GLOBAL TEXT ---
    text = element_text(family = base_family, size = base_size, colour = "black"),
    
    # --- PLOT ELEMENTS (Outer Canvas) ---
    plot.background = element_rect(fill = "white", colour = NA), 
    plot.title = element_text(face = "bold", size = rel(1.2)), 
    plot.subtitle = element_text(face = "plain", size = rel(1)), 
    plot.caption = element_text(face = "italic", size = rel(0.8)), 
    
    # --- AXIS LINES & TICKS ---
    axis.line = element_line(colour = "black", linewidth = 1.5),  
    axis.line.x = element_line(colour = "black"), 
    axis.line.y = element_line(colour = "black"), 
    
    # FIXED: Changed linewidth from 21.5 down to 1.5
    axis.ticks = element_line(colour = "black", linewidth = 1.5), 
    axis.ticks.x = element_line(colour = "black"), 
    axis.ticks.y = element_line(colour = "black"), 
    
    # --- AXIS TITLES & TEXT ---
    axis.title = element_text(face = "bold", size = rel(1.2)), 
    axis.title.x = element_text(margin = margin(t = 20)), 
    axis.title.y = element_text(margin = margin(r = 20), angle = 90), 
    
    axis.text = element_text(colour = "gray20"), 
    # FIXED: Added margin(t = 10) to push the text down from the axis line
    axis.text.x = element_text(margin = margin(t = 10), angle = 0, vjust = 1, hjust = 0.5), 
    axis.text.y = element_text(margin = margin(r = 10)), 
    
    # --- PANEL ELEMENTS ---
    panel.background = element_rect(fill = "white", colour = NA), 
    panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5), 
    
    panel.grid.major =  element_blank(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    
    # --- LEGEND ---
    legend.position = "right", 
    legend.background = element_rect(fill = "white", colour = NA), 
    legend.key = element_rect(fill = NA, colour = NA), 
    legend.title = element_text(face = "bold", size = rel(1.1)), 
    legend.text = element_text(face = "plain", size = rel(0.9)), 
    
    # --- FACET STRIPS ---
    strip.background = element_rect(fill = "gray90", colour = "black", linewidth = 1.5), 
    strip.text = element_text(face = "bold", size = base_size), 
    strip.text.x = element_text(margin = margin(t = 10, b = 10)), 
    strip.text.y = element_text(angle = -90) 
  )
}