# 20_viz_utils.R

# Define commonly used themes, plots, color schemes, and labelers to keep
#   consistent HI DoH CASPER branding

library(scales)
library(tidyverse)

# Theming ======================================================================

# Coloring ---------------------------------------------------------------------
# Colorblind check:
# https://davidmathlogic.com/colorblind/#%23C91F26-%23EC5F4C-%23FFC929-%23597FD2-%236A4477-%23000000

# Using ggplot naming
#  - color = lines
#  - fill = inside

color.palette <- c("#c91f26", "#ec5f4c", "#ffc929", "#597fd2", "#6a4477")
other.palette <- c("#cccccc", "#555555")
errbar.color <- "#111111"
labtext.color <- "#ffffff"

# Yes/No Coloring
yesno.fill <- list(
  "Yes"      = last(color.palette),
  "No"       = color.palette[1],
  "Unsure"   = other.palette[1],
  "Declined" = last(other.palette),
  "Accent"   = color.palette[4]
)

prep.fill <- list(
  "Well Prepared" = last(color.palette),
  "Somewhat Prepared" = color.palette[3],
  "Not Prepared"      = color.palette[1],
  "Unsure"            = last(other.palette)
)

supp.fill <- list(
  "Food" = color.palette[3],
  "Water" = color.palette[4],
  "Both" = last(color.palette)
)

lowhi.fill <- c(other.palette[1], color.palette)

# Font -------------------------------------------------------------------------
windowsFonts(Calibri = windowsFont("Calibri"))

# Theme ------------------------------------------------------------------------
casper.theme <-
  # Base font is Calibri size 14
  theme_minimal(base_family = "Calibri") +
  theme(
    text = element_text(size = 12),
    
    # Title
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.title.position = "plot", # center over whole plot
    
    # Legend
    legend.position = "",
    
    # Keep only y-axis
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis title and tick size
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  )

# Labelling ====================================================================
format.percent <- function(x) {
  if_else(
    x < 5, 
    "", 
    paste0(round(x, 0), "%")
  )
}

# Plotting =====================================================================
plot.bar <- function(data, x_var, y_var = "Pct", fill_var = "Answer",
                     ymin_var = "Pct_95CI_L", ymax_var = "Pct_95CI_U",
                     title = NULL, horizontal = FALSE, fill_list = yesno.fill) {
  p <- ggplot(
    data,
    mapping = aes(fill = .data[[fill_var]], label = .data[[y_var]], 
                  x = .data[[x_var]], y = .data[[y_var]], 
                  ymin = .data[[ymin_var]], ymax = .data[[ymax_var]])
  ) +
    geom_col() +
    geom_errorbar(color = errbar.color, width = 0.2, linewidth = 0.6) +
    scale_fill_manual(values = fill_list) +
    scale_y_continuous(
      labels = label_percent(scale=1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = title,
      x = NULL,
      y = "Percent of Households"
    ) +
    casper.theme
  
  if(horizontal) {
    p +
      geom_text(
        aes(label = format.percent(.data[[y_var]]), y = .data[[y_var]]/2),
        color = labtext.color,
        fontface = "bold",
        size = 8,
        size.unit = "pt",
        hjust = 0.5,
        vjust = 0.5
      ) +
      coord_flip() +
      theme(
        panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust=0.5)
      ) 
  } else {
    p +
      geom_text(
        aes(label = format.percent(.data[[y_var]]), y = .data[[y_var]]/2),
        color = labtext.color,
        fontface = "bold",
        size = 8,
        size.unit = "pt",
        hjust = 0.5,
        vjust = 0.5
      )
  }
}

# Saving =======================================================================
save.plot <- function(plot, filename) {
  ggsave(
    filename = filename,
    plot = plot, 
    height = 2.5,
    width = 3.6,
    units = "in"
  )
}
