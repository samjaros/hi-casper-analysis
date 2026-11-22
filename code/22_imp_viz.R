#22_imp_viz.R

# Visualize impact of Hurricane Lowell

library(here)

source(here("code/20_viz_utils.R"))

# Setup ========================================================================
imp_table <- readRDS(here("data/impact_table.rds"))
donut_width <- 0.5

# Damage, Report to County, File Claim =========================================
damage.fill <- list(
  "None" = color.palette[5],
  "Minimal" = color.palette[4],
  "Damaged, but repairable" = color.palette[3],
  "Yes" = color.palette[1],
  "No" = color.palette[2],
  "Unsure" = other.palette[2]
)

home_damaged_donut <- imp_table %>%
  filter(
    !is.na(Pct),
    Question == "imp_damage_to_home"
  ) %>%
  mutate(
    fill_val = Answer,
    x_min = donut_width,
    x_max = 1,
    y_max = cumsum(Pct),
    y_min = lag(y_max, default = 0)
  ) %>%
  ggplot(aes(xmax = x_max, xmin = x_min, ymax = y_max, ymin = y_min, fill = fill_val)) +
  geom_rect(color = NA, show.legend = T) +
  geom_text(
    aes(
      label = format.percent(Pct),
      x = x_min + (x_max - x_min) / 2,
      y = y_min + (y_max - y_min) / 2
    ),
    color = labtext.color,
    fontface = "bold",
    size = 8,
    size.unit = "pt",
    hjust = 0.5,
    vjust = 0.5
  ) +
  scale_fill_manual(
    values = damage.fill,
    limits = names(damage.fill),
    drop = F
  ) +
  xlim(c(0,1)) +
  guides(fill = guide_legend(nrow = 2, byrow=T)) +
  coord_polar(theta = "y", start = -2.6) +
  casper.theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.key.size = unit(0.05, "npc"),
    legend.position = "bottom",
    legend.margin = margin(t = -0.05, r = -0.5, b = -0.02, unit = "npc"),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = -0.05, r = 0.45, b = -0.05, unit = "npc")
  )
home_damaged_donut

report_county_donut <- imp_table %>%
  filter(
    !is.na(Pct),
    Question == "imp_damage_report_cnty"
  ) %>%
  mutate(
    fill_val = Answer,
    x_min = donut_width,
    x_max = 1,
    y_max = cumsum(Pct),
    y_min = lag(y_max, default = 0)
  ) %>%
  ggplot(aes(xmax = x_max, xmin = x_min, ymax = y_max, ymin = y_min, fill = fill_val)) +
  ggtitle("Reported to County") +
  geom_rect(color = NA) +
  geom_text(
    aes(
      color = fill_val,
      label = format.percent(Pct),
      x = x_min + (x_max - x_min) / 2,
      y = y_min + (y_max - y_min) / 2
    ),
    fontface = "bold",
    size = 8,
    size.unit = "pt",
    hjust = 0.5,
    vjust = 0.5
  ) +
  scale_color_manual(values = yesno.color, guide = "none") +
  scale_fill_manual(values = yesno.fill) +
  xlim(c(0,1)) +
  coord_polar(theta = "y") +
  casper.theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.title = element_text(size=10, face = "plain", margin = margin(b=-5))
  )
report_county_donut

insurance_claim_donut <- imp_table %>%
  filter(
    !is.na(Pct),
    Question == "imp_damage_ins_claim"
  ) %>%
  mutate(
    Answer = if_else(Answer=="No insurance", "No", Answer),
    fill_val = Answer,
    x_min = donut_width,
    x_max = 1,
    y_max = cumsum(Pct),
    y_min = lag(y_max, default = 0)
  ) %>%
  ggplot(aes(xmax = x_max, xmin = x_min, ymax = y_max, ymin = y_min, fill = fill_val)) +
  ggtitle("Made Insurance Claim") +
  geom_rect(color = NA) +
  geom_text(
    aes(
      color = fill_val,
      label = format.percent(Pct),
      x = x_min + (x_max - x_min) / 2,
      y = y_min + (y_max - y_min) / 2
    ),
    fontface = "bold",
    size = 8,
    size.unit = "pt",
    hjust = 0.5,
    vjust = 0.5
  ) +
  scale_color_manual(values = yesno.color, guide = "none") +
  scale_fill_manual(values = yesno.fill) +
  xlim(c(0,1)) +
  coord_polar(theta = "y") +
  casper.theme +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.title = element_text(size=10, face = "plain", margin = margin(b=-5))
  )
insurance_claim_donut

damaged_home_tri_donut <- home_damaged_donut + 
  inset_element(
    report_county_donut, 
    left = 0.2,
    bottom = 0.5,
    right = 1,
    top = 1,
    align_to = "full"
  ) +
  inset_element(
    insurance_claim_donut, 
    left = 0.4,
    bottom = 0.08,
    right = 1.2,
    top = 0.58,
    align_to = "full"
  ) +
  plot_annotation(
    title = "Residence Damage, and If It Was\nReported to the County or Insurance",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13,
                                margin = margin(r=-0.9, unit = "npc")),
    )
  )
damaged_home_tri_donut
ggsave(
  filename = here("images/damaged_home_tri_donut.png"),
  plot = damaged_home_tri_donut, 
  height = 2.7,
  width = 3.65,
  units = "in",
  bg = "white" 
)

# Community Resources Sought ===================================================
comm_res_bars <- imp_table %>%
  filter(
    startsWith(Question, "imp_comm_res__"),
    Answer == "Yes"
  ) %>%
  mutate(
    x_val = 
      str_remove(Question, "imp_comm_res__") %>%
      fct() %>%
      fct_reorder(Pct) %>% # Order by percent, smallest first
      fct_relevel("None", "Unsure"), # Move Unsure and None to front (bottom of horizontal)
    # Unsure & None are different color
    fill_val = recode_values(
      x_val,
      "Unsure" ~ "Declined",
      "None"   ~ "None",
      default = "Yes" 
    )
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val", 
    title = "Community Resources Used by Type", horizontal = T
  )
comm_res_bars
save.plot(comm_res_bars, here("images/imp_comm_res_bars.png"))

