# 23_sup_viz.R

# Supplies Visualizations

library(here)
library(ggpattern)
library(patchwork)

source(here("code/20_viz_utils.R"))

# Setup ========================================================================
supp_table <- readRDS(here("data/supp_table.rds"))

donut_width <- 0.5
dodge_width <- 0.9

# Food & Water =================================================================
supp.fill <- list(
  "Food" = color.palette[1],
  "Water" = color.palette[5],
  "Both" = color.palette[1]
)

fw_vars <- do.call(
  paste0,
  expand.grid(
    "supp_",
    c("food_", "water_", "fw_"),
    c("3d", "7d", "14d")
  )
)

food_water_bars <- supp_table %>%
  filter(
    Question %in% fw_vars,
    Answer == "Yes"
  ) %>%
  mutate(
    x_val = case_when(
      str_detect(Question, "3d")  ~ "3 Days",
      str_detect(Question, "7d")  ~ "7 Days",
      str_detect(Question, "14d") ~ "14 Days"
    ) %>% fct(c("3 Days", "7 Days", "14 Days")),
    fill_val = case_when(
      str_detect(Question, "food")  ~ "Food",
      str_detect(Question, "water") ~ "Water",
      str_detect(Question, "fw")    ~ "Both"
    ) %>% fct(c("Food", "Water", "Both"))
  ) %>%
  ggplot(
    aes(x = x_val, y = Pct, ymin = Pct_95CI_L, ymax = Pct_95CI_U,
        fill = fill_val, pattern = fill_val)
  ) +
  geom_col_pattern(
    pattern_colour = NA,
    pattern_fill = supp.fill[2],
    pattern_density = 0.5,
    position = position_dodge(width = dodge_width)
  ) +
  geom_errorbar(
    color = errbar.color,
    width = 0.2,
    linewidth = 0.6,
    position = position_dodge(width = dodge_width)
  ) +
  geom_text(
    aes(label = format.percent(Pct), y = Pct/2),
    color = labtext.color,
    fontface = "bold",
    size = 8,
    size.unit = "pt",
    hjust = 0.5,
    vjust = 0.5,
    position = position_dodge(width = dodge_width)
  ) +
  scale_fill_manual(name = NULL, values = supp.fill) +
  scale_pattern_manual(
    name = NULL,
    values = c("Food" = "none", "Water" = "none", "Both" = "stripe")
  ) +
  scale_y_continuous(
    labels = label_percent(scale=1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Household Food & Water",
    x = NULL,
    y = "Percent of Households"
  ) +
  casper.theme +
  theme(
    legend.box.spacing = unit(0, "in"),
    legend.justification = "center",
    legend.location = "plot",
    legend.position = "bottom",
    legend.key.size = unit(0.1, "in")
  )
food_water_bars
save.plot(food_water_bars, here("images/supp_food_water_bars.png"))

# Electricity-Dependent Health Needs & Have Backup Power =======================
dep_med_equp_donut <- supp_table %>%
  filter(
    !is.na(Pct),
    Question == "supp_med_electr"
  ) %>%
  mutate(
    fill_val = Answer,
    x_min = donut_width,
    x_max = 1,
    y_max = cumsum(Pct),
    y_min = lag(y_max, default = 0)
  ) %>%
  ggplot(aes(xmax = x_max, xmin = x_min, ymax = y_max, ymin = y_min, fill = fill_val)) +
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
    scale_fill_manual(values = yesno.fill,
                      breaks = c("Yes", "No")) + # Remove Unsure
    xlim(c(0,1)) +
    coord_polar(theta = "y", start = 1) +
    casper.theme +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.key.size = unit(0.05, "npc"),
      legend.position = "bottom",
      legend.margin = margin(t=-0.05, r = -0.3, b=0.01, unit = "npc"),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = -0.05, r = 0.3, unit = "npc")
    )
dep_med_equp_donut

have_backup_donut <- supp_table %>%
  filter(
    !is.na(Pct),
    Question == "supp_med_electr_backup"
  ) %>%
  mutate(
    fill_val = Answer,
    x_min = donut_width,
    x_max = 1,
    y_max = cumsum(Pct),
    y_min = lag(y_max, default = 0)
  ) %>%
  ggplot(aes(xmax = x_max, xmin = x_min, ymax = y_max, ymin = y_min, fill = fill_val)) +
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
    legend.position = "none",
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
have_backup_donut

elec_med_need_dbl_donut <- dep_med_equp_donut + 
  inset_element(
    have_backup_donut, 
    left = 0.4,
    bottom = 0.2,
    right = 1.15,
    top = 0.85,
    align_to = "full"
  ) +
  plot_annotation(
    title = "Households with Electricity-Dependent\nMedical Needs and Backup Power Status",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13,
                                margin = margin(r=-0.62, unit = "npc")),
    )
  )
elec_med_need_dbl_donut
ggsave(
  filename = here("images/supp_elec_med_need_donut.png"),
  plot = elec_med_need_dbl_donut, 
  height = 2.5,
  width = 3.65,
  units = "in",
  bg = "white" 
)

# Emergeny supplies & generator ================================================
kitgen.fill <- list(
  "Emergency\nKit" = color.palette[1],
  "Generator" = color.palette[5],
  "Both" = color.palette[1]
)

emerg_kitgen_bars <- supp_table %>%
  filter(
    Question %in% paste0("supp_emerg_", c("kit", "gen", "kitgen")),
    Answer == "Yes"
  ) %>%
  mutate(
    fill_val = case_when(
      Question == "supp_emerg_kit" ~ "Emergency\nKit",
      Question == "supp_emerg_gen" ~ "Generator",
      Question == "supp_emerg_kitgen"   ~ "Both"
    ) %>% fct(c("Emergency\nKit", "Generator", "Both")),
    x_val = fill_val
  ) %>%
  ggplot(
    aes(x = x_val, y = Pct, ymin = Pct_95CI_L, ymax = Pct_95CI_U,
        fill = fill_val, pattern = fill_val)
  ) +
  geom_col_pattern(
    pattern_colour = NA,
    pattern_fill = kitgen.fill[2],
    pattern_density = 0.5,
    position = position_dodge(width = dodge_width)
  ) +
  geom_errorbar(
    color = errbar.color,
    width = 0.2,
    linewidth = 0.6,
    position = position_dodge(width = dodge_width)
  ) +
  geom_text(
    aes(label = format.percent(Pct), y = Pct/2),
    color = labtext.color,
    fontface = "bold",
    size = 8,
    size.unit = "pt",
    hjust = 0.5,
    vjust = 0.5,
    position = position_dodge(width = dodge_width)
  ) +
  scale_fill_manual(name = NULL, values = kitgen.fill) +
  scale_pattern_manual(
    name = NULL,
    values = c("Emergency\nKit" = "none", "Generator" = "none", "Both" = "stripe")
  ) +
  scale_y_continuous(
    labels = label_percent(scale=1),
    breaks = seq(0,100,25),
    limits = c(0,100)
  ) +
  guides(
    fill = "none",
    pattern = "none"
  ) +
  labs(
    title = "Household Emergency Supplies",
    x = NULL,
    y = "Percent of Households"
  ) +
  casper.theme +
  theme(
    legend.box.spacing = unit(0, "in"),
    legend.justification = "center",
    legend.location = "plot",
    legend.position = "bottom",
    legend.key.size = unit(0.1, "in")
  )
emerg_kitgen_bars
save.plot(emerg_kitgen_bars, here("images/supp_kit_generator_bars.png"))

# Sought Community Resources ===================================================
supp_comm_res_bar <- supp_table %>%
  filter(
    startsWith(Question, "imm_com_res__"),
    Answer == "Yes"
  ) %>%
  mutate(
    x_val = 
      str_remove(Question, "imm_com_res__") %>%
      str_to_title() %>%
      # TODO: stopping spot
      factor() %>%
      fct_recode(
        `Buy generator\nor backup`  = "Buy generator/backup",
        `Buy emergency\nsupplies`   = "Buy emergency supplies",
        `Fortify windows\nor doors` = "Fortify windows/doors",
        `Other`                     = "Different Answer"
      ) %>%
      fct_reorder(Pct) %>% # Order by percent, smallest first
      fct_relevel("Other", "Unsure"), # Move Unsure and None to front (bottom of horizontal)
    # Unsure & None are different color
    fill_val = recode_values(
      as.character(x_val),
      "Unsure" ~ "Declined",
      default = "Yes" 
    )
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val", 
    title = "Changes to Preparedness", horizontal = T
  ) +
  theme(
    axis.text.y = element_text(size = 8)
  )
qual_change_plans
save.plot(qual_change_plans, here("images/qual_change_plans_bars.png"))

