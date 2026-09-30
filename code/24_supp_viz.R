# 24_sup_viz.R

# Supplies Visualizations

library(here)

source(here("code/20_viz_utils.R"))

# Setup ========================================================================
supp_table <- readRDS(here("data/supp_table.rds"))

dodge_width <- 0.9

# Food & Water =================================================================
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
        fill = fill_val)
  ) +
  geom_col(position = position_dodge(width = dodge_width)) +
  geom_errorbar(color = errbar.color, width = 0.2, linewidth = 0.6,
                position = position_dodge(width = dodge_width)) +
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
  scale_y_continuous(
    labels = label_percent(scale=1),
    expand = expansion(mult = c(0, 0.05))
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
food_water_bars
save.plot(food_water_bars, here("images/supp_food_water_bars.png"))

# Fire Safety Devices ==========================================================
fire_safety_bars <- supp_table %>%
  filter(
    Question %in% c("supp_fire_exting", "supp_smoke_detect"),
    Answer != "Declined"
  ) %>%
  mutate(
    x_val = Answer %>%
      fct_relevel("Yes", "No", "Unsure"),
    fill_val = 
      fct_recode(
        Question,
        "Working fire extinguisher" = "supp_fire_exting",
        "Working smoke detector" = "supp_smoke_detect"
      )
  ) %>%
  ggplot(
    aes(x = x_val, y = Pct, ymin = Pct_95CI_L, ymax = Pct_95CI_U,
        fill = fill_val)
  ) +
  geom_col(position = position_dodge(width = fw_dodge_width)) +
  geom_errorbar(color = errbar.color, width = 0.2, linewidth = 0.6,
                position = position_dodge(width = fw_dodge_width)) +
  geom_text(
    aes(label = format.percent(Pct), y = Pct/2),
    color = labtext.color,
    fontface = "bold",
    size = 8,
    size.unit = "pt",
    hjust = 0.5,
    vjust = 0.5,
    position = position_dodge(width = fw_dodge_width)
  ) +
  scale_fill_manual(name = NULL, values = color.palette[c(2, 5)]) +
  scale_y_continuous(
    labels = label_percent(scale=1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Fire Safety Devices",
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
fire_safety_bars
save.plot(fire_safety_bars, here("images/fire_safety_bars.png"))
