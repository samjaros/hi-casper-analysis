# 25_hlth_viz.R

# Health Visualizations

library(here)

source(here("code/20_viz_utils.R"))

# Import =======================================================================
hlth_table <- readRDS(here("data/health_table.rds"))

# Vaccine Importance ===========================================================
vax_impt_bar <- hlth_table %>%
  filter(
    Question == "supp_vax_important",
    Answer != "Declined"
  ) %>%
  mutate(
    x_val = fct_recode(
      Answer,
      `Not at all important: 1` = "1",
      `Very Important: 5`       = "5"
    ) %>% fct_rev() %>% fct_relevel("Unsure"),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val",
    title = "Importance of Staying\nUp-to-Date on Vaccines", horizontal = T
  ) +
  theme(
    axis.text.y = element_text(hjust = 1)
  )
vax_impt_bar
save.plot(vax_impt_bar, here("images/hlth_vax_impt_bar.png"))

# DEET Insect Repellent ========================================================
deet_bar <- hlth_table %>%
  filter(
    Question == "supp_mosquito_deet"
  ) %>%
  mutate(
    x_val = fct_recode(
      Answer,
      `Not at all important: 1` = "1",
      `Very Important: 5`       = "5"
    ) %>% fct_rev() %>% fct_relevel("Unsure"),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val",
    title = "Importance of Staying\nUp-to-Date on Vaccines", horizontal = T
  ) +
  theme(
    axis.text.y = element_text(hjust = 1)
  )
deet_bar
save.plot(deet_bar, here("images/hlth_deet_bar.png"))

# Prevent Mosquito Breeding ====================================================
prevent_mosquitoes_bar <- hlth_table %>%
  filter(
    Question == "supp_mosquito_prevent_cat",
    !is.na(Pct)
  ) %>%
  mutate(
    x_val = Answer %>%
      fct_relevel(
        c("Daily", "Weekly", "Every 8+ days", "Never", "Unsure")
      ) %>% fct_rev(),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val",
    title = "Frequency of Taking Measures to\nPrevent Mosquitoes from Breeding",
    horizontal = T
  ) +
  theme(
    axis.text.y = element_text(hjust = 1)
  )
prevent_mosquitoes_bar
save.plot(prevent_mosquitoes_bar, here("images/hlth_prevent_mosquitoes_bar.png"))

# Food Security ================================================================
food_security_bar <- hlth_table %>%
  filter(
    Question == "hlth_food_ran_out",
    !is.na(Pct)
  ) %>%
  mutate(
    x_val = Answer %>%
      fct_relevel(
        c("Usually", "Sometimes", "Rarely", "Never", "Unsure")
      ) %>% fct_rev(),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val",
    title = "How Often Households\nRan Out of Food",
    horizontal = T
  ) +
  theme(
    axis.text.y = element_text(hjust = 1)
  )
food_security_bar
save.plot(food_security_bar, here("images/hlth_food_security_bar.png"))

# Overall Health ===============================================================
overall_bars <- hlth_table %>%
  filter(
    Question %in% c("hlth_physical", "hlth_mental"),
    Answer != "Declined"
  ) %>%
  mutate(
    y_val = 
      fct_recode(
        Question,
        `Physical` = "hlth_physical",
        `Mental`   = "hlth_mental"
      ) %>% fct_rev(),
    fill_val = fct_recode(
      Answer,
      `Very poor` = "1",
      `Poor`      = "2",
      `Fair`      = "3",
      `Good`      = "4",
      `Very good` = "5"
    ) %>% 
      fct_drop() %>%
      fct_relevel(
        c("Very good", "Good", "Fair", "Poor", "Very poor", "Unsure")
      )
  ) %>%
  ggplot(
    aes(x = Pct, y = y_val, fill = fill_val)
  ) +
  geom_col(position = position_stack()) +
  scale_fill_manual(
    name = NULL,
    values = rev(lowhi.fill)
  ) +
  geom_text(
    aes(label = format.percent(Pct)),
    position = position_stack(vjust = 0.5),
    color = labtext.color,
    fontface = "bold",
    size = 8,
    size.unit = "pt",
    hjust = 0.5,
    vjust = 0.5
  ) +
  scale_x_continuous(
    labels = label_percent(scale=1),
    expand = expansion(add = c(0, 3))
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Overall Household Health and Wellbeing",
    x = "Percent of Households",
    y = NULL, 
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      nrow = 2,
      position = "bottom",
      reverse = TRUE)
  ) +
  theme_minimal(base_family = "Calibri", base_size = 10) +
  theme(
    # Title
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.title.position = "plot", # center over whole plot
    
    # Keep only x-axis
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Increase axis title and tick size
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold", size = 12),
    
    # Legend
    legend.box.spacing = unit(0, "in"),
    legend.justification = "center",
    legend.location = "plot",
    legend.key.size = unit(0.1, "in")
  )
overall_bars
save.plot(overall_bars, here("images/hlth_overall_bars.png"))
