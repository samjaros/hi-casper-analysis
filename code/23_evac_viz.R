# 23_evac_viz.R

# Emergency evacuation visualizations

library(here)

source(here("code/20_viz_utils.R"))

# Import =======================================================================
evac_table <- readRDS(here("data/evac_table.rds"))

# Conditions ===================================================================
conditions_bars <- evac_table %>%
  filter(
    str_detect(Question, "evac_.*_disease"),
    Answer == "Yes"
  ) %>%
  mutate(
    x_val = 
      fct_recode(
        Question,
        `Chronic disease\nrequiring\nmedication` = "evac_chronic_disease",
        `Physical or\ndevelopmental\ndisability` = "evac_phys_disease",
        `Mental health\ncondition`               = "evac_ment_disease"
      ) %>%
      fct_reorder(Pct, .desc = T), # Order by percent, biggest first
    # Unsure & None are different color
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val", 
    title = "Conditions Delaying Evacuation"
  )
conditions_bars
save.plot(conditions_bars, here("images/evac_condition_bars.png"))

# Emergency Plans ==============================================================
plans_bars <- evac_table %>%
  filter(
    Question %in% c("evac_comm_plan", "evac_meeting_place", "evac_impt_docs"),
    Answer == "Yes"
  ) %>%
  mutate(
    x_val =
      fct_recode(
        Question,
        `Communication\nplan`                     = "evac_comm_plan",
        `Designated\nmeeting place`               = "evac_meeting_place",
        `Secured copies of\nimportant\ndocuments` = "evac_impt_docs"
      ) %>%
      fct_reorder(Pct, .desc = T),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val",
    title = "Household Emergency Plans"
  )
plans_bars
save.plot(plans_bars, here("images/evac_plans_bars.png"))

# Hurricane Evacuation =========================================================
hurr_bars <- evac_table %>%
  filter(
    Question %in% paste0("evac_hur_cat", 1:5),
    Answer != "Declined"
  ) %>%
  mutate(
    y_val = 
      fct_recode(
        Question,
        `Category 1` = "evac_hur_cat1",
        `Category 2` = "evac_hur_cat2",
        `Category 3` = "evac_hur_cat3",
        `Category 4` = "evac_hur_cat4",
        `Category 5` = "evac_hur_cat5"
      ),
    fill_val = fct_relevel(
      Answer,
      "Shelter in place at home", "Public shelter", "Friend/family's home",
      "Other", "Unsure"
    ) %>% fct_rev()
  ) %>%
  ggplot(
    aes(x = Pct, y = y_val, fill = fill_val)
  ) +
  geom_col(position = position_stack()) +
  scale_fill_manual(
    name = NULL,
    values = color.palette
  ) +
  scale_x_continuous(
    labels = label_percent(scale=1),
    expand = expansion(add = c(0, 3))
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Evacuation Location\nby Hurricane Category",
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
hurr_bars
save.plot(hurr_bars, here("images/evac_hurr_bars.png"))

# Evacuation Barriers ==========================================================
evac_barrier_bars <- evac_table %>%
  filter(
    Question == "evac_main_barrier",
    !is.na(Pct)
  ) %>%
  mutate(
    x_val =
      fct_recode(
        Answer,
        `Concern about leaving pets` = "Concern about leaving pet(s)",
        `Leaving property vacant` = "Concern about leaving property vacant",
        `Don't know where to go` = "Uncertainty about where to go",
        `Household will evacuate` = "No barriers - household will evacuate",
        `Household won't evacuate`= "No barriers - household would choose not to evacuate"
      ) %>%
      fct_relevel(
        "Concern about leaving pets",
        "Don't know where to go",
        "Leaving property vacant",
        "Health or mobility issues",
        "Lack of transportation",
        "Inconvenient or expensive",
        "Unsure",
        "Other",
        "Household will evacuate",
        "Household won't evacuate"
      ) %>% fct_rev(),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val",
    title = "Main Barrier to Evacuation", horizontal = T
  ) +
  theme(
    axis.text.y = element_text(size = , hjust = 1)
  )
evac_barrier_bars
save.plot(evac_barrier_bars, here("images/evac_barrier_bars.png"))

# Emergency Information Source =================================================
info_bars <- evac_table %>%
  filter(
    Question == "evac_main_info_source",
    !is.na(Pct)
  ) %>%
  mutate(
    x_val = 
      fct_recode(
        Answer,
        `Social\nmedia` = "Social media",
        `Radio\nor TV` = "Radio or TV",
        `Gov't\nwebsite` = "Government website",
        `Friends\n& family` = "Community, friends, or family"
      ) %>%
      fct_reorder(Pct, .desc = T) %>% # Order by percent, biggest first
      fct_relevel("Other", after = Inf), # Other always at the end
    # Unsure & None are different color
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val", 
    title = "Main Source of Emergency Information"
  ) +
  theme(axis.text.x = element_text(size=10))
info_bars
save.plot(info_bars, here("images/evac_info_bars.png"))
