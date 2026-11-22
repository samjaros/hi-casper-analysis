# 26_qual_viz.R

# Visualize Quantified Qualitative Data

library(here)

source(here("code/20_viz_utils.R"))

# Setup ========================================================================
qual_table <- readRDS(here("data/qual_table.rds"))

# Changes to Emergency Preparedness/Plans ======================================
qual_change_plans <- qual_table %>%
  filter(
    startsWith(Question, "supp_post_hurr_ch_recode_"),
    Answer == "Yes"
  ) %>%
  mutate(
    x_val = 
      str_remove(Question, "supp_post_hurr_ch_recode_") %>%
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
