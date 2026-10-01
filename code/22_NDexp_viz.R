# 22_NDexp_viz.R

# Visualize natural disaster experience variables

library(here)
source(here("code/20_viz_utils.R"))

# Import =======================================================================
NDexp_table <- readRDS(here("data/NDexp_table.rds"))

# Affected by ND by Type =======================================================
ND_type_bars <- NDexp_table %>%
  filter(
    startsWith(Question, "NDexp_type_"),
    Answer == "Yes"
  ) %>%
  mutate(
    x_val = 
        fct_recode(
          Question,
          Hurricane  = "NDexp_type_hurricane", 
          None       = "NDexp_type_none",
          Flood      = "NDexp_type_flood",
          Tsunami    = "NDexp_type_tsunami",
          Earthquake = "NDexp_type_earthquake",
          Eruption   = "NDexp_type_eruption",
          Landslide  = "NDexp_type_landslide",
          Wildfire   = "NDexp_type_wildfire",
          Unsure     = "NDexp_type_unsure"
        ) %>%
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
    title = "Natural Disasters Experienced", horizontal = T
  )
ND_type_bars
save.plot(ND_type_bars, here("images/NDexp_ND_type_bars.png"))


# Prepared for ND ==============================================================
ND_prepared_bars <- NDexp_table %>%
  filter(
    Question == "NDexp_prepared",
    !is.na(Pct)
  ) %>%
  mutate(
    fill_val = Answer,
    x_val = fct_recode(
      Answer,
      `Well\nPrepared`     = "Well Prepared",
      `Somewhat\nPrepared` = "Somewhat Prepared",
      `Not\nPrepared`      = "Not Prepared",
      Unsure               = "Unsure",
    ) %>% fct_rev()
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val",
    title = "Natural Disaster Preparedness", fill_list = prep.fill
  )
ND_prepared_bars
save.plot(ND_prepared_bars, here("images/NDexp_ND_prepared_bars.png"))

# Home Safe for ND =============================================================
# Using an icon visualization for this
