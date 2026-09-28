# 21_demog_viz.R

# Plots for demographics

library(here)
source(here("code/20_viz_utils.R"))

# Import =======================================================================
demo_table <- readRDS(here("data/demo_table.rds"))

# Age ==========================================================================
age_bars <- demo_table %>%
  filter(
    Question %in% c("demo_hh_infants", "demo_hh_kids", "demo_hh_adults", "demo_hh_elders"),
    Answer == "Yes"
  ) %>%
  mutate(
    x_val = factor(
      Question,
      levels = c("demo_hh_infants", "demo_hh_kids", "demo_hh_adults", "demo_hh_elders"),
      labels = c("Infants\n(<2 yrs old)", "Kids\n(2-17 yrs old)", "Adults\n(18-64 yrs old)", "Elders\n(65 or older)"),
    ),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", title = "Households by Member Age", horizontal = T
  )
age_bars
save.plot(age_bars, here("images/demo_age_bars.png"))

# Own/Rent =====================================================================
own_rent_bars <- demo_table %>%
  filter(
    Question == "demo_own_rent",
    !is.na(Pct)
  ) %>%
  mutate(
    x_val = factor(
      Answer,
      levels = c("Own", "Rent", "Other")
    ),
    fill_val = "Yes"
  ) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val", title = "Own vs Rent the Residence"
  )
own_rent_bars
save.plot(own_rent_bars, here("images/demo_own_rent_bars.png"))

# Language =====================================================================
language_bars <- demo_table %>%
  filter(
    Question == "demo_langs_cat",
    !is.na(Pct)
  ) %>%
  mutate(
    x_val = factor(
      Answer,
      levels = c("Only English", "English & 1+ Other") # Suppressing individual languages b/c count too small
    ),
    fill_val = "Yes"
  ) %>%
  filter(!is.na(x_val)) %>%
  plot.bar(
    x_var = "x_val", fill_var = "fill_val", title = "Language(s) Spoken at Home"
  )
language_bars
save.plot(language_bars, here("images/demo_language_bars.png"))
