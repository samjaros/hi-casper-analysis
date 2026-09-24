# 2_clean_vars.R

# Factor categorical variables
# Create indicator variables

library(tidyverse)
library(lubridate)

# Import =======================================================================
casper_2026 <- readRDS("./data/casper_2026_raw.rds")

# Fix Invalid Obs ==============================================================

# Mismatch household count -----------------------------------------------------
# If there is no mismatch, all good
# If the HH total is 1, randomly assign adult or elder
# All other mismatches leave alone (for now)
set.seed(123)
casper_fix <- casper_2026 %>%
  # Define "wrong" surveys & provide a random number for coin flips
  mutate(
    t_wrong_1 = 
      (demo_n_hh != demo_n_u2 + demo_n_2to17 + demo_n_18to64 + demo_n_65p) &
      (demo_n_hh == 1),
    t_rand = runif(n())
  ) %>%
  # Define what to do with the "wrong" surveys
  mutate(
    demo_n_u2 = if_else(t_wrong_1, 0, demo_n_u2),
    demo_n_2to17 = if_else(t_wrong_1, 0, demo_n_2to17),
    demo_n_18to64 = case_when(
      t_wrong_1 & t_rand < 0.5  ~ 1,
      t_wrong_1 & t_rand >= 0.5 ~ 0,
      T                         ~ demo_n_18to64
    ),
    demo_n_65p = case_when(
      t_wrong_1 ~ 1 - demo_n_18to64,
      T         ~ demo_n_65p
    )
  ) %>%
  # Remove temporary variables
  select(-starts_with("t_"))

# Data Wrangling ===============================================================
casper_clean <- casper_fix

# Indicator Variables ----------------------------------------------------------
# Ages
casper_clean <- casper_clean %>%
  mutate(
    demo_hh_infants = if_else(demo_n_u2 > 0, "Yes", "No", missing = NA_character_),
    demo_hh_kids = if_else(demo_n_2to17 > 0, "Yes", "No", missing = NA_character_),
    demo_hh_adults = if_else(demo_n_18to64 > 0, "Yes", "No", missing = NA_character_),
    demo_hh_elders = if_else(demo_n_65p > 0, "Yes", "No", missing = NA_character_)
  )

# Languages
# table(casper_2026$demo_language_other)

# Create indicators with search, then create a clean, combined variable
casper_clean <- casper_clean %>%
  # All lowercase for searching
  mutate(
    demo_language = str_to_lower(demo_language),
    demo_language_other = str_to_lower(demo_language_other)
  ) %>%
  # Indicator column for each language
  mutate(
    demo_lang_english = if_else(
      str_detect(demo_language, "english"), "Yes", "No", missing = "No"),
    demo_lang_burmese = if_else(
      str_detect(demo_language_other, "burmese"), "Yes", "No", missing = "No"),
    demo_lang_french = if_else(
      str_detect(demo_language_other, "french"), "Yes", "No", missing = "No"),
    demo_lang_hawaiian = if_else(
      str_detect(demo_language_other, "hawaii"), "Yes", "No", missing = "No"),
    demo_lang_japanese = if_else(
      str_detect(demo_language_other, "japanese"), "Yes", "No", missing = "No"),
    demo_lang_korean = if_else(
      str_detect(demo_language_other, "korean"), "Yes", "No", missing = "No"),
    demo_lang_marshallese = if_else(
      str_detect(demo_language_other, "marshallese"), "Yes", "No", missing = "No"),
    demo_lang_pidgin = if_else(
      str_detect(demo_language_other, "pidgin|pigeon"), "Yes", "No", missing = "No"),
    demo_lang_filipino = if_else(
      str_detect(demo_language_other, "filipino|tagalog"), "Yes", "No", missing = "No"),
    demo_lang_pohnpeian = if_else(
      str_detect(demo_language_other, "pohnpeian"), "Yes", "No", missing = "No"),
    demo_lang_portuguese = if_else(
      str_detect(demo_language_other, "portuguese"), "Yes", "No", missing = "No"),
    demo_lang_spanish = if_else(
      str_detect(demo_language_other, "spanish"), "Yes", "No", missing = "No"),
    demo_lang_german = if_else(
      str_detect(demo_language_other, "german"), "Yes", "No", missing = "No"),
    demo_lang_ilocano = if_else(
      str_detect(demo_language_other, "ilocano"), "Yes", "No", missing = "No"),
    demo_lang_kosrae = if_else(
      str_detect(demo_language_other, "kosrae"), "Yes", "No", missing = "No")
  ) %>%
  mutate(
    demo_langs = apply(
      select(., starts_with("demo_lang_")),
      1,
      function(row) {
        paste(
          str_to_title(str_remove(names(row), "demo_lang_"))[row == "Yes"],
          collapse = ", "
          )
      }
    )
  )

# Create language breakdown
#  - Only English
#  - English + at least 1 other
#  - No English (just put the language(s) they speak)
casper_clean <- casper_clean %>%
  mutate(demo_langs_cat = 
           case_when(
             demo_langs == "English"           ~ "Only English",
             str_detect(demo_langs, "English") ~ "English & 1+ Other",
             T                                 ~ demo_langs
           ))

# Language Data Checks
# First 2 columns have the raw data, second two have processed data
casper_clean %>%
  select(c(starts_with("demo_lang"), -starts_with("demo_lang_"))) %>%
  distinct() %>%
  View("Lang Processed Check")

# First 2 columns have the raw data, rest have indicators. Every language should be indicated
casper_clean %>%
  select(c(starts_with("demo_language"), starts_with("demo_lang_"))) %>%
  distinct() %>%
  View("Lang Indicator Check")

# Natural disaster experience --------------------------------------------------
# Get data as indicator columns
# TODO: Should these be NA if they "weren't affected" by a disaster?
casper_clean <- casper_clean %>%
  # Keep raw data in separate column
  # "Other" data could be overwritten, so rename
  mutate(
    NDexp_types = NDexp_type,
    NDexp_type_other_ans = NDexp_type_other
  ) %>%
  # Get longer version of data
  separate_longer_delim(
    cols = NDexp_type,
    delim = ", "
  ) %>%
  # Clean up names to use as columns
  mutate(
    NDexp_type = replace_values(
      NDexp_type,
      "Hurricane/Tropical Storm" ~ "hurricane",
      "Flooding"                 ~ "flood",
      "Earthquake"               ~ "earthquake",
      "Tsunami"                  ~ "tsunami",
      "Landslide/Mudslide"       ~ "landslide",
      "Volcanic Eruption"        ~ "eruption",
      "Wildfire"                 ~ "wildfire",
      "Other"                    ~ "other",
      "Unsure"                   ~ "unsure",
      NA                         ~ "none"
    ),
    dummy = "Yes"
  ) %>%
  pivot_wider(
    names_from = NDexp_type,
    names_glue = "NDexp_type_{NDexp_type}",
    values_from = dummy,
    values_fill = "No"
  )

# Factor Yes/No Variables ------------------------------------------------------
# Correct Declined to Decline
casper_clean <- casper_clean %>%
  mutate(across(where(is.character), ~str_replace(.x, "Decline$", "Declined")))

# Get Yes/No columns
yesno_options <- c("Yes", "No", "Unsure", "Declined", NA_character_)
yesno_cols <- names(casper_clean)[
  sapply(
    names(casper_clean), 
    function(col) {
      all(casper_clean[[col]] %in% yesno_options) & !all(is.na(casper_clean[[col]]))
    }
  )
]

casper_clean <- casper_clean %>%
  mutate(across(all_of(yesno_cols), ~factor(.x, levels = yesno_options)))

# Date Times -------------------------------------------------------------------
datetime_cols <- names(casper_clean)[endsWith(names(casper_clean), "datetime")]

casper_clean <- casper_clean %>%
  mutate(across(all_of(datetime_cols), ~with_tz(.x, tzone = "HST")))

# Factor Likert Variables ------------------------------------------------------
likert_options <- c("1", "2", "3", "4", "5", "Unsure", "Declined")
likert_cols <- c("supp_vax_important", "hlth_physical", "hlth_mental")

casper_clean <- casper_clean %>%
  mutate(across(all_of(likert_cols), ~factor(.x, levels = likert_options)))

# Hurricane Variables ----------------------------------------------------------
hur_options <- c(
  "Shelter in place at home", "Friend/family's home", "Public shelter", "Other",
  "Unsure", "Declined"
)
hur_cols <- paste0("evac_hur_cat", 1:5)

casper_clean <- casper_clean %>%
  mutate(across(all_of(hur_cols), ~factor(.x, levels = hur_options)))

# Others -----------------------------------------------------------------------
casper_clean$demo_own_rent <- factor(
  casper_clean$demo_own_rent,
  levels = c("Own", "Rent", "Other", "Unsure", "Declined")
)

casper_clean$supp_mosquito_prevent <- factor(
  casper_clean$supp_mosquito_prevent,
  levels = c(
    "Daily", "Weekly", "Monthly", "A few times per year", "Once a year", 
    "Never", "Unsure", "Declined"
  )
)

casper_clean$hlth_food_ran_out <- factor(
  casper_clean$hlth_food_ran_out,
  levels = c(
    "Always", "Usually", "Sometimes", "Rarely", "Never", "Unsure", "Declined"
  )
)

casper_clean$evac_main_barrier <- factor(
  casper_clean$evac_main_barrier,
  levels = c(
    "Uncertainty about where to go", "Concern about leaving pet(s)",
    "Concern about leaving property vacant", "Lack of transportation",
    "Health or mobility issues", "Inconvenient or expensive", "Other",
    "No barriers - household will evacuate", 
    "No barriers - household would choose not to evacuate",
    "Unsure", "Declined"
  )
)

casper_clean$NDexp_damage <- factor(
  casper_clean$NDexp_damage,
  levels = c(
    "None/Minimal", "Damaged, but repairable", "Destroyed", "Unsure", "Declined"
  )
)

casper_clean$NDexp_prepared <- factor(
  casper_clean$NDexp_prepared,
  levels = c(
    "Not Prepared", "Somewhat Prepared", "Well Prepared", "Unsure", "Declined"
  )
)

casper_clean$evac_main_info_source <- factor(
  casper_clean$evac_main_info_source,
  levels = c(
    "Social media", "Radio or TV", "Government website", 
    "Community, friends, or family", "Newspaper or magazines", 
    "Other", "Unsure", "Declined"
  )
)

# Output =======================================================================
saveRDS(
  casper_clean %>%
    select(starts_with(c("surv_", "demo_", "NDexp_", "evac_", "supp_", "hlth_"))),
  "./data/casper_2026_clean.rds"
)
