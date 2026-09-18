# 2_clean_vars.R

# Factor categorical variables
# Create indicator variables

library(tidyverse)
library(lubridate)

# Import =======================================================================
casper_2026 <- readRDS("./data/casper_2026_raw.rds")

# Cleaning =====================================================================
casper_clean <- casper_2026

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
  mutate(demo_language = str_to_lower(demo_language),
         demo_language_other = str_to_lower(demo_language_other)) %>%
  # Indicator column for each language
  # TODO: should this be logical?
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
      str_detect(demo_language_other, "ilocano"), "Yes", "No", missing = "No")
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

# Factor Yes/No Variables ------------------------------------------------------
# Correct Declined to Decline
casper_clean <- casper_clean %>%
  mutate(across(everything(), ~str_replace(.x, "Decline$", "Declined")))

# Get Yes/No columns
yesno_options <- c("Yes", "No", "Unsure", "Declined")
yesno_cols <- names(casper_clean)[
  sapply(names(casper_clean), 
         function(col){
           all(casper_clean[[col]] %in% yesno_options)
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
hur_options <- c("Shelter in place at home", "Friend/family's home",
                  "Public shelter", "Other", "Unsure", "Declined")
hur_cols <- paste0("evac_hur_cat", 1:5)

casper_clean <- casper_clean %>%
  mutate(across(all_of(hur_cols), ~factor(.x, levels = hur_options)))

# Others -----------------------------------------------------------------------
casper_clean$demo_own_rent = factor(
  casper_clean$demo_own_rent,
  levels = c("Own", "Rent", "Other", "Unsure", "Declined")
)

casper_clean$supp_mosquito_prevent = factor(
  casper_clean$supp_mosquito_prevent,
  levels = c("Daily", "Weekly", "Monthly", "A few times per year", "Once a year",
             "Never", "Unsure", "Declined")
)

casper_clean$hlth_food_ran_out = factor(
  casper_clean$hlth_food_ran_out,
  levels = c("Always", "Usually", "Sometimes", "Rarely", "Never", "Unsure",
             "Declined")
)

# Output =======================================================================
saveRDS(casper_clean %>%
          select(starts_with(c("surv_", "demo_", "NDexp_", "evac_", "supp_", "hlth_"))),
        "./data/casper_2026_clean.rds")
