# 2_clean_vars.R

# Factor categorical variables
# Create indicator variables

library(tidyverse)
library(lubridate)

# Set to true to examine language values
# Do this if you're running the code on a new survey
do.lang.check <- F

# Options ======================================================================
# 2026 Kauai Recovery CASPER - using number occupied households from census
file.prefix <- "casper_kauai26_recovery_" # Prefix to add to output file(s)
survey.id <- "kauai26_recovery" # Unique identifier for this CASPER, usually including at least the year and island

# Import =======================================================================
data_raw <- readRDS(here("data", paste0(file.prefix, "data_raw.rds")))

# Fix Invalid Obs ==============================================================
# None
data_fix <- data_raw

# Combine duplicate columns ----------------------------------------------------
data_fix <- data_fix %>%
  mutate(
    imp_comm_res_barriers = if_else(
      is.na(imp_comm_res_barriers),
      imp_comm_res_barriers_other,
      imp_comm_res_barriers
    ),
    imp_comm_res_bar_descr = if_else(
      is.na(imp_comm_res_bar_descr),
      imp_comm_res_bar_descr_other,
      imp_comm_res_bar_descr
    )
  ) %>%
  select(-any_of(c("imp_comm_res_barriers_other", "imp_comm_res_bar_descr_other")))

# Data Wrangling ===============================================================
data_clean <- data_fix %>%
  # Create relevant identifiers for longitudinal analyses/viz
  mutate(surv_casper_id = survey.id,
         surv_cluster_id = 
           paste0(
             survey.id, "_",
             str_to_lower(str_replace_all(surv_cluster, " ", "_"))
           )
  )

# Indicator Variables ----------------------------------------------------------
## Ages
data_clean <- data_clean %>%
  mutate(
    demo_hh_infants = if_else(demo_n_u2 > 0,     "Yes", "No", missing = NA_character_),
    demo_hh_kids    = if_else(demo_n_2to17 > 0,  "Yes", "No", missing = NA_character_),
    demo_hh_adults  = if_else(demo_n_18to64 > 0, "Yes", "No", missing = NA_character_),
    demo_hh_elders  = if_else(demo_n_65p > 0,    "Yes", "No", missing = NA_character_)
  )

## Languages
if (do.lang.check) {
  table(data_clean$demo_language_other)
}

# Create indicators with search, then create a clean, combined variable
data_clean <- data_clean %>%
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
    demo_lang_chamorro = if_else(
      str_detect(demo_language_other, "chamorro"), "Yes", "No", missing = "No"),
    `demo_lang_french creole` = if_else(
      str_detect(demo_language_other, "creole"), "Yes", "No", missing = "No"),
    `demo_lang_olelo hawaii` = if_else(
      str_detect(demo_language_other, "hawaii|olelo"), "Yes", "No", missing = "No"),
    demo_lang_italian = if_else(
      str_detect(demo_language_other, "italian"), "Yes", "No", missing = "No"),
    demo_lang_japanese = if_else(
      str_detect(demo_language_other, "japanese"), "Yes", "No", missing = "No"),
    demo_lang_korean = if_else(
      str_detect(demo_language_other, "korean"), "Yes", "No", missing = "No"),
    demo_lang_mandarin = if_else(
      str_detect(demo_language_other, "mandarin|chinese"), "Yes", "No", missing = "No"),
    demo_lang_marshallese = if_else(
      str_detect(demo_language_other, "marshallese"), "Yes", "No", missing = "No"),
    demo_lang_pidgin = if_else(
      str_detect(demo_language_other, "pidgin|pigeon|pidin"), "Yes", "No", missing = "No"),
    demo_lang_tagalog = if_else(
      str_detect(demo_language_other, "filipino|tagalog|pilipino"), "Yes", "No", missing = "No"),
    demo_lang_pohnpeian = if_else(
      str_detect(demo_language_other, "pohnpeian"), "Yes", "No", missing = "No"),
    demo_lang_portuguese = if_else(
      str_detect(demo_language_other, "portuguese"), "Yes", "No", missing = "No"),
    demo_lang_spanish = if_else(
      str_detect(demo_language_other, "spanish"), "Yes", "No", missing = "No"),
    demo_lang_german = if_else(
      str_detect(demo_language_other, "german"), "Yes", "No", missing = "No"),
    demo_lang_ilocano = if_else(
      str_detect(demo_language_other, "il+ocano|ilokano"), "Yes", "No", missing = "No"),
    demo_lang_kosrae = if_else(
      str_detect(demo_language_other, "kosrae"), "Yes", "No", missing = "No"),
    demo_lang_visayan = if_else(
      str_detect(demo_language_other, "visayan"), "Yes", "No", missing = "No")
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

# Remove demo_lang_ columns containing all "No"s
all_no_cols <- data_clean %>%
  select(starts_with("demo_lang_")) %>%
  select(where(\(x) all(x == "No"))) %>%
  names()
data_clean <- data_clean %>%
  select(-any_of(all_no_cols))

# Create language breakdown
#  - Only English
#  - English + at least 1 other
#  - No English (just put the language(s) they speak)
data_clean <- data_clean %>%
  mutate(
    demo_langs_cat = 
      case_when(
        demo_langs == "English"           ~ "Only English",
        str_detect(demo_langs, "English") ~ "English & 1+ Other",
        T                                 ~ demo_langs
      )
  )

# Language Data Checks
# First 2 columns have the raw data, second two have processed data
if(do.lang.check) {
  data_clean %>%
    select(c(starts_with("demo_lang"), -starts_with("demo_lang_"))) %>%
    distinct() %>%
    View("Lang Processed Check")
  
  # First 2 columns have the raw data, rest have indicators. Every language should be indicated
  data_clean %>%
    select(c(starts_with("demo_language"), starts_with("demo_lang_"))) %>%
    distinct() %>%
    View("Lang Indicator Check")
}

## Community resources
# Get community resource select all question as indicator columns
community_resources_wide <- data_clean %>%
  # Remove existing imp_comm_res_... columns when running this code multipe times
  select(surv_id, imp_comm_resources, imp_comm_res_other_ans) %>%
  # Get longer version of data
  separate_longer_delim(
    cols = imp_comm_resources,
    delim = ", "
  ) %>%
  # Clean up names to use as columns
  mutate(
    imp_comm_resources = replace_values(
      imp_comm_resources,
      "Agency on Elderly affairs programs"          ~ "Information",
      "Airbnb/Hotel room"                           ~ "Other",
      "American Red Cross"                          ~ "Other",
      "Catholic Charities"                          ~ "Other",
      "Food Bank"                                   ~ "Food",
      "Hurricane Business Grant Program"            ~ "Financial",
      "Hurricane Recovery Centers in Koloa/Hanalei" ~ "Information",
      "Malama Kauai"                                ~ "Food",
      "OHA's relief program"                        ~ "Financial",
      "Small Business Association"                  ~ "Other",
      "SNAP"                                        ~ "Financial",
      "Unemployment"                                ~ "Financial",
      "211"                                         ~ "Information",
      "Other"                                           ~ "Other",
      "None"                                            ~ "None",
      "Unsure"                                          ~ "Unsure",
      "Declined"                                        ~ "Declined",
      NA                                                ~ "None"
    ),
    dummy = "Yes"
  ) %>%
  mutate(
    imp_comm_resources = case_when(
      imp_comm_resources == "Other" & 
        str_detect(str_to_lower(imp_comm_res_other_ans), "food") ~ "Food",
      imp_comm_resources == "Other" & 
        imp_comm_res_other_ans == "TANF"                         ~ "Financial",
      imp_comm_resources == "Other" & 
        imp_comm_res_other_ans == "Kauai cares"                  ~ "Other",
      T ~ imp_comm_resources
    )
  ) %>%
  select(-imp_comm_res_other_ans) %>%
  distinct() %>%
  pivot_wider(
    names_from = imp_comm_resources,
    names_glue = "imp_comm_res__{imp_comm_resources}",
    values_from = dummy,
    values_fill = "No"  
  )

data_clean <- data_clean %>%
  # Remove existing imp_comm_res_... columns when running this code multipe times
  select(-starts_with("imp_comm_res__")) %>%
  left_join(
    community_resources_wide,
    by = join_by(surv_id)
  )
  
# Survey cannot be "None", "Unsure", or "Declined" if another row is filled
escape_cols <- c("imp_comm_res__None", "imp_comm_res__Unsure", "imp_comm_res__Declined")
comm_res_vars <- data_clean %>%
  select(starts_with("imp_comm_res__"), -any_of(escape_cols)) %>%
  names()
data_clean <- data_clean %>%
  mutate(
    across(
      all_of(escape_cols),
      \(x) if_else(
        if_any(
          any_of(comm_res_vars),
          \(y) y == "Yes"
        ),
        "No",
        x
      )
    )
  )

# Both food and water ----------------------------------------------------------
data_clean <- data_clean %>%
  replace_na(
    list(
      supp_water_7d  = "No",
      supp_water_14d = "No",
      supp_food_7d   = "No",
      supp_food_14d  = "No"
    )
  ) 

data_clean$supp_fw_3d <- 
  if_else(
    data_clean$supp_water_3d == "Yes" & data_clean$supp_food_3d == "Yes",
    "Yes",
    "No",
    missing = "No"
  )

data_clean$supp_fw_7d <- 
  if_else(
    data_clean$supp_water_7d == "Yes" & data_clean$supp_food_7d == "Yes",
    "Yes",
    "No",
    missing = "No"
  )

data_clean$supp_fw_14d <- 
  if_else(
    data_clean$supp_water_14d == "Yes" & data_clean$supp_food_14d == "Yes",
    "Yes",
    "No",
    missing = "No"
  )

# Both emergency supplies & generator ------------------------------------------
data_clean$supp_emerg_kitgen <-
  if_else(
    data_clean$supp_emerg_kit == "Yes" & data_clean$supp_emerg_gen == "Yes",
    "Yes",
    "No",
    missing = "No"
  )

# Factor Yes/No Variables ------------------------------------------------------
# Correct Declined to Decline
#data_clean <- data_clean %>%
#  mutate(across(where(is.character), ~str_replace(.x, "Decline$", "Declined")))

# Get Yes/No columns
yesno_options <- c("Yes", "No", "Unsure", "Declined", NA_character_)
yesno_cols <- names(data_clean)[
  sapply(
    names(data_clean), 
    function(col) {
      all(data_clean[[col]] %in% yesno_options) & !all(is.na(data_clean[[col]]))
    }
  )
]

data_clean <- data_clean %>%
  mutate(across(all_of(yesno_cols), \(x) factor(x, levels = yesno_options)))

# Date Times -------------------------------------------------------------------
datetime_cols <- names(data_clean)[endsWith(names(data_clean), "datetime")]

data_clean <- data_clean %>%
  mutate(across(all_of(datetime_cols), ~with_tz(.x, tzone = "HST")))

# Factor Likert Variables ------------------------------------------------------
# Also create numeric versions for mean/median analysis
likert_options <- c("1", "2", "3", "4", "5", "Unsure", "Declined")
likert_cols <- c("imp_govt_response", "hlth_physical", "hlth_mental")

data_clean <- data_clean %>%
  mutate(across(all_of(likert_cols),
                ~factor(.x, levels = likert_options))) %>%
  mutate(across(all_of(likert_cols), 
                ~suppressWarnings(as.numeric(as.character(.x))),
                .names = "{.col}_n"))

# Other Variables ----------------------------------------------------------
data_clean$imp_evacuate_where <- factor(
  data_clean$imp_evacuate_where,
  levels =  c(
    "Public shelter", "Friend/family's home", "Workplace", "Other", "Unsure",
    "Declined"
  )
)

data_clean$demo_own_rent <- factor(
  data_clean$demo_own_rent,
  levels = c("Own", "Rent", "Other", "Unsure", "Declined")
)

data_clean$imp_damage_to_home <- factor(
  data_clean$imp_damage_to_home,
  levels = c("None", "Minimal", "Damaged, but repairable", "Destroyed",
             "Unsure", "Declined"
  )
)

data_clean$imp_damage_ins_claim <- factor(
    data_clean$imp_damage_ins_claim,
    levels = c(
      "Yes", "No", "Not yet, but planning to", "No insurance", "Unsure",
      "Declined"
    )
  )

data_clean$hlth_prev_food_support <- factor(
  data_clean$hlth_prev_food_support,
  levels = c(
    "Often", "Sometimes", "Rarely", "Never", "Unsure", "Declined"
  )
)

data_clean$hlth_rent_concern <- factor(
  data_clean$hlth_rent_concern,
  levels = c(
    "Very concerned", "Somewhat concerned", "Not concerned", "Unsure",
    "Declined"
  )
)

data_clean$imp_trusted_info <- factor(
  data_clean$imp_trusted_info,
  levels = c(
    "County of Kauai (KEMA, kauairecovers.org, CoK Facebook/Instagram, Everbridge)",
    "Other government website", "Radio or TV", "Newspaper or news website",
    "Social media", "Friends or family", "Church", "Community or charitable organization",
    "Word of mouth",  "Other", "Unsure", "Declined"
  )
) %>%
  fct_recode(`County of Kauai` = "County of Kauai (KEMA, kauairecovers.org, CoK Facebook/Instagram, Everbridge)")

# Recategorize open text needed emergency supplies into multiple indicator variables
data_clean <- data_clean %>%
  # All lowercase for searching
  mutate(
    supp_need_supp_describe = str_to_lower(supp_need_supp_describe),
  ) %>%
  # Indicator column for each type of needed supplies
  mutate(
    supp_need_supp_generator = if_else(
      str_detect(supp_need_supp_describe, "generator"), "Yes", "No", missing = "No"),
    supp_need_supp_gas = if_else(
      str_detect(supp_need_supp_describe, "gas"), "Yes", "No", missing = "No"),  
    supp_need_supp_ice = if_else(
      str_detect(supp_need_supp_describe, "ice"), "Yes", "No", missing = "No")
  )

# Output =======================================================================
saveRDS(
  data_clean %>%
    select(starts_with(c("surv_", "demo_", "supp_", "imp_",  "hlth_"))),
  here("data", paste0(file.prefix, "data_clean.rds"))
)
