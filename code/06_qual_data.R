# 06_qual_data.R

# Export qualitative data for coding
# Import qualitative data for analysis
#   - focusing on weighting and percentages, so turn coded data into something
#     that can be described as a count

library(here)
library(openxlsx)
library(tidyverse)

# Options ======================================================================
file.prefix <- "casper_kauai26_recovery_"
do.tocode.export <- F

# Import =======================================================================
casper_clean <- readRDS(here("data", paste0(file.prefix, "data_clean.rds")))

# Export for Coding ============================================================
if (do.tocode.export) {
  qual_to_code <- casper_clean %>%
    select(surv_id, surv_datetime, supp_post_hurr_ch_descr, imp_info_no_radio, imp_govt_focus)
  
  write.xlsx(
    list(
      `Q11 Changes` = qual_to_code %>%
        select(surv_id, supp_post_hurr_ch_descr) %>%
        drop_na(supp_post_hurr_ch_descr),
      `Q19 Info Sources` = qual_to_code %>%
        select(surv_id, imp_info_no_radio) %>%
        drop_na(imp_info_no_radio),
      `Q23 Govt Focus` = qual_to_code %>%
        select(surv_id, imp_govt_focus) %>%
        drop_na(imp_govt_focus)
    ),
    file = here("data/Kauai26R Qualitative Questions to Code.xlsx")
  )
}

# Import Coded =================================================================
supp_post_coded <- read.xlsx(
  here("data/Kauai26R Qualitative Questions Coded.xlsx"),
  sheet = 1
) %>%
  rename(
    supp_post_hurr_ch_descr = 2,
    supp_post_hurr_ch_coded1 = Final.1,
    supp_post_hurr_ch_coded2 = Final.2,
    supp_post_hurr_ch_coded3 = Final.3
  ) %>%
  select(surv_id, starts_with("supp_post_hurr_ch_")) %>%
  unite("concat_codes", starts_with("supp_post_hurr_ch_coded"), sep = ";",
        remove = T, na.rm = T) %>%
  separate_longer_delim(concat_codes, delim = ";") %>%
  mutate(
    recat_codes =
      recode_values(
        concat_codes,
        "Obtiain generator or backup power"      ~ "Buy generator/backup",
        "More water"                             ~ "More water",
        "Obtain or replenish emergency supplies" ~ "Buy emergency supplies",
        "Protect windows and/or doors better during storm" ~ "Fortify windows/doors",
        "More food"                              ~ "More food",
        "Clear yard, clear debris"               ~ "Clear debris",
        "Unsure"                                 ~ "Unsure",
        default                                  = "Different Answer"
      ) %>%
      factor(),
    dummy = "Yes"
  )

q11_changes <- supp_post_coded %>%
  pivot_wider(
    id_cols = surv_id,
    names_from = concat_codes,
    names_prefix = "supp_post_hurr_ch__",
    values_from = dummy,
    values_fill = "No"
  ) %>%
  left_join(
    supp_post_coded %>%
      select(surv_id, recat_codes, dummy) %>%
      distinct() %>%
      pivot_wider(
        id_cols = surv_id,
        names_from = recat_codes,
        names_prefix = "supp_post_hurr_ch_recode_",
        values_from = dummy,
        values_fill = "No"
      ),
    by = join_by(surv_id)
  )

q19_info_sources <- read.xlsx(
  here("data/Kauai26R Qualitative Questions Coded.xlsx"),
  sheet = 2
) %>%
  select(
    surv_id,
    imp_info_no_radio = 2,
    imp_info_no_radio_coded = Final
  ) %>%
  mutate(
    imp_info_no_radio_recoded = replace_values(
      imp_info_no_radio_coded,
      "Fire  Dept"                                        ~ "County",
      c("Friends or family", "Word of mouth")             ~ "Friends/family/neighbors",
      "Not applicable"                                    ~ "None",
      "Online (Internet or cell service - not specified)" ~ "Internet/cell (unspecified)",
      c("Radio or TV", "VHF radio")                       ~ "Radio/TV",
      c("Social media - Facebook", "Social media - Instagram",
        "Social media - not specified", "Social media - YouTube") ~ "Social media"
    )
  )

q23_govt_focus <- read.xlsx(
  here("data/Kauai26R Qualitative Questions Coded.xlsx"),
  sheet = 3
) %>%
  select(
    surv_id,
    imp_govt_focus = 2,
    imp_govt_focus_coded = Final
  )

# Assemble =====================================================================
casper_clean_qual <- casper_clean %>%
  select(surv_id) %>%
  left_join(
    q11_changes,
    by = join_by(surv_id)
  ) %>%
  left_join(
    q19_info_sources,
    by = join_by(surv_id)
  ) %>%
  left_join(
    q23_govt_focus,
    by = join_by(surv_id)
  )

# Export =======================================================================
saveRDS(casper_clean_qual, here("data", paste0(file.prefix, "data_clean_wqual.rds")))
