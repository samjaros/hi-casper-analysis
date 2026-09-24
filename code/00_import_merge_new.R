# 0_import_clean_merge_data.R

# Import and do initial cleaning/formatting of CASPER dataset
#   - Rename columns
#   - Calculate weighting
# Merge new year data with previous data

# Raw data comes from epicollect

library(tidyverse)

# Options ======================================================================
total.hh.sample <- 4054 # Number of households in this year's sampling frame
total.clusters <- 29
# 2026 W Hawaii - using number from tax maps & ArcGIS, not census

# Import Data ==================================================================
casper_import_raw <- read_csv("./raw_data/form-1__2026-west-hawaii-casper.csv",
                              show_col_types = F)

# Rename Columns ===============================================================
casper_2026 <- casper_import_raw %>%

  # Title only holds redundant info
  select(-title) %>%
  
  # Rename variables from start of question to relevant subjects
  rename(
    # Survey variables
    surv_id = `ec5_uuid`,                            # Universal ID
    surv_datetime = `created_at`,                    # When survey was started
    surv_upload_datetime = `uploaded_at`,            # When survey was saved to cloud
    surv_surveyor = `2_Surveyor_Name`,               # Surveyor that collected data
    surv_cluster = `3_Cluster_Number`,               # Cluster for this survey
    surv_survey_num = `4_Survey_Number`,             # Survey number within this cluster
    # Demographic variables
    demo_n_hh = `6_Including_yourself`,              # Q1 Total HH members
    demo_n_u2 = `7_How_many_of_those_`,              # Q1 <2y
    demo_n_2to17 = `8_How_many_of_those_`,           # Q1 2-17
    demo_n_18to64 = `9_How_many_of_those_`,          # Q1 18-64
    demo_n_65p = `10_How_many_of_those`,             # Q1 >65
    demo_own_rent = `11_Does_your_househo`,          # Q2 Own/Rent
    demo_own_rent_other = `12_If_other_please_s`,    # Q2o Own/Rent Other  
    demo_language = `13_What_languages_ar`,          # Q3 Language
    demo_language_other = `14_If_other_what_lan`,    # Q3o Language Other
    # Natural disaster experience
    NDexp_affected = `15_Has_your_househol`,         # Q4a HH affected by disaster
    NDexp_type = `16_What_type_of_natu`,             # Q4b disaster type
    NDexp_type_other = `17_Please_Specify`,          # Q4bo disaster type other
    NDexp_displaced = `18_Within_the_past_2`,        # Q4c HH displaced
    NDexp_displaced_days = `19_For_how_many_days`,   # Q4cn HH displaced days count
    NDexp_damage = `20_How_would_you_des`,           # Q4d home damaged
    NDexp_prepared = `21_After_the_most_re`,         # Q5 Is HH prepared
    NDexp_safe = `22_When_considering_`,             # Q6 Is HH safe
    NDexp_not_safe = `23_Please_Specify`,            # Q6o How is HH not safe
    # Evacuation
    evac_chronic_disease = `25_Chronic_disease_r`,   # Q7a HH chronic disease
    evac_phys_disease = `26_Physical_or_devel`,      # Q7b HH physical disability
    evac_ment_disease = `27_Mental_health_con`,      # Q7c HH mental disease
    evac_assist = `28_Does_your_househo`,            # Q8a HH needs assistance
    evac_assist_need = `29_If_yes_please_des`,       # Q8ao HH what assist needed
    evac_assist_org = `30_If_yes_are_you_aw`,        # Q8b aware of community org to help
    evac_comm_plan = `32_Communication_pla`,         # Q9a have comm plan
    evac_meeting_place = `33_Designated_meetin`,     # Q9b have meeting place
    evac_impt_docs = `34_Copies_of_importa`,         # Q9c have important docs
    evac_hur_cat1 = `36_Category_1_7495_m`,          # Q10a where shelter C1
    evac_hur_cat1_other = `37_If_other_please_s`,    # Q10ao C1 other shelter
    evac_hur_cat2 = `38_Category_2_96110_`,          # Q10b where shelter C2
    evac_hur_cat2_other = `39_If_other_please_s`,    # Q10bo C2 other shelter
    evac_hur_cat3 = `40_Category_3_111129`,          # Q10c where shelter C3
    evac_hur_cat3_other = `41_If_other_please_s`,    # Q10co C3 other shelter
    evac_hur_cat4 = `42_Category_4_130156`,          # Q10d where shelter C4
    evac_hur_cat4_other = `43_If_other_please_s`,    # Q10do C4 other shelter
    evac_hur_cat5 = `44_Category_5_157_mp`,          # Q10e where shelter C5
    evac_hur_cat5_other = `45_If_other_please_s`,    # Q10eo C5 other shelter
    evac_tsunami_zone = `46_Is_your_household`,      # Q11 Is HH in tsunami evac zone
    evac_prevent_fire = `47_Has_your_househol`,      # Q12 Has HH prevent wildfire
    evac_main_barrier = `48_What_main_barrier`,      # Q13 Main barrier to evac
    evac_barrier_other = `49_If_other_please_s`,     # Q13o Other barrier
    evac_main_info_source = `50_What_is_your_hous`,  # Q14 Main source of info
    evac_info_source_other = `51_If_other_please_s`, # Q14o Other info source
    evac_alert_signup = `52_Have_you_or_anyon`,      # Q15 HH sign up for alerts
    # Supplies
    supp_aware_14d = `53_Is_your_household`,         # Q16 aware of 14 days food/water/med
    supp_water_3d = `54_Does_your_househo`,          # Q17a water 3 days
    supp_water_7d = `55_Does_your_househo`,          # Q17b water 7 days
    supp_water_14d = `56_Does_your_househo`,         # Q17c water 14 days
    supp_food_3d = `57_Does_your_househo`,           # Q18a food 3 days
    supp_food_7d = `58_Does_your_househo`,           # Q18b food 7 days
    supp_food_14d = `59_Does_your_househo`,          # Q18c food 14 days
    supp_need_meds = `60_Does_anyone_in_yo`,         # Q19a HH need daily meds
    supp_meds_7d = `61_If_yes_does_your_`,           # Q19b meds 7 days
    supp_meds_14d = `62_If_yes_does_your_`,          # Q19c meds 14 days
    supp_fire_exting = `63_Does_your_househo`,       # Q20 HH have fire extinguisher
    supp_smoke_detect = `64_Does_your_househo`,      # Q21 HH have smoke detector
    supp_mosquito_deet = `65_Does_your_househo`,     # Q22 HH use DEET
    supp_mosquito_prevent = `66_How_often_does_yo`,  # Q23 prevent mosquito
    supp_vax_important = `67_On_a_scale_from_1`,     # Q24 vaccine importance
    # Health
    hlth_food_ran_out = `68_During_the_past_1`,      # Q25 food ran out
    hlth_physical = `69_On_a_scale_of_1_t`,          # Q26 physical health
    hlth_mental = `70_On_a_scale_of_1_t`,            # Q27 mental health
    hlth_doh_help = `71_Does_your_househo`           # Q28 can doh offer help
  )

# Weighting ====================================================================

# Check number of clusters
paste0("Number of clusters in data: ",
       length(unique(casper_2026$surv_cluster)))
# Should be 29, one cluster will not appear because there are no eligible HH

# Calculate the weights
casper_2026 <- casper_2026 %>%
  group_by(surv_cluster) %>%
  mutate(surv_weight = total.hh.sample / (n() * total.clusters)) %>%
  ungroup()

# Validation ===================================================================

# Check for duplicate surveys, should be 0 rows --------------------------------
dupe_surveys <- casper_2026 %>%
  group_by(surv_cluster, surv_survey_num) %>%
  filter(n() > 1)

if (nrow(dupe_surveys) > 0) {
  warning("At least two surveys share the same Cluster/Survey number combination.")
  View(dupe_surveys)
}

# Check for mismatching household counts ---------------------------------------
hh_count_off <- casper_2026 %>%
  filter(
    demo_n_hh == 0 |
    demo_n_hh != demo_n_u2 + demo_n_2to17 + demo_n_18to64 + demo_n_65p
  )

if (nrow(hh_count_off) > 0) {
  warning("At least one survey has a mismatch between the total household members and the sum of members by age.",
          call. = F)
  View(hh_count_off)
}

# Output =======================================================================
saveRDS(casper_2026, "./data/casper_2026_raw.rds")

# Compare Weighting ============================================================
# Examine the difference in weights between 29 and 30 clusters
wgt_compare <- casper_2026 %>%
  select(surv_id, surv_cluster, surv_survey_num) %>%
  group_by(surv_cluster) %>%
  mutate(surv_weight29 = total.hh.sample / (n() * 29),
         surv_weight30 = total.hh.sample / (n() * 30),
         weight_diff = surv_weight29 - surv_weight30,
         delta_pct = weight_diff / surv_weight30)
