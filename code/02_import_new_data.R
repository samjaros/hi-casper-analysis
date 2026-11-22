# 00_import_clean_merge_data.R

# Import and do initial cleaning/formatting of CASPER dataset
#   - Rename columns
#   - Calculate weighting

# Raw data comes from epicollect

library(here)
library(tidyverse)

# Options ======================================================================
# 2026 Kauai Recovery CASPER - using number occupied households from census
file.prefix <- "casper_kauai26_recovery_" # Prefix to add to output file(s)

total.hh.sample <- 24712 # Number of households in this year's sampling frame
total.clusters <- 30 # Usually 30

# Import Data ==================================================================
data_import_raw <- read_csv(here("raw_data/form-1__survey.csv"),
                            show_col_types = F)

# Rename Columns ===============================================================
data_rename <- data_import_raw %>%

  # Title only holds redundant info
  select(-title) %>%
  
  # Rename variables from start of question to relevant subjects
  rename(
    
    # Survey -------------------------------------------------------------------
    surv_id = `ec5_uuid`,                              # Universal ID
    surv_datetime = `created_at`,                      # When survey was started
    surv_upload_datetime = `uploaded_at`,              # When survey was saved to cloud
    surv_surveyor = `2_Surveyor_Name`,                 # Surveyor that collected data
    surv_cluster = `3_Cluster_Number`,                 # Cluster for this survey
    surv_survey_num = `4_Survey_Number`,               # Survey number within this cluster

    # Demographic --------------------------------------------------------------
    demo_n_hh = `6_Including_yourself`,                # Q1   Total HH members
    demo_n_u2 = `7_How_many_of_those_`,                # Q1   <2y
    demo_n_2to17 = `8_How_many_of_those_`,             # Q1   2-17
    demo_n_18to64 = `9_How_many_of_those_`,            # Q1   18-64
    demo_n_65p = `10_How_many_of_those`,               # Q1   >65
    demo_language = `11_What_languages_ar`,            # Q2   Language
    demo_language_other = `12_What_languages_Pl`,      # Q2o  Language Other
    demo_own_rent = `13_Does_your_househo`,            # Q3   Own/Rent
    demo_own_rent_other = `14_Please_specify`,         # Q3o  Own/Rent Other  

    # Supplies -----------------------------------------------------------------
    supp_water_3d = `15_Immediately_after`,            # Q4a  water 3 days
    supp_water_7d = `16_Did_your_househol`,            # Q4b  water 7 days
    supp_water_14d = `17_Did_your_househol`,           # Q4c  water 14 days
    supp_food_3d = `18_Immediately_after`,             # Q5a  food 3 days
    supp_food_7d = `19_Did_your_househol`,             # Q5b  food 7 days
    supp_food_14d = `20_Did_your_househol`,            # Q5c  food 14 days
    supp_meds_prescription = `21_Does_anyone_in_yo`,   # Q6a  HH need presc meds
    supp_meds_run_out = `22_Did_any_of_those_`,        # Q6b  HH run out of presc meds
    supp_meds_refill_prob = `23_Did_they_experien`,    # Q6c  med refill issues
    supp_meds_refill_descr = `24_If_yes_please_des`,   # Q6d  med refill issues description
    supp_med_electr = `25_Is_anyone_in_your`,          # Q7a  depend on electricity for med equip or drugs
    supp_med_electr_backup = `26_Did_your_househol`,   # Q7b  electricity backup for med equip or drugs
    supp_emerg_kit = `27_Did_your_househol`,           # Q8   emergency supply kit
    supp_emerg_gen = `28_Did_your_househol`,           # Q9   emergency generator
    supp_needed_emerg_supp = `29_Did_your_househol`,   # Q10  needed missing emergency supplies
    supp_need_supp_describe = '30_Please_describe',    # Q10b missing emergency supplies description
    supp_post_hurr_ch = `31_After_the_hurrica`,        # Q11  post-hurricane change
    supp_post_hurr_ch_descr = `32_Please_describe_t`,  # Q11  describe post-hurricane change description

    # Impacts ------------------------------------------------------------------
    imp_evacuate =`33_Did_your_househol`,              # Q12  did HH evacuate
    imp_evacuate_where = `34_Where_did_you_and`,       # Q12b where did HH evacuate to
    imp_evacuate_where_spec = `35_Please_specify`,     # Q12  other specify where did HH evacuate
    imp_home_safe = `36_Does_your_househo`,            # Q13  household feel home is safe
    imp_home_not_safe_descr = `37_Can_you_please_sh`,  # Q13b if home not safe share why
    imp_damage_to_home =`38_How_would_your_ho`,        # Q14  damage to home
    imp_damage_report_cnty = `39_Did_your_househol`,   # Q14b report damage to county of Kauai
    imp_damage_ins_claim = `40_Did_your_househol`,     # Q14c file homeowners insurance claim   
    imp_job_loss =`41_As_a_result_of_th`,              # Q15  anyone lose job or have income reduced
    imp_comm_resources =`42_Since_the_hurrica`,        # Q16  apply for community resources - all that apply
    imp_comm_res_other_ans =`45_Please_specify_wh`,    # Q16  other apply for community resources other specify

    # When Other not selected
    imp_comm_res_barriers =`43_Did_your_househol`,     # Q17  community resources appl barriers
    imp_comm_res_bar_descr =`44_Please_describe_t`,    # Q17b community resources appl barriers description
    
    # When Other selected, alone or in combination with listed community resources
    imp_comm_res_barriers_other =`46_Did_your_househol`,  # Q17  community resources appl barriers when other selected
    imp_comm_res_bar_descr_other =`47_Please_describe_t`, # Q17b community resources appl barriers description when other selected
    
    # TODO: The two barrier yes/no questions need to be combined.  Each respondent will only answer one of them.
    # TODO: The two barrier please describe  questions need to be combined.  Each respondent will only answer one of them.
    
    imp_neighbors = `48_After_the_hurrica`,            # Q18  neighbors check in
    imp_info_no_radio = `49_After_the_hurrica`,        # Q19  info source without radio - text response
    imp_county_alerts = `50_Is_your_household`,        # Q20  signed up for county emergency alerts
    imp_trusted_info = `51_Overall_what_was_`,         # Q21  most trusted info source
    imp_trusted_info_social = `52_Please_specify_wh`,  # Q21  most trusted info source - social media specify
    imp_trusted_info_other = `53_Please_specify`,      # Q21  most trusted info source - other specify
    imp_govt_response = `54_On_a_scale_of_15_`,        # Q22  govt response on scale 1 to 5
    imp_govt_focus = `55_What_is_the_most_`,           # Q23  govt most important issue for continued recovery
    imp_unmet_needs = `56_Does_your_househo`,          # Q24  any unmet needs related to hurricane
    imp_unmet_needs_descr = `57_Please_describe_F`,    # Q24b any unmet needs related to hurricane description
    
    
    # General health -----------------------------------------------------------
    hlth_health_insurance = `58_Do_all_members_of`,    # Q25  all HH members have health insurance
    hlth_no_insurance_desc = `59_Can_you_please_te`,   # Q25b describe HH members with no health insurance
    hlth_storm_injuries = `60_Were_you_or_anyon`,      # Q26  injures during or after hurricane
    hlth_physical = `62_How_would_you_rat`,            # Q27a physical health
    hlth_mental = `63_How_would_you_rat`,              # Q27b mental health
    hlth_sought_mh_services = `64_Since_the_hurrica`,  # Q28  sought mental health support
    hlth_can_find_mh_serv = `65_Does_your_househo`,    # Q29  know where to seek mental health support
    hlth_food_support = `66_Since_the_storm_h`,        # Q30  received food support
    hlth_prev_food_support = `67_Has_your_househol`,   # Q30b previously received food support
    hlth_rent_concern = `68_How_concerned_is_`,        # Q31  concerned about next months rent or mortgage
    hlth_concerns_needs = `69_Does_your_househo`,      # Q32  concerns not yet discussed
    hlth_concerns_descr = `70_Please_describe_F`,      # Q32b describe concerns not yet discussed
    hlth_anything_else = `71_Is_there_anything`,       # Q33  anything else tell DOH
    hlth_anything_else_descr = `72_Please_describe`,   # Q33b anything else tell DOH describe
  )

# Weighting ====================================================================

# Check number of clusters
paste0("Number of clusters in data: ",
       length(unique(data_rename$surv_cluster)))

# Calculate the weights
data_weight <- data_rename %>%
  group_by(surv_cluster) %>%
  mutate(surv_weight = total.hh.sample / (n() * total.clusters)) %>%
  ungroup()

# Validation ===================================================================

# Check for duplicate surveys, should be 0 rows --------------------------------
dupe_surveys <- data_weight %>%
  group_by(surv_cluster, surv_survey_num) %>%
  filter(n() > 1)

if (nrow(dupe_surveys) > 0) {
  warning("At least two surveys share the same Cluster/Survey number combination.")
  View(dupe_surveys)
}

# Check for mismatching household counts ---------------------------------------
hh_count_off <- data_weight %>%
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
saveRDS(data_weight, here("data", paste0(file.prefix, "data_raw.rds")))
