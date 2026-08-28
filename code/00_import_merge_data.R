# 0_import_clean_merge_data.R

# Import and do initial cleaning/formatting of CASPER dataset
#   - Rename columns
#   - Calculate weighting
# Merge new year data with previous data


# Raw data comes from epicollect

library(tidyverse)

# Options ======================================================================
total.hh.sample <- 24712 # Number of households in this year's sampling frame

# Import Data ==================================================================
casper_import_raw <- read_csv("./raw_data/2026_casper_data.csv")
casper_full <- read_csv("./raw_data/Kauai CASPER data combined.csv", guess_max = 10000)

# Rename Columns ===============================================================
casper_2026 <- casper_import_raw %>%

  # TODO: Are there informative prefixes we could be using for easier selection?
  
  # Rename variables from start of question to relevant subjects
  rename(Total_HH_Members = `6_Including_yourself`, #Q1
         Infants = `7_How_many_of_those_`, #Q1
         Kids = `8_How_many_of_those_`, #Q1
         Adults = `9_How_many_of_those_`, #Q1
         Elders = `10_How_many_of_those`, #Q1
         Language = `11_What_languages_ar`, #Q2a
         Language_Other = `12_If_other_what_lan`, #Q2a
         Own_Rent = `13_Does_your_househo`, #Q3a
         Own_Rent_Other = `14_If_other_please_s`, #Q3a
         Insurance_Loss = `15_Has_your_househol`, #Q3b
         Comm_Plan = `17_Communication_pla`, #Q4a
         Meeting_Place = `18_Designated_meetin`, #Q4b
         Important_Docs = `19_Copies_of_importa`, #Q4c 
         Med_Equip = `20_Is_anyone_in_your`, #Q5a
         Backup_Power = `21_If_yes_does_your_`, #Q5b 
         Tsunami_Zone_Familiar = `22_Is_your_household`, #Q6a
         Tsunami_Zone_Info = `23_If_no_does_your_h`, #Q6b
         Wildfires_Protection = `24_Has_your_househol`, #Q7
         Receive_Alerts = `25_Have_you_or_anyon`, #Q8
         KEMA_Familiar = `26_Is_your_household`, #Q9
         Cat1 = `28_Category_1_7495_m`, #Q10
         Cat2 = `30_Category_2_96110_`, #Q10
         Cat3 = `32_Category_3_111129`, #Q10
         Cat4 = `34_Category_4_130156`, #Q10
         Cat5 = `36_Category_5_157`, #Q10
         Cat1_Other = `29_If_other_shelter_`, #Q10
         Cat2_Other = `31_If_other_shelter_`, #Q10 
         Cat3_Other = `33_If_other_shelter_`, #Q10
         Cat4_Other = `35_If_other_shelter_`, #Q10
         Cat5_Other = `37_If_other_shelter_`, #Q10
         Barrier = `38_What_main_barrier`, #Q11
         Additional_Evac_Needs = `40_Has_your_househol`, #Q12
         Measles_Aware = `41_Is_your_household`, #Q13
         Measles_Concern = `42_How_concerned_is_`, #Q14
         Chickenpox_Aware  = `43_Is_your_household`, #Q15
         Chickenpox_Concern = `44_How_concerned_is_`, #Q16
         Vaccines_Important = `45_How_important_doe`, #Q17
         Mosquito_Concern = `46_Mosquitoes_in_Haw`, #Q18
         Mosquito_Breeding = `47_How_often_does_yo`, #Q19
         Specific_Doctor = `48_Do_your_household`, #Q20
         Provider_Within_Year = `49_Have_your_househo`, #Q21
         Skip_Med_Care = `50_In_the_past_12_mo`, #Q22
         Phys_Health = `52_How_would_you_rat`, #Q23a
         Ment_Health = `53_How_would_you_rat`, #Q23b
         Community_Connected = `54_On_a_scale_of_1__`, #Q24
         Often_Gather = `55_How_often_does_yo`, #Q25
         Often_Groups = `56_How_often_does_yo`, #Q26
         Rent_Concern = `57_How_concerned_is_`, #Q27
         Food_Last = `58_During_the_past_1`, #Q28
         Food_Support = `59_During_the_past_1`, #Q29
         Naloxone = `60_Naloxone_is_used_`, #Q30
         Concerns_Needs = `61_Does_your_househo`, #Q31
         Recommendations = `62_We_conduct_this_s` #Q32
  )

# TODO: Unsure purpose of code below, seems like it was maybe used before manual
#         naming? May be a good idea to remove to avoid misformatting if no
#         longer useful.

casper_2026 <- casper_2026 %>%
  rename_with(~ str_remove(., "^\\d+_")) %>%  #cleaning variable names
  rename_with(~ str_to_title(.)) %>% 
  rename_with(~ str_replace_all(str_to_title(str_replace_all(., "_", " ")), " ", "_")) %>% 
  mutate(Created_At = as_datetime(Created_At))

# Weighting ====================================================================
# Weight = Total HH sampling frame / # HH interviewed in cluster * # clusters

# TODO: Should weight be calculated later so that it can change if methods change?

n_clusters <- length(unique(casper_2026$Cluster_Number)) # Should be 30

casper_2026 <- casper_2026 %>%
  add_count(Cluster_Number, name = "completed") %>%
  mutate(weight = total.hh.sample / (completed * n_clusters))

length(unique(casper_2026$Cluster_Number)) #check that completed clusters = 30
str(casper_import_raw) #variable names and types

# Merge ========================================================================
# Adding 2026 data to all previous years

# TODO: Could improve readability by only renaming variables once
# TODO: Standardize naming - some use PascalCase, some use Pascal_Snake_Case

combined_data <- casper_2026 %>%
  mutate(year = 2026) %>% 
  rename("ec5_uuid" = "Ec5_Uuid",
         "ClusterNum" = "Cluster_Number",
         "SurveyNum" = "Survey_Number",
         "TotalHHMembers" = "Total_Hh_Members",
         "Infants" = "Infants",
         "Kids" = "Kids",
         "Adults" = "Adults",
         "Elders" = "Elders",
         "Languages" = "Language",
         "Language_Other" = "Language_Other",
         "Own_Rent" = "Own_Rent",
         "Own_Rent_Other" = "Own_Rent_Other",
         "Insurance_loss" = "Insurance_Loss",
         "CommPlan" = "Comm_Plan",
         "MeetPlace" = "Meeting_Place",
         "SafeDocs" = "Important_Docs",
         "Electric_dependent" = "Med_Equip",
         "Backup_PSU" = "Backup_Power",
         "TsunamiZone" = "Tsunami_Zone_Familiar",
         "Find_Zone_Info" = "Tsunami_Zone_Info",
         "WildfireMeasures" = "Wildfires_Protection",
         "Receive_Alerts" = "Receive_Alerts",
         "KEMA" = "Kema_Familiar",
         "Cat1" = "Cat1",
         "Cat2" = "Cat2",
         "Cat3" = "Cat3",
         "Cat4" = "Cat4",
         "Cat5" = "Cat5",
         "OtherCat1" = "Cat1_Other",
         "OtherCat2" = "Cat2_Other",
         "OtherCat3" = "Cat3_Other",
         "OtherCat4" = "Cat4_Other",
         "OtherCat5" = "Cat5_Other",
         "Evac_Barrier" = "Barrier",
         "MeaslesOutbreak" = "Measles_Aware",
         "MeaslesConcern" = "Measles_Concern",
         "Vaccinate" = "Vaccines_Important",
         "Ment_Health_2026" = "Ment_Health",
         "Rent_Concern" = "Rent_Concern",
         "Other_Concern" = "Concerns_Needs",
         "Surveyor" = "Surveyor_Name",
         "Weight" = "weight") %>%
  bind_rows(casper_full) %>%
  select(-c(Uploaded_At, Title, Created_At, Completed)) %>% 
  rename_with(str_to_sentence)

table(combined_data$Year)
str(combined_data) #variable names and types

# Output =======================================================================
write_csv(combined_data, "./data/Kauai_CASPER_data_all_years.csv")
saveRDS(casper_2026, "./data/casper_2026_raw.rds")
saveRDS(combined_data, "./data/casper_combined.rds")
