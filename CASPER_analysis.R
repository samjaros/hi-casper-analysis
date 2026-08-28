# OLD CODE - for reference when building new pipeline

#CASPER analysis

#install.packages('this.path')

#set working directory
setwd(this.path::here())

# Load libraries ----------------------------------------------------------
library(openxlsx)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(srvyr)
library(scales)
library(janitor)
library(snakecase) #cleaning variable names
library(patchwork) #for combining two graphs
library(powerjoin) #for joining datasets without changing names
library(lubridate)
library(survey)


# Import Data -------------------------------------------------------------
casper_import_raw = read_csv("../Data/2026_casper_data.csv")
casper_full = read_csv("../Data/Kauai CASPER data combined.csv")

#renaming variables and cleaning variable names
casper_2026 <- casper_import_raw %>%
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

casper_2026 <- casper_2026 %>%
  rename_with(~ str_remove(., "^\\d+_")) %>%  #cleaning variable names
  rename_with(~ to_any_case(., case = "title")) %>% 
  rename_with(~ str_replace_all(str_to_title(str_replace_all(., "_", " ")), " ", "_")) %>% 
  mutate(Created_At = as_datetime(Created_At))


# Weighting ---------------------------------------------------------------
# Weight = Total number of HH in sampling frame / 
# Number of HH interviewed within cluster * Number of Clusters selected  (30)
total_households<-24712 #numer of total households in sampling frame

casper_2026 <- casper_2026 %>%
  add_count(Cluster_Number, name = "completed") %>%
  mutate(weight = total_households / (completed*30))

length(unique(casper_2026$Cluster_Number)) #check that completed clusters = 30
str(casper_import_raw) #variable names and types


# Adding 2026 data to all previous years ----------------------------------

combined_data <- casper_2026 %>%
  mutate(year = 2026) %>% 
  rename("ec5_uuid" = "Ec_5_Uuid",
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
          "Cat1" = "Cat_1",
          "Cat2" = "Cat_2",
          "Cat3" = "Cat_3",
          "Cat4" = "Cat_4",
          "Cat5" = "Cat_5",
          "OtherCat1" = "Cat_1_Other",
          "OtherCat2" = "Cat_2_Other",
          "OtherCat3" = "Cat_3_Other",
          "OtherCat4" = "Cat_4_Other",
          "OtherCat5" = "Cat_5_Other",
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
  select(-c(Uploaded_At, Title, Created_At,Completed)) %>% 
  rename_with(str_to_sentence)

  table(combined_data$Year)

write_csv(combined_data, "../Data/Kauai_CASPER_data_all_years.csv")

str(combined_data) #variable names and types

# Clean variables ---------------------------------------------------------
  # Substitute 0s with "No" for at least one member in each group
  # Create a validation variable for if Total_Hh_Members = SUM(Age groups)
hurricane_vars <- c("Cat_1", "Cat_2", "Cat_3", "Cat_4", "Cat_5")   #Q10

casper_2026 <- casper_2026 %>%
  mutate(
    Infants_2 = if_else(as.numeric(Infants) > 0, "Yes", "No", missing = NA_character_),
    Kids_2    = if_else(as.numeric(Kids) > 0, "Yes", "No", missing = NA_character_ ),
    Adults_2  = if_else(as.numeric(Adults) > 0, "Yes", "No", missing = NA_character_),
    Elders_2  = if_else(as.numeric(Elders) > 0, "Yes", "No", missing = NA_character_), 
    
    HHMembers_by_age = as.numeric(Infants) + as.numeric(Kids) + 
      as.numeric(Adults) + as.numeric(Elders),
    
    HHMember_match = case_when(
      is.na(Total_Hh_Members) | is.na(HHMembers_by_age) ~ NA_character_,
      Total_Hh_Members == HHMembers_by_age ~ "Match",
      TRUE ~ "Mismatch"),
    
    Cat_5 = case_when(
      Cat_5 == "Shelter in place in at home" ~ "Shelter in place at home",
      TRUE ~ Cat_5),
    
    across(all_of(hurricane_vars), str_to_sentence))


table(casper_2026$HHMember_match) #investigate mismatches
look<-casper_2026 %>% select(Cluster_Number, Survey_Number, Total_Hh_Members, Infants_2, Kids_2, Adults_2, Elders_2, HHMembers_by_age, HHMember_match) %>% 
  filter(HHMember_match == 'Mismatch')

# Substitute Language = Other with Language_Other 
casper_2026 <- casper_2026 %>% 
  mutate(Language_Other = str_to_title(Language_Other), #standardize response formatting
    Language_Combined = case_when(
    Language == "Other" & !is.na(Language_Other) ~ Language_Other,
    Language == "Other" & is.na(Language_Other) ~ "Other (unspecified)",
    TRUE ~ Language
  ))

table(casper_2026$Language_Combined)
table(casper_2026$Language_Other) #look at other languages and clean

# creating yes/no variable for each language
casper_2026 <- casper_2026 %>% 
  mutate(Ilocano = case_when(str_detect(Language_Other, "Ilocano|Ilokano|Ilikano|Ulakono|Ilikano") ~ 'Yes', TRUE ~ "No"),
         Tagalog = case_when(str_detect(Language_Other, "Tagalog|Filipino|Tagalo") ~ 'Yes', TRUE ~ "No"),
         Japanese = case_when(str_detect(Language_Other, "Japanese") ~ "Yes", TRUE ~ "No"),
         Spanish = case_when(str_detect(Language_Other, "Spanish") ~ "Yes", TRUE ~ "No"),
         Pidgin = case_when(str_detect(Language_Other, "Pidgin|Pigeon") ~ "Yes", TRUE ~ "No"),
         Hawaiian = case_when(str_detect(Language_Other, "Hawaiian|Hawaiin") ~ "Yes", TRUE ~ "No"),
         Visayan = case_when(str_detect(Language_Other, "Vizian|Visaya") ~ "Yes", TRUE ~ "No"),
         Thai = case_when(str_detect(Language_Other, "Thai") ~ "Yes", TRUE ~ "No"),
         Mandarin = case_when(str_detect(Language_Other, "Chinese Mandarin|Chinese") ~ "Yes", TRUE ~ "No"),
         Korean = case_when(str_detect(Language_Other, "Korean") ~ "Yes", TRUE ~ "No"),
         Laotian = case_when(str_detect(Language_Other, "Laotian") ~ "Yes", TRUE ~ "No"),
         French = case_when(str_detect(Language_Other, "French") ~ "Yes", TRUE ~ "No"),
         Greek = case_when(str_detect(Language_Other, "Greek") ~ "Yes", TRUE ~ "No")
  )

# Create new numeric alternative for likert questions
casper_2026 <- casper_2026 %>%  
  mutate(Community_Connected_2 = case_when(
    str_detect(Community_Connected, "1") ~ 1,
    str_detect(Community_Connected, "2") ~ 2,
    str_detect(Community_Connected, "3") ~ 3,
    str_detect(Community_Connected, "4") ~ 4,
    str_detect(Community_Connected, "5") ~ 5,
    TRUE                                 ~ NA  # Default value if none of the above are found
  ))

table(casper_2026$Community_Connected_2) #check to see if there are any 0s

likert_vars <- c("Phys_Health", "Ment_Health","Community_Connected_2") #Q23a,b,24

casper_2026 <- casper_2026 %>%
  mutate(across(
    all_of(likert_vars),
    ~ case_when(
      .x %in% c("1", "2", "3", "4", "5") ~ as.numeric(.x),
      .x %in% c("Unsure", "Declined") ~ NA_real_
    ),
    .names = "{.col}_num"
  ))

# Export clean dataset
write_csv(casper_2026, "../Data/Hawaii_CASPER_2026_cleaned.csv")


# Format Variables --------------------------------------------------------
# LIST ALL VARIABLES WITH YES/NO/UNSURE/DECLINED ANSWERS
yesno_vars <- c("Infants_2","Kids_2","Adults_2","Elders_2",              #Q1
                "Insurance_Loss",                                        #Q3b 
                "Comm_Plan", "Meeting_Place", "Important_Docs",          #Q4a,b,c
                "Med_Equip","Backup_Power",                              #Q5a,b
                "Tsunami_Zone_Familiar","Tsunami_Zone_Info",             #Q6a,b
                "Wildfires_Protection","Receive_Alerts","Kema_Familiar", #Q7,Q8,Q9
                "Measles_Aware","Chickenpox_Aware",                      #13,15
                "Skip_Med_Care",                                         #Q22
                "Food_Support","Naloxone","Concerns_Needs")              #29,30,31

  #this orders the values to make visualization easier
casper_2026 <- casper_2026 %>%
  mutate(across(all_of(yesno_vars), ~factor(.x, levels = c("Yes", "No", "Unsure", "Declined"))))

# HURRICANE CATEGORIES
hurricane_vars <- c("Cat_1", "Cat_2", "Cat_3", "Cat_4", "Cat_5")   #Q10

casper_2026 <- casper_2026 %>%
  mutate(across(all_of(hurricane_vars), ~factor(.x, levels = c("Shelter in place at home", "Friend/family's home", "Public shelter", "Workplace", "Other", "Unsure", "Declined"))))

#LIKERT
casper_2026 <- casper_2026 %>%                                 #Q23a, 23b, 24
  mutate(across(all_of(likert_vars), ~factor(.x, levels = c("1", "2", "3", "4", "5","Unsure", "Declined"))))

#OTHERS
casper_2026$Own_Rent = factor(casper_2026$Own_Rent, levels = c("Own", "Rent","Other", "Unsure", "Declined")) #Q3a
casper_2026$Additional_Evac_Needs = factor(casper_2026$Additional_Evac_Needs, levels = c("Yes", "No", "Unsure", "Declined", "Not Applicable")) #Q12
casper_2026$Measles_Concern = factor(casper_2026$Measles_Concern, levels = c("Very concerned", "Somewhat concerned","Not concerned", "Unsure", "Declined")) #Q14
casper_2026$Chickenpox_Concern = factor(casper_2026$Chickenpox_Concern, levels = c("Very concerned", "Somewhat concerned","Not concerned", "Unsure", "Declined")) #Q16
casper_2026$Vaccines_Important = factor(casper_2026$Vaccines_Important, levels = c("Very important", "Somewhat important","Not important", "Unsure", "Declined")) #Q17
casper_2026$Mosquito_Concern = factor(casper_2026$Mosquito_Concern, levels = c("Very concerned", "Somewhat concerned","Not concerned", "Unsure", "Declined")) #Q18
casper_2026$Mosquito_Breeding = factor(casper_2026$Mosquito_Breeding, levels = c("Daily", "Weekly", "Monthly","A few times per year", "Once a year",  "Never","Unsure", "Declined")) #Q19
casper_2026$Specific_Doctor = factor(casper_2026$Specific_Doctor, levels = c("All", "Most", "Some", "None", "Unsure", "Declined")) #Q20
casper_2026$Provider_Within_Year = factor(casper_2026$Provider_Within_Year, levels = c("All", "Most", "Some", "None", "Unsure", "Declined")) #Q21
casper_2026$Often_Gather = factor(casper_2026$Often_Gather, levels = c("Daily", "A few times per week", "Weekly","A few times per month", "Rarely","Never","Unsure","Declined")) #Q25
casper_2026$Often_Groups = factor(casper_2026$Often_Groups, levels = c("Daily", "A few times per week", "Weekly","A few times per month", "Rarely","Never","Unsure","Declined")) #Q26
casper_2026$Rent_Concern = factor(casper_2026$Rent_Concern, levels = c("Very concerned", "Somewhat concerned","Not concerned", "Unsure", "Declined")) #Q27
casper_2026$Food_Last = factor(casper_2026$Food_Last, levels = c("Always", "Usually","Sometimes", "Rarely", "Never", "Unsure", "Declined")) #Q28



# Keiki = HH with kids under 18

casper_2026 <- casper_2026 %>%
  mutate(
    keiki = if_else(
      Infants_2 == "Yes" | Kids_2 == "Yes",
      "Yes",
      "No",
      missing = NA_character_
    )
  )

table(casper_2026$keiki)

# Srvyr analysis ----------------------------------------------------------

# Convert to srvyr design
srv_design <- as_survey_design(casper_2026, ids = Cluster_Number, weights = weight)


# Tables ------------------------------------------------------------------
# Means
numeric_vars <- c("Total_Hh_Members", "Phys_Health_num", "Ment_Health_num", "Community_Connected_2_num")

summarize_mean <- function(numeric_vars) {
  mean_results <- srv_design %>%
    summarize(
      Mean = survey_mean(.data[[numeric_vars]], 
                         na.rm = TRUE, 
                         vartype = c("ci", "se")))
  
  tibble(
    Variable = numeric_vars,
    Mean = mean_results$Mean,
    Mean_LCI = mean_results$Mean_low,
    Mean_UCI = mean_results$Mean_upp,
    Mean_SE = mean_results$Mean_se)
}

# Combine results
mean_table <- map_dfr(numeric_vars, summarize_mean)

# Median Table
srv_design %>%
  summarize(median = survey_quantile(Total_Hh_Members, quantiles = 0.5, na.rm = TRUE, vartype = "ci"))

summarize_median <- function(numeric_vars) {
  median_results <- srv_design %>%
    summarize(
      median = survey_quantile(.data[[numeric_vars]], quantiles = 0.5, na.rm = TRUE, vartype = "ci")
    )
  
  tibble(
    Variable = numeric_vars,
    median = median_results$median_q50 ,
    median_LCI = median_results$median_q50_low ,
    median_UCI = median_results$median_q50_upp,
  )
}

# Combine results
median_table <- map_dfr(numeric_vars, summarize_median)




# Demographics
# List of categorical variables to summarize
demo_vars <- c("Infants_2","Kids_2","Adults_2","Elders_2", "keiki","Own_Rent","Insurance_Loss", "Language_Combined")


summarize_demo <- function(demo_vars) {srv_design %>%
    filter(!is.na(.data[[demo_vars]])) %>%      #.data[[demo_vars]] tells R to look in the data for a column name, in this case, from the list demo_vars and drops missing
    group_by(.data[[demo_vars]]) %>%            #grouping by values in each variable
    summarize(
      Frequency = n(),                          #count of responses for each variable
      Estimated_HH = round(survey_total(vartype = NULL), 0),
      Percent = round(survey_mean(vartype = c("ci", "se"), na.rm = TRUE) * 100, 2) #Calculates the weighted percentage of the population that falls into this category. The vartype = "ci" tells R to automatically calculate the 95% Confidence Intervals (the lower and upper bounds of uncertainty for that percentage).
        ) %>%
    mutate(Variable = demo_vars, Category = .data[[demo_vars]]) %>% #lists demo_vars in a column called category
    select(Variable, Category, Frequency, Estimated_HH, Percent, Percent_low, Percent_upp, Percent_se)
}

# Combine results
demo_table <- map_dfr(demo_vars, summarize_demo)





# Evacuation Planning
# List of categorical variables to summarize
evac_vars <- c("Comm_Plan", "Meeting_Place", "Important_Docs",
               "Med_Equip","Backup_Power",                   #not sure if these belong here
               "Tsunami_Zone_Familiar", "Tsunami_Zone_Info",           
               "Wildfires_Protection","Receive_Alerts","Kema_Familiar",
               "Cat_1", "Cat_2", "Cat_3", "Cat_4", "Cat_5",
               "Barrier","Additional_Evac_Needs")

summarize_evac <- function(evac_vars) {srv_design %>%
    #filter(!is.na(.data[[evac_vars]])) %>%
    group_by(.data[[evac_vars]],.drop = FALSE) %>%
    summarize(
      Frequency = n(),
      Estimated_HH = round(survey_total(vartype = NULL), 0),
      Percent = round(survey_mean(vartype = c("ci", "se"), 
                                  na.rm = TRUE) * 100, 2) 
    ) %>%
    mutate(Variable = evac_vars, Category = .data[[evac_vars]]) %>%
    select(Variable, Category, Frequency, Estimated_HH, Percent, 
           Percent_low, Percent_upp, Percent_se)
}


# Combine results
evac_table <- map_dfr(evac_vars, summarize_evac)




# Infectious Diseases
# List of categorical variables to summarize
ID_vars <- c("Mosquito_Concern", "Mosquito_Breeding", "Measles_Aware","Chickenpox_Aware", "Measles_Concern", "Chickenpox_Concern", "Vaccines_Important")


summarize_ID <- function(ID_vars) {srv_design %>%
    filter(!is.na(.data[[ID_vars]])) %>%
    group_by(.data[[ID_vars]]) %>%
    summarize(
      Frequency = n(),
      Estimated_HH = survey_total(vartype = NULL),
      Percent = survey_mean(proportion = TRUE, vartype = c("ci", "se"), na.rm = TRUE)
    ) %>%
    mutate(Variable = ID_vars, Category = .data[[ID_vars]]) %>%
    select(Variable, Category, Frequency, Estimated_HH, Percent, Percent_low, Percent_upp, Percent_se)
}

# Combine results
ID_table <- map_dfr(ID_vars, summarize_ID)



# General Questions
# List of categorical variables to summarize
gen_vars <- c("Rent_Concern", "Food_Last", "Food_Support", "Naloxone","Ment_Health","Phys_Health","Community_Connected",
              "Often_Gather","Often_Groups","Specific_Doctor","Provider_Within_Year","Skip_Med_Care",
              "Concerns_Needs")

summarize_gen <- function(gen_vars) {srv_design %>%
    filter(!is.na(.data[[gen_vars]])) %>%
    group_by(.data[[gen_vars]]) %>%
    summarize(
      Frequency = n(),
      Estimated_HH = round(survey_total(vartype = NULL), 0),
      Percent = round(survey_mean(vartype = c("ci", "se"), na.rm = TRUE) * 100, 2) #Calculates the weighted percentage of the population that falls into this category. The vartype = "ci" tells R to automatically calculate the 95% Confidence Intervals (the lower and upper bounds of uncertainty for that percentage).
    ) %>%
    mutate(Variable = gen_vars, Category = .data[[gen_vars]]) %>%
    select(Variable, Category, Frequency, Estimated_HH, Percent, Percent_low, Percent_upp, Percent_se)
}

# Combine results
gen_table <- map_dfr(gen_vars, summarize_gen)




#Languages
lang_vars <- c("Ilocano","Japanese","Mandarin","Hawaiian","Pidgin","Spanish","Tagalog","Thai","Visayan")

summarize_lang <- function(lang_vars) {srv_design %>%
    filter(!is.na(.data[[lang_vars]])) %>%
    group_by(.data[[lang_vars]]) %>%
    summarize(
      Frequency = n(),
      Estimated_HH = round(survey_total(vartype = NULL), 0),
      Percent = round(survey_mean(vartype = c("ci", "se"), na.rm = TRUE) * 100, 2) #Calculates the weighted percentage of the population that falls into this category. The vartype = "ci" tells R to automatically calculate the 95% Confidence Intervals (the lower and upper bounds of uncertainty for that percentage).
    ) %>%
    mutate(Variable = lang_vars, Category = .data[[lang_vars]]) %>%
    select(Variable, Category, Frequency, Estimated_HH, Percent, Percent_low, Percent_upp, Percent_se)
}

# Combine results
lang_table <- map_dfr(lang_vars, summarize_lang)




#combine tables into one with each on a different sheet
# 1. Put your individual summary tables into a named list
my_sheets <- list(
  "Demo_Table"    = demo_table,
  "Evac_Table"    = evac_table,
  "ID_table" = ID_table,
  "Gen_table" = gen_table,
  "Lang_table" = lang_table)

# 2. Export them all into one file with different sheets
write.xlsx(my_sheets, file = "../Data/CASPER_2026_data_tables.xlsx")

# Keiki -------------------------------------------------------------------
# 2x2 weighted cross-tab
food_last_by_keiki <- srv_design %>%
  group_by(keiki, Food_Last) %>%
  summarize(
    n = unweighted(n()),
    Weighted_n = survey_total(),
    Percent = survey_mean(proportion = TRUE, na.rm = TRUE),
    .groups = "drop"
  )


Vaccines_Important_by_keiki <- srv_design %>%
  group_by(keiki, Vaccines_Important) %>%
  summarize(
    n = unweighted(n()),
    Weighted_n = survey_total(),
    Percent = survey_mean(proportion = TRUE, na.rm = TRUE),
    .groups = "drop"
  )


# Chi square tests --------------------------------------------------------
# srv_design for full dataset across years
srv_design_full <- as_survey_design(combined_data, ids = Clusternum, weights = Weight)


# Electric dependent 25 v 26 ----------------------------------------------
table(combined_data$Electric_dependent, combined_data$Year)

look<-combined_data %>% 
  filter(Electric_dependent != "Unsure",
         Electric_dependent != "Declined")

table(look$Electric_dependent, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2025, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Electric_dependent, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)


# Wildfires 2024 v 2026 ---------------------------------------------------
table(combined_data$Wildfiremeasures, combined_data$Year)

look<-combined_data %>% 
  filter(Wildfiremeasures != "Unsure",
         Wildfiremeasures != "Declined")

table(look$Wildfiremeasures, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2024, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Wildfiremeasures, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)





wildfire <- c("Wildfiremeasures")

summarize_wildfire <- function(wildfire) {srv_design_full %>%
    group_by(Year, .data[[wildfire]],.drop = FALSE) %>%
    summarize(
      Frequency = n(),
      Estimated_HH = round(survey_total(vartype = NULL), 0),
      Percent = round(survey_mean(vartype = c("ci", "se"), 
                                  na.rm = TRUE) * 100, 2) 
    ) %>%
    mutate(Variable = wildfire, Category = .data[[wildfire]]) %>%
    select(Variable, Category, Frequency, Estimated_HH, Percent, 
           Percent_low, Percent_upp, Percent_se)
}


# Combine results
wildfire_table <- map_dfr(wildfire, summarize_wildfire) %>% 
  filter(Year %in% c(2024, 2026))







# Public Shelter first year v 2026 ----------------------------------------
table(combined_data$Cat5, combined_data$Year)

look<-combined_data %>% 
  mutate(Cat5 = case_when(
    Cat5 == "Friend/family's home" ~ "other",
    Cat5 == "Other" ~ "other",
    Cat5 == "Shelter in place (at home)" ~ "other",
    Cat5 == "Shelter in place in at home" ~ "other",
    Cat5 == "Workplace" ~ "other",
    TRUE   ~ Cat5 # Handles anything that didn't match above
  )) %>% 
  filter(Cat5 != "Unsure",
         Cat5 != "Declined")

table(look$Cat5, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2019, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Receive_alerts, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)


# tsunami zone 2024 v 2026 ------------------------------------------------
# data under two different variable names
table(combined_data$Evac_zone_familiar, combined_data$Year)
table(combined_data$Tsunamizone, combined_data$Year)
new<-combined_data %>% 
  mutate(tsunami_new = coalesce(Evac_zone_familiar, Tsunamizone))

table(new$tsunami_new, new$Year)

look<-new %>% 
  filter(tsunami_new != "Unsure")

table(look$tsunami_new, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2024, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + tsunami_new, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)


# alerts 2019 v 2026 ------------------------------------------------------
table(combined_data$Receive_alerts, combined_data$Year)

look<-combined_data %>% 
  filter(Receive_alerts != "Unsure")%>% 
  filter(Year %in% c(2019,2026))

table(look$Receive_alerts, look$Year) 

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2019, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Receive_alerts, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)

# measles concern very&somewhat vs not 2019 v 2026 ------------------------
table(combined_data$Measlesconcern, combined_data$Year)

look<-combined_data %>% 
  mutate(Measlesconcern = case_when(
    Measlesconcern == "Very concerned" ~ "concerned",
    Measlesconcern == "Somewhat concerned" ~ "concerned",
    # Vaccinate == "Unsure" ~ "Not important",
    # Vaccinate == "Declined" ~ "Not important",
    TRUE   ~ Measlesconcern # Handles anything that didn't match above
  )) %>% 
  filter(Measlesconcern != "Unsure",
         Measlesconcern != "Declined")

table(look$Measlesconcern, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2019, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Measlesconcern, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)


# vaccines not important 2025 v 2026 --------------------------------------
table(combined_data$Vaccinate, combined_data$Year)

look<-combined_data %>% 
  mutate(Vaccinate = case_when(
    Vaccinate == "Not important" ~ "Not important",
    Vaccinate == "Somewhat important" ~ "Not important",
    # Vaccinate == "Unsure" ~ "Not important",
    # Vaccinate == "Declined" ~ "Not important",
    TRUE   ~ Vaccinate # Handles anything that didn't match above
  )) %>% 
  filter(Vaccinate != "Unsure",
         Vaccinate != "Declined")

table(look$Vaccinate, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2025, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Vaccinate, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)


# vaccines important 2019 v 2026 ------------------------------------------

table(combined_data$Vaccinate, combined_data$Year)

look<-combined_data %>% 
  mutate(Vaccinate = case_when(
    Vaccinate == "Not important" ~ "Not important",
    Vaccinate == "Somewhat important" ~ "Not important",
    # Vaccinate == "Unsure" ~ "Not important",
    # Vaccinate == "Declined" ~ "Not important",
    TRUE   ~ Vaccinate # Handles anything that didn't match above
  )) %>% 
  filter(Vaccinate != "Unsure",
         Vaccinate != "Declined") %>% 
  filter(Year %in% c(2019, 2026))

table(look$Vaccinate, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2019, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Vaccinate, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)

# concern for rent (very and somewhat vs none) 2025 v 2026 ----------------
table(combined_data$Rent_concern, combined_data$Year)

look<-combined_data %>% 
  mutate(Rent_concern = case_when(
    Rent_concern == "Very concerned" ~ "concerned",
    Rent_concern == "Somewhat concerned" ~ "concerned",
    # Vaccinate == "Unsure" ~ "Not important",
    # Vaccinate == "Declined" ~ "Not important",
    TRUE   ~ Rent_concern # Handles anything that didn't match above
  )) %>% 
  filter(Rent_concern != "Unsure",
         Rent_concern != "Declined")

table(look$Rent_concern, look$Year)

srv_design_look <- as_survey_design(look, ids = Clusternum, weights = Weight)

survey_filtered <- srv_design_look %>%
  filter(Year %in% c(2025, 2026))

weighted_chisq <- svychisq(
  formula = ~ Year + Rent_concern, 
  design = survey_filtered,
  statistic = "F" #“F”, “Chisq”, “Wald”, “adjWald”, “lincom”, “saddlepoint”, “wls-score”
)

print(weighted_chisq)

# Visualizations ----------------------------------------------------------

# Color schemes:
blue_colors <- c("#001B69", "#1476D1", "#29D7EC", "#4682B4", "#005A70","blue","lightblue","darkblue","#6baed6", "#4292c6", "#545454", "#a6a6a6")

custom_percent_formatter <- function(x) {
  # Multiply by 100 to check the whole percentage scale
  pct_val <- x * 100
  
  ifelse(pct_val > 0 & pct_val < 1, 
         "<1%", 
         paste0(round(pct_val, 0), "%"))
}

# Q1 Distribution of Age -----------------------------------------------------
plot_data<-demo_table %>% 
  filter(Variable %in% c("Infants_2","Kids_2","Adults_2","Elders_2"),
         Category == "Yes")%>% 
  mutate(Variable = case_when(
    Variable == "Infants_2"  ~ "Infants (<2 yrs old)",
    Variable == "Kids_2"  ~ "Kids (2-17 yrs old)",
    Variable == "Adults_2" ~ "Adults (18-64 yrs old)",
    Variable == "Elders_2" ~ "Elders (65 or older)",
    # Catch-all to keep everything else exactly as it was:
    TRUE  ~ Variable 
  )) %>% 
  mutate(Variable = factor(Variable, levels = c("Infants (<2 yrs old)", 
                                                "Kids (2-17 yrs old)", 
                                                "Adults (18-64 yrs old)", 
                                                "Elders (65 or older)"))) %>% 
  mutate(Percent_low = if_else(Percent_low < 0, 0, Percent_low)) #change negative lower CI to 0

ggplot(plot_data, aes(x = Variable, y = Percent, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c(
    "Yes" = "#4682B4",
    "No"  = "#005A70",
    "Unsure" = "#a6a6a6",
    "Declined" = "#545454"),     
    breaks = c("Yes", "No", "Unsure","Declined")
  ) +
  geom_text(aes(label = percent(Percent/100)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  labs(
    title = "At Least One Member in Each Age Group",
    x = "Age Group",
    y = "Percent of Households",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 25),
    legend.position = "",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)), # Y-axis title
    axis.text.x  = element_text(size = 15, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 15, color = "black")                         # Y-axis tick labels
  )+
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp), 
    width = 0.15,        # Width of the horizontal caps on the error bars
    color = "#545454",   # Clean charcoal gray color
    linewidth = 0.8
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1, scale = 1))+ #formatting the y-axis
  scale_x_discrete(labels = c("Infants\n(<2 yrs old)", "Kids\n(2-17 yrs old)", "Adults\n(18-64 yrs old)","Elders\n(65 or older)"))

# Q2 Languages ------------------------------------------------------------
#NEED TO WEIGHT THIS FIRST TABLE
table(casper_2026$Language_Combined)

plot_data <- srv_design %>%
  mutate(
    Language_Group = case_when(
      # If it says English and contains a comma or another language indicator:
      Language_Combined == "English, Other" | Language_Combined == "Other, English" ~ "English + At least one other Language",
      
      # If it only contains English with no other text:
      Language_Combined == "English" ~ "English only",
      
      Language_Combined == "Vizian" ~ "Visayan only",
      Language_Combined == "Ilocano" ~ "Ilocano only",
      
      # Optional catch-all for missing/refused data
      is.na(Language_Combined)                                                      ~ NA_character_,
      TRUE                                                                          ~ "English only" 
    )
  ) %>% 
  filter(!is.na(Language_Group)) %>%
  group_by(Language_Group) %>%
  summarize(
    Percent = survey_mean(proportion = TRUE, na.rm = TRUE) * 100
  ) 

ggplot(plot_data, aes(x = Language_Group, y = Percent)) +
  geom_bar(stat = "identity", fill = "#1476D1", width = 0.7) +      # Draw the vertical bars with the specific blue fill
  scale_y_continuous(                                               # Set y-axis to 0-100% and append the % sign to labels
    limits = c(0, 100), 
    breaks = seq(0, 100, by = 20), 
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  # Map labels
  labs(
    title = "Number of Languages Spoken in the Household",
    x = NULL,
    y = "Percent of Households"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  scale_x_discrete(labels = label_wrap(10))+
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_line(color = "#EBF0EB"), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


#Language_Other

long_data <- casper_2026 %>%
  pivot_longer(cols = c(Ilocano, Tagalog, Japanese, Spanish, Pidgin, Hawaiian,Visayan, Thai, Mandarin, Korean), 
               names_to = "Category", values_to = "Response")

plot_data <- long_data %>%
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() 

ggplot(plot_data, aes(x = Category, y = Percent, fill = Response)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c(
    "Yes" = "#001B69",
    "No"  = "#005A70",
    "Unsure" = "#a6a6a6",
    "Declined" = "#545454"),     
    breaks = c("Yes", "No", "Unsure","Declined")
  ) +
  geom_text(aes(label = percent(Percent/100)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  labs(
    title = "Languages Spoken Among Households on Kauai",
    x = "Language",
    y = "Percent of Households",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )




# Q3a Own vs. Rent over time --------------------------------------------------------
#prior years data
old_data<-read.xlsx("../Data/Own_Rent_Table_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year))


plot_data <- srv_design %>%
  group_by(Own_Rent) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>%  #survey_mean calculates weighted percents
  # Ensure categories stay in the exact order: Own -> Rent -> Other
  mutate(Own_Rent = factor(Own_Rent, levels = c("Own", "Rent", "Other"))) %>% 
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(!Own_Rent %in% c("Unsure","Declined", "Other")) #dropping unusre and declined

ggplot(plot_data, aes(x = factor(Own_Rent, levels = c("Own", "Rent", "Other")), y = Percent, fill = factor(Year), group = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "#666666") +
  scale_fill_manual(values = c(
    "2017" = "peachpuff",
    "2019" = "#FDA172",  
    "2020" = "#ED7117", 
    "2022" = "#DD571C",
    "2023" = "darkorange3", 
    "2024" = "#B2560D", 
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  ),
  guide = guide_legend(nrow = 1)) +

  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  labs(
    title = "Own vs Rent the Residence",
    x = NULL,
    y = "Percent of Households",
    fill = "Year"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = percent_format(scale = 1))


# Q3b Homeowners Insurance Loss -------------------------------------------
old_data<-read.xlsx("../Data/Insurance_Loss_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year),
         Variable = 'Insurance_Loss') %>% 
  rename(Category = Insurance_Loss)

table(casper_2026$Insurance_Loss)

plot_data <- demo_table %>% 
  filter(Variable == "Insurance_Loss") %>% 
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(Category %in% c("Yes","No","Unsure"))


ggplot(plot_data, aes(x = factor(Category,levels=c("Yes","No","Unsure")), y = Percent, fill = factor(Year), group = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "#666666") +
  scale_fill_manual(values = c(
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  )) +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  labs(
    title = "Homeowners Insurance Loss Due\nto Rising Costs",
    x = NULL,
    y = "Percent of Households",
    fill = "Year"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = percent_format(scale = 1))










plot_data <- srv_design %>%
  filter(!is.na(Insurance_Loss)) %>%  
  group_by(Insurance_Loss) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>% # Compute weighted proportions and convert to percentages (0-100)
  mutate(Insurance_Loss = factor(Insurance_Loss, levels = c("Yes", "No", "Unsure","Declined"))) %>% 
  filter(Insurance_Loss %in% c("Yes","No"))

ggplot(plot_data, aes(x=Insurance_Loss, y = Percent)) +
  geom_bar(stat = "identity", fill = "#001B69", width = 0.8) +
  # Use the automatically generated design-adjusted confidence intervals
  geom_errorbar(aes(ymin = Percent_low, ymax = Percent_upp), width = 0.1, color = "#444444") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20), expand = c(0, 0)) +
  labs(
    title = "Homeowners Insurance Loss Due to\nRising Costs",
    x = NULL,
    y = "Percent of Households"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 10)),
    axis.text = element_text(size = 15, color = "black"),
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    panel.border = element_rect(color = NA, fill = NA, size = 1),
    axis.ticks = element_line(color = "#888888")
  )

# 4abc Emergency Plans ----------------------------------------------------
old_data_1<-read.xlsx("../Data/Communication_Plan_Table_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year),
         Category = "Comm_Plan") %>% 
  rename(Response = Comm_Plan)
old_data_2<-read.xlsx("../Data/Meeting_Place_Table_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year),
         Category = "Meeting_Place") %>% 
  rename(Response = Meeting_Place)
old_data_3<-read.xlsx("../Data/Important_Documents_Table_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year),
         Category = "Important_Docs") %>% 
  rename(Response = Important_Docs)

emerg_plans_data <- 
  
  
plot_data <- evac_table %>%
  rename(Category = Variable,
         Response = Category) %>% 
  filter(Category %in% c("Comm_Plan","Meeting_Place","Important_Docs")) %>% 
  mutate(Year = 2026) %>% 
  bind_rows(old_data_1,old_data_2,old_data_3) %>% 
  filter(!Response %in% c("Unsure","Declined","No")) %>%  #dropping no, unsure and declined
  mutate(Category = case_when(
    Category == "Comm_Plan"      ~ "Household\ncommunication plan",
    Category == "Meeting_Place"  ~ "Designated\nmeeting place",
    Category == "Important_Docs" ~ "Copies of\nimportant documents in\na safe place",
    # Catch-all to keep everything else exactly as it was:
    TRUE                         ~ Category 
  ))


ggplot(plot_data, aes(x = Category, y = Percent, fill = factor(Year))) + # Wrap Year in factor()
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color="black") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()+
  labs(
    title = "Comparison of Variables by Year",
    x = "Variables",
    y = "Percentage"
  )+
  scale_fill_manual(values = c(
    "2017" = "peachpuff",
    "2019" = "#FDA172",  
    "2020" = "#ED7117", 
    "2022" = "#DD571C",
    "2023" = "darkorange3", 
    "2024" = "#B2560D", 
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  ),
  guide = guide_legend(nrow = 1)) +
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  labs(
    title = "Emergency Plans",
    x = "",
    y = "Percent of Households",
    fill = "Year"
  )  +
  theme(
    legend.position = "bottom",
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.title.x = element_text(face = "bold", size = 15, color = "black"),
    
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


# Q5a&b Dependence on Electricity/Backup Power over time -------------------------------

old_data<-read.xlsx("../Data/Electricity_Dependent_MedEquip_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year))

plot_data <- srv_design %>%
  group_by(Med_Equip) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>%  #survey_mean calculates weighted percents
  # Ensure categories stay in the exact order: Own -> Rent -> Other
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(Med_Equip == "Yes") #only keeping Yes


ggplot(plot_data, aes(x = factor(Year), y = Percent, fill = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = NA) +
  scale_fill_manual(values = c(
    "2017" = "peachpuff",
    "2019" = "#FDA172",  
    "2020" = "#ED7117", 
    "2022" = "#DD571C",
    "2023" = "darkorange3", 
    "2024" = "#B2560D", 
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  ))  +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  labs(
    title = "Households with Electricity-Dependent\nHealth Needs",
    x = "",
    y = "Percent of Households",
    fill = "Year"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    legend.position = "none",
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.title.x = element_text(face = "bold", size = 15, color = "black"),
    
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 25),
                     labels = percent_format(scale = 1))



# Main survey question data
data_main <- evac_table %>% 
  filter(Variable == "Med_Equip") %>% 
  mutate(Category = factor(c("Yes", "No", "Unsure", "Declined")))

# Follow-up question data
data_sub <- evac_table %>% 
  filter(Variable == "Backup_Power") 

#function to create donut visuals for each dataset
make_donut <- function(data, colors, title_text, n_text, rotation_angle = 0) {
  
  data <- data %>%
    mutate(
      fraction = Frequency / sum(Frequency),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1)),
      label_pos = (ymin + ymax) / 2,
      text_color = ifelse(Category %in% c("No", "Unsure") & n_text == "32", "black", "white")
    )
  
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = Category)) +
    geom_rect(color = "white", linewidth = 1) + 
    
    # Add the start argument here to rotate the entire wheel
    coord_polar(theta = "y", start = rotation_angle) +
    
    xlim(c(0, 4.5)) + 
    scale_fill_manual(values = colors) +
    
    geom_text(
      aes(x = 3.25, y = label_pos, label = Category, color = text_color),
      fontface = "bold",
      size = 4.5
    ) +
    scale_color_identity() + 
    theme_void() + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, vjust = -2)
    ) +
    labs(title = paste0(title_text, "\n(n=", n_text, ")"))
}

# Define color palettes matching your image
colors_main <- c("No" = "#1476D1", "Yes" = "#001B69", "Unsure" = "#7F7F7F")
colors_sub  <- c("Yes" = "#001B69", "No" = "#1476D1", "Unsure" = "#7F7F7F")

# Generate the individual plots
plot_left  <- make_donut(data_main, colors_main, "Dependence on Electricity", "xx", 
                         rotation_angle = -2.3) #use this to rotate the first graph
plot_right <- make_donut(data_sub, colors_sub, "Has a Backup Power\nSupply", "xx",
                         rotation_angle = 1.5) #use this to rotate the second graph

# Combine them side-by-side using patchwork
final_plot <- plot_left + plot_right + 
  plot_annotation(
    title = "Dependence on Electricity for Health Needs and Subsequent\nBackup Power Supply",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

# Display the plot
print(final_plot)

table(casper_2026$Med_Equip)

# Q6a&b Tsunami zone familiar ----------------------------------------------------------------------
#NEED TO WEIGHT THIS?!

# Main survey question data
data_main <- srv_design %>%
  group_by(Tsunami_Zone_Familiar) %>%
  summarize(Frequency = survey_mean(na.rm = TRUE), # survey_mean gives us the weighted proportion (fraction) directly
  n_raw = unweighted(n())) %>% # unweighted n for the title string
  rename(Category = Tsunami_Zone_Familiar)# Rename column to match your original function's 'Category' structure

# Follow-up question data
data_sub <- srv_design %>%
  filter(!is.na(Tsunami_Zone_Info)) %>% 
  group_by(Tsunami_Zone_Info) %>%
  summarize(Frequency = survey_mean(na.rm = TRUE), # survey_mean gives us the weighted proportion (fraction) directly
            n_raw = unweighted(n())) %>% # unweighted n for the title string
  rename(Category = Tsunami_Zone_Info)# Rename column to match your original function's 'Category' structure

#function to create donut visuals for each dataset
make_donut <- function(data, colors, title_text, n_text, rotation_angle = 0) {
  
  data <- data %>%
    mutate(
      fraction = Frequency / sum(Frequency),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1)),
      label_pos = (ymin + ymax) / 2,
      text_color = ifelse(Category %in% c("No", "Unsure") & n_text == "32", "black", "white")
    )
  
  ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, fill = Category)) +
    geom_rect(color = "white", linewidth = 1) + 
    
    # Add the start argument here to rotate the entire wheel
    coord_polar(theta = "y", start = rotation_angle) +
    
    xlim(c(0, 4.5)) + 
    scale_fill_manual(values = colors) +
    
    geom_text(
      aes(x = 3.25, y = label_pos, label = Category, color = text_color),
      fontface = "bold",
      size = 4.5
    ) +
    scale_color_identity() + 
    theme_void() + 
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, vjust = -2)
    ) +
    labs(title = paste0(title_text, "\n(n=", n_text, ")"))
}

# Define color palettes matching your image
colors_main <- c("No" = "#1476D1", "Yes" = "#001B69", "Unsure" = "#7F7F7F")
colors_sub  <- c("Yes" = "#001B69", "No" = "#1476D1", "Unsure" = "#7F7F7F")

# Generate the individual plots
plot_left  <- make_donut(data_main, colors_main, "Knowledge of Tsunami Zones Isalnd-Wide", "xx", 
                         rotation_angle = -2.3) #use this to rotate the first graph
plot_right <- make_donut(data_sub, colors_sub, "Knows Where to Find\nTsunami Zone Info", "xx",
                         rotation_angle = 1.5) #use this to rotate the second graph

# Combine them side-by-side using patchwork
final_plot <- plot_left + plot_right + 
  plot_annotation(
    title = "Tsunami Zone Awareness and Subsequent\nKnowledge on Where to Find\n Tsunami Zone Info",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

# Display the plot
print(final_plot)


# Q7,8,9 ----------------------------------------------------------------------
long_data <- casper_2026 %>%
  pivot_longer(cols = c(Wildfires_Protection,Receive_Alerts,Kema_Familiar), names_to = "Category", values_to = "Response")

plot_data <- long_data %>%
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() %>% 
  mutate(Category = case_when(
    Category == "Wildfires_Protection"      ~ "Wildfire Protection\nMeasures",
    Category == "Receive_Alerts"  ~ "Receives Emergencyd\nAlerts",
    Category == "Kema_Familiar" ~ "Familiar with\nKEMA",
    # Catch-all to keep everything else exactly as it was:
    TRUE                         ~ Category 
  )) %>% 
  mutate(Response = factor(Response, levels = c("Declined","Unsure","No", "Yes")))

ggplot(plot_data, aes(x = Category, y = Percent, fill = Response)) +
  geom_col(position = "stack", width = 0.7, reverse = TRUE) +
  scale_fill_manual(values = c(
    "Yes" = "#001B69",
    "No"  = "#005A70",
    "Unsure" = "#a6a6a6",
    "Declined" = "#545454"),     
    breaks = c("Yes", "No", "Unsure","Declined")
  ) +
  labs(
    title = "Preparedness",
    x = "Preparedness Measure",
    y = "Percent of Households",
    fill = NULL
  ) +
  geom_text(aes(label = percent(Percent/100)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# Q9 over time KEMA ------------------------------------------------------------

old_data<-read.xlsx("../Data/KEMA_Familiar_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year))

plot_data <- srv_design %>%
  group_by(Kema_Familiar) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>%  #survey_mean calculates weighted percents
  # Ensure categories stay in the exact order: Own -> Rent -> Other
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(Kema_Familiar == "Yes") #only keeping Yes


ggplot(plot_data, aes(x = factor(Year), y = Percent, fill = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "#666666", size = 0.3) +
  scale_fill_manual(values = c(
    "2017" = "peachpuff",
    "2019" = "#FDA172",  
    "2020" = "#ED7117", 
    "2022" = "#DD571C",
    "2023" = "darkorange3", 
    "2024" = "#B2560D", 
    "2026" = "#4682B4"   
  ))  +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  labs(
    title = "Households Familiar with\nKauai.gov/kema",
    x = "",
    y = "Percent of Households",
    fill = "Year"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    legend.position = "none",
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    axis.title.x = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  scale_y_continuous(labels = percent_format(scale = 1))


 
# Q10 Hurricane Data ------------------------------------------------------

plot_data<-evac_table %>% 
  filter(Variable %in% c("Cat_1","Cat_2","Cat_3","Cat_4","Cat_5"))%>% 
  mutate(Category = str_to_sentence(Category),
    Variable = case_when(
    Variable == "Cat_1"  ~ "Category 1",
    Variable == "Cat_2"  ~ "Category 2",
    Variable == "Cat_3"  ~ "Category 3",
    Variable == "Cat_4"  ~ "Category 4",
    Variable == "Cat_5"  ~ "Category 5",
    
    # Catch-all to keep everything else exactly as it was:
    TRUE  ~ Variable 
  )) %>% 
  mutate(Variable = factor(Variable, levels = rev(c("Category 1","Category 2","Category 3","Category 4","Category 5" ))),
         Category = factor(Category, levels = rev(c("Shelter in place at home","Friend/family's home","Public shelter","Workplace",  "Other","Unsure","Declined")))) %>% 
  mutate(
    # Create a string label column based on the value
    Label_Text = case_when(
      Percent == 0 ~ "",                                
      Percent <= 5 ~ "",                             
      TRUE         ~ paste0(round(Percent, 0), "%") ))



ggplot(plot_data, aes(x = Variable, y = Percent, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Shelter in place at home" = "#001B69",
    "Friend/family's home"  = "#005A70",
    "Public shelter"   = "#1476D1",
    "Workplace" = "blue",
    "Other"            = "#29D7EC",
    "Unsure"           = "#a6a6a6",
    "Declined"         = "#545454"
  )) +
  geom_text(aes(label = Label_Text), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4) +
  labs(
    title = "Hurricane Response by Category of Storm",
    x = "Category of Storm",
    y = "Percent of Households",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Y-axis title
    axis.text.x  = element_text(size = 13, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 13, color = "black")                         # Y-axis tick labels
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale = 1))+
  guides(fill = guide_legend(byrow = TRUE, reverse = TRUE))


# Public shelter by category of storm as horizontal bar
plot_data<-evac_table %>% 
  filter(Category == "Public shelter")%>% 
  mutate(Category = str_to_sentence(Category),
         Variable = case_when(
           Variable == "Cat_1"  ~ "Category 1",
           Variable == "Cat_2"  ~ "Category 2",
           Variable == "Cat_3"  ~ "Category 3",
           Variable == "Cat_4"  ~ "Category 4",
           Variable == "Cat_5"  ~ "Category 5",
           
           # Catch-all to keep everything else exactly as it was:
           TRUE  ~ Variable 
         )) %>% 
  mutate(Variable = factor(Variable, levels = rev(c("Category 1","Category 2","Category 3","Category 4","Category 5" ))))

ggplot(plot_data, aes(x = Variable, y = Percent, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Public shelter"   = "#4682b4"
  )) +
  geom_text(aes(label = percent(Percent, accuracy = 1, scale=1)), 
            position = position_stack(vjust = 0.2),
            color = "white",
            size = 4) +
  labs(
    title = "Public Shelter Evacuation\nby Category of Storm",
    x = "Category of Storm",
    y = "Percent of Households",
    fill = NULL
  ) +
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  )+
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.position = "",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Y-axis title
    axis.text.x  = element_text(size = 13, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 13, color = "black")                         # Y-axis tick labels
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale=1))

# Q11 barriers to evacuation ----------------------------------------------
plot_data <- casper_2026 %>% 
  pivot_longer(cols = c(Barrier), names_to = "Category", values_to = "Response") %>% 
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() %>% 
  filter(Response != "Declined",
         Response != "Unsure",
         Response != "Other",
         Response != "No barriers (HH will evacuate)",
         Response != "No barriers (but HH would not evacuate)") %>% 
  mutate(Response = factor(Response, c("Other", "Health or mobility issues","Uncertainty about where to go","Concern about leaving property vacant","Evacuation routes blocked due to debris/flooding","Concern about leaving pet(s)","Traffic congestion")))+
  mutate()

plot_data <- casper_2026 %>% 
  pivot_longer(cols = c(Barrier), names_to = "Category", values_to = "Response") %>% 
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() %>% 
  filter(Response != "Declined",
         Response != "Unsure",
         Response != "Other",
         Response != "No barriers (HH will evacuate)",
         Response != "No barriers (but HH would not evacuate)") %>% 
  mutate(Response = factor(Response, levels = c("Other", "Health or mobility issues","Uncertainty about where to go","Concern about leaving property vacant","Evacuation routes blocked due to debris/flooding","Concern about leaving pet(s)","Traffic congestion"))) %>% # <-- Changed + to %>%
  mutate(
    Label_Text = case_when(
      Percent <= 5 ~ "",
      TRUE         ~ percent(Percent / 100, accuracy = 1) # Turns 12.3 into "12%"
    )
  )

table(casper_2026$Barrier)

ggplot(plot_data, aes(x = Response, y = Percent, fill = Response)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Concern about leaving pet(s)" = "#001B69",
    "Evacuation routes blocked due to debris/flooding"  = "#005A70",
    "Uncertainty about where to go"   = "#1476D1",
    "Health or mobility issues" = "lightblue",
    "Traffic congestion"            = "#29D7EC",
    "Concern about leaving property vacant" = "blue",
    "Other"           = "#a6a6a6"
  )) +
  
  geom_text(aes(label = Label_Text), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8) +
  labs(
    title = "Evacuation Barriers",
    x = "",
    y = "Percent of Households",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.position = "",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Y-axis title
    axis.text.x  = element_text(size = 13, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 13, color = "black")                         # Y-axis tick labels
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale=1))+
  scale_x_discrete(labels = scales::label_wrap(20))




# Q12 additional evac needs -----------------------------------------------
table(casper_import_raw$`40_Has_your_househol`)

# Q13 measles -------------------------------------------------------------------
plot_data <- srv_design %>%
  filter(!is.na(Measles_Aware)) %>%  
  group_by(Measles_Aware) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) # Compute weighted proportions and convert to percentages (0-100)

ggplot(plot_data, aes(x=Measles_Aware, y = Percent)) +
  geom_bar(stat = "identity", fill = "#4682B4", color = "#555555", width = 0.8) +
  # Use the automatically generated design-adjusted confidence intervals
  geom_errorbar(aes(ymin = Percent_low, ymax = Percent_upp), width = 0.1, color = "#444444") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20), expand = c(0, 0)) +
  labs(
    title = "Awareness of Measles Outbreaks",
    x = NULL,
    y = "Percent"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 10)),
    axis.text = element_text(size = 15, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )+
  scale_y_continuous(labels = percent_format(scale = 1))




# Q13 measles aware over time ---------------------------------------------

old_data<-read.xlsx("../Data/Measles_Aware_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year))

plot_data <- srv_design %>%
  group_by(Measles_Aware) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>%  #survey_mean calculates weighted percents
  # Ensure categories stay in the exact order: Own -> Rent -> Other
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(Measles_Aware == "Yes") #only keeping Yes


ggplot(plot_data, aes(x = factor(Year), y = Percent, fill = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "#666666") +
  scale_fill_manual(values = c(
    "2019" = "#FDA172",  
    "2024" = "#B2560D", 
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  )) +
  
  geom_text(aes(label = percent(Percent, accuracy = 1, scale=1)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8) +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  labs(
    title = "Households Aware of Measles\nOutbreak by Year",
    x = "",
    y = "Percent of Households",
    fill = "Year"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    legend.position = "none",
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    axis.title.x = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )+
  scale_y_continuous(labels = percent_format(scale = 1))


# measles concern ---------------------------------------------------------
old_data<-read.xlsx("../Data/Measles_Concern_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year))

plot_data <- ID_table %>% 
  filter(Variable == "Measles_Concern",
         Category != "Unsure",
         Category != "Declined") %>% 
  mutate(Year = 2026,
         Percent = Percent*100,
         Percent_low = Percent_low*100,
         Percent_upp = Percent_upp*100) %>% 
  bind_rows(old_data) %>% 
  filter(!Category %in% c("Declined","Unsure"))

ggplot(plot_data, aes(x = Category, y = Percent, fill = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "#666666") +
  coord_flip()+
  scale_fill_manual(values = c(
    "2017" = "peachpuff",
    "2019" = "#FDA172",  
    "2020" = "#ED7117", 
    "2022" = "#DD571C",
    "2023" = "darkorange3", 
    "2024" = "#B2560D", 
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  ))  +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  scale_x_discrete(labels = c(
    "Not concerned"   = "Not\nConcerned",
    "Somewhat concerned"  = "Somewhat\nConcerned",
    "Very concerned" = "Very\nConcerned"
  ))+
  labs(
    title = "Concern About Impact of\nMeasles on Kauaʻi",
    x = NULL,
    y = "Percent of Households",
    fill = "Year"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.title.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = percent_format(scale = 1))


# chickenpox aware  -----------------------------------------------
plot_data <- srv_design %>%
  filter(!is.na(Chickenpox_Aware)) %>%  
  group_by(Chickenpox_Aware) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) # Compute weighted proportions and convert to percentages (0-100)

ggplot(plot_data, aes(x=Chickenpox_Aware, y = Percent)) +
  geom_bar(stat = "identity", fill = "#4682B4", color = "#555555", width = 0.8) +
  # Use the automatically generated design-adjusted confidence intervals
  geom_errorbar(aes(ymin = Percent_low, ymax = Percent_upp), width = 0.1, color = "#444444") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20), expand = c(0, 0)) +
  labs(
    title = "Awareness of Kauaʻi\nChickenpox Outbreak",
    x = NULL,
    y = "Percent"
  ) +
  geom_text(aes(label = percent(Percent, accuracy = 1, scale=1)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 10)),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 10)),
    axis.text = element_text(size = 15, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )+
  scale_y_continuous(labels = percent_format(scale = 1))


# chickenpox concern ------------------------------------------------------
plot_data<-ID_table %>% 
  filter(Variable == "Chickenpox_Concern",
         Category != "Unsure",
         Category != "Declined") 


ggplot(plot_data, aes(x = Percent, y = Category, fill = Category))+
  geom_col()+
  scale_fill_manual(values = c(
    "Very concerned" = "#001B69",
    "Somewhat concerned"  = "#005A70",
    "Not concerned"   = "#1476D1"
  ))  +
  scale_y_discrete(labels = c(
    "Very concerned" = "Very\nConcerned",
    "Somewhat concerned"  = "Somewhat\nConcerned",
    "Not concerned"   = "Not\nConcerned"
  ))+
  labs(
    title = "Concern About Impact of\nChickenpox on Kauaʻi",
    x = "Percent of Households",
    y = NULL
    ) +
    geom_text(aes(label = percent(Percent, accuracy = 1)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8) +
  geom_errorbar(
    aes(xmin = Percent_low, xmax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  )+
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.title.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = ""
  )+
  scale_x_continuous(labels = percent_format())

# Q17 vaccines ---------------------------------------------------------------------
old_data<-read.xlsx("../Data/Vaccines_Important_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year))

plot_data <- srv_design %>%
  group_by(Vaccines_Important) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>%  #survey_mean calculates weighted percents
  # Ensure categories stay in the exact order: Own -> Rent -> Other
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(!Vaccines_Important %in% c("Declined","Unsure"))

write.xlsx(plot_data, file = "../Data/vax_imp_by_yr.xlsx")

ggplot(plot_data, aes(x = factor(Vaccines_Important, levels = c("Not important", "Somewhat important","Very important")), y = Percent, fill = factor(Year), group = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "#666666") +
  coord_flip()+
  scale_fill_manual(values = c(
    "2017" = "peachpuff",
    "2019" = "#FDA172",  
    "2020" = "#ED7117", 
    "2022" = "#DD571C",
    "2023" = "darkorange3", 
    "2024" = "#B2560D", 
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  ))  +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  scale_x_discrete(labels = c(
    "Not important"   = "Not\nImportant",
    "Somewhat important"  = "Somewhat\nImportant",
    "Very important" = "Very\nImportant"
  ))+
  labs(
    title = "Importance of Staying\nUp To Date on Vaccines",
    x = NULL,
    y = "Percent of Households",
    fill = "Year"
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.title.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = percent_format(scale = 1))



# Q18 mosquito concern ---------------------------------------------------------------------
plot_data<-ID_table %>% 
  filter(Variable == "Mosquito_Concern",
         Category != "Unsure")

ggplot(plot_data, aes(x = factor(Category,levels = c("Not concerned","Somewhat concerned","Very concerned")), y = Percent)) +
  geom_col(position = "stack", width = 0.7,fill = "#4682B4") +
  coord_flip() +                                     #makes it into horizonal bar chart
  geom_text(aes(label = custom_percent_formatter(Percent)), #adds percents to bars
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8) +
  labs(
    title = "Mosquito-Borne Disease\nConcern",
    x = "Concern",
    y = "Percent of Households",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
    legend.position = "bottom",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_blank(), 
    axis.text.x  = element_text(size = 15, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 15, color = "black")                         # Y-axis tick labels
  ) +
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp), 
    width = 0.15,        # Width of the horizontal caps on the error bars
    color = "#545454",   # Clean charcoal gray color
    linewidth = 0.8
  )+
  scale_y_continuous(
    labels = percent_format(accuracy = 1), 
    limits = c(0, .6), # 1.05 adds a little headroom above 100% for the error bars
    expand = c(0, 0)     # Removes the empty padding at the bottom of the bars
  )+
  scale_x_discrete(labels = label_wrap(10))



# Q19 mosquito breeding prevention ----------------------------------------
plot_data<-ID_table %>% 
  filter(Variable == "Mosquito_Breeding",
         !Category %in% c("Unsure","Declined")) %>% 
  mutate(Percent = round(Percent*100),
         Percent_low = round(Percent_low*100),
         Percent_upp = round(Percent_upp*100))


ggplot(plot_data, aes(x = Category, y = Percent, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizontal bar chart
  scale_fill_manual(values = c(
    "Daily" = "#001B69",
    "Monthly"  = "#005A70",
    "Weekly"   = "#1476D1",
    "A few times per year" = "blue",
    "Rarely"            = "#29D7EC",
    "Never"           = "#a6a6a6"
  )) +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  geom_text(aes(label = percent(Percent, accuracy = 1, scale=1)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8) +
  labs(
    title = "Frequency of Mosquito Breeding Prevention",
    x = "",
    y = "Percent of Households",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.position = "",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Y-axis title
    axis.text.x  = element_text(size = 13, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 13, color = "black")                         # Y-axis tick labels
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale=1))+
  scale_x_discrete(labels = scales::label_wrap(20),
                   limits = rev)






# Q20 specific doctor ---------------------------------------
plot_data<-gen_table %>% 
  filter(Variable == "Specific_Doctor",
         Category != "Unsure",
         Category != "Declined") %>% 
  mutate(Variable = case_when(
    Variable == "Specific_Doctor" ~ "Has a Specific\nDoctor",
  ))  %>% 
  mutate(Category = factor(Category, levels = c("All","Most","Some","None")))%>% 
  mutate(
    # Create a string label column based on the value
    Label_Text = case_when(
      Percent == 0 ~ "",                                
      Percent <= 5 ~ "",                             
      TRUE         ~ paste0(round(Percent, 0), "%") ))


ggplot(plot_data, aes(x = Category, y = Percent, fill = Category)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = c(
    "All" = "#001B69",
    "Most" = "blue",
    "Some" = "#005A70", 
    "None" = "#1476D1"
  )) +
  
  
  # cant do error bars for grouped categories

  labs(
    title = "Household Members with\n a Regular Source of Healthcare",
    x = NULL,
    y = "Percent of Households",
    fill = "Household Members"
  ) +
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  geom_text(aes(label = Label_Text), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 6) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = ""
  )+
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale=1))
    # breaks = seq(0, 1, by = 0.2),
    # expand = c(0, 0) # Flushes bars perfectly to the axis line
    # 


# seen provider in last year ----------------------------------------------
plot_data<-gen_table %>% 
  filter(Variable == "Provider_Within_Year",
         Category != "Unsure",
         Category != "Declined") %>% 
  mutate(Category = factor(Category, levels = c("All","Most","Some","None")))%>% 
  mutate(
    # Create a string label column based on the value
    Label_Text = case_when(
      Percent == 0 ~ "",                                
      Percent <= 5 ~ "",                             
      TRUE         ~ paste0(round(Percent, 0), "%") ))


ggplot(plot_data, aes(x = Category, y = Percent, fill = Category)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  scale_fill_manual(values = c(
    "All" = "#001B69",
    "Most" = "blue",
    "Some" = "#005A70", 
    "None" = "#1476D1"
  )) +
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  
  # cant do error bars for grouped categories
  
  labs(
    title = "Household Members who\nHave Seen Their Provider\nin the Past Year",
    x = NULL,
    y = "Percent of Households",
    fill = "Household Members"
  ) +
  geom_text(aes(label = Label_Text), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 6) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 25, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = ""
  )+
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale=1))



# Q23 a&b &24 Physical and Mental Health and connection -----------------------------------------
plot_data<-gen_table %>% 
  filter(Variable %in% c("Phys_Health","Ment_Health"),
         Category != "Unsure") %>% 
  mutate(Category = case_when(
    str_detect(Category, "1") ~ '1',
    str_detect(Category, "2") ~ '2',
    str_detect(Category, "3") ~ '3',
    str_detect(Category, "4") ~ '4',
    str_detect(Category, "5") ~ '5',
    TRUE ~ Category  # Default value if none of the above are found
  ),
  Variable = case_when(
    Variable == "Ment_Health" ~ "Mental\nHealth",
    Variable == "Phys_Health" ~ "Physical\nHealth",
  )) %>% 
  mutate(
    # Create a string label column based on the value
    Label_Text = case_when(
      Percent == 0 ~ "",                                
      Percent <= 5 ~ "",                             
      TRUE         ~ paste0(round(Percent, 0), "%") ), 
  Category = factor(Category, levels = c("5", "4", "3", "2", "1")
  ))


ggplot(plot_data, aes(x = Variable, y = Percent, fill = Category)) +
  geom_col(position = "fill", width = 0.6, color = "white", linewidth = 0.3,show.legend = TRUE) +
  coord_flip() +
  scale_fill_manual(values = c(
    "1" = "#001B69",
    "2"  = "#005A70",
    "3"   = "#1476D1",
    "4" = "blue",
    "5"  = "#29D7EC",
    "Other" = "lightblue",
    "Unsure"           = "#a6a6a6",
    "Declined"         = "#545454"
  ),
  labels = c(
    "1" = "1 - Very Poor",
    "2"  = "2 - Poor",
    "3"   = "3 - Fair",
    "4" = "4 - Good",
    "5"  = "5 Very Good"
  ),
  drop = FALSE
  ) +
  geom_text(
    aes(label = Label_Text), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 8) +
  
  # Format x-axis (which becomes the bottom horizontal axis after coord_flip)
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0) # Flushes bars perfectly to the axis line
  ) +
  
  labs(
    title = "Physical and Mental Health Ratings",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  
  # Theme modifications to match the clean image layout
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 30, margin = margin(b = 20, t=5)),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    
    plot.margin = margin(r=20, l=20),
    
    # Remove background grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Emphasize clean axis line rules
    axis.line.x = element_line(color = "black", linewidth = 0.6),
    axis.line.y = element_line(color = "black", linewidth = 0.6),
    axis.text.x = element_text(color = "black", size = 14, margin = margin(t = 5)),
    axis.text.y = element_text(color = "black", size = 14, face = "bold")
  )+
  guides(fill = guide_legend(reverse = TRUE))



# Q24 community connectedness ---------------------------------------------
plot_data<-gen_table %>% 
  filter(Variable == "Community_Connected",
         Category != "Unsure") %>% 
  mutate(Category = case_when(
    str_detect(Category, "1") ~ '1',
    str_detect(Category, "2") ~ '2',
    str_detect(Category, "3") ~ '3',
    str_detect(Category, "4") ~ '4',
    str_detect(Category, "5") ~ '5',
    TRUE ~ Category  # Default value if none of the above are found
  ),
  Variable = case_when(
    Variable == "Community_Connected" ~ "Community\nConnectedness"
  )) %>% 
  mutate(
    # Create a string label column based on the value
    Label_Text = case_when(
      Percent == 0 ~ "",                                
      Percent <= 5 ~ "",                             
      TRUE         ~ paste0(round(Percent, 0), "%") ), 
    Category = factor(Category, levels = c("5", "4", "3", "2", "1")))


ggplot(plot_data, aes(x = Variable, y = Percent, fill = Category)) +
  geom_col(position = "fill", width = 0.6, color = "white", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c(
    "1" = "#001B69",
    "2"  = "#005A70",
    "3"   = "#1476D1",
    "4" = "blue",
    "5"  = "#29D7EC",
    "Other" = "lightblue",
    "Unsure"           = "#a6a6a6",
    "Declined"         = "#545454"
  ),
  labels = c(
    "1" = "1 - Very Disconnected",
    "2"  = "2 - Somewhat Disconnected",
    "3"   = "3 - Neutral",
    "4" = "4 - Somewhat Connected",
    "5"  = "5 Very Connected"
  )) +
  geom_text(
    aes(label = Label_Text), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 8) +
  
  # Format x-axis (which becomes the bottom horizontal axis after coord_flip)
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0) # Flushes bars perfectly to the axis line
  ) +
  
  labs(
    title = "Community Connectedness\nRatings",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  
  # Theme modifications to match the clean image layout
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 30, margin = margin(b = 20, t=5)),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    
    plot.margin = margin(r=20, l=20),
    
    # Remove background grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Emphasize clean axis line rules
    axis.line.x = element_line(color = "black", linewidth = 0.6),
    axis.line.y = element_line(color = "black", linewidth = 0.6),
    axis.text.x = element_text(color = "black", size = 14, margin = margin(t = 5)),
    axis.text.y = element_text(color = "black", size = 14, face = "bold")
  )+
  guides(fill = guide_legend(reverse = TRUE))




# Q25 gather --------------------------------------------------------------
plot_data <- casper_2026 %>% 
  pivot_longer(cols = c(Often_Gather), names_to = "Category", values_to = "Response") %>% 
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() %>% 
  filter(Response != "Declined",
         Response != "Unsure") %>% 
  mutate(Response = factor(Response, levels = c("Daily","A few times per week","Weekly","A few times per month","Montly","Rarely")))


ggplot(plot_data, aes(x = Response, y = Percent, fill = Response)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Daily" = "#001B69",
    "A few times per week"  = "#005A70",
    "Weekly"   = "#1476D1",
    "A few times per month" = "blue",
    "Rarely"            = "#29D7EC",
    "Never"           = "#a6a6a6"
  ))  +
  geom_text(aes(label = percent(Percent, accuracy = 1, scale=1)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 8) +
  labs(
    title = "Frequency of Gathering with\nFriends, Family, or Neighbors",
    x = "",
    y = "Percent of Households",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.position = "",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Y-axis title
    axis.text.x  = element_text(size = 13, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 13, color = "black")                         # Y-axis tick labels
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale=1))+
  scale_x_discrete(labels = scales::label_wrap(20),
                   limits = rev)


# Q27 Concern about paying next months rent ---------------------------------------------------------------------

old_data<-read.xlsx("../Data/Rent_Concern_Combined.xlsx") %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year))

plot_data <- srv_design %>%
  group_by(Rent_Concern) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>%  #survey_mean calculates weighted percents
  # Ensure categories stay in the exact order: Own -> Rent -> Other
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(!Rent_Concern %in% c("Declined","Unsure"))



ggplot(plot_data, aes(x = factor(Rent_Concern, levels = c("Not concerned", "Somewhat concerned","Very concerned")), y = Percent, fill = factor(Year), group = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "#666666") +
  coord_flip()+
  scale_fill_manual(values = c(
    "2020" = "#ED7117", 
    "2022" = "#DD571C",
    "2023" = "darkorange3", 
    "2024" = "#B2560D", 
    "2025" = "#7A3803",
    "2026" = "#4682B4"   
  ),
  guide = guide_legend(nrow = 1))  +
  
  # 2. Add error bars. Match the dodge width of geom_col
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp),
    position = position_dodge(width = 0.85),
    width = 0.25,
    color = "#4D4D4D",
    size = 0.5
  ) +
  scale_x_discrete(labels = c(
    "Not concerned"   = "Not\nConcerned",
    "Somewhat concerned"  = "Somewhat\nConcerned",
    "Very concerned" = "Very\nConcerned"
  ))+
  labs(
    title = "Concern for Ability to\nPay Next Month's Rent/Mortgage",
    x = NULL,
    y = "Percent of Households",
    fill = "Year",
  ) +
  # Apply minimal style to match the image layout
  theme_minimal() +
  theme(
    # Center and bold the title
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, margin = margin(b = 20)),
    # Format axis text size and bold style
    axis.title.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.x = element_text(face = "bold", size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.y = element_text(face = "bold", size = 15, color = "black", margin = margin(r = 15)),
    # Keep only the horizontal major grid lines
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )+
  scale_y_continuous(labels = percent_format(scale = 1))

table(casper_2026$Food_Last)
table(casper_2026$Food_Support)















# Q28 food not last -------------------------------------------------------
plot_data<-gen_table %>% 
  filter(Variable == "Food_Last",
         !Category %in% c("Unsure","Declined","Never")) %>% 
  mutate(Category = factor(Category, c("Never","Rarely","Sometimes","Usually","Always"))) %>% 
  mutate(Percent_new = if_else(round(Percent) < 2, NA_real_, round(Percent)))


ggplot(plot_data, aes(x = Category, y = Percent, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Always" = "#001B69",
    "Usually"  = "#005A70",
    "Sometimes"   = "#1476D1",
    "Rarely" = "blue",
    "Never"            = "#29D7EC"
  )) +
  labs(
    title = "How Often Households Ran Out\nof Food in the Past 12 Months",
    x = "",
    y = "Percent of Households",
    fill = NULL
  ) +
  geom_text(
    aes(label = percent(Percent_new, scale=1)), 
    position = position_fill(vjust = 0.5), 
    color = "white", 
    size = 6) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.position = "",
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank(),  # Removes minor grid lines
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), # X-axis title
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Y-axis title
    axis.text.x  = element_text(size = 13, color = "black"),                        # X-axis tick labels
    axis.text.y  = element_text(size = 13, color = "black")                         # Y-axis tick labels
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1, scale=1))+
  scale_x_discrete(labels = scales::label_wrap(20))




# measles, chickenpox, mosquitos all three graph ------------------------------------------

plot_data <- ID_table %>%
  filter(Variable %in% c("Measles_Concern", "Chickenpox_Concern", "Mosquito_Concern")) %>% 
  mutate(Variable = case_when(
    Variable == "Measles_Concern"      ~ "Measles",
    Variable == "Chickenpox_Concern"  ~ "Chickenpox",
    Variable == "Mosquito_Concern" ~ "Mosquito-borne diseases",
    # Catch-all to keep everything else exactly as it was:
    TRUE                         ~ Variable 
  )) %>% 
  mutate(Category = factor(Category, levels = c("Declined","Unsure","Not concerned", "Somewhat concerned","Very concerned"))) %>% 
  filter(Category != "Declined",
         Category != "Unsure") %>% 
  mutate(Category = case_when(
    Category == "Not concerned" ~ "Not\nConcerned",
    Category == "Somewhat concerned" ~ "Somewhat\nConcerned",
    Category == "Very concerned" ~ "Very\nConcerned"
  ))


ggplot(plot_data, aes(x = Category, y = Percent, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  coord_flip()+
  scale_fill_manual(
    values = c(
      "Measles" = "#001B69",
      "Chickenpox"  = "#005A70",
      "Mosquito-borne diseases" = "blue"
    ),     
    breaks = c( "Mosquito-borne diseases","Measles","Chickenpox") 
  ) +
  labs(
    title = "Concern for Infectious Diseases",
    x = "",
    y = "Percent of Households",
    fill = NULL
  ) +
  geom_errorbar(
    aes(ymin = Percent_low, ymax = Percent_upp), 
    position = position_dodge(width = 0.7), 
    width = 0.1, 
    color = "#444444"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.position = "bottom", # Restored legend visibility if needed, change to "none" to hide entirely
    
    # --- REMOVE BACKGROUND AND GRID LINES ---
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    
    # --- INCREASE AXIS LABEL AND TEXT SIZES ---
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)), 
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)), 
    axis.text.x  = element_text(size = 13, color = "black"),                         
    axis.text.y  = element_text(size = 13, color = "black")                          
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.5)
  )
