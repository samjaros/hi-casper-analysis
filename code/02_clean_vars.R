# 2_clean_vars.R

# Factor categorical variables
# Create indicator variables

library(tidyverse)

# Import =======================================================================
casper_2026 <- readRDS("./data/casper_2026_raw.rds")

# Cleaning =====================================================================
# Clean variables ---------------------------------------------------------
# Substitute 0s with "No" for at least one member in each group
# Create a validation variable for if Total_Hh_Members = SUM(Age groups)
hurricane_vars <- c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5")   #Q10

casper_2026_clean <- casper_2026 %>%
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
    
    Cat5 = case_when(
      Cat5 == "Shelter in place in at home" ~ "Shelter in place at home",
      TRUE ~ Cat5),
    
    across(all_of(hurricane_vars), str_to_sentence))


table(casper_2026_clean$HHMember_match) #investigate mismatches
look <- casper_2026_clean %>%
  select(Cluster_Number, Survey_Number, Total_Hh_Members, Infants_2,
         Kids_2, Adults_2, Elders_2, HHMembers_by_age, HHMember_match) %>% 
  filter(HHMember_match == 'Mismatch')

# Substitute Language = Other with Language_Other 
casper_2026_clean <- casper_2026_clean %>% 
  mutate(Language_Other = str_to_title(Language_Other), #standardize response formatting
         Language_Combined = case_when(
           Language == "Other" & !is.na(Language_Other) ~ Language_Other,
           Language == "Other" & is.na(Language_Other) ~ "Other (unspecified)",
           TRUE ~ Language
         ))

table(casper_2026_clean$Language_Combined)
table(casper_2026_clean$Language_Other) #look at other languages and clean

# creating yes/no variable for each language
casper_2026_clean <- casper_2026_clean %>% 
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
casper_2026_clean <- casper_2026_clean %>%  
  mutate(Community_Connected_2 = case_when(
    str_detect(Community_Connected, "1") ~ 1,
    str_detect(Community_Connected, "2") ~ 2,
    str_detect(Community_Connected, "3") ~ 3,
    str_detect(Community_Connected, "4") ~ 4,
    str_detect(Community_Connected, "5") ~ 5,
    TRUE                                 ~ NA  # Default value if none of the above are found
  ))

table(casper_2026_clean$Community_Connected_2) #check to see if there are any 0s

likert_vars <- c("Phys_Health", "Ment_Health", "Community_Connected") #Q23a,b,24

casper_2026_clean <- casper_2026_clean %>%
  mutate(across(
    all_of(likert_vars),
    ~ case_when(
      .x %in% c("1", "2", "3", "4", "5") ~ as.numeric(.x),
      .x %in% c("Unsure", "Declined") ~ NA_real_
    ),
    .names = "{.col}_num"
  ))

# Factor Categorical -----------------------------------------------------------
# LIST ALL VARIABLES WITH YES/NO/UNSURE/DECLINED ANSWERS
yesno_vars <- c(#"Infants", "Kids", "Adults", "Elders",                         #Q1 - Not true, these are numeric answers
                "Insurance_Loss",                                               #Q3b 
                "Comm_Plan", "Meeting_Place", "Important_Docs",                 #Q4a,b,c
                "Med_Equip", "Backup_Power",                                    #Q5a,b
                "Tsunami_Zone_Familiar", "Tsunami_Zone_Info",                   #Q6a,b
                "Wildfires_Protection", "Receive_Alerts", "Kema_Familiar",      #Q7,Q8,Q9
                "Measles_Aware", "Chickenpox_Aware",                            #Q13,Q15
                "Skip_Med_Care",                                                #Q22
                "Food_Support", "Naloxone", "Concerns_Needs")                   #Q29,Q30,Q31

#this orders the values to make visualization easier
casper26_fmtd <- casper_2026_clean %>%
  mutate(across(all_of(yesno_vars), 
                ~factor(.x, levels = c("Yes", "No", "Unsure", "Declined"))))

# HURRICANE CATEGORIES
hurricane_vars <- c("Cat1", "Cat2", "Cat3", "Cat4", "Cat5")                     #Q10

casper26_fmtd <- casper26_fmtd %>%
  mutate(across(all_of(hurricane_vars), 
                ~factor(.x, levels = c("Shelter in place at home",
                                       "Friend/family's home",
                                       "Public shelter",
                                       "Workplace",
                                       "Other",
                                       "Unsure",
                                       "Declined"))))

#LIKERT
likert_vars <- c("Phys_Health", "Ment_Health", "Community_Connected")           #Q23a, 23b, 24

casper26_fmtd <- casper26_fmtd %>%
  mutate(across(all_of(likert_vars), 
                ~factor(.x, levels = c("1",
                                       "2",
                                       "3",
                                       "4",
                                       "5",
                                       "Unsure",
                                       "Declined"))))

#OTHERS
casper26_fmtd$Own_Rent = factor(                                                #Q3a
  casper26_fmtd$Own_Rent,
  levels = c("Own", "Rent","Other", "Unsure", "Declined"))
casper26_fmtd$Additional_Evac_Needs = factor(                                   #Q12
  casper26_fmtd$Additional_Evac_Needs,
  levels = c("Yes", "No", "Unsure", "Declined", "Not Applicable"))
casper26_fmtd$Measles_Concern = factor(                                         #Q14
  casper26_fmtd$Measles_Concern,
  levels = c("Very concerned", 
             "Somewhat concerned",
             "Not concerned",
             "Unsure",
             "Declined"))
casper26_fmtd$Chickenpox_Concern = factor(                                      #Q16
  casper26_fmtd$Chickenpox_Concern,
  levels = c("Very concerned",
             "Somewhat concerned",
             "Not concerned",
             "Unsure",
             "Declined"))
casper26_fmtd$Vaccines_Important = factor(                                      #Q17
  casper26_fmtd$Vaccines_Important,
  levels = c("Very important",
             "Somewhat important",
             "Not important",
             "Unsure",
             "Declined"))
casper26_fmtd$Mosquito_Concern = factor(                                        #Q18
  casper26_fmtd$Mosquito_Concern,
  levels = c("Very concerned",
             "Somewhat concerned",
             "Not concerned",
             "Unsure",
             "Declined"))
casper26_fmtd$Mosquito_Breeding = factor(                                       #Q19
  casper26_fmtd$Mosquito_Breeding,
  levels = c("Daily",
             "Weekly",
             "Monthly",
             "A few times per year",
             "Once a year",
             "Never",
             "Unsure",
             "Declined"))
casper26_fmtd$Specific_Doctor = factor(                                         #Q20
  casper26_fmtd$Specific_Doctor,
  levels = c("All",
             "Most",
             "Some",
             "None",
             "Unsure",
             "Declined"))
casper26_fmtd$Provider_Within_Year = factor(                                    #Q21
  casper26_fmtd$Provider_Within_Year,
  levels = c("All",
             "Most",
             "Some",
             "None",
             "Unsure",
             "Declined"))
casper26_fmtd$Often_Gather = factor(                                            #Q25
  casper26_fmtd$Often_Gather,
  levels = c("Daily",
             "A few times per week",
             "Weekly",
             "A few times per month",
             "Rarely",
             "Never",
             "Unsure",
             "Declined"))
casper26_fmtd$Often_Groups = factor(                                            #Q26
  casper26_fmtd$Often_Groups,
  levels = c("Daily",
             "A few times per week",
             "Weekly",
             "A few times per month",
             "Rarely",
             "Never",
             "Unsure",
             "Declined"))
casper26_fmtd$Rent_Concern = factor(                                            #Q27
  casper26_fmtd$Rent_Concern,
  levels = c("Very concerned",
             "Somewhat concerned",
             "Not concerned",
             "Unsure",
             "Declined"))
casper26_fmtd$Food_Last = factor(                                               #Q28
  casper26_fmtd$Food_Last,
  levels = c("Always",
             "Usually",
             "Sometimes",
             "Rarely",
             "Never",
             "Unsure",
             "Declined"))

# Indicator Variables ----------------------------------------------------------
# Keiki = HH with kids under 18
casper26_fmtd <- casper26_fmtd %>%
  mutate(
    keiki = if_else(
      Infants > 0 | Kids > 0,
      "Yes",
      "No",
      missing = NA_character_
    )
  )

table(casper26_fmtd$keiki, useNA="ifany")

# Output =======================================================================
saveRDS(casper26_fmtd, "./data/casper_2026_clean.rds")

