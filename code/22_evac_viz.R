# 22_evac_viz.R

# Emergency/evacuation visualizations

library(here)
library(openxlsx)
library(patchwork)
library(scales)
library(srvyr)
library(tidyverse)

# Import =======================================================================
casper_2026 <- readRDS(here("data/casper_2026_clean.rds"))
evac_table <- readRDS(here("data/evac_table.rds"))
srv_design <- readRDS(here("data/srv_design.rds"))

custom_percent_formatter <- function(x) {
  # Multiply by 100 to check the whole percentage scale
  pct_val <- x * 100
  
  ifelse(pct_val > 0 & pct_val < 1, 
         "<1%", 
         paste0(round(pct_val, 0), "%"))
}

# Emergency Planning ===========================================================
# Data -------------------------------------------------------------------------
old_data_1 <- read.xlsx(here("raw_data/Communication_Plan_Table_Combined.xlsx")) %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year),
         Category = "Comm_Plan") %>% 
  rename(Response = Comm_Plan)
old_data_2 <- read.xlsx(here("raw_data/Meeting_Place_Table_Combined.xlsx")) %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year),
         Category = "Meeting_Place") %>% 
  rename(Response = Meeting_Place)
old_data_3<-read.xlsx(here("raw_data/Important_Documents_Table_Combined.xlsx")) %>% 
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

# Viz --------------------------------------------------------------------------
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

# Electricity Dependence =======================================================
# Data -------------------------------------------------------------------------
old_data <- read.xlsx(here("raw_data/Electricity_Dependent_MedEquip_Combined.xlsx")) %>% 
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

# Viz --------------------------------------------------------------------------
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


# Health Needs & Backup Electricity ============================================
# Data -------------------------------------------------------------------------

# Main survey question data
data_main <- evac_table %>% 
  filter(Variable == "Med_Equip") %>% 
  mutate(Category = factor(c("Yes", "No", "Unsure", "Declined")))

# Follow-up question data
data_sub <- evac_table %>% 
  filter(Variable == "Backup_Power") 

# Viz --------------------------------------------------------------------------

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

# Tsunami Zone =================================================================
# Data -------------------------------------------------------------------------
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

# Viz --------------------------------------------------------------------------
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


# 
# Familiar with Alerts =========================================================
# Data -------------------------------------------------------------------------
long_data <- casper_2026 %>%
  pivot_longer(cols = c(Wildfires_Protection,
                        Receive_Alerts,
                        Kema_Familiar),
               names_to = "Category",
               values_to = "Response")

plot_data <- long_data %>%
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() %>% 
  mutate(Category = case_when(
    Category == "Wildfires_Protection" ~ "Wildfire Protection\nMeasures",
    Category == "Receive_Alerts"       ~ "Receives Emergencyd\nAlerts",
    Category == "Kema_Familiar"        ~ "Familiar with\nKEMA",
    # Catch-all to keep everything else exactly as it was:
    TRUE                               ~ Category 
  )) %>% 
  mutate(Response = factor(Response, levels = c("Declined", "Unsure", "No", "Yes")))

# Viz --------------------------------------------------------------------------
ggplot(plot_data, aes(x = Category, y = Percent, fill = Response)) +
  geom_col(position = position_stack(reverse = T), width = 0.7) +
  scale_fill_manual(values = c(
    "Yes" = "#001B69",
    "No"  = "#005A70",
    "Unsure" = "#a6a6a6",
    "Declined" = "#545454"),     
    breaks = c("Yes", "No", "Unsure", "Declined")
  ) +
  labs(
    title = "Preparedness",
    x = "Preparedness Measure",
    y = "Percent of Households",
    fill = NULL
  ) +
  geom_text(aes(label = custom_percent_formatter(Percent/100)), 
            position = position_stack(vjust = 0.5, reverse = T),
            color = "white",
            size = 4) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

# KEMA Over Time ===============================================================
# Data -------------------------------------------------------------------------
old_data <- read.xlsx(here("raw_data/KEMA_Familiar_Combined.xlsx")) %>% 
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

# Viz --------------------------------------------------------------------------
ggplot(plot_data, aes(x = factor(Year), y = Percent, fill = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85), 
           width = 0.8, color = "#666666", size = 0.3) +
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

# Hurricanes ===================================================================
# Data -------------------------------------------------------------------------
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
  mutate(Variable = factor(Variable,
                           levels = rev(c("Category 1", "Category 2", 
                                          "Category 3", "Category 4",
                                          "Category 5" ))),
         Category = factor(Category,
                           levels = rev(c("Shelter in place at home",
                                          "Friend/family's home",
                                          "Public shelter",
                                          "Workplace", "Other", 
                                          "Unsure", "Declined")))) %>% 
  mutate(
    # Create a string label column based on the value
    Label_Text = case_when(
      Percent == 0 ~ "",                                
      Percent <= 5 ~ "",                             
      TRUE         ~ paste0(round(Percent, 0), "%") ))

# Viz --------------------------------------------------------------------------
ggplot(plot_data, aes(x = Variable, y = Percent, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Shelter in place at home" = "#001B69",
    "Friend/family's home"     = "#005A70",
    "Public shelter"           = "#1476D1",
    "Workplace"                = "blue",
    "Other"                    = "#29D7EC",
    "Unsure"                   = "#a6a6a6",
    "Declined"                 = "#545454"
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


# Hurricane Shelter by Cat =====================================================
# Data -------------------------------------------------------------------------
plot_data <- evac_table %>% 
  filter(Category == "Public shelter")%>% 
  mutate(Category = str_to_sentence(Category),
         Variable = case_when(
           Variable == "Cat1"  ~ "Category 1",
           Variable == "Cat2"  ~ "Category 2",
           Variable == "Cat3"  ~ "Category 3",
           Variable == "Cat4"  ~ "Category 4",
           Variable == "Cat5"  ~ "Category 5",
           
           # Catch-all to keep everything else exactly as it was:
           TRUE  ~ Variable 
         )) %>% 
  mutate(Variable = factor(Variable, 
                           levels = rev(c("Category 1", "Category 2",
                                          "Category 3", "Category 4",
                                          "Category 5" ))),
         Label_text = paste0(round(Percent), "%"))

# Viz --------------------------------------------------------------------------
ggplot(plot_data, aes(x = Variable, y = Percent, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Public shelter"   = "#4682b4"
  )) +
  geom_text(aes(label = Label_text), 
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
    linewidth = 0.5
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
    labels = label_percent(accuracy = 1, scale=1))


# Barriers to Evacuation =======================================================
# Data -------------------------------------------------------------------------
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
  mutate(Response = factor(Response, 
                           c("Other",
                             "Health or mobility issues",
                             "Uncertainty about where to go",
                             "Concern about leaving property vacant",
                             "Evacuation routes blocked due to debris/flooding",
                             "Concern about leaving pet(s)",
                             "Traffic congestion")))

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
  mutate(Response = factor(Response, 
                           levels = c("Other",
                                      "Health or mobility issues",
                                      "Uncertainty about where to go",
                                      "Concern about leaving property vacant",
                                      "Evacuation routes blocked due to debris/flooding",
                                      "Concern about leaving pet(s)",
                                      "Traffic congestion"))) %>% # <-- Changed + to %>%
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

# Additional Evacuation Needs ==================================================
table(casper_2026$Additional_Evac_Needs)
