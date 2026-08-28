# 23_id_viz.R

# Infectious Disease Visualizations

library(here)
library(scales)
library(srvyr)
library(tidyverse)

# Import =======================================================================
ID_table <- readRDS(here("data/id_table.rds"))
srv_design <- readRDS(here("data/srv_design.rds"))

custom_percent_formatter <- function(x) {
  # Multiply by 100 to check the whole percentage scale
  pct_val <- x * 100
  
  ifelse(pct_val > 0 & pct_val < 1, 
         "<1%", 
         paste0(round(pct_val, 0), "%"))
}

# Measles ======================================================================
## Data -------------------------------------------------------------------------
plot_data <- srv_design %>%
  filter(!is.na(Measles_Aware)) %>%  
  group_by(Measles_Aware) %>%
  summarize(Percent = survey_mean(vartype = "ci") * 100) # Compute weighted proportions and convert to percentages (0-100)

## Viz --------------------------------------------------------------------------
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

# Measles Awareness by Time ====================================================
## Data -------------------------------------------------------------------------
old_data <- read.xlsx(here("raw_data/Measles_Aware_Combined.xlsx")) %>% 
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

## Viz --------------------------------------------------------------------------
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

# Concerned About Measles ======================================================
## Data -------------------------------------------------------------------------
old_data <- read.xlsx(here("raw_data/Measles_Concern_Combined.xlsx")) %>% 
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

## Viz --------------------------------------------------------------------------
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

# Chickenpox Awareness =========================================================
## Data -------------------------------------------------------------------------
plot_data <- srv_design %>%
  filter(!is.na(Chickenpox_Aware)) %>%  
  group_by(Chickenpox_Aware) %>%
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) # Compute weighted proportions and convert to percentages (0-100)

## Viz --------------------------------------------------------------------------
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

# Chickenpox Conern ============================================================
## Data ------------------------------------------------------------------------
plot_data <- ID_table %>% 
  filter(Variable == "Chickenpox_Concern",
         Category != "Unsure",
         Category != "Declined") 

## Viz -------------------------------------------------------------------------
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
  geom_text(aes(label = percent(Percent/100, accuracy = 1)), 
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
  scale_x_continuous(labels = label_percent(scale = 1))

# Vaccines =====================================================================
## Data ------------------------------------------------------------------------
old_data <- read.xlsx(here("raw_data/Vaccines_Important_Combined.xlsx")) %>% 
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

write.xlsx(plot_data, file = here("data/vax_imp_by_yr.xlsx"))

## Viz -------------------------------------------------------------------------
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

# 

# Mosquito Concern =============================================================
## Data ------------------------------------------------------------------------
plot_data <- ID_table %>% 
  filter(Variable == "Mosquito_Concern",
         Category != "Unsure")

## Viz -------------------------------------------------------------------------
ggplot(plot_data, 
       aes(x = factor(Category,
                      levels = c("Not concerned", "Somewhat concerned", "Very concerned")), 
           y = Percent)) +
  geom_col(position = "stack", width = 0.7,fill = "#4682B4") +
  coord_flip() +                                     #makes it into horizonal bar chart
  geom_text(aes(label = custom_percent_formatter(Percent/100)), #adds percents to bars
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
    labels = label_percent(accuracy = 1, scale = 1), 
    limits = c(0, 60), # 1.05 adds a little headroom above 100% for the error bars
    expand = c(0, 5)     # Removes the empty padding at the bottom of the bars
  )+
  scale_x_discrete(labels = label_wrap(10))

# Mosquito Breeding Prevention =================================================
## Data ------------------------------------------------------------------------
plot_data <- ID_table %>% 
  filter(Variable == "Mosquito_Breeding",
         !Category %in% c("Unsure","Declined")) %>% 
  mutate(Percent = round(Percent),
         Percent_low = round(Percent_low),
         Percent_upp = round(Percent_upp))

## Viz -------------------------------------------------------------------------
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
    labels = label_percent(accuracy = 1, scale=1))+
  scale_x_discrete(labels = scales::label_wrap(20),
                   limits = rev)

# Measles, Chickenpox, and Mosquitos ===========================================
## Data ------------------------------------------------------------------------
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
    labels = label_percent(accuracy = 1, scale =1),
    limits = c(0, 50)
  )
