# 24_gen_viz.R

# General Visualizations

library(here)
library(tidyverse)

# Setup ========================================================================
gen_table <- readRDS(here("data/gen_table.rds"))
casper_2026 <- readRDS(here("data/casper_2026_clean.rds"))

# Have a Specific Doctor =======================================================
## Data ------------------------------------------------------------------------
plot_data <- gen_table %>% 
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

## Viz -------------------------------------------------------------------------
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

# Seen Provider This Year ======================================================
## Data ------------------------------------------------------------------------
plot_data <- gen_table %>% 
  filter(Variable == "Provider_Within_Year",
         Category != "Unsure",
         Category != "Declined") %>% 
  mutate(Category = factor(Category, levels = c("All","Most","Some","None"))) %>% 
  mutate(
    # Create a string label column based on the value
    Label_Text = case_when(
      Percent == 0 ~ "",                                
      Percent <= 5 ~ "",                             
      TRUE         ~ paste0(round(Percent, 0), "%")))

## Viz -------------------------------------------------------------------------
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

# Physical and Mental Health ===================================================
## Data ------------------------------------------------------------------------
plot_data <- gen_table %>% 
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

## Viz -------------------------------------------------------------------------
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

# Community Connectedness ======================================================
## Data ------------------------------------------------------------------------
plot_data <- gen_table %>% 
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

## Viz -------------------------------------------------------------------------
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

# Often Gather =================================================================
## Data ------------------------------------------------------------------------
plot_data <- casper_2026 %>% 
  pivot_longer(cols = c(Often_Gather), names_to = "Category", values_to = "Response") %>% 
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() %>% 
  filter(Response != "Declined",
         Response != "Unsure") %>% 
  mutate(Response = factor(Response, levels = c("Daily",
                                                "A few times per week",
                                                "Weekly",
                                                "A few times per month",
                                                "Montly",
                                                "Rarely")))

## Viz -------------------------------------------------------------------------
ggplot(plot_data, aes(x = Response, y = Percent, fill = Response)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +                                     #makes it into horizonal bar chart
  scale_fill_manual(values = c(
    "Daily"                 = "#001B69",
    "A few times per week"  = "#005A70",
    "Weekly"                = "#1476D1",
    "A few times per month" = "blue",
    "Rarely"                = "#29D7EC",
    "Never"                 = "#a6a6a6"
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

# Conern About Rent ============================================================
## Data ------------------------------------------------------------------------
old_data <- read.xlsx(here("raw_data/Rent_Concern_Combined.xlsx")) %>% 
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

## Viz -------------------------------------------------------------------------
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

# Food Not Lasting =============================================================
## Data ------------------------------------------------------------------------
plot_data <- gen_table %>% 
  filter(Variable == "Food_Last",
         !Category %in% c("Unsure", "Declined", "Never")) %>% 
  mutate(Category = factor(Category, c("Never", "Rarely", "Sometimes", "Usually", "Always"))) %>% 
  mutate(Percent_new = if_else(round(Percent) < 2, NA_real_, round(Percent)))

## Viz -------------------------------------------------------------------------
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
