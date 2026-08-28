# 21_demog_viz.R

# Plots for demographics

library(here)
library(openxlsx)
library(scales)
library(tidyverse)

# Import =======================================================================
srv_design <- readRDS(here("data/srv_design.rds"))
demo_table <- readRDS(here("data/demo_table.rds"))

# Color schemes:
blue_colors <- c("#001B69", "#1476D1", "#29D7EC", "#4682B4", "#005A70","blue","lightblue","darkblue","#6baed6", "#4292c6", "#545454", "#a6a6a6")

custom_percent_formatter <- function(x) {
  # Multiply by 100 to check the whole percentage scale
  pct_val <- x * 100
  
  ifelse(pct_val > 0 & pct_val < 1, 
         "<1%", 
         paste0(round(pct_val, 0), "%"))
}

# Age Bars =====================================================================
# Data -------------------------------------------------------------------------
plot_data <- demo_table %>% 
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

# Plot -------------------------------------------------------------------------
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

# Own vs Rent by Year ==========================================================
# Data -------------------------------------------------------------------------

#prior years data
old_data <- read.xlsx(here("raw_data/Own_Rent_Table_Combined.xlsx")) %>% 
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

# Plot -------------------------------------------------------------------------
ggplot(plot_data, 
       aes(x = factor(Own_Rent, levels = c("Own", "Rent", "Other")),
           y = Percent, fill = factor(Year), group = factor(Year))) +
  geom_col(position = position_dodge(width = 0.85),
           width = 0.8,
           color = "#666666") +
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
    linewidth = 0.5
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

# Homeowner Insurance Loss =====================================================
# Data -------------------------------------------------------------------------
old_data <- read.xlsx(here("raw_data/Insurance_Loss_Combined.xlsx")) %>% 
  mutate(Percent = as.numeric(Percent),
         Percent_low = as.numeric(Percent_low),
         Percent_upp = as.numeric(Percent_upp),
         Year = as.numeric(Year),
         Variable = 'Insurance_Loss') %>% 
  rename(Category = Insurance_Loss)

table(casper_2026$Insurance_Loss, useNA = "ifany")

plot_data <- demo_table %>% 
  filter(Variable == "Insurance_Loss") %>% 
  mutate(Year = 2026) %>% 
  bind_rows(old_data) %>% 
  filter(Category %in% c("Yes","No","Unsure"))

# Plot -------------------------------------------------------------------------
ggplot(plot_data,
       aes(x = factor(Category,levels=c("Yes","No","Unsure")),
           y = Percent, fill = factor(Year), group = factor(Year))) +
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
  ) +
  scale_y_continuous(labels = percent_format(scale = 1))

# Data -------------------------------------------------------------------------
plot_data <- srv_design %>%
  filter(!is.na(Insurance_Loss)) %>%  
  group_by(Insurance_Loss) %>%
  # Compute weighted proportions and convert to percentages (0-100)
  summarize(Percent = survey_mean(vartype = "ci", na.rm = TRUE) * 100) %>% 
  mutate(Insurance_Loss = factor(Insurance_Loss, levels = c("Yes", "No", "Unsure", "Declined"))) %>% 
  filter(Insurance_Loss %in% c("Yes","No"))

# Plot -------------------------------------------------------------------------
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
    panel.border = element_rect(color = NA, fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "#888888")
  )

# Export =======================================================================
# ggsave()
