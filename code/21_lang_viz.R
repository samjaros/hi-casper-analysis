# 21_lang_viz.R

# Visualize language variables

library(here)
library(tidyverse)

# Import =======================================================================
srv_design <- readRDS(here("data/srv_design.rds"))
casper_2026 <- readRDS(here("data/casper_2026_clean.rds"))

# Primary languages ============================================================
# Data -------------------------------------------------------------------------
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

# Plot -------------------------------------------------------------------------
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

# Other Languages ==============================================================
# Data -------------------------------------------------------------------------
long_data <- casper_2026 %>%
  pivot_longer(cols = c(Ilocano, Tagalog, Japanese, Spanish, Pidgin, Hawaiian, 
                        Visayan, Thai, Mandarin, Korean), 
               names_to = "Category", values_to = "Response")

plot_data <- long_data %>%
  group_by(Category, Response) %>%
  summarize(weighted_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = 100 * weighted_n / sum(weighted_n)) %>%
  ungroup() 

# Plot -------------------------------------------------------------------------
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

# Export =======================================================================
# ggsave()
