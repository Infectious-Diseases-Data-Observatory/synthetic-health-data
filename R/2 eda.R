source("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/LB_syn_wide/prepprocessing.R")



# --- EDA --- #



# Synthpop compare() function
  # real vs synthetic distributions of variables
# Own comparison
  # line chart of real vs synthetic means of HGB over time for planned visit weeks
    # pivot longer to SDTM format
  # Box plot of real vs synthetic for planned visit 
    # 




LB_syn <- DMLB_syn %>% select(1, 9:14)
LB_real <- DMLB %>% select(1, 9:14)


# Pivot real longer
LB_real_long <- LB_real %>%
  pivot_longer(cols = starts_with("HGB_"), names_to = "LBDY", 
               names_prefix = "HGB_", values_to = "LBSTRESN") %>%
  mutate(LBDY = as.numeric(LBDY))

# Pivot synthetic longer
LB_syn_long <- LB_syn %>%
  pivot_longer(cols = starts_with("HGB_"), names_to = "LBDY", 
               names_prefix = "HGB_", values_to = "LBSTRESN") %>%
  mutate(LBDY = as.numeric(LBDY))


# Summary stats for real
summary_real <- LB_real_long %>%
  group_by(LBDY) %>%
  summarise(
    mean = mean(LBSTRESN, na.rm = TRUE),
    sd = sd(LBSTRESN, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se = sd / sqrt(n))

# Summary stats for synthetic
summary_syn <- LB_syn_long %>%
  group_by(LBDY) %>%
  summarise(
    mean = mean(LBSTRESN, na.rm = TRUE),
    sd = sd(LBSTRESN, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(se = sd / sqrt(n))


# PLOT line of means over time
plot_1 <- ggplot() +
  geom_line(data = summary_real, 
            aes(x = LBDY, y = mean, color = "Real"), linewidth = 1, group = 1) +
  geom_line(data = summary_syn, 
            aes(x = LBDY, y = mean, color = "Synthetic"), linewidth = 1, group = 1) +
  labs(x = "Day",
       y = "HGB (g/dL)",
       color = "Dataset") +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "#AEC6CF", "Synthetic" = "#FFB6C1")) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) 

ggsave(filename = "C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Images/means.png", 
       plot = plot_1, width = 6, height = 3)



# Combine real and synthetic
LB_combined <- bind_rows(
  LB_real_long %>% mutate(Dataset = "Real"),
  LB_syn_long %>% mutate(Dataset = "Synthetic")
)

# PLOT box plot comparing real and synthetic data for planned weeks only
plot_2 <- ggplot(LB_combined, aes(x = as.factor(LBDY), y = LBSTRESN, fill = Dataset)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  labs(x = "Day",
       y = "HGB (g/dL)",
       fill = "Dataset") +
  theme_minimal() +
  scale_fill_manual(values = c("Real" = "#AEC6CF", "Synthetic" = "#FFB6C1")) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)) 

ggsave(filename = "C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Images/boxplot.png", 
       plot = plot_2, width = 6, height = 4)
