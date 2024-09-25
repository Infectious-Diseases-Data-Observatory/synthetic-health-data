source("C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/LB_syn_wide/eda.R")



# --- MODELLING --- #



# - Linear Models - #


lm_real <- lm(LBSTRESN ~ LBDY, LB_real_long)
summary(lm_real)

lm_syn <- lm(LBSTRESN ~ LBDY, LB_syn_long)
summary(lm_syn)


# ACF
png(filename = "C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Images/lm_acf.png",
    width = 600, height = 300)
acf_real_grob_lm <- as.grob(function() acf(resid(lm_real), lag.max = 5,  main = "")) %>% grid.arrange()
dev.off()


# - Linear Mixed Models - #


lmm_int_real <- lmer(LBSTRESN ~ LBDY + (1 | USUBJID), data = LB_real_long, REML = FALSE)
summary(lmm_int_real)

lmm_int_syn <- lmer(LBSTRESN ~ LBDY + (1 | USUBJID), data = LB_syn_long, REML = FALSE)
summary(lmm_int_syn)


# Extract AIC, BIC for both models
data.frame(Model = c("Real", "Synthetic"),
             AIC = c(AIC(lmm_int_real), AIC(lmm_int_syn)))


# ACF
acf_real_grob <- as.grob(function() acf(resid(lmm_int_real), lag.max = 5, main = ""))
acf_syn_grob <- as.grob(function() acf(resid(lmm_int_syn), lag.max = 5, main = ""))

# QQ
qq_real_grob <- as.grob(function() {
  qqnorm(ranef(lmm_int_real)$USUBJID[,1], col = "#AEC6CF", main = "")
  qqline(ranef(lmm_int_real)$USUBJID[,1])
})

qq_syn_grob <- as.grob(function() {
  qqnorm(ranef(lmm_int_syn)$USUBJID[,1], col = "#FFB6C1", main = "")
  qqline(ranef(lmm_int_syn)$USUBJID[,1])
})


# Residuals plot
residuals_real <- resid(lmm_int_real)
fitted_real <- fitted(lmm_int_real)

residuals_syn <- resid(lmm_int_syn)
fitted_syn <- fitted(lmm_int_syn)


# Residuals
resid_real_plot <- ggplot(data.frame(Fitted = fitted_real, Residuals = residuals_real), 
                          aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#AEC6CF", alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.7) +
  labs(x = "Fitted values", y = "Residuals") +
  theme_minimal()

resid_syn_plot <- ggplot(data.frame(Fitted = fitted_syn, Residuals = residuals_syn), 
                         aes(x = Fitted, y = Residuals)) +
  geom_point(color = "#FFB6C1", alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.7) +
  labs(x = "Fitted values", y = "Residuals") +
  theme_minimal()


# Scale-location
residuals_real_std <- residuals_real / sd(residuals_real)
residuals_syn_std <- residuals_syn / sd(residuals_syn)

scale_real_plot <- ggplot(data.frame(Fitted = fitted_real, 
                                     Scaled_Residuals = sqrt(abs(residuals_real_std))), 
                          aes(x = Fitted, y = Scaled_Residuals)) +
  geom_point(color = "#AEC6CF", alpha = 0.2) +  # Pastel red color for points
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.7) +
  labs(x = "Fitted values", y = "√|Standardized Residuals|") +
  theme_minimal()

scale_syn_plot <- ggplot(data.frame(Fitted = fitted_syn, 
                                    Scaled_Residuals = sqrt(abs(residuals_syn_std))), 
                         aes(x = Fitted, y = Scaled_Residuals)) +
  geom_point(color = "#FFB6C1", alpha = 0.2) +  # Pastel red color for points
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.7) +
  labs(x = "Fitted values", y = "√|Standardized Residuals|") +
  theme_minimal()


# 2x2
png(filename = "C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Images/acf_qq_plot.png", 
    width = 1000, height = 600)
grid.arrange(acf_real_grob, qq_real_grob,
                                   acf_syn_grob, qq_syn_grob, 
                                   ncol = 2, nrow = 2)
dev.off()

resids_plot <- grid.arrange(resid_real_plot, scale_real_plot,
                                     resid_syn_plot, scale_syn_plot,
                                     ncol = 2, nrow = 2)
ggsave(filename = "C:/Users/bengr/OneDrive/Acadamic/IDDO/Code/Images/resids_plot.png", 
       plot = resids_plot, width = 10, height = 6)


