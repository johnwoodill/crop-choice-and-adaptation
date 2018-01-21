library(tidyverse)
library(ggthemes)



# Regression Models
corn_mod <- readRDS("models/mod_corn.rds")
cotton_mod <- readRDS("models/mod_cotton.rds")
hay_mod <- readRDS("models/mod_hay.rds")
soybean_mod <- readRDS("models/mod_soybean.rds")
wheat_mod <- readRDS("models/mod_wheat.rds")

# Remove trend coefficients to make it easier to plot
corn_mod_coef <- corn_mod$coefficients[-c(grep("trend", rownames(corn_mod$coefficients)))]
cotton_mod_coef <- cotton_mod$coefficients[-c(grep("trend", rownames(cotton_mod$coefficients)))]
hay_mod_coef <- hay_mod$coefficients[-c(grep("trend", rownames(hay_mod$coefficients)))]
soybean_mod_coef <- soybean_mod$coefficients[-c(grep("trend", rownames(soybean_mod$coefficients)))]
wheat_mod_coef <- wheat_mod$coefficients[-c(grep("trend", rownames(wheat_mod$coefficients)))]

corn_mod_se <- corn_mod$cse[-c(grep("trend", names(corn_mod$cse)))]
cotton_mod_se <- cotton_mod$cse[-c(grep("trend", names(cotton_mod$cse)))]
hay_mod_se <- hay_mod$cse[-c(grep("trend", names(hay_mod$cse)))]
soybean_mod_se <- soybean_mod$cse[-c(grep("trend", names(soybean_mod$cse)))]
wheat_mod_se <- wheat_mod$cse[-c(grep("trend", names(wheat_mod$cse)))]

# Build data frame for plot
# Weather effects
pdat <- data.frame(var = rep(c("DD (0-10C)", "DD (10-30C)", "DD (30C)"), 5),
                    crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), each = 3),
                  coef = c(corn_mod_coef[1:3], cotton_mod_coef[1:3], hay_mod_coef[1:3], soybean_mod_coef[1:3],
                             wheat_mod_coef[1:3]),
                  se = c(corn_mod_se[1:3], cotton_mod_se[1:3], hay_mod_se[1:3], soybean_mod_se[1:3],
                             wheat_mod_se[1:3]))

ggplot(pdat, aes(y = 100*coef, x = var)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(effect~var, labeller = label_wrap_gen(multi_line=TRUE)) + 
  facet_wrap(~crop) +
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Log(Crop Rev. per Acre)") +
  xlab(NULL) 

ggsave("figures/crop_reg_plot.pdf", width = 6, height = 4)

