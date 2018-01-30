library(tidyverse)
library(ggthemes)




# Regression Models
modten <- readRDS("models/sur_share_model_ten.rds")
modtwenty <- readRDS("models/sur_share_model_twenty.rds")
modthirty <- readRDS("models/sur_share_model_thirty.rds")

# Remove trend coefficients to make it easier to plot
modten_coef <- modten$coefficients[-c(grep("trend", names(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", names(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", names(modthirty$coefficients)))]


# Climate effects
pdat <- data.frame(var = rep(c("DD (0-10C)", "DD (10-30C)", "DD (30C)"), 15),
                    interval = rep(c("10-year", "20-year", "30-year"), each = 15),
                    crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), by = 5, each = 3),
                    coef = c(modten_coef[c(1:3, 6:8, 11:13, 16:18, 21:23)], 
                             modtwenty_coef[c(1:3, 6:8, 11:13, 16:18, 21:23)], 
                             modthirty_coef[c(1:3, 6:8, 11:13, 16:18, 21:23)]),
                    se = rep(1, 45))
                    ,
                    se = c(modten$bs.se[c(1:3, 6:8, 11:13, 16:18, 21:23)], 
                            modtwenty$bs.se[c(1:3, 6:8, 11:13, 16:18, 21:23)], 
                            modthirty$bs.se[c(1:3, 6:8, 11:13, 16:18, 21:23)]))

ggplot(pdat, aes(y = 100*coef, x = var)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(crop~interval, ncol = 3) +
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Log(Crop Rev. per Acre)") +
  xlab(NULL) 

ggsave("figures/sur_share_reg_plot.pdf", width = 6, height = 4)

