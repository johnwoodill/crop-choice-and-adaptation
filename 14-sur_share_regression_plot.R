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

modten$cl_se <- modten$cl_se[-c(grep("trend", modten$cl_se$coef)), ]
modtwenty$cl_se <- modtwenty$cl_se[-c(grep("trend", modtwenty$cl_se$coef)), ]
modthirty$cl_se <- modthirty$cl_se[-c(grep("trend", modthirty$cl_se$coef)), ]


# Climate effects
pdat <- data.frame(var = rep(c("DD (0-10C) RM", "DD (10-30C) RM", "DD (30C) RM"), 15),
                    interval = rep(c("10-year", "11-year", "12-year"), each = 15),
                    crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), by = 5, each = 3),
                    coef = c(modten_coef[c(1:3, 6:8, 11:13, 16:18, 21:23)], 
                             modtwenty_coef[c(1:3, 6:8, 11:13, 16:18, 21:23)], 
                             modthirty_coef[c(1:3, 6:8, 11:13, 16:18, 21:23)]),
                    se = c(modten$bs_se[c(1:3, 6:8, 11:13, 16:18, 21:23), 1], 
                            modtwenty$bs_se[c(1:3, 6:8, 11:13, 16:18, 21:23),1 ], 
                            modthirty$bs_se[c(1:3, 6:8, 11:13, 16:18, 21:23), 1]))

pdat <- filter(pdat, interval == "10-year")
pdat$interval <- NULL
ggplot(pdat, aes(y = 100*coef, x = var)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(~crop, ncol = 3) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Z-score") +
  xlab(NULL) 

ggsave("figures/sur_share_reg_plot_10.pdf", width = 6, height = 4)

ggplot(filter(pdat, interval == "11-year"), aes(y = 100*coef, x = var)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(crop~interval, ncol = 3) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Z-score") +
  xlab(NULL) 

ggsave("figures/sur_share_reg_plot_11.pdf", width = 6, height = 4)

ggplot(filter(pdat, interval == "12-year"), aes(y = 100*coef, x = var)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(crop~interval, ncol = 3) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Z-score") +
  xlab(NULL) 

ggsave("figures/sur_share_reg_plot_12.pdf", width = 6, height = 4)
