library(tidyverse)
library(ggthemes)



# Regression Models
modten <- readRDS("models/rev_crop_modten.rds")
modtwenty <- readRDS("models/rev_crop_modtwenty.rds")
modthirty <- readRDS("models/rev_crop_modthirty.rds")

# Remove trend coefficients to make it easier to plot
modten_coef <- modten$coefficients[-c(grep("trend", rownames(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", rownames(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", rownames(modthirty$coefficients)))]

modten_cse <- modten$cse[-c(grep("trend", names(modten$cse)))]
modtwenty_cse <- modtwenty$cse[-c(grep("trend", names(modtwenty$cse)))]
modthirty_cse <- modthirty$cse[-c(grep("trend", names(modthirty$cse)))]

# Build data frame for plot
# Weather effects
pdat1 <- data.frame(var = rep(c("DD (0-10C)", "DD (10-30C)", "DD (30C)"), 3),
                    interval = rep(c("10-year", "11-year", "12-year"), each = 3),
                    effect = "Weather-effect",
                    coef = c(modten_coef[1:3], modtwenty_coef[1:3], modthirty_coef[1:3]),
                    cse = c(modten_cse[1:3], modtwenty_cse[1:3], modthirty_cse[1:3]))

pdat2 <- data.frame(var = rep(c("DD (0-10C)", "DD (10-30C)", "DD (30C)"), 3),
                    interval = rep(c("10-year", "11-year", "12-year"), each = 3),
                    effect = "Climate-effect",
                    coef = c(modten_coef[6:8], modtwenty_coef[6:8], modthirty_coef[6:8]),
                    cse = c(modten_cse[6:8], modtwenty_cse[6:8], modthirty_cse[6:8]))
rownames(pdat2) <- NULL

pdat <- rbind(pdat1, pdat2)

ggplot(pdat, aes(y = 100*coef, x = var)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - cse*1.96), ymax = 100*(coef + cse*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(effect~interval) +
  ylim(100*min(pdat$coef - pdat$cse*1.96), 100*max(pdat$coef + pdat$cse*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Log(Crop Rev. per Acre)") +
  xlab(NULL) 

ggsave("figures/aggregate_rev_reg_plot.pdf", width = 6, height = 4)

