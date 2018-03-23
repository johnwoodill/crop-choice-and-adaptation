library(tidyverse)
library(ggthemes)

# Load SUR models
rev_sur <- readRDS("models/sur_rev_model.rds")

# Remove trend coefficients to make it easier to plot
rev_sur$coefficients <- rev_sur$coefficients[-c(grep("trend", names(rev_sur$coefficients)))]


# Weather-effects
pdat <- data.frame(var = rep(c("DD (0-10C)", "DD (10-30C)", "DD (30C)"), 5),
                    crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), each = 3),
                    coef = c(rev_sur$coefficients[c(1:3, 6:8, 11:13, 16:18, 21:23)]),
                    se = rev_sur$bs.se[c(1:3, 6:8, 11:13, 16:18, 21:23), 1])


ggplot(pdat, aes(y = 100*coef, x = var)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  facet_wrap(~crop) + 
  # ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Log(Revenue per acre)") +
  xlab(NULL) 
ggsave("figures/sur_reg_plot.pdf", width = 6, height = 4)

