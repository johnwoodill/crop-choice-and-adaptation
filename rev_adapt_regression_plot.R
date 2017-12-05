library(tidyverse)
library(ggthemes)

# modsixty5 <- readRDS("models/modsixty5.rds")
# modthirty5 <- readRDS("models/modthirty5.rds")
# modtwenty5 <- readRDS("models/modtwenty5.rds")
# modten5 <- readRDS("models/modten5.rds")
# modfive5 <- readRDS("models/modfive5.rds")


modfive5 <- readRDS("models/modfive_1.rds")
modten5 <- readRDS("models/modten_1.rds")
modtwenty5 <- readRDS("models/modtwenty_1.rds")
modthirty5 <- readRDS("models/modthirty_1.rds")
modsixty5 <- readRDS("models/modsixty_1.rds")

# Build data frame for plot

# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive5$coefficients[1:3], modten5$coefficients[1:3], modtwenty5$coefficients[1:3],
                             modthirty5$coefficients[1:3], modsixty5$coefficients[1:3]),
                  se = c(modfive5$cse[1:3], modten5$cse[1:3], modtwenty5$cse[1:3],
                             modthirty5$cse[1:3], modsixty5$cse[1:3]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive5$coefficients[6:8], modten5$coefficients[6:8], modtwenty5$coefficients[6:8],
                             modthirty5$coefficients[6:8], modsixty5$coefficients[1:3]),
                  se = c(modfive5$cse[6:8], modten5$cse[6:8], modtwenty5$cse[6:8],
                             modthirty5$cse[6:8], modsixty5$cse[1:3]),
                  effect = "Climate-effect")
pdat <- rbind(pdat1, pdat2)

rownames(pdat) <- NULL
pdat$interv <- factor(pdat$interv, levels = c("5-year", "10-year", "20-year", "30-year", "60-year"))

ggplot(pdat, aes(y = 100*coef, x = interv, color = var)) + 
  theme_tufte(base_size = 14) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(effect~var, labeller = label_wrap_gen(multi_line=TRUE)) + 
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA)) +
  ylab("% Change in Log Revenue per Acre") +
  xlab(NULL) 
  
  
