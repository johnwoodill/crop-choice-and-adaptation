library(tidyverse)
library(ggthemes)



# Load SUR models
modfive <- readRDS("models/sur_model_five.rds")
modten <- readRDS("models/sur_model_ten.rds")
modtwenty <- readRDS("models/sur_model_twenty.rds")
modthirty <- readRDS("models/sur_model_thirty.rds")
modsixty <- readRDS("models/sur_model_sixty.rds")


length(modfive$coefficients)/5
# 62

# Corn
# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive$coefficients[1:3], modten$coefficients[1:3], modtwenty$coefficients[1:3],
                             modthirty$coefficients[1:3], modsixty$coefficients[1:3]),
                  se = c(modfive$cse[1:3], modten$cse[1:3], modtwenty$cse[1:3],
                             modthirty$cse[1:3], modsixty$cse[1:3]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive$coefficients[6:8], modten$coefficients[6:8], modtwenty$coefficients[6:8],
                             modthirty$coefficients[6:8], modsixty$coefficients[1:3]),
                  se = c(modfive$cse[6:8], modten$cse[6:8], modtwenty$cse[6:8],
                             modthirty$cse[6:8], modsixty$cse[1:3]),
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
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Corn Z-score") +
  xlab(NULL) 
ggsave("figures/corn_sur_plot.pdf", width = 6, height = 4)




# Cotton
# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive$coefficients[63:65], modten$coefficients[63:65], modtwenty$coefficients[63:65],
                             modthirty$coefficients[63:65], modsixty$coefficients[6:8]),
                  se = c(modfive$cse[63:65], modten$cse[63:65], modtwenty$cse[63:65],
                             modthirty$cse[63:65], modsixty$cse[6:8]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive$coefficients[68:70], modten$coefficients[68:70], modtwenty$coefficients[68:70],
                             modthirty$coefficients[68:70], modsixty$coefficients[6:8]),
                  se = c(modfive$cse[68:70], modten$cse[68:70], modtwenty$cse[68:70],
                             modthirty$cse[68:70], modsixty$cse[6:8]),
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
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Corn Z-score") +
  xlab(NULL) 
ggsave("figures/corn_sur_plot.pdf", width = 6, height = 4)

