library(tidyverse)
library(ggthemes)



# Load SUR models
modfive <- readRDS("models/sur_model_five.rds")
modten <- readRDS("models/sur_model_ten.rds")
modtwenty <- readRDS("models/sur_model_twenty.rds")
modthirty <- readRDS("models/sur_model_thirty.rds")
modsixty <- readRDS("models/sur_model_sixty.rds")

# Remove trend coefficients to make it easier to plot
modfive_coef <- modfive$coefficients[-c(grep("trend", names(modfive$coefficients)))]
modten_coef <- modten$coefficients[-c(grep("trend", names(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", names(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", names(modthirty$coefficients)))]
modsixty_coef <- modsixty$coefficients


length(modfive$coefficients)/5
nrow(modfive$bs.se)/5

# 62

# Corn
# Weather-effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[1:3], modten_coef[1:3], modtwenty_coef[1:3],
                             modthirty_coef[1:3], modsixty_coef[1:3]),
                  se = c(modfive$bs.se[1:3, 1], modten$bs.se[1:3, 1], modtwenty$bs.se[1:3, 1],
                             modthirty$bs.se[1:3, 1], modsixty$bs.se[1:3, 1]),
                  effect = "Weather-effect")

# Climate-effects
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[6:8], modten_coef[6:8], modtwenty_coef[6:8],
                             modthirty_coef[6:8], modsixty_coef[1:3]),
                  se = c(modfive$bs.se[6:8, 1], modten$bs.se[6:8, 1], modtwenty$bs.se[6:8, 1],
                             modthirty$bs.se[6:8, 1], modsixty$bs.se[1:3, 1]),
                  effect = "Climate-effect")
pdat <- rbind(pdat1, pdat2)

rownames(pdat) <- NULL
pdat$interv <- factor(pdat$interv, levels = c("5-year", "10-year", "20-year", "30-year", "60-year"))

ggplot(pdat, aes(y = 100*coef, x = interv)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(effect~var, labeller = label_wrap_gen(multi_line=TRUE)) + 
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("s.d. from mean (z-score) \n (Corn)") +
  xlab(NULL) 
ggsave("figures/corn_sur_reg_plot.pdf", width = 6, height = 4)


# Cotton
# Weather-effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[11:13], modten_coef[11:13], modtwenty_coef[11:13],
                             modthirty_coef[11:13], modsixty_coef[6:8]),
                  se = c(modfive$bs.se[11:13, 1], modten$bs.se[11:13, 1], modtwenty$bs.se[11:13, 1],
                             modthirty$bs.se[11:13, 1], modsixty$bs.se[6:8, 1]),
                  effect = "Weather-effect")

# Climate-effects
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[16:18], modten_coef[16:18], modtwenty_coef[16:18],
                             modthirty_coef[16:18], modsixty_coef[6:8]),
                  se = c(modfive$bs.se[16:18, 1], modten$bs.se[16:18, 1], modtwenty$bs.se[16:18, 1],
                             modthirty$bs.se[6:8, 1], modsixty$bs.se[6:8, 1]),
                  effect = "Climate-effect")
pdat <- rbind(pdat1, pdat2)

rownames(pdat) <- NULL
pdat$interv <- factor(pdat$interv, levels = c("5-year", "10-year", "20-year", "30-year", "60-year"))

ggplot(pdat, aes(y = 100*coef, x = interv)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(effect~var, labeller = label_wrap_gen(multi_line=TRUE)) + 
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("s.d. from mean (z-score) \n (Cotton)") +
  xlab(NULL) 
ggsave("figures/cotton_sur_reg_plot.pdf", width = 6, height = 4)


# Hay
# Weather-effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[21:23], modten_coef[21:23], modtwenty_coef[21:23],
                             modthirty_coef[21:23], modsixty_coef[11:13]),
                  se = c(modfive$bs.se[21:23, 1], modten$bs.se[21:23, 1], modtwenty$bs.se[21:23, 1],
                             modthirty$bs.se[21:23, 1], modsixty$bs.se[11:13, 1]),
                  effect = "Weather-effect")

# Climate-effects
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[26:28], modten_coef[26:28], modtwenty_coef[26:28],
                             modthirty_coef[26:28], modsixty_coef[11:13]),
                  se = c(modfive$bs.se[26:28, 1], modten$bs.se[26:28, 1], modtwenty$bs.se[26:28, 1],
                             modthirty$bs.se[26:28, 1], modsixty$bs.se[11:13, 1]),
                  effect = "Climate-effect")
pdat <- rbind(pdat1, pdat2)

rownames(pdat) <- NULL
pdat$interv <- factor(pdat$interv, levels = c("5-year", "10-year", "20-year", "30-year", "60-year"))

ggplot(pdat, aes(y = 100*coef, x = interv)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(effect~var, labeller = label_wrap_gen(multi_line=TRUE)) + 
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("s.d. from mean (z-score) \n (Hay)") +
  xlab(NULL) 
ggsave("figures/hay_sur_reg_plot.pdf", width = 6, height = 4)



# Soybean
# Weather-effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[31:33], modten_coef[31:33], modtwenty_coef[31:33],
                             modthirty_coef[31:33], modsixty_coef[16:18]),
                  se = c(modfive$bs.se[31:33, 1], modten$bs.se[31:33, 1], modtwenty$bs.se[31:33, 1],
                             modthirty$bs.se[31:33, 1], modsixty$bs.se[16:18, 1]),
                  effect = "Weather-effect")

# Climate-effects
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[36:38], modten_coef[36:38], modtwenty_coef[36:38],
                             modthirty_coef[36:38], modsixty_coef[16:18]),
                  se = c(modfive$bs.se[36:38, 1], modten$bs.se[36:38, 1], modtwenty$bs.se[36:38, 1],
                             modthirty$bs.se[36:38, 1], modsixty$bs.se[16:18, 1]),
                  effect = "Climate-effect")
pdat <- rbind(pdat1, pdat2)

rownames(pdat) <- NULL
pdat$interv <- factor(pdat$interv, levels = c("5-year", "10-year", "20-year", "30-year", "60-year"))

ggplot(pdat, aes(y = 100*coef, x = interv)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(effect~var, labeller = label_wrap_gen(multi_line=TRUE)) + 
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("s.d. from mean (z-score) \n (Soybean)") +
  xlab(NULL) 
ggsave("figures/soybean_sur_reg_plot.pdf", width = 6, height = 4)




# Wheat
# Weather-effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[41:43], modten_coef[41:43], modtwenty_coef[41:43],
                             modthirty_coef[41:43], modsixty_coef[21:23]),
                  se = c(modfive$bs.se[41:43, 1], modten$bs.se[41:43, 1], modtwenty$bs.se[41:43, 1],
                             modthirty$bs.se[41:43, 1], modsixty$bs.se[21:23, 1]),
                  effect = "Weather-effect")

# Climate-effects
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[46:48], modten_coef[46:48], modtwenty_coef[46:48],
                             modthirty_coef[46:48], modsixty_coef[21:23]),
                  se = c(modfive$bs.se[46:48, 1], modten$bs.se[46:48, 1], modtwenty$bs.se[46:48, 1],
                             modthirty$bs.se[46:48, 1], modsixty$bs.se[21:23, 1]),
                  effect = "Climate-effect")
pdat <- rbind(pdat1, pdat2)

rownames(pdat) <- NULL
pdat$interv <- factor(pdat$interv, levels = c("5-year", "10-year", "20-year", "30-year", "60-year"))

ggplot(pdat, aes(y = 100*coef, x = interv)) + 
  theme_tufte(base_size = 10) +
  geom_point() + 
  geom_errorbar(aes(ymin = 100*(coef - se*1.96), ymax = 100*(coef + se*1.96)), width = 0.1, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(effect~var, labeller = label_wrap_gen(multi_line=TRUE)) + 
  ylim(100*min(pdat$coef - pdat$se*1.96), 100*max(pdat$coef + pdat$se*1.96)) +
  theme(legend.position = "none",
        panel.border = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("s.d. from mean (z-score) \n (Wheat)") +
  xlab(NULL) 
ggsave("figures/wheat_sur_reg_plot.pdf", width = 6, height = 4)
