library(tidyverse)
library(ggthemes)



# Corn
modfive <- readRDS("models/modfive_corn.rds")
modten <- readRDS("models/modten_corn.rds")
modtwenty <- readRDS("models/modtwenty_corn.rds")
modthirty <- readRDS("models/modthirty_corn.rds")
modsixty <- readRDS("models/modsixty_corn.rds")

# Build data frame for plot

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
  ylab("Log(Corn Rev. per Acre)") +
  xlab(NULL) 
ggsave("figures/corn_reg_plot.pdf", width = 6, height = 4)



# Cotton
modfive <- readRDS("models/modfive_cotton.rds")
modten <- readRDS("models/modten_cotton.rds")
modtwenty <- readRDS("models/modtwenty_cotton.rds")
modthirty <- readRDS("models/modthirty_cotton.rds")
modsixty <- readRDS("models/modsixty_cotton.rds")

# Build data frame for plot

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
  ylab("Log(Cotton Rev. per Acre)") +
  xlab(NULL) 

ggsave("figures/cotton_reg_plot.pdf", width = 6, height = 4)

# Hay
modfive <- readRDS("models/modfive_hay.rds")
modten <- readRDS("models/modten_hay.rds")
modtwenty <- readRDS("models/modtwenty_hay.rds")
modthirty <- readRDS("models/modthirty_hay.rds")
modsixty <- readRDS("models/modsixty_hay.rds")

# Build data frame for plot

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
  ylab("Log(Hay Rev. per Acre)") +
  xlab(NULL) 

ggsave("figures/hay_reg_plot.pdf", width = 6, height = 4)


# soybean
modfive <- readRDS("models/modfive_soybean.rds")
modten <- readRDS("models/modten_soybean.rds")
modtwenty <- readRDS("models/modtwenty_soybean.rds")
modthirty <- readRDS("models/modthirty_soybean.rds")
modsixty <- readRDS("models/modsixty_soybean.rds")

# Build data frame for plot

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
  ylab("Log(Soybean Rev. per Acre)") +
  xlab(NULL) 

ggsave("figures/soybean_reg_plot.pdf", width = 6, height = 4)


# Wheat
modfive <- readRDS("models/modfive_wheat.rds")
modten <- readRDS("models/modten_wheat.rds")
modtwenty <- readRDS("models/modtwenty_wheat.rds")
modthirty <- readRDS("models/modthirty_wheat.rds")
modsixty <- readRDS("models/modsixty_wheat.rds")

# Build data frame for plot

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
  ylab("Log(Wheat Rev. per Acre)") +
  xlab(NULL) 

ggsave("figures/wheat_reg_plot.pdf", width = 6, height = 4)