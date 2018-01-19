library(tidyverse)
library(ggthemes)



# Corn
modten <- readRDS("models/mod_corn.rds")

# Remove trend coefficients to make it easier to plot
modfive_coef <- modfive$coefficients[-c(grep("trend", rownames(modfive$coefficients)))]
modten_coef <- modten$coefficients[-c(grep("trend", rownames(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", rownames(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", rownames(modthirty$coefficients)))]
modsixty_coef <- modsixty$coefficients

modfive_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modten_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modtwenty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modthirty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modsixty_se <- modsixty$cse

# Build data frame for plot

# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[1:3], modten_coef[1:3], modtwenty_coef[1:3],
                             modthirty_coef[1:3], modsixty_coef[1:3]),
                  se = c(modfive_se[1:3], modten_se[1:3], modtwenty_se[1:3],
                             modthirty_se[1:3], modsixty_se[1:3]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[6:8], modten_coef[6:8], modtwenty_coef[6:8],
                             modthirty_coef[6:8], modsixty_coef[1:3]),
                  se = c(modfive_se[6:8], modten_se[6:8], modtwenty_se[6:8],
                             modthirty_se[6:8], modsixty_se[1:3]),
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
  ylab("Log(Corn Rev. per Acre)") +
  xlab(NULL) 
ggsave("figures/corn_reg_plot.pdf", width = 6, height = 4)


# Cotton
modfive <- readRDS("models/modfive_cotton.rds")
modten <- readRDS("models/modten_cotton.rds")
modtwenty <- readRDS("models/modtwenty_cotton.rds")
modthirty <- readRDS("models/modthirty_cotton.rds")
modsixty <- readRDS("models/modsixty_cotton.rds")

# Remove trend coefficients to make it easier to plot
modfive_coef <- modfive$coefficients[-c(grep("trend", rownames(modfive$coefficients)))]
modten_coef <- modten$coefficients[-c(grep("trend", rownames(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", rownames(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", rownames(modthirty$coefficients)))]
modsixty_coef <- modsixty$coefficients

modfive_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modten_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modtwenty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modthirty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modsixty_se <- modsixty$cse

# Build data frame for plot

# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[1:3], modten_coef[1:3], modtwenty_coef[1:3],
                             modthirty_coef[1:3], modsixty_coef[1:3]),
                  se = c(modfive_se[1:3], modten_se[1:3], modtwenty_se[1:3],
                             modthirty_se[1:3], modsixty_se[1:3]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[6:8], modten_coef[6:8], modtwenty_coef[6:8],
                             modthirty_coef[6:8], modsixty_coef[1:3]),
                  se = c(modfive_se[6:8], modten_se[6:8], modtwenty_se[6:8],
                             modthirty_se[6:8], modsixty_se[1:3]),
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
  ylab("Log(Cotton Rev. per Acre)") +
  xlab(NULL) 
ggsave("figures/cotton_reg_plot.pdf", width = 6, height = 4)


# Hay
modfive <- readRDS("models/modfive_hay.rds")
modten <- readRDS("models/modten_hay.rds")
modtwenty <- readRDS("models/modtwenty_hay.rds")
modthirty <- readRDS("models/modthirty_hay.rds")
modsixty <- readRDS("models/modsixty_hay.rds")

# Remove trend coefficients to make it easier to plot
modfive_coef <- modfive$coefficients[-c(grep("trend", rownames(modfive$coefficients)))]
modten_coef <- modten$coefficients[-c(grep("trend", rownames(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", rownames(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", rownames(modthirty$coefficients)))]
modsixty_coef <- modsixty$coefficients

modfive_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modten_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modtwenty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modthirty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modsixty_se <- modsixty$cse

# Build data frame for plot

# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[1:3], modten_coef[1:3], modtwenty_coef[1:3],
                             modthirty_coef[1:3], modsixty_coef[1:3]),
                  se = c(modfive_se[1:3], modten_se[1:3], modtwenty_se[1:3],
                             modthirty_se[1:3], modsixty_se[1:3]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[6:8], modten_coef[6:8], modtwenty_coef[6:8],
                             modthirty_coef[6:8], modsixty_coef[1:3]),
                  se = c(modfive_se[6:8], modten_se[6:8], modtwenty_se[6:8],
                             modthirty_se[6:8], modsixty_se[1:3]),
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
  ylab("Log(Hay Rev. per Acre)") +
  xlab(NULL) 
ggsave("figures/hay_reg_plot.pdf", width = 6, height = 4)




# Soybean
modfive <- readRDS("models/modfive_soybean.rds")
modten <- readRDS("models/modten_soybean.rds")
modtwenty <- readRDS("models/modtwenty_soybean.rds")
modthirty <- readRDS("models/modthirty_soybean.rds")
modsixty <- readRDS("models/modsixty_soybean.rds")

# Remove trend coefficients to make it easier to plot
modfive_coef <- modfive$coefficients[-c(grep("trend", rownames(modfive$coefficients)))]
modten_coef <- modten$coefficients[-c(grep("trend", rownames(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", rownames(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", rownames(modthirty$coefficients)))]
modsixty_coef <- modsixty$coefficients

modfive_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modten_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modtwenty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modthirty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modsixty_se <- modsixty$cse

# Build data frame for plot

# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[1:3], modten_coef[1:3], modtwenty_coef[1:3],
                             modthirty_coef[1:3], modsixty_coef[1:3]),
                  se = c(modfive_se[1:3], modten_se[1:3], modtwenty_se[1:3],
                             modthirty_se[1:3], modsixty_se[1:3]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[6:8], modten_coef[6:8], modtwenty_coef[6:8],
                             modthirty_coef[6:8], modsixty_coef[1:3]),
                  se = c(modfive_se[6:8], modten_se[6:8], modtwenty_se[6:8],
                             modthirty_se[6:8], modsixty_se[1:3]),
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
  ylab("Log(Soybean Rev. per Acre)") +
  xlab(NULL) 
ggsave("figures/soybean_reg_plot.pdf", width = 6, height = 4)





# Wheat
modfive <- readRDS("models/modfive_wheat.rds")
modten <- readRDS("models/modten_wheat.rds")
modtwenty <- readRDS("models/modtwenty_wheat.rds")
modthirty <- readRDS("models/modthirty_wheat.rds")
modsixty <- readRDS("models/modsixty_wheat.rds")

# Remove trend coefficients to make it easier to plot
modfive_coef <- modfive$coefficients[-c(grep("trend", rownames(modfive$coefficients)))]
modten_coef <- modten$coefficients[-c(grep("trend", rownames(modten$coefficients)))]
modtwenty_coef <- modtwenty$coefficients[-c(grep("trend", rownames(modtwenty$coefficients)))]
modthirty_coef <- modthirty$coefficients[-c(grep("trend", rownames(modthirty$coefficients)))]
modsixty_coef <- modsixty$coefficients

modfive_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modten_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modtwenty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modthirty_se <- modfive$cse[-c(grep("trend", names(modfive$cse)))]
modsixty_se <- modsixty$cse

# Build data frame for plot

# Weather effects
pdat1 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[1:3], modten_coef[1:3], modtwenty_coef[1:3],
                             modthirty_coef[1:3], modsixty_coef[1:3]),
                  se = c(modfive_se[1:3], modten_se[1:3], modtwenty_se[1:3],
                             modthirty_se[1:3], modsixty_se[1:3]),
                  effect = "Weather-effect")
pdat2 <- data.frame(var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 5),
                    interv = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = 3),
                  coef = c(modfive_coef[6:8], modten_coef[6:8], modtwenty_coef[6:8],
                             modthirty_coef[6:8], modsixty_coef[1:3]),
                  se = c(modfive_se[6:8], modten_se[6:8], modtwenty_se[6:8],
                             modthirty_se[6:8], modsixty_se[1:3]),
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
  ylab("Log(Wheat Rev. per Acre)") +
  xlab(NULL) 
ggsave("figures/wheat_reg_plot.pdf", width = 6, height = 4)