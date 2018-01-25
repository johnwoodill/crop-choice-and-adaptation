library(tidyverse)
library(lfe)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat$trend_sq <- cropdat$trend^2

# Load models
# corn_mod <- readRDS("models/mod_corn.rds")
# cotton_mod <- readRDS("models/mod_cotton.rds")
# hay_mod <- readRDS("models/mod_hay.rds")
# soybean_mod <- readRDS("models/mod_soybean.rds")
# wheat_mod <- readRDS("models/mod_wheat.rds")

modten <- readRDS("models/rev_crop_modten_1.rds")
modtwenty <- readRDS("models/rev_crop_modtwenty_1.rds")
modthirty <- readRDS("models/rev_crop_modthirty_1.rds")

terms_ten = c("dday0_10", "dday10_30", "dday30","prec", "prec_sq", "dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten")
terms_twenty = c("dday0_10", "dday10_30", "dday30","prec", "prec_sq", "dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_twenty_twenty")
terms_thirty = c("dday0_10", "dday10_30", "dday30","prec", "prec_sq", "dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty")

# Load changes in degree day data
p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")


#---------------------------------------------------------------------------------
# 10-year
# Get predictions for weather conditional on climate (restrict terms to weather)
modten_0p <- predictFelm(modten, newdata = cropdat, var.terms = terms_ten)
modten_1p <- predictFelm(modten, newdata = p1, var.terms = terms_ten)
modten_2p <- predictFelm(modten, newdata = p2, var.terms = terms_ten)
modten_3p <- predictFelm(modten, newdata = p3, var.terms = terms_ten)
modten_4p <- predictFelm(modten, newdata = p4, var.terms = terms_ten)
modten_5p <- predictFelm(modten, newdata = p5, var.terms = terms_ten)

# Total predicted revenue per acre
modten_p0 <- sum(exp(modten_0p$fit + modten_0p$res + modten_0p$effect) - 1)
modten_p1 <- sum(exp(modten_1p$fit + modten_1p$res + modten_1p$effect) - 1)
modten_p2 <- sum(exp(modten_2p$fit + modten_2p$res + modten_2p$effect) - 1) 
modten_p3 <- sum(exp(modten_3p$fit + modten_3p$res + modten_3p$effect) - 1) 
modten_p4 <- sum(exp(modten_4p$fit + modten_4p$res + modten_4p$effect) - 1) 
modten_p5 <- sum(exp(modten_5p$fit + modten_5p$res + modten_5p$effect) - 1) 

# Get standard errors of sum
modten_p0_ci <- sum(exp(modten_0p$felm.se.fit) - 1)
modten_p1_ci <- sum(exp(modten_1p$felm.se.fit) - 1)
modten_p2_ci <- sum(exp(modten_2p$felm.se.fit) - 1)
modten_p3_ci <- sum(exp(modten_3p$felm.se.fit) - 1)
modten_p4_ci <- sum(exp(modten_4p$felm.se.fit) - 1)
modten_p5_ci <- sum(exp(modten_5p$felm.se.fit) - 1)

modten_p0_fit <- exp(modten_0p$fit + modten_0p$res + modten_0p$effect) - 1
modten_p1_fit <- exp(modten_1p$fit + modten_1p$res + modten_1p$effect) - 1
modten_p2_fit <- exp(modten_2p$fit + modten_2p$res + modten_2p$effect) - 1
modten_p3_fit <- exp(modten_3p$fit + modten_3p$res + modten_3p$effect) - 1
modten_p4_fit <- exp(modten_4p$fit + modten_4p$res + modten_4p$effect) - 1
modten_p5_fit <- exp(modten_5p$fit + modten_5p$res + modten_5p$effect) - 1


pdat_modten <- data.frame(effect = rep(c("Total-effect")),
                        interval = "10-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(modten_p0, modten_p1, modten_p2, modten_p3, modten_p4, modten_p5),
                   ci = c(modten_p0_ci, modten_p1_ci, modten_p2_ci, modten_p3_ci, modten_p4_ci, modten_p5_ci))
pdat_modten


#---------------------------------------------------------------------------------
# 20-year
# Get predictions for weather conditional on climate (restrict terms to weather)
modtwenty_0p <- predictFelm(modtwenty, newdata = cropdat, var.terms = terms)
modtwenty_1p <- predictFelm(modtwenty, newdata = p1, var.terms = terms)
modtwenty_2p <- predictFelm(modtwenty, newdata = p2, var.terms = terms)
modtwenty_3p <- predictFelm(modtwenty, newdata = p3, var.terms = terms)
modtwenty_4p <- predictFelm(modtwenty, newdata = p4, var.terms = terms)
modtwenty_5p <- predictFelm(modtwenty, newdata = p5, var.terms = terms)

# Total predicted revenue per acre
modtwenty_p0 <- sum(exp(modtwenty_0p$fit + modtwenty_0p$res + modtwenty_0p$effect) - 1)
modtwenty_p1 <- sum(exp(modtwenty_1p$fit + modtwenty_1p$res + modtwenty_1p$effect) - 1)
modtwenty_p2 <- sum(exp(modtwenty_2p$fit + modtwenty_2p$res + modtwenty_2p$effect) - 1) 
modtwenty_p3 <- sum(exp(modtwenty_3p$fit + modtwenty_3p$res + modtwenty_3p$effect) - 1) 
modtwenty_p4 <- sum(exp(modtwenty_4p$fit + modtwenty_4p$res + modtwenty_4p$effect) - 1) 
modtwenty_p5 <- sum(exp(modtwenty_5p$fit + modtwenty_5p$res + modtwenty_5p$effect) - 1) 

# Get standard errors of sum
modtwenty_p0_ci <- sum(exp(modtwenty_0p$felm.se.fit) - 1)
modtwenty_p1_ci <- sum(exp(modtwenty_1p$felm.se.fit) - 1)
modtwenty_p2_ci <- sum(exp(modtwenty_2p$felm.se.fit) - 1)
modtwenty_p3_ci <- sum(exp(modtwenty_3p$felm.se.fit) - 1)
modtwenty_p4_ci <- sum(exp(modtwenty_4p$felm.se.fit) - 1)
modtwenty_p5_ci <- sum(exp(modtwenty_5p$felm.se.fit) - 1)

modtwenty_p0_fit <- exp(modtwenty_0p$fit + modtwenty_0p$res + modtwenty_0p$effect) - 1
modtwenty_p1_fit <- exp(modtwenty_1p$fit + modtwenty_1p$res + modtwenty_1p$effect) - 1
modtwenty_p2_fit <- exp(modtwenty_2p$fit + modtwenty_2p$res + modtwenty_2p$effect) - 1
modtwenty_p3_fit <- exp(modtwenty_3p$fit + modtwenty_3p$res + modtwenty_3p$effect) - 1
modtwenty_p4_fit <- exp(modtwenty_4p$fit + modtwenty_4p$res + modtwenty_4p$effect) - 1
modtwenty_p5_fit <- exp(modtwenty_5p$fit + modtwenty_5p$res + modtwenty_5p$effect) - 1


pdat_modtwenty <- data.frame(effect = rep(c("Total-effect")),
                        interval = "20-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(modtwenty_p0, modtwenty_p1, modtwenty_p2, modtwenty_p3, modtwenty_p4, modtwenty_p5),
                   ci = c(modtwenty_p0_ci, modtwenty_p1_ci, modtwenty_p2_ci, modtwenty_p3_ci, modtwenty_p4_ci, modtwenty_p5_ci))
pdat_modtwenty


#---------------------------------------------------------------------------------
# 30-year
# Get predictions for weather conditional on climate (restrict terms to weather)
modthirty_0p <- predictFelm(modthirty, newdata = cropdat, var.terms = terms)
modthirty_1p <- predictFelm(modthirty, newdata = p1, var.terms = terms)
modthirty_2p <- predictFelm(modthirty, newdata = p2, var.terms = terms)
modthirty_3p <- predictFelm(modthirty, newdata = p3, var.terms = terms)
modthirty_4p <- predictFelm(modthirty, newdata = p4, var.terms = terms)
modthirty_5p <- predictFelm(modthirty, newdata = p5, var.terms = terms)

# Total predicted revenue per acre
modthirty_p0 <- sum(exp(modthirty_0p$fit + modthirty_0p$res + modthirty_0p$effect) - 1)
modthirty_p1 <- sum(exp(modthirty_1p$fit + modthirty_1p$res + modthirty_1p$effect) - 1)
modthirty_p2 <- sum(exp(modthirty_2p$fit + modthirty_2p$res + modthirty_2p$effect) - 1) 
modthirty_p3 <- sum(exp(modthirty_3p$fit + modthirty_3p$res + modthirty_3p$effect) - 1) 
modthirty_p4 <- sum(exp(modthirty_4p$fit + modthirty_4p$res + modthirty_4p$effect) - 1) 
modthirty_p5 <- sum(exp(modthirty_5p$fit + modthirty_5p$res + modthirty_5p$effect) - 1) 

# Get standard errors of sum
modthirty_p0_ci <- sum(exp(modthirty_0p$felm.se.fit) - 1)
modthirty_p1_ci <- sum(exp(modthirty_1p$felm.se.fit) - 1)
modthirty_p2_ci <- sum(exp(modthirty_2p$felm.se.fit) - 1)
modthirty_p3_ci <- sum(exp(modthirty_3p$felm.se.fit) - 1)
modthirty_p4_ci <- sum(exp(modthirty_4p$felm.se.fit) - 1)
modthirty_p5_ci <- sum(exp(modthirty_5p$felm.se.fit) - 1)

modthirty_p0_fit <- exp(modthirty_0p$fit + modthirty_0p$res + modthirty_0p$effect) - 1
modthirty_p1_fit <- exp(modthirty_1p$fit + modthirty_1p$res + modthirty_1p$effect) - 1
modthirty_p2_fit <- exp(modthirty_2p$fit + modthirty_2p$res + modthirty_2p$effect) - 1
modthirty_p3_fit <- exp(modthirty_3p$fit + modthirty_3p$res + modthirty_3p$effect) - 1
modthirty_p4_fit <- exp(modthirty_4p$fit + modthirty_4p$res + modthirty_4p$effect) - 1
modthirty_p5_fit <- exp(modthirty_5p$fit + modthirty_5p$res + modthirty_5p$effect) - 1


pdat_modthirty <- data.frame(effect = rep(c("Total-effect")),
                        interval = "30-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(modthirty_p0, modthirty_p1, modthirty_p2, modthirty_p3, modthirty_p4, modthirty_p5),
                   ci = c(modthirty_p0_ci, modthirty_p1_ci, modthirty_p2_ci, modthirty_p3_ci, modthirty_p4_ci, modthirty_p5_ci))
pdat_modthirty

#-----------------------------------------------------------------------------
# Aggregate data

# Predicted Effects
adat <- data.frame(effect = "Total-effect", 
                   modten_rev.pred = c(modten_p0_fit, modten_p1_fit, modten_p2_fit, modten_p3_fit, modten_p4_fit, modten_p5_fit),
                   modtwenty_rev.pred = c(modtwenty_p0_fit, modtwenty_p1_fit, modtwenty_p2_fit, modtwenty_p3_fit, modtwenty_p4_fit, modtwenty_p5_fit),
                   modthirty_rev.pred = c(modthirty_p0_fit, modthirty_p1_fit, modthirty_p2_fit, modthirty_p3_fit, modthirty_p4_fit, modthirty_p5_fit),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 3, each = length(modten_p0_fit)/3),
                   interval = rep(c("10-year", "20-year", "30-year"), 6, each = length(modten_p0_fit)/3))
head(adat)                            
saveRDS(adat, "data/rev_crop_pred.rds")
                              
# Predicted percentage change
pdat <- rbind(pdat_modten, pdat_modtwenty, pdat_modthirty)
pdat$change_min <- pdat$sum - pdat$ci*1.96
pdat$change_max <- pdat$sum + pdat$ci*1.96
pdat

saveRDS(pdat, "data/rev_crop_predictions.rds")

pdat <- pdat %>% 
  group_by(interval) %>% 
  mutate(change =  (sum/first(sum) - 1),
         change_min = (change_min/first(sum) - 1),
         change_max = (change_max/first(sum) - 1))

pdat
pdat$change <- pdat$change*100
pdat$change_min <- pdat$change_min*100
pdat$change_max <- pdat$change_max*100

# Predicted values
#############################
#############################
#############################
#############################
#############################
#############################



saveRDS(pdat, "data/rev_crop_predictions.rds")
pdat <- readRDS("data/rev_crop_predictions.rds")

ggplot(pdat, aes(temp, change)) + 
  geom_ribbon(aes(ymax = change_max, ymin = change_min, x = temp), fill = "#C0CCD9", alpha = 0.5 ) +
  geom_line() +
  geom_point(size = 0.5) +
  #geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Revenue per acre") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  # theme(legend.position = "top", 
       #legend.justification = c("left", "top"), 
       # legend.box.background = element_rect(colour = "grey"), 
       # legend.title = element_blank(), legend.key = element_blank()) +
  #theme(legend.position = c(.85,1), 
  #     legend.justification = c("left", "top"), 
  #     legend.box.background = element_rect(colour = "grey"), 
  #     legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~interval) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5)

ggsave("figures/4-predicted_crop.pdf", device = "pdf", width = 6, height = 4)

# ggplot(sixtypdat, aes(temp, change)) + geom_line()
#plot(c(c1, c2, c3, c4, c5))
#plot(c(ld1, ld2, ld3, ld4, ld5))
#plot(c(p1, p2, p3, p4, p5))


