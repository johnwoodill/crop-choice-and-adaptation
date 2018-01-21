library(tidyverse)
library(lfe)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat$trend_sq <- cropdat$trend^2

# Load models
corn_mod <- readRDS("models/mod_corn.rds")
cotton_mod <- readRDS("models/mod_cotton.rds")
hay_mod <- readRDS("models/mod_hay.rds")
soybean_mod <- readRDS("models/mod_soybean.rds")
wheat_mod <- readRDS("models/mod_wheat.rds")

terms = c("dday0_10", "dday10_30", "dday30", "prec_sq")

# Load changes in degree day data
p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")


#---------------------------------------------------------------------------------
# Corn
# Get predictions for weather conditional on climate (restrict terms to weather)
corn_0p <- predictFelm(corn_mod, newdata = cropdat, var.terms = terms)
corn_1p <- predictFelm(corn_mod, newdata = p1, var.terms = terms)
corn_2p <- predictFelm(corn_mod, newdata = p2, var.terms = terms)
corn_3p <- predictFelm(corn_mod, newdata = p3, var.terms = terms)
corn_4p <- predictFelm(corn_mod, newdata = p4, var.terms = terms)
corn_5p <- predictFelm(corn_mod, newdata = p5, var.terms = terms)

# Total predicted revenue per acre
corn_p0 <- sum(exp(corn_0p$fit + corn_0p$res + corn_0p$effect) - 1)
corn_p1 <- sum(exp(corn_1p$fit + corn_1p$res + corn_1p$effect) - 1)
corn_p2 <- sum(exp(corn_2p$fit + corn_2p$res + corn_2p$effect) - 1) 
corn_p3 <- sum(exp(corn_3p$fit + corn_3p$res + corn_3p$effect) - 1) 
corn_p4 <- sum(exp(corn_4p$fit + corn_4p$res + corn_4p$effect) - 1) 
corn_p5 <- sum(exp(corn_5p$fit + corn_5p$res + corn_5p$effect) - 1) 

# Get standard errors of sum
corn_p0_ci <- sum(exp(corn_0p$felm.se.fit) - 1)
corn_p1_ci <- sum(exp(corn_1p$felm.se.fit) - 1)
corn_p2_ci <- sum(exp(corn_2p$felm.se.fit) - 1)
corn_p3_ci <- sum(exp(corn_3p$felm.se.fit) - 1)
corn_p4_ci <- sum(exp(corn_4p$felm.se.fit) - 1)
corn_p5_ci <- sum(exp(corn_5p$felm.se.fit) - 1)

corn_p0_fit <- exp(corn_0p$fit + corn_0p$res + corn_0p$effect) - 1
corn_p1_fit <- exp(corn_1p$fit + corn_1p$res + corn_1p$effect) - 1
corn_p2_fit <- exp(corn_2p$fit + corn_2p$res + corn_2p$effect) - 1
corn_p3_fit <- exp(corn_3p$fit + corn_3p$res + corn_3p$effect) - 1
corn_p4_fit <- exp(corn_4p$fit + corn_4p$res + corn_4p$effect) - 1
corn_p5_fit <- exp(corn_5p$fit + corn_5p$res + corn_5p$effect) - 1


pdat_corn <- data.frame(effect = rep(c("Weather-effect")),
                        crop = "Corn",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(corn_p0, corn_p1, corn_p2, corn_p3, corn_p4, corn_p5),
                   ci = c(corn_p0_ci, corn_p1_ci, corn_p2_ci, corn_p3_ci, corn_p4_ci, corn_p5_ci))
pdat_corn



# cotton
# Get predictions for weather conditional on climate (restrict terms to weather)
cotton_0p <- predictFelm(felm.fit = cotton_mod, newdata = cropdat, var.terms = terms)
cotton_1p <- predictFelm(cotton_mod, newdata = p1, var.terms = terms)
cotton_2p <- predictFelm(cotton_mod, newdata = p2, var.terms = terms)
cotton_3p <- predictFelm(cotton_mod, newdata = p3, var.terms = terms)
cotton_4p <- predictFelm(cotton_mod, newdata = p4, var.terms = terms)
cotton_5p <- predictFelm(cotton_mod, newdata = p5, var.terms = terms)

# Total predicted revenue per acre
cotton_p0 <- sum(exp(cotton_0p$fit + cotton_0p$res + cotton_0p$effect) - 1)
cotton_p1 <- sum(exp(cotton_1p$fit + cotton_1p$res + cotton_1p$effect) - 1)
cotton_p2 <- sum(exp(cotton_2p$fit + cotton_2p$res + cotton_2p$effect) - 1) 
cotton_p3 <- sum(exp(cotton_3p$fit + cotton_3p$res + cotton_3p$effect) - 1) 
cotton_p4 <- sum(exp(cotton_4p$fit + cotton_4p$res + cotton_4p$effect) - 1) 
cotton_p5 <- sum(exp(cotton_5p$fit + cotton_5p$res + cotton_5p$effect) - 1) 

# Get standard errors of sum
cotton_p0_ci <- sum(exp(cotton_0p$felm.se.fit) - 1)
cotton_p1_ci <- sum(exp(cotton_1p$felm.se.fit) - 1)
cotton_p2_ci <- sum(exp(cotton_2p$felm.se.fit) - 1)
cotton_p3_ci <- sum(exp(cotton_3p$felm.se.fit) - 1)
cotton_p4_ci <- sum(exp(cotton_4p$felm.se.fit) - 1)
cotton_p5_ci <- sum(exp(cotton_5p$felm.se.fit) - 1)

cotton_p0_fit <- exp(cotton_0p$fit + cotton_0p$res + cotton_0p$effect) - 1
cotton_p1_fit <- exp(cotton_1p$fit + cotton_1p$res + cotton_1p$effect) - 1
cotton_p2_fit <- exp(cotton_2p$fit + cotton_2p$res + cotton_2p$effect) - 1
cotton_p3_fit <- exp(cotton_3p$fit + cotton_3p$res + cotton_3p$effect) - 1
cotton_p4_fit <- exp(cotton_4p$fit + cotton_4p$res + cotton_4p$effect) - 1
cotton_p5_fit <- exp(cotton_5p$fit + cotton_5p$res + cotton_5p$effect) - 1

# cotton_0p <- predictFelm(felm.fit = cotton_mod, newdata = cropdat, var.terms = c("dday0_10"))
# 
# head(exp(cotton_0p$fit + cotton_0p$res + cotton_0p$effect) - 1)
# head(exp(cotton_0p$felm.se.fit + cotton_0p$res + cotton_0p$effect) - 1)

pdat_cotton <- data.frame(effect = rep(c("Weather-effect")),
                        crop = "Cotton",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(cotton_p0, cotton_p1, cotton_p2, cotton_p3, cotton_p4, cotton_p5),
                   ci = c(cotton_p0_ci, cotton_p1_ci, cotton_p2_ci, cotton_p3_ci, cotton_p4_ci, cotton_p5_ci))
pdat_cotton


#---------------------------------------------------------------------------------
# hay
# Get predictions for weather conditional on climate (restrict terms to weather)
hay_0p <- predictFelm(hay_mod, newdata = cropdat, var.terms = terms)
hay_1p <- predictFelm(hay_mod, newdata = p1, var.terms = terms)
hay_2p <- predictFelm(hay_mod, newdata = p2, var.terms = terms)
hay_3p <- predictFelm(hay_mod, newdata = p3, var.terms = terms)
hay_4p <- predictFelm(hay_mod, newdata = p4, var.terms = terms)
hay_5p <- predictFelm(hay_mod, newdata = p5, var.terms = terms)

# Total predicted revenue per acre
hay_p0 <- sum(exp(hay_0p$fit + hay_0p$res + hay_0p$effect) - 1)
hay_p1 <- sum(exp(hay_1p$fit + hay_1p$res + hay_1p$effect) - 1)
hay_p2 <- sum(exp(hay_2p$fit + hay_2p$res + hay_2p$effect) - 1) 
hay_p3 <- sum(exp(hay_3p$fit + hay_3p$res + hay_3p$effect) - 1) 
hay_p4 <- sum(exp(hay_4p$fit + hay_4p$res + hay_4p$effect) - 1) 
hay_p5 <- sum(exp(hay_5p$fit + hay_5p$res + hay_5p$effect) - 1) 

# Get standard errors of sum
hay_p0_ci <- sum(exp(hay_0p$felm.se.fit) - 1)
hay_p1_ci <- sum(exp(hay_1p$felm.se.fit) - 1)
hay_p2_ci <- sum(exp(hay_2p$felm.se.fit) - 1)
hay_p3_ci <- sum(exp(hay_3p$felm.se.fit) - 1)
hay_p4_ci <- sum(exp(hay_4p$felm.se.fit) - 1)
hay_p5_ci <- sum(exp(hay_5p$felm.se.fit) - 1)


hay_p0_fit <- exp(hay_0p$fit + hay_0p$res + hay_0p$effect) - 1
hay_p1_fit <- exp(hay_1p$fit + hay_1p$res + hay_1p$effect) - 1
hay_p2_fit <- exp(hay_2p$fit + hay_2p$res + hay_2p$effect) - 1
hay_p3_fit <- exp(hay_3p$fit + hay_3p$res + hay_3p$effect) - 1
hay_p4_fit <- exp(hay_4p$fit + hay_4p$res + hay_4p$effect) - 1
hay_p5_fit <- exp(hay_5p$fit + hay_5p$res + hay_5p$effect) - 1


pdat_hay <- data.frame(effect = rep(c("Weather-effect")),
                        crop = "Hay",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(hay_p0, hay_p1, hay_p2, hay_p3, hay_p4, hay_p5),
                   ci = c(hay_p0_ci, hay_p1_ci, hay_p2_ci, hay_p3_ci, hay_p4_ci, hay_p5_ci))
pdat_hay


#---------------------------------------------------------------------------------
# soybean
# Get predictions for weather conditional on climate (restrict terms to weather)
soybean_0p <- predictFelm(soybean_mod, newdata = cropdat, var.terms = terms)
soybean_1p <- predictFelm(soybean_mod, newdata = p1, var.terms = terms)
soybean_2p <- predictFelm(soybean_mod, newdata = p2, var.terms = terms)
soybean_3p <- predictFelm(soybean_mod, newdata = p3, var.terms = terms)
soybean_4p <- predictFelm(soybean_mod, newdata = p4, var.terms = terms)
soybean_5p <- predictFelm(soybean_mod, newdata = p5, var.terms = terms)

# Total predicted revenue per acre
soybean_p0 <- sum(exp(soybean_0p$fit + soybean_0p$res + soybean_0p$effect) - 1)
soybean_p1 <- sum(exp(soybean_1p$fit + soybean_1p$res + soybean_1p$effect) - 1)
soybean_p2 <- sum(exp(soybean_2p$fit + soybean_2p$res + soybean_2p$effect) - 1) 
soybean_p3 <- sum(exp(soybean_3p$fit + soybean_3p$res + soybean_3p$effect) - 1) 
soybean_p4 <- sum(exp(soybean_4p$fit + soybean_4p$res + soybean_4p$effect) - 1) 
soybean_p5 <- sum(exp(soybean_5p$fit + soybean_5p$res + soybean_5p$effect) - 1) 

# Get standard errors of sum
soybean_p0_ci <- sum(exp(soybean_0p$felm.se.fit) - 1)
soybean_p1_ci <- sum(exp(soybean_1p$felm.se.fit) - 1)
soybean_p2_ci <- sum(exp(soybean_2p$felm.se.fit) - 1)
soybean_p3_ci <- sum(exp(soybean_3p$felm.se.fit) - 1)
soybean_p4_ci <- sum(exp(soybean_4p$felm.se.fit) - 1)
soybean_p5_ci <- sum(exp(soybean_5p$felm.se.fit) - 1)

soybean_p0_fit <- exp(soybean_0p$fit + soybean_0p$res + soybean_0p$effect) - 1
soybean_p1_fit <- exp(soybean_1p$fit + soybean_1p$res + soybean_1p$effect) - 1
soybean_p2_fit <- exp(soybean_2p$fit + soybean_2p$res + soybean_2p$effect) - 1
soybean_p3_fit <- exp(soybean_3p$fit + soybean_3p$res + soybean_3p$effect) - 1
soybean_p4_fit <- exp(soybean_4p$fit + soybean_4p$res + soybean_4p$effect) - 1
soybean_p5_fit <- exp(soybean_5p$fit + soybean_5p$res + soybean_5p$effect) - 1



pdat_soybean <- data.frame(effect = rep(c("Weather-effect")),
                        crop = "Soybean",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(soybean_p0, soybean_p1, soybean_p2, soybean_p3, soybean_p4, soybean_p5),
                   ci = c(soybean_p0_ci, soybean_p1_ci, soybean_p2_ci, soybean_p3_ci, soybean_p4_ci, soybean_p5_ci))
pdat_soybean




#---------------------------------------------------------------------------------
# wheat
# Get predictions for weather conditional on climate (restrict terms to weather)
wheat_0p <- predictFelm(wheat_mod, newdata = cropdat, var.terms = terms)
wheat_1p <- predictFelm(wheat_mod, newdata = p1, var.terms = terms)
wheat_2p <- predictFelm(wheat_mod, newdata = p2, var.terms = terms)
wheat_3p <- predictFelm(wheat_mod, newdata = p3, var.terms = terms)
wheat_4p <- predictFelm(wheat_mod, newdata = p4, var.terms = terms)
wheat_5p <- predictFelm(wheat_mod, newdata = p5, var.terms = terms)

# Total predicted revenue per acre
wheat_p0 <- sum(exp(wheat_0p$fit + wheat_0p$res + wheat_0p$effect) - 1)
wheat_p1 <- sum(exp(wheat_1p$fit + wheat_1p$res + wheat_1p$effect) - 1)
wheat_p2 <- sum(exp(wheat_2p$fit + wheat_2p$res + wheat_2p$effect) - 1) 
wheat_p3 <- sum(exp(wheat_3p$fit + wheat_3p$res + wheat_3p$effect) - 1) 
wheat_p4 <- sum(exp(wheat_4p$fit + wheat_4p$res + wheat_4p$effect) - 1) 
wheat_p5 <- sum(exp(wheat_5p$fit + wheat_5p$res + wheat_5p$effect) - 1) 

# Get standard errors of sum
wheat_p0_ci <- sum(exp(wheat_0p$felm.se.fit) - 1)
wheat_p1_ci <- sum(exp(wheat_1p$felm.se.fit) - 1)
wheat_p2_ci <- sum(exp(wheat_2p$felm.se.fit) - 1)
wheat_p3_ci <- sum(exp(wheat_3p$felm.se.fit) - 1)
wheat_p4_ci <- sum(exp(wheat_4p$felm.se.fit) - 1)
wheat_p5_ci <- sum(exp(wheat_5p$felm.se.fit) - 1)

wheat_p0_fit <- exp(wheat_0p$fit + wheat_0p$res + wheat_0p$effect) - 1
wheat_p1_fit <- exp(wheat_1p$fit + wheat_1p$res + wheat_1p$effect) - 1
wheat_p2_fit <- exp(wheat_2p$fit + wheat_2p$res + wheat_2p$effect) - 1
wheat_p3_fit <- exp(wheat_3p$fit + wheat_3p$res + wheat_3p$effect) - 1
wheat_p4_fit <- exp(wheat_4p$fit + wheat_4p$res + wheat_4p$effect) - 1
wheat_p5_fit <- exp(wheat_5p$fit + wheat_5p$res + wheat_5p$effect) - 1



pdat_wheat <- data.frame(effect = rep(c("Weather-effect")),
                        crop = "Wheat",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(wheat_p0, wheat_p1, wheat_p2, wheat_p3, wheat_p4, wheat_p5),
                   ci = c(wheat_p0_ci, wheat_p1_ci, wheat_p2_ci, wheat_p3_ci, wheat_p4_ci, wheat_p5_ci))
pdat_wheat

#-----------------------------------------------------------------------------
# Aggregate data

# Predicted Effects
adat <- data.frame(effect = "Weather-effect", 
                   corn_rev.pred = c(corn_p0_fit, corn_p1_fit, corn_p2_fit, corn_p3_fit, corn_p4_fit, corn_p5_fit),
                   cotton_rev.pred = c(cotton_p0_fit, cotton_p1_fit, cotton_p2_fit, cotton_p3_fit, cotton_p4_fit, cotton_p5_fit),
                   hay_rev.pred = c(hay_p0_fit, hay_p1_fit, hay_p2_fit, hay_p3_fit, hay_p4_fit, hay_p5_fit),
                   soybean_rev.pred = c(soybean_p0_fit, soybean_p1_fit, soybean_p2_fit, soybean_p3_fit, soybean_p4_fit, soybean_p5_fit),
                   wheat_rev.pred = c(wheat_p0_fit, wheat_p1_fit, wheat_p2_fit, wheat_p3_fit, wheat_p4_fit, wheat_p5_fit),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 5, each = length(corn_p0_fit)/5),
                   crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), 6, each = length(corn_p0_fit)/5))
head(adat)                            
saveRDS(adat, "data/rev_crop_pred.rds")
                              
# Predicted percentage change
pdat <- rbind(pdat_corn, pdat_cotton, pdat_hay, pdat_soybean, pdat_wheat)
pdat$change_min <- pdat$sum - pdat$ci*1.96
pdat$change_max <- pdat$sum + pdat$ci*1.96
pdat

saveRDS(pdat, "data/rev_crop_predictions.rds")

pdat <- pdat %>% 
  group_by(crop) %>% 
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
  # geom_point(size = 0.5) +
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
  facet_wrap(~crop) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5)

ggsave("figures/4-predicted_crop.pdf", device = "pdf", width = 6, height = 4)

# ggplot(sixtypdat, aes(temp, change)) + geom_line()
#plot(c(c1, c2, c3, c4, c5))
#plot(c(ld1, ld2, ld3, ld4, ld5))
#plot(c(p1, p2, p3, p4, p5))


