library(tidyverse)
library(lfe)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat$trend_sq <- cropdat$trend^2
cropdat$fips <- factor(cropdat$fips)

# Load models
modten <- readRDS("models/rev_crop_modten.rds")
modtwenty <- readRDS("models/rev_crop_modtwenty.rds")
modthirty <- readRDS("models/rev_crop_modthirty.rds")

wcterms_ten = c("dday0_10", "dday10_30", "dday30","prec", "prec_sq", "dday0_10_rm10", "dday10_30_rm10", "dday30_rm10", "prec_rm10", "prec_sq_rm10")
wcterms_twenty = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "dday0_10_rm11", "dday10_30_rm11", "dday30_rm11", "prec_rm11", "prec_sq_rm11")
wcterms_thirty = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "dday0_10_rm12", "dday10_30_rm12", "dday30_rm12", "prec_rm12", "prec_sq_rm12")

wterms <- c("dday0_10", "dday10_30", "dday30","prec", "prec_sq")
cterms_ten <- c("dday0_10_rm10", "dday10_30_rm10", "dday30_rm10", "prec_rm10", "prec_sq_rm10")
cterms_twenty <- c("dday0_10_rm11", "dday10_30_rm11", "dday30_rm11", "prec_rm11", "prec_sq_rm11")
cterms_thirty <- c("dday0_10_rm12", "dday10_30_rm12", "dday30_rm12", "prec_rm12", "prec_sq_rm12")

# Load changes in degree day data
p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")


#--------------------------------------------------------------------------------
# Weather-Climate-effect
#---------------------------------------------------------------------------------
# 10-year
# Get predictions for weather conditional on climate (restrict terms to weather)
wcmodten_0p <- predictFelm(felm.fit = modten, newdata = cropdat, var.terms = wcterms_ten)
wcmodten_1p <- predictFelm(felm.fit = modten, newdata = p1, var.terms = wcterms_ten)
wcmodten_2p <- predictFelm(modten, newdata = p2, var.terms = wcterms_ten)
wcmodten_3p <- predictFelm(modten, newdata = p3, var.terms = wcterms_ten)
wcmodten_4p <- predictFelm(modten, newdata = p4, var.terms = wcterms_ten)
wcmodten_5p <- predictFelm(modten, newdata = p5, var.terms = wcterms_ten)

# Total predicted revenue per acre
wcmodten_0p$sum <- sum(exp(wcmodten_0p$fit + wcmodten_0p$res + wcmodten_0p$effect) - 1)
wcmodten_1p$sum <- sum(exp(wcmodten_1p$fit + wcmodten_1p$res + wcmodten_1p$effect) - 1)
wcmodten_2p$sum <- sum(exp(wcmodten_2p$fit + wcmodten_2p$res + wcmodten_2p$effect) - 1) 
wcmodten_3p$sum <- sum(exp(wcmodten_3p$fit + wcmodten_3p$res + wcmodten_3p$effect) - 1) 
wcmodten_4p$sum <- sum(exp(wcmodten_4p$fit + wcmodten_4p$res + wcmodten_4p$effect) - 1) 
wcmodten_5p$sum <- sum(exp(wcmodten_5p$fit + wcmodten_5p$res + wcmodten_5p$effect) - 1) 

# Get standard errors of sum
wcmodten_p0_ci <- sum(exp(wcmodten_0p$felm.se.fit + wcmodten_0p$res + wcmodten_0p$effect) - 1)
wcmodten_p1_ci <- sum(exp(wcmodten_1p$felm.se.fit + wcmodten_1p$res + wcmodten_1p$effect) - 1)
wcmodten_p2_ci <- sum(exp(wcmodten_2p$felm.se.fit + wcmodten_2p$res + wcmodten_2p$effect) - 1)
wcmodten_p3_ci <- sum(exp(wcmodten_3p$felm.se.fit + wcmodten_3p$res + wcmodten_3p$effect) - 1)
wcmodten_p4_ci <- sum(exp(wcmodten_4p$felm.se.fit + wcmodten_4p$res + wcmodten_4p$effect) - 1)
wcmodten_p5_ci <- sum(exp(wcmodten_5p$felm.se.fit + wcmodten_5p$res + wcmodten_5p$effect) - 1)

wcmodten_p0_se <- as.numeric(unlist(exp(wcmodten_0p$felm.se.fit ))) - 1
wcmodten_p1_se <- as.numeric(unlist(exp(wcmodten_1p$felm.se.fit ))) - 1
wcmodten_p2_se <- as.numeric(unlist(exp(wcmodten_2p$felm.se.fit ))) - 1
wcmodten_p3_se <- as.numeric(unlist(exp(wcmodten_3p$felm.se.fit ))) - 1
wcmodten_p4_se <- as.numeric(unlist(exp(wcmodten_4p$felm.se.fit))) - 1
wcmodten_p5_se <- as.numeric(unlist(exp(wcmodten_5p$felm.se.fit ))) - 1

wcmodten_p0_fit <- exp(wcmodten_0p$fit + wcmodten_0p$res + wcmodten_0p$effect) - 1
wcmodten_p1_fit <- exp(wcmodten_1p$fit + wcmodten_1p$res + wcmodten_1p$effect) - 1
wcmodten_p2_fit <- exp(wcmodten_2p$fit + wcmodten_2p$res + wcmodten_2p$effect) - 1
wcmodten_p3_fit <- exp(wcmodten_3p$fit + wcmodten_3p$res + wcmodten_3p$effect) - 1
wcmodten_p4_fit <- exp(wcmodten_4p$fit + wcmodten_4p$res + wcmodten_4p$effect) - 1
wcmodten_p5_fit <- exp(wcmodten_5p$fit + wcmodten_5p$res + wcmodten_5p$effect) - 1


pdat_wcmodten <- data.frame(effect = "Weather-Climate-effect",
                   interval = "10-year",
                   temp = c(0, 1, 2, 3, 4, 5),
                   sum = c(wcmodten_0p$sum, wcmodten_1p$sum, wcmodten_2p$sum, wcmodten_3p$sum, wcmodten_4p$sum, wcmodten_5p$sum),
                   ci = c(wcmodten_p0_ci, wcmodten_p1_ci, wcmodten_p2_ci, wcmodten_p3_ci, wcmodten_p4_ci, wcmodten_p5_ci))
head(pdat_wcmodten)


#---------------------------------------------------------------------------------
# 20-year
# Get predictions for weather conditional on climate (restrict terms to weather)
wcmodtwenty_0p <- predictFelm(modtwenty, newdata = cropdat, var.terms = wcterms_twenty)
wcmodtwenty_1p <- predictFelm(modtwenty, newdata = p1, var.terms = wcterms_twenty)
wcmodtwenty_2p <- predictFelm(modtwenty, newdata = p2, var.terms = wcterms_twenty)
wcmodtwenty_3p <- predictFelm(modtwenty, newdata = p3, var.terms = wcterms_twenty)
wcmodtwenty_4p <- predictFelm(modtwenty, newdata = p4, var.terms = wcterms_twenty)
wcmodtwenty_5p <- predictFelm(modtwenty, newdata = p5, var.terms = wcterms_twenty)

# Total predicted revenue per acre
wcmodtwenty_0p$sum <- sum(exp(wcmodtwenty_0p$fit + wcmodtwenty_0p$res + wcmodtwenty_0p$effect) - 1)
wcmodtwenty_1p$sum <- sum(exp(wcmodtwenty_1p$fit + wcmodtwenty_1p$res + wcmodtwenty_1p$effect) - 1)
wcmodtwenty_2p$sum <- sum(exp(wcmodtwenty_2p$fit + wcmodtwenty_2p$res + wcmodtwenty_2p$effect) - 1) 
wcmodtwenty_3p$sum <- sum(exp(wcmodtwenty_3p$fit + wcmodtwenty_3p$res + wcmodtwenty_3p$effect) - 1) 
wcmodtwenty_4p$sum <- sum(exp(wcmodtwenty_4p$fit + wcmodtwenty_4p$res + wcmodtwenty_4p$effect) - 1) 
wcmodtwenty_5p$sum <- sum(exp(wcmodtwenty_5p$fit + wcmodtwenty_5p$res + wcmodtwenty_5p$effect) - 1) 

# Get standard errors of sum
wcmodtwenty_p0_ci <- sum(exp(wcmodtwenty_0p$felm.se.fit) - 1)
wcmodtwenty_p1_ci <- sum(exp(wcmodtwenty_1p$felm.se.fit) - 1)
wcmodtwenty_p2_ci <- sum(exp(wcmodtwenty_2p$felm.se.fit) - 1)
wcmodtwenty_p3_ci <- sum(exp(wcmodtwenty_3p$felm.se.fit) - 1)
wcmodtwenty_p4_ci <- sum(exp(wcmodtwenty_4p$felm.se.fit) - 1)
wcmodtwenty_p5_ci <- sum(exp(wcmodtwenty_5p$felm.se.fit) - 1)

wcmodtwenty_p0_se <- as.numeric(unlist(exp(wcmodtwenty_0p$felm.se.fit))) - 1
wcmodtwenty_p1_se <- as.numeric(unlist(exp(wcmodtwenty_1p$felm.se.fit))) - 1
wcmodtwenty_p2_se <- as.numeric(unlist(exp(wcmodtwenty_2p$felm.se.fit))) - 1
wcmodtwenty_p3_se <- as.numeric(unlist(exp(wcmodtwenty_3p$felm.se.fit))) - 1
wcmodtwenty_p4_se <- as.numeric(unlist(exp(wcmodtwenty_4p$felm.se.fit))) - 1
wcmodtwenty_p5_se <- as.numeric(unlist(exp(wcmodtwenty_5p$felm.se.fit))) - 1

wcmodtwenty_p0_fit <- exp(wcmodtwenty_0p$fit + wcmodtwenty_0p$res + wcmodtwenty_0p$effect) - 1
wcmodtwenty_p1_fit <- exp(wcmodtwenty_1p$fit + wcmodtwenty_1p$res + wcmodtwenty_1p$effect) - 1
wcmodtwenty_p2_fit <- exp(wcmodtwenty_2p$fit + wcmodtwenty_2p$res + wcmodtwenty_2p$effect) - 1
wcmodtwenty_p3_fit <- exp(wcmodtwenty_3p$fit + wcmodtwenty_3p$res + wcmodtwenty_3p$effect) - 1
wcmodtwenty_p4_fit <- exp(wcmodtwenty_4p$fit + wcmodtwenty_4p$res + wcmodtwenty_4p$effect) - 1
wcmodtwenty_p5_fit <- exp(wcmodtwenty_5p$fit + wcmodtwenty_5p$res + wcmodtwenty_5p$effect) - 1


pdat_wcmodtwenty <- data.frame(effect = rep(c("Weather-Climate-effect")),
                        interval = "11-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(wcmodtwenty_0p$sum, wcmodtwenty_1p$sum, wcmodtwenty_2p$sum, wcmodtwenty_3p$sum, wcmodtwenty_4p$sum, wcmodtwenty_5p$sum),
                   ci = c(wcmodtwenty_p0_ci, wcmodtwenty_p1_ci, wcmodtwenty_p2_ci, wcmodtwenty_p3_ci, wcmodtwenty_p4_ci, wcmodtwenty_p5_ci))
head(pdat_wcmodtwenty)

#---------------------------------------------------------------------------------
# 30-year
# Get predictions for weather conditional on climate (restrict terms to weather)
wcmodthirty_0p <- predictFelm(modthirty, newdata = cropdat, var.terms = wcterms_thirty)
wcmodthirty_1p <- predictFelm(modthirty, newdata = p1, var.terms = wcterms_thirty)
wcmodthirty_2p <- predictFelm(modthirty, newdata = p2, var.terms = wcterms_thirty)
wcmodthirty_3p <- predictFelm(modthirty, newdata = p3, var.terms = wcterms_thirty)
wcmodthirty_4p <- predictFelm(modthirty, newdata = p4, var.terms = wcterms_thirty)
wcmodthirty_5p <- predictFelm(modthirty, newdata = p5, var.terms = wcterms_thirty)

# Total predicted revenue per acre
wcmodthirty_0p$sum <- sum(exp(wcmodthirty_0p$fit + wcmodthirty_0p$res + wcmodthirty_0p$effect) - 1)
wcmodthirty_1p$sum <- sum(exp(wcmodthirty_1p$fit + wcmodthirty_1p$res + wcmodthirty_1p$effect) - 1)
wcmodthirty_2p$sum <- sum(exp(wcmodthirty_2p$fit + wcmodthirty_2p$res + wcmodthirty_2p$effect) - 1) 
wcmodthirty_3p$sum <- sum(exp(wcmodthirty_3p$fit + wcmodthirty_3p$res + wcmodthirty_3p$effect) - 1) 
wcmodthirty_4p$sum <- sum(exp(wcmodthirty_4p$fit + wcmodthirty_4p$res + wcmodthirty_4p$effect) - 1) 
wcmodthirty_5p$sum <- sum(exp(wcmodthirty_5p$fit + wcmodthirty_5p$res + wcmodthirty_5p$effect) - 1) 

# Get standard errors of sum
wcmodthirty_p0_ci <- sum(exp(wcmodthirty_0p$felm.se.fit) - 1)
wcmodthirty_p1_ci <- sum(exp(wcmodthirty_1p$felm.se.fit) - 1)
wcmodthirty_p2_ci <- sum(exp(wcmodthirty_2p$felm.se.fit) - 1)
wcmodthirty_p3_ci <- sum(exp(wcmodthirty_3p$felm.se.fit) - 1)
wcmodthirty_p4_ci <- sum(exp(wcmodthirty_4p$felm.se.fit) - 1)
wcmodthirty_p5_ci <- sum(exp(wcmodthirty_5p$felm.se.fit) - 1)

wcmodthirty_p0_se <- as.numeric(unlist(exp(wcmodthirty_0p$felm.se.fit))) - 1
wcmodthirty_p1_se <- as.numeric(unlist(exp(wcmodthirty_1p$felm.se.fit))) - 1
wcmodthirty_p2_se <- as.numeric(unlist(exp(wcmodthirty_2p$felm.se.fit))) - 1
wcmodthirty_p3_se <- as.numeric(unlist(exp(wcmodthirty_3p$felm.se.fit))) - 1
wcmodthirty_p4_se <- as.numeric(unlist(exp(wcmodthirty_4p$felm.se.fit))) - 1
wcmodthirty_p5_se <- as.numeric(unlist(exp(wcmodthirty_5p$felm.se.fit))) - 1

wcmodthirty_p0_fit <- exp(wcmodthirty_0p$fit + wcmodthirty_0p$res + wcmodthirty_0p$effect) - 1
wcmodthirty_p1_fit <- exp(wcmodthirty_1p$fit + wcmodthirty_1p$res + wcmodthirty_1p$effect) - 1
wcmodthirty_p2_fit <- exp(wcmodthirty_2p$fit + wcmodthirty_2p$res + wcmodthirty_2p$effect) - 1
wcmodthirty_p3_fit <- exp(wcmodthirty_3p$fit + wcmodthirty_3p$res + wcmodthirty_3p$effect) - 1
wcmodthirty_p4_fit <- exp(wcmodthirty_4p$fit + wcmodthirty_4p$res + wcmodthirty_4p$effect) - 1
wcmodthirty_p5_fit <- exp(wcmodthirty_5p$fit + wcmodthirty_5p$res + wcmodthirty_5p$effect) - 1


pdat_wcmodthirty <- data.frame(effect = rep(c("Weather-Climate-effect")),
                        interval = "12-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(wcmodthirty_0p$sum, wcmodthirty_1p$sum, wcmodthirty_2p$sum, wcmodthirty_3p$sum, wcmodthirty_4p$sum, wcmodthirty_5p$sum),
                   ci = c(wcmodthirty_p0_ci, wcmodthirty_p1_ci, wcmodthirty_p2_ci, wcmodthirty_p3_ci, wcmodthirty_p4_ci, wcmodthirty_p5_ci))
head(pdat_wcmodthirty)



#--------------------------------------------------------------------------------
# Weather-effect
#---------------------------------------------------------------------------------
# 10-year
# Get predictions for weather conditional on climate (restrict terms to weather)
wmodten_0p <- predictFelm(modten, newdata = cropdat, var.terms = wterms)
wmodten_1p <- predictFelm(modten, newdata = p1, var.terms = wterms)
wmodten_2p <- predictFelm(modten, newdata = p2, var.terms = wterms)
wmodten_3p <- predictFelm(modten, newdata = p3, var.terms = wterms)
wmodten_4p <- predictFelm(modten, newdata = p4, var.terms = wterms)
wmodten_5p <- predictFelm(modten, newdata = p5, var.terms = wterms)

# Total predicted revenue per acre
wmodten_0p$sum <- sum(exp(wmodten_0p$fit + wmodten_0p$res + wmodten_0p$effect) - 1)
wmodten_1p$sum <- sum(exp(wmodten_1p$fit + wmodten_1p$res + wmodten_1p$effect) - 1)
wmodten_2p$sum <- sum(exp(wmodten_2p$fit + wmodten_2p$res + wmodten_2p$effect) - 1) 
wmodten_3p$sum <- sum(exp(wmodten_3p$fit + wmodten_3p$res + wmodten_3p$effect) - 1) 
wmodten_4p$sum <- sum(exp(wmodten_4p$fit + wmodten_4p$res + wmodten_4p$effect) - 1) 
wmodten_5p$sum <- sum(exp(wmodten_5p$fit + wmodten_5p$res + wmodten_5p$effect) - 1) 

# Get standard errors of sum
wmodten_p0_ci <- sum(exp(wmodten_0p$felm.se.fit) - 1)
wmodten_p1_ci <- sum(exp(wmodten_1p$felm.se.fit) - 1)
wmodten_p2_ci <- sum(exp(wmodten_2p$felm.se.fit) - 1)
wmodten_p3_ci <- sum(exp(wmodten_3p$felm.se.fit) - 1)
wmodten_p4_ci <- sum(exp(wmodten_4p$felm.se.fit) - 1)
wmodten_p5_ci <- sum(exp(wmodten_5p$felm.se.fit) - 1)

wmodten_p0_se <- as.numeric(unlist(exp(wmodten_0p$felm.se.fit))) - 1
wmodten_p1_se <- as.numeric(unlist(exp(wmodten_1p$felm.se.fit))) - 1
wmodten_p2_se <- as.numeric(unlist(exp(wmodten_2p$felm.se.fit))) - 1
wmodten_p3_se <- as.numeric(unlist(exp(wmodten_3p$felm.se.fit))) - 1
wmodten_p4_se <- as.numeric(unlist(exp(wmodten_4p$felm.se.fit))) - 1
wmodten_p5_se <- as.numeric(unlist(exp(wmodten_5p$felm.se.fit))) - 1

wmodten_p0_fit <- exp(wmodten_0p$fit + wmodten_0p$res + wmodten_0p$effect) - 1
wmodten_p1_fit <- exp(wmodten_1p$fit + wmodten_1p$res + wmodten_1p$effect) - 1
wmodten_p2_fit <- exp(wmodten_2p$fit + wmodten_2p$res + wmodten_2p$effect) - 1
wmodten_p3_fit <- exp(wmodten_3p$fit + wmodten_3p$res + wmodten_3p$effect) - 1
wmodten_p4_fit <- exp(wmodten_4p$fit + wmodten_4p$res + wmodten_4p$effect) - 1
wmodten_p5_fit <- exp(wmodten_5p$fit + wmodten_5p$res + wmodten_5p$effect) - 1


pdat_wmodten <- data.frame(effect = rep(c("Weather-effect")),
                        interval = "10-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(wmodten_0p$sum, wmodten_1p$sum, wmodten_2p$sum, wmodten_3p$sum, wmodten_4p$sum, wmodten_5p$sum),
                   ci = c(wmodten_p0_ci, wmodten_p1_ci, wmodten_p2_ci, wmodten_p3_ci, wmodten_p4_ci, wmodten_p5_ci))
head(pdat_wmodten)


#---------------------------------------------------------------------------------
# 20-year
# Get predictions for weather conditional on climate (restrict terms to weather)
wmodtwenty_0p <- predictFelm(modtwenty, newdata = cropdat, var.terms = wterms)
wmodtwenty_1p <- predictFelm(modtwenty, newdata = p1, var.terms = wterms)
wmodtwenty_2p <- predictFelm(modtwenty, newdata = p2, var.terms = wterms)
wmodtwenty_3p <- predictFelm(modtwenty, newdata = p3, var.terms = wterms)
wmodtwenty_4p <- predictFelm(modtwenty, newdata = p4, var.terms = wterms)
wmodtwenty_5p <- predictFelm(modtwenty, newdata = p5, var.terms = wterms)

# Total predicted revenue per acre
wmodtwenty_0p$sum <- sum(exp(wmodtwenty_0p$fit + wmodtwenty_0p$res + wmodtwenty_0p$effect) - 1)
wmodtwenty_1p$sum <- sum(exp(wmodtwenty_1p$fit + wmodtwenty_1p$res + wmodtwenty_1p$effect) - 1)
wmodtwenty_2p$sum <- sum(exp(wmodtwenty_2p$fit + wmodtwenty_2p$res + wmodtwenty_2p$effect) - 1) 
wmodtwenty_3p$sum <- sum(exp(wmodtwenty_3p$fit + wmodtwenty_3p$res + wmodtwenty_3p$effect) - 1) 
wmodtwenty_4p$sum <- sum(exp(wmodtwenty_4p$fit + wmodtwenty_4p$res + wmodtwenty_4p$effect) - 1) 
wmodtwenty_5p$sum <- sum(exp(wmodtwenty_5p$fit + wmodtwenty_5p$res + wmodtwenty_5p$effect) - 1) 

# Get standard errors of sum
wmodtwenty_p0_ci <- sum(exp(wmodtwenty_0p$felm.se.fit) - 1)
wmodtwenty_p1_ci <- sum(exp(wmodtwenty_1p$felm.se.fit) - 1)
wmodtwenty_p2_ci <- sum(exp(wmodtwenty_2p$felm.se.fit) - 1)
wmodtwenty_p3_ci <- sum(exp(wmodtwenty_3p$felm.se.fit) - 1)
wmodtwenty_p4_ci <- sum(exp(wmodtwenty_4p$felm.se.fit) - 1)
wmodtwenty_p5_ci <- sum(exp(wmodtwenty_5p$felm.se.fit) - 1)

wmodtwenty_p0_se <- as.numeric(unlist(exp(wmodtwenty_0p$felm.se.fit))) - 1
wmodtwenty_p1_se <- as.numeric(unlist(exp(wmodtwenty_1p$felm.se.fit))) - 1
wmodtwenty_p2_se <- as.numeric(unlist(exp(wmodtwenty_2p$felm.se.fit))) - 1
wmodtwenty_p3_se <- as.numeric(unlist(exp(wmodtwenty_3p$felm.se.fit))) - 1
wmodtwenty_p4_se <- as.numeric(unlist(exp(wmodtwenty_4p$felm.se.fit))) - 1
wmodtwenty_p5_se <- as.numeric(unlist(exp(wmodtwenty_5p$felm.se.fit))) - 1

wmodtwenty_p0_fit <- exp(wmodtwenty_0p$fit + wmodtwenty_0p$res + wmodtwenty_0p$effect) - 1
wmodtwenty_p1_fit <- exp(wmodtwenty_1p$fit + wmodtwenty_1p$res + wmodtwenty_1p$effect) - 1
wmodtwenty_p2_fit <- exp(wmodtwenty_2p$fit + wmodtwenty_2p$res + wmodtwenty_2p$effect) - 1
wmodtwenty_p3_fit <- exp(wmodtwenty_3p$fit + wmodtwenty_3p$res + wmodtwenty_3p$effect) - 1
wmodtwenty_p4_fit <- exp(wmodtwenty_4p$fit + wmodtwenty_4p$res + wmodtwenty_4p$effect) - 1
wmodtwenty_p5_fit <- exp(wmodtwenty_5p$fit + wmodtwenty_5p$res + wmodtwenty_5p$effect) - 1


pdat_wmodtwenty <- data.frame(effect = rep(c("Weather-effect")),
                        interval = "11-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(wmodtwenty_0p$sum, wmodtwenty_1p$sum, wmodtwenty_2p$sum, wmodtwenty_3p$sum, wmodtwenty_4p$sum, wmodtwenty_5p$sum),
                   ci = c(wmodtwenty_p0_ci, wmodtwenty_p1_ci, wmodtwenty_p2_ci, wmodtwenty_p3_ci, wmodtwenty_p4_ci, wmodtwenty_p5_ci))
head(pdat_wmodtwenty)

#---------------------------------------------------------------------------------
# 30-year
# Get predictions for weather conditional on climate (restrict terms to weather)
wmodthirty_0p <- predictFelm(modthirty, newdata = cropdat, var.terms = wterms)
wmodthirty_1p <- predictFelm(modthirty, newdata = p1, var.terms = wterms)
wmodthirty_2p <- predictFelm(modthirty, newdata = p2, var.terms = wterms)
wmodthirty_3p <- predictFelm(modthirty, newdata = p3, var.terms = wterms)
wmodthirty_4p <- predictFelm(modthirty, newdata = p4, var.terms = wterms)
wmodthirty_5p <- predictFelm(modthirty, newdata = p5, var.terms = wterms)

# Total predicted revenue per acre
wmodthirty_0p$sum <- sum(exp(wmodthirty_0p$fit + wmodthirty_0p$res + wmodthirty_0p$effect) - 1)
wmodthirty_1p$sum <- sum(exp(wmodthirty_1p$fit + wmodthirty_1p$res + wmodthirty_1p$effect) - 1)
wmodthirty_2p$sum <- sum(exp(wmodthirty_2p$fit + wmodthirty_2p$res + wmodthirty_2p$effect) - 1) 
wmodthirty_3p$sum <- sum(exp(wmodthirty_3p$fit + wmodthirty_3p$res + wmodthirty_3p$effect) - 1) 
wmodthirty_4p$sum <- sum(exp(wmodthirty_4p$fit + wmodthirty_4p$res + wmodthirty_4p$effect) - 1) 
wmodthirty_5p$sum <- sum(exp(wmodthirty_5p$fit + wmodthirty_5p$res + wmodthirty_5p$effect) - 1) 

# Get standard errors of sum
wmodthirty_p0_ci <- sum(exp(wmodthirty_0p$felm.se.fit) - 1)
wmodthirty_p1_ci <- sum(exp(wmodthirty_1p$felm.se.fit) - 1)
wmodthirty_p2_ci <- sum(exp(wmodthirty_2p$felm.se.fit) - 1)
wmodthirty_p3_ci <- sum(exp(wmodthirty_3p$felm.se.fit) - 1)
wmodthirty_p4_ci <- sum(exp(wmodthirty_4p$felm.se.fit) - 1)
wmodthirty_p5_ci <- sum(exp(wmodthirty_5p$felm.se.fit) - 1)

wmodthirty_p0_se <- as.numeric(unlist(exp(wmodthirty_0p$felm.se.fit))) - 1
wmodthirty_p1_se <- as.numeric(unlist(exp(wmodthirty_1p$felm.se.fit))) - 1
wmodthirty_p2_se <- as.numeric(unlist(exp(wmodthirty_2p$felm.se.fit))) - 1
wmodthirty_p3_se <- as.numeric(unlist(exp(wmodthirty_3p$felm.se.fit))) - 1
wmodthirty_p4_se <- as.numeric(unlist(exp(wmodthirty_4p$felm.se.fit))) - 1
wmodthirty_p5_se <- as.numeric(unlist(exp(wmodthirty_5p$felm.se.fit))) - 1

wmodthirty_p0_fit <- exp(wmodthirty_0p$fit + wmodthirty_0p$res + wmodthirty_0p$effect) - 1
wmodthirty_p1_fit <- exp(wmodthirty_1p$fit + wmodthirty_1p$res + wmodthirty_1p$effect) - 1
wmodthirty_p2_fit <- exp(wmodthirty_2p$fit + wmodthirty_2p$res + wmodthirty_2p$effect) - 1
wmodthirty_p3_fit <- exp(wmodthirty_3p$fit + wmodthirty_3p$res + wmodthirty_3p$effect) - 1
wmodthirty_p4_fit <- exp(wmodthirty_4p$fit + wmodthirty_4p$res + wmodthirty_4p$effect) - 1
wmodthirty_p5_fit <- exp(wmodthirty_5p$fit + wmodthirty_5p$res + wmodthirty_5p$effect) - 1


pdat_wmodthirty <- data.frame(effect = rep(c("Weather-effect")),
                        interval = "12-year",
                   temp = rep(c(0, 1, 2, 3, 4, 5)),
                   sum = c(wmodthirty_0p$sum, wmodthirty_1p$sum, wmodthirty_2p$sum, wmodthirty_3p$sum, wmodthirty_4p$sum, wmodthirty_5p$sum),
                   ci = c(wmodthirty_p0_ci, wmodthirty_p1_ci, wmodthirty_p2_ci, wmodthirty_p3_ci, wmodthirty_p4_ci, wmodthirty_p5_ci))
head(pdat_wmodthirty)



#-----------------------------------------------------------------------------
# Aggregate data

# Predicted Effects
adat <- data.frame(effect = "Weather-climate-effect", 
                   interval = rep(c("10-year", "11-year", "12-year"), each = length(wcmodten_p0_fit)*6),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 3, each = length(wcmodten_p0_fit)),
                   rev.pred = c(wcmodten_p0_fit, wcmodten_p1_fit, wcmodten_p2_fit, wcmodten_p3_fit, wcmodten_p4_fit, wcmodten_p5_fit, 
                                wcmodtwenty_p0_fit, wcmodtwenty_p1_fit, wcmodtwenty_p2_fit, wcmodtwenty_p3_fit, wcmodtwenty_p4_fit, wcmodtwenty_p5_fit,
                                wcmodthirty_p0_fit, wcmodthirty_p1_fit, wcmodthirty_p2_fit, wcmodthirty_p3_fit, wcmodthirty_p4_fit, wcmodthirty_p5_fit),
                   rev.se = c(wcmodten_p0_se, wcmodten_p1_se, wcmodten_p2_se, wcmodten_p3_se, wcmodten_p4_se, wcmodten_p5_se,
                               wcmodtwenty_p0_se, wcmodtwenty_p1_se, wcmodtwenty_p2_se, wcmodtwenty_p3_se, wcmodtwenty_p4_se, wcmodtwenty_p5_se,
                               wcmodthirty_p0_se, wcmodthirty_p1_se, wcmodthirty_p2_se, wcmodthirty_p3_se, wcmodthirty_p4_se, wcmodthirty_p5_se))

head(adat)

bdat <- data.frame(effect = "Weather-effect", 
                   interval = rep(c("10-year", "11-year", "12-year"), each = length(wmodten_p0_fit)*6),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 3, each = length(wmodten_p0_fit)),
                   rev.pred = c(wmodten_p0_fit, wmodten_p1_fit, wmodten_p2_fit, wmodten_p3_fit, wmodten_p4_fit, wmodten_p5_fit, 
                                wmodtwenty_p0_fit, wmodtwenty_p1_fit, wmodtwenty_p2_fit, wmodtwenty_p3_fit, wmodtwenty_p4_fit, wmodtwenty_p5_fit,
                                wmodthirty_p0_fit, wmodthirty_p1_fit, wmodthirty_p2_fit, wmodthirty_p3_fit, wmodthirty_p4_fit, wmodthirty_p5_fit),
                   rev.se = c(wmodten_p0_se, wmodten_p1_se, wmodten_p2_se, wmodten_p3_se, wmodten_p4_se, wmodten_p5_se,
                               wmodtwenty_p0_se, wmodtwenty_p1_se, wmodtwenty_p2_se, wmodtwenty_p3_se, wmodtwenty_p4_se, wmodtwenty_p5_se,
                               wmodthirty_p0_se, wmodthirty_p1_se, wmodthirty_p2_se, wmodthirty_p3_se, wmodthirty_p4_se, wmodthirty_p5_se))


# bdat <- data.frame(effect = "Weather-effect", 
#                    modten_rev.pred = c(wmodten_p0_fit, wmodten_p1_fit, wmodten_p2_fit, wmodten_p3_fit, wmodten_p4_fit, wmodten_p5_fit),
#                    modtwenty_rev.pred = c(wmodtwenty_p0_fit, wmodtwenty_p1_fit, wmodtwenty_p2_fit, wmodtwenty_p3_fit, wmodtwenty_p4_fit, wmodtwenty_p5_fit),
#                    modthirty_rev.pred = c(wmodthirty_p0_fit, wmodthirty_p1_fit, wmodthirty_p2_fit, wmodthirty_p3_fit, wmodthirty_p4_fit, wmodthirty_p5_fit),
#                    modten_rev.se = c(wmodten_p0_se, wmodten_p1_se, wmodten_p2_se, wmodten_p3_se, wmodten_p4_se, wmodten_p5_se),
#                    modtwenty_rev.se = c(wmodtwenty_p0_se, wmodtwenty_p1_se, wmodtwenty_p2_se, wmodtwenty_p3_se, wmodtwenty_p4_se, wmodtwenty_p5_se),
#                    modthirty_rev.se = c(wmodthirty_p0_se, wmodthirty_p1_se, wmodthirty_p2_se, wmodthirty_p3_se, wmodthirty_p4_se, wmodthirty_p5_se),
#                    temp = rep(c(0, 1, 2, 3, 4, 5), each = length(wmodten_p0_fit)))

head(adat)
head(bdat)
dat <- rbind(adat, bdat)
saveRDS(dat, "data/rev_crop_predictions.rds")
# dat                              
# Predicted percentage change
pdat <- rbind(pdat_wcmodten, pdat_wcmodtwenty, pdat_wcmodthirty, pdat_wmodten, pdat_wmodtwenty, pdat_wmodthirty)
pdat$change_min <- pdat$sum - pdat$ci*1.96
pdat$change_max <- pdat$sum + pdat$ci*1.96
pdat



pdat <- pdat %>% 
  group_by(interval, effect) %>% 
  mutate(change =  (sum/first(sum) - 1),
         change_min = (change_min/first(sum) - 1),
         change_max = (change_max/first(sum) - 1))

pdat
pdat$change <- pdat$change*100
pdat$change_min <- pdat$change_min*100
pdat$change_max <- pdat$change_max*100

saveRDS(pdat, "data/rev_crop_perc_change.rds")
pdat <- readRDS("data/rev_crop_perc_change.rds")

ggplot(pdat, aes(temp, change, group = effect)) + 
  # geom_ribbon(aes(ymax = change_max, ymin = change_min, x = temp), fill = "#C0CCD9", alpha = 0.5 ) +
  geom_line(aes(color = effect)) +
  geom_point(aes(color = effect), size = 0.5) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Revenue per acre") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~interval, ncol = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) 
  

ggsave("figures/rev_crop_predictions.pdf", device = "pdf", width = 6, height = 4)



