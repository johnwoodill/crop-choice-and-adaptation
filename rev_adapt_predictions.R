library(tidyverse)
library(lfe)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

# Load models
pmod1 <- readRDS("models/modfive5.rds")
pmod2 <- readRDS("models/modten5.rds")
pmod3 <- readRDS("models/modtwenty5.rds")
pmod4 <- readRDS("models/modthirty5.rds")
pmod5 <- readRDS("models/modsixty5.rds")

# Load changes in degree day data
p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")

# Get predictions for weather conditional on climate (restrict terms to weather)
w1_0p <- predictFelm(pmod1, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w1_1p <- predictFelm(pmod1, newdata = p1, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w1_2p <- predictFelm(pmod1, newdata = p2, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w1_3p <- predictFelm(pmod1, newdata = p3, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w1_4p <- predictFelm(pmod1, newdata = p4, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w1_5p <- predictFelm(pmod1, newdata = p5, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))

w2_0p <- predictFelm(pmod2, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w2_1p <- predictFelm(pmod2, newdata = p1, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w2_2p <- predictFelm(pmod2, newdata = p2, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w2_3p <- predictFelm(pmod2, newdata = p3, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w2_4p <- predictFelm(pmod2, newdata = p4, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w2_5p <- predictFelm(pmod2, newdata = p5, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))

w3_0p <- predictFelm(pmod3, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w3_1p <- predictFelm(pmod3, newdata = p1, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w3_2p <- predictFelm(pmod3, newdata = p2, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w3_3p <- predictFelm(pmod3, newdata = p3, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w3_4p <- predictFelm(pmod3, newdata = p4, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w3_5p <- predictFelm(pmod3, newdata = p5, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))

w4_0p <- predictFelm(pmod4, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w4_1p <- predictFelm(pmod4, newdata = p1, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w4_2p <- predictFelm(pmod4, newdata = p2, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w4_3p <- predictFelm(pmod4, newdata = p3, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w4_4p <- predictFelm(pmod4, newdata = p4, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))
w4_5p <- predictFelm(pmod4, newdata = p5, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq"))

# Total predicted revenue per acre
wa0 <- sum(exp(w1_0p$fit + w1_0p$res + w1_0p$effect))
wa1 <- sum(exp(w1_1p$fit + w1_1p$res + w1_1p$effect))
wa2 <- sum(exp(w1_2p$fit + w1_2p$res + w1_2p$effect)) 
wa3 <- sum(exp(w1_3p$fit + w1_3p$res + w1_3p$effect)) 
wa4 <- sum(exp(w1_4p$fit + w1_4p$res + w1_4p$effect)) 
wa5 <- sum(exp(w1_5p$fit + w1_5p$res + w1_5p$effect)) 

wb0 <- sum(exp(w2_0p$fit + w2_0p$res + w2_0p$effect))
wb1 <- sum(exp(w2_1p$fit + w2_1p$res + w2_1p$effect)) 
wb2 <- sum(exp(w2_2p$fit + w2_2p$res + w2_2p$effect)) 
wb3 <- sum(exp(w2_3p$fit + w2_3p$res + w2_3p$effect)) 
wb4 <- sum(exp(w2_4p$fit + w2_4p$res + w2_4p$effect)) 
wb5 <- sum(exp(w2_5p$fit + w2_5p$res + w2_5p$effect)) 

wc0 <- sum(exp(w3_0p$fit + w3_0p$res + w3_0p$effect))
wc1 <- sum(exp(w3_1p$fit + w3_1p$res + w3_1p$effect)) 
wc2 <- sum(exp(w3_2p$fit + w3_2p$res + w3_2p$effect)) 
wc3 <- sum(exp(w3_3p$fit + w3_3p$res + w3_3p$effect)) 
wc4 <- sum(exp(w3_4p$fit + w3_4p$res + w3_4p$effect)) 
wc5 <- sum(exp(w3_5p$fit + w3_5p$res + w3_5p$effect)) 

wd0 <- sum(exp(w4_0p$fit + w4_0p$res + w4_0p$effect))
wd1 <- sum(exp(w4_1p$fit + w4_1p$res + w4_1p$effect)) 
wd2 <- sum(exp(w4_2p$fit + w4_2p$res + w4_2p$effect)) 
wd3 <- sum(exp(w4_3p$fit + w4_3p$res + w4_3p$effect)) 
wd4 <- sum(exp(w4_4p$fit + w4_4p$res + w4_4p$effect)) 
wd5 <- sum(exp(w4_5p$fit + w4_5p$res + w4_5p$effect)) 

# Get standard errors of sum
wa0_ci <- sum(w1_0p$se.fit)*1.96
wa1_ci <- sum(w1_1p$se.fit)*1.96
wa2_ci <- sum(w1_2p$se.fit)*1.96
wa3_ci <- sum(w1_3p$se.fit)*1.96
wa4_ci <- sum(w1_4p$se.fit)*1.96
wa5_ci <- sum(w1_5p$se.fit)*1.96

wb0_ci <- sum(w2_0p$se.fit)*1.96
wb1_ci <- sum(w2_1p$se.fit)*1.96
wb2_ci <- sum(w2_2p$se.fit)*1.96
wb3_ci <- sum(w2_3p$se.fit)*1.96
wb4_ci <- sum(w2_4p$se.fit)*1.96
wb5_ci <- sum(w2_5p$se.fit)*1.96

wc0_ci <- sum(w3_0p$se.fit)*1.96
wc1_ci <- sum(w3_1p$se.fit)*1.96
wc2_ci <- sum(w3_2p$se.fit)*1.96
wc3_ci <- sum(w3_3p$se.fit)*1.96
wc4_ci <- sum(w3_4p$se.fit)*1.96
wc5_ci <- sum(w3_5p$se.fit)*1.96

wd0_ci <- sum(w4_0p$se.fit)*1.96
wd1_ci <- sum(w4_1p$se.fit)*1.96
wd2_ci <- sum(w4_2p$se.fit)*1.96
wd3_ci <- sum(w4_3p$se.fit)*1.96
wd4_ci <- sum(w4_4p$se.fit)*1.96
wd5_ci <- sum(w4_5p$se.fit)*1.96


# Get predictions for climate conditional on weather (restrict terms to climate)
c1_0p <- predictFelm(pmod1, terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five"))
c1_1p <- predictFelm(pmod1, newdata = p1, terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five"))
c1_2p <- predictFelm(pmod1, newdata = p2, terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five"))
c1_3p <- predictFelm(pmod1, newdata = p3, terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five"))
c1_4p <- predictFelm(pmod1, newdata = p4, terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five"))
c1_5p <- predictFelm(pmod1, newdata = p5, terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five"))

c2_0p <- predictFelm(pmod2, terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten"))
c2_1p <- predictFelm(pmod2, newdata = p1, terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten"))
c2_2p <- predictFelm(pmod2, newdata = p2, terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten"))
c2_3p <- predictFelm(pmod2, newdata = p3, terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten"))
c2_4p <- predictFelm(pmod2, newdata = p4, terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten"))
c2_5p <- predictFelm(pmod2, newdata = p5, terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten"))

c3_0p <- predictFelm(pmod3, terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty"))
c3_1p <- predictFelm(pmod3, newdata = p1, terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty"))
c3_2p <- predictFelm(pmod3, newdata = p2, terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty"))
c3_3p <- predictFelm(pmod3, newdata = p3, terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty"))
c3_4p <- predictFelm(pmod3, newdata = p4, terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty"))
c3_5p <- predictFelm(pmod3, newdata = p5, terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty"))

c4_0p <- predictFelm(pmod4, terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty"))
c4_1p <- predictFelm(pmod4, newdata = p1, terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty"))
c4_2p <- predictFelm(pmod4, newdata = p2, terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty"))
c4_3p <- predictFelm(pmod4, newdata = p3, terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty"))
c4_4p <- predictFelm(pmod4, newdata = p4, terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty"))
c4_5p <- predictFelm(pmod4, newdata = p5, terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty"))

c5_0p <- predictFelm(pmod5, terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty"))
c5_1p <- predictFelm(pmod5, newdata = p1, terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty"))
c5_2p <- predictFelm(pmod5, newdata = p2, terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty"))
c5_3p <- predictFelm(pmod5, newdata = p3, terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty"))
c5_4p <- predictFelm(pmod5, newdata = p4, terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty"))
c5_5p <- predictFelm(pmod5, newdata = p5, terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty"))

# Total predicted revenue per acre
ca0 <- sum(exp(c1_0p$fit + c1_0p$res + c1_0p$effect))
ca1 <- sum(exp(c1_1p$fit + c1_1p$res + c1_1p$effect)) 
ca2 <- sum(exp(c1_2p$fit + c1_2p$res + c1_2p$effect)) 
ca3 <- sum(exp(c1_3p$fit + c1_3p$res + c1_3p$effect)) 
ca4 <- sum(exp(c1_4p$fit + c1_4p$res + c1_4p$effect)) 
ca5 <- sum(exp(c1_5p$fit + c1_5p$res + c1_5p$effect))

cb0 <- sum(exp(c2_0p$fit + c2_0p$res + c2_0p$effect))
cb1 <- sum(exp(c2_1p$fit + c2_1p$res + c2_1p$effect)) 
cb2 <- sum(exp(c2_2p$fit + c2_2p$res + c2_2p$effect)) 
cb3 <- sum(exp(c2_3p$fit + c2_3p$res + c2_3p$effect)) 
cb4 <- sum(exp(c2_4p$fit + c2_4p$res + c2_4p$effect)) 
cb5 <- sum(exp(c2_5p$fit + c2_5p$res + c2_5p$effect))

cc0 <- sum(exp(c3_0p$fit + c3_0p$res + c3_0p$effect))
cc1 <- sum(exp(c3_1p$fit + c3_1p$res + c3_1p$effect)) 
cc2 <- sum(exp(c3_2p$fit + c3_2p$res + c3_2p$effect)) 
cc3 <- sum(exp(c3_3p$fit + c3_3p$res + c3_3p$effect)) 
cc4 <- sum(exp(c3_4p$fit + c3_4p$res + c3_4p$effect)) 
cc5 <- sum(exp(c3_5p$fit + c3_5p$res + c3_5p$effect))

cd0 <- sum(exp(c4_0p$fit + c4_0p$res + c4_0p$effect))
cd1 <- sum(exp(c4_1p$fit + c4_1p$res + c4_1p$effect)) 
cd2 <- sum(exp(c4_2p$fit + c4_2p$res + c4_2p$effect)) 
cd3 <- sum(exp(c4_3p$fit + c4_3p$res + c4_3p$effect)) 
cd4 <- sum(exp(c4_4p$fit + c4_4p$res + c4_4p$effect)) 
cd5 <- sum(exp(c4_5p$fit + c4_5p$res + c4_5p$effect))

ce0 <- sum(exp(c5_0p$fit + c5_0p$res + c5_0p$effect))
ce1 <- sum(exp(c5_1p$fit + c5_1p$res + c5_1p$effect)) 
ce2 <- sum(exp(c5_2p$fit + c5_2p$res + c5_2p$effect)) 
ce3 <- sum(exp(c5_3p$fit + c5_3p$res + c5_3p$effect)) 
ce4 <- sum(exp(c5_4p$fit + c5_4p$res + c5_4p$effect)) 
ce5 <- sum(exp(c5_5p$fit + c5_5p$res + c5_5p$effect))

# Get standard errors of sum
ca0_ci <- sum(c1_0p$se.fit)*1.96
ca1_ci <- sum(c1_1p$se.fit)*1.96
ca2_ci <- sum(c1_2p$se.fit)*1.96
ca3_ci <- sum(c1_3p$se.fit)*1.96
ca4_ci <- sum(c1_4p$se.fit)*1.96
ca5_ci <- sum(c1_5p$se.fit)*1.96

cb0_ci <- sum(c2_0p$se.fit)*1.96
cb1_ci <- sum(c2_1p$se.fit)*1.96
cb2_ci <- sum(c2_2p$se.fit)*1.96
cb3_ci <- sum(c2_3p$se.fit)*1.96
cb4_ci <- sum(c2_4p$se.fit)*1.96
cb5_ci <- sum(c2_5p$se.fit)*1.96

cc0_ci <- sum(c3_0p$se.fit)*1.96
cc1_ci <- sum(c3_1p$se.fit)*1.96
cc2_ci <- sum(c3_2p$se.fit)*1.96
cc3_ci <- sum(c3_3p$se.fit)*1.96
cc4_ci <- sum(c3_4p$se.fit)*1.96
cc5_ci <- sum(c3_5p$se.fit)*1.96

cd0_ci <- sum(c4_0p$se.fit)*1.96
cd1_ci <- sum(c4_1p$se.fit)*1.96
cd2_ci <- sum(c4_2p$se.fit)*1.96
cd3_ci <- sum(c4_3p$se.fit)*1.96
cd4_ci <- sum(c4_4p$se.fit)*1.96
cd5_ci <- sum(c4_5p$se.fit)*1.96

ce0_ci <- sum(c5_0p$se.fit)*1.96
ce1_ci <- sum(c5_1p$se.fit)*1.96
ce2_ci <- sum(c5_2p$se.fit)*1.96
ce3_ci <- sum(c5_3p$se.fit)*1.96
ce4_ci <- sum(c5_4p$se.fit)*1.96
ce5_ci <- sum(c5_5p$se.fit)*1.96




pdat <- data.frame(effect = rep(c("Weather-effect", "Climate-effect"), each = 30),
                   model = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), 2, each = 6),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 10),
                   sum = c(wa0, wa1, wa2, wa3, wa4, wa5,
                           wb0, wb1, wb2, wb3, wb4, wb5,
                           wc0, wc1, wc2, wc3, wc4, wc5,
                           wd0, wd1, wd2, wd3, wd4, wd5,
                           ce0, ce1, ce2, ce3, ce4, ce5, # Same
                           ca0, ca1, ca2, ca3, ca4, ca5,
                           cb0, cb1, cb2, cb3, cb4, cb5,
                           cc0, cc1, cc2, cc3, cc4, cc5,
                           cd0, cd1, cd2, cd3, cd4, cd5,
                           ce0, ce1, ce2, ce3, ce4, ce5), # Same
                   ci = c(wa0_ci, wa1_ci, wa2_ci, wa3_ci, wa4_ci, wa5_ci,
                           wb0_ci, wb1_ci, wb2_ci, wb3_ci, wb4_ci, wb5_ci,
                           wc0_ci, wc1_ci, wc2_ci, wc3_ci, wc4_ci, wc5_ci,
                           wd0_ci, wd1_ci, wd2_ci, wd3_ci, wd4_ci, wd5_ci,
                           ce0_ci, ce1_ci, ce2_ci, ce3, ce4_ci, ce5_ci,
                          ca0_ci, ca1_ci, ca2_ci, ca3_ci, ca4_ci, ca5_ci,
                           cb0_ci, cb1_ci, cb2_ci, cb3_ci, cb4_ci, cb5_ci,
                           cc0_ci, cc1_ci, cc2_ci, cc3_ci, cc4_ci, cc5_ci,
                           cd0_ci, cd1_ci, cd2_ci, cd3_ci, cd4_ci, cd5_ci,
                           ce0_ci, ce1_ci, ce2_ci, ce3, ce4_ci, ce5_ci))
pdat
pdat <- filter(pdat, model != "60-year")
pdat
pdat <- pdat %>% 
  group_by(effect, model) %>% 
  mutate(change =  (sum - first(sum))/first(sum),
         change_min = ((sum - ci) - (first(sum) - first(ci)))/(first(sum) - first(ci)),
         change_max = ((sum + ci) - (first(sum) + first(ci)))/(first(sum) + first(ci)))
pdat
pdat$change <- pdat$change*100
pdat$change_min <- pdat$change_min*100
pdat$change_max <- pdat$change_max*100

pdat$model <- factor(pdat$model, labels = c("5-year", "10-year", "20-year", "30-year"))

ggplot(pdat, aes(temp, change)) + 
  geom_ribbon(aes(ymax = change_max, ymin = change_min, x = temp, fill = effect, group = effect), fill = "grey", alpha = 0.5 ) +
  geom_line(aes(color = effect)) +
  #geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
  theme_tufte(base_size = 12) +
  ylab("% Change in Revenue per acre") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = c(.85,1), 
       legend.justification = c("left", "top"), 
       legend.box.background = element_rect(colour = "grey"), 
       legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~model) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
  

#plot(c(c1, c2, c3, c4, c5))
#plot(c(ld1, ld2, ld3, ld4, ld5))
#plot(c(p1, p2, p3, p4, p5))


