library(tidyverse)
library(lfe)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat$trend_sq <- cropdat$trend^2

# Load models
pmod1 <- readRDS("models/modfive_wheat.rds")
pmod2 <- readRDS("models/modten_wheat.rds")
pmod3 <- readRDS("models/modtwenty_wheat.rds")
pmod4 <- readRDS("models/modthirty_wheat.rds")
pmod5 <- readRDS("models/modsixty_wheat.rds")


# Load changes in degree day data
p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")


cons.terms_w <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq")
cons.terms_c_five <- c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five")
cons.terms_c_ten <- c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten")
cons.terms_c_twenty <- c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty")
cons.terms_c_thirty <- c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty")

w_terms <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq")



# Get predictions for weather conditional on climate (restrict terms to weather)
w1_0p <- predictFelm(pmod1, var.terms= w_terms, cons.terms = cons.terms_c_five)
w1_1p <- predictFelm(pmod1, newdata = p1, var.terms= w_terms, cons.terms = cons.terms_c_five)
w1_2p <- predictFelm(pmod1, newdata = p2, var.terms= w_terms, cons.terms = cons.terms_c_five)
w1_3p <- predictFelm(pmod1, newdata = p3, var.terms= w_terms, cons.terms = cons.terms_c_five)
w1_4p <- predictFelm(pmod1, newdata = p4, var.terms= w_terms, cons.terms = cons.terms_c_five)
w1_5p <- predictFelm(pmod1, newdata = p5, var.terms= w_terms, cons.terms = cons.terms_c_five)

w2_0p <- predictFelm(pmod2, var.terms= w_terms, cons.terms = cons.terms_c_ten)
w2_1p <- predictFelm(pmod2, newdata = p1, var.terms= w_terms, cons.terms = cons.terms_c_ten)
w2_2p <- predictFelm(pmod2, newdata = p2, var.terms= w_terms, cons.terms = cons.terms_c_ten)
w2_3p <- predictFelm(pmod2, newdata = p3, var.terms= w_terms, cons.terms = cons.terms_c_ten)
w2_4p <- predictFelm(pmod2, newdata = p4, var.terms= w_terms, cons.terms = cons.terms_c_ten)
w2_5p <- predictFelm(pmod2, newdata = p5, var.terms= w_terms, cons.terms = cons.terms_c_ten)

w3_0p <- predictFelm(pmod3, var.terms= w_terms, cons.terms = cons.terms_c_twenty)
w3_1p <- predictFelm(pmod3, newdata = p1, var.terms= w_terms, cons.terms = cons.terms_c_twenty)
w3_2p <- predictFelm(pmod3, newdata = p2, var.terms= w_terms, cons.terms = cons.terms_c_twenty)
w3_3p <- predictFelm(pmod3, newdata = p3, var.terms= w_terms, cons.terms = cons.terms_c_twenty)
w3_4p <- predictFelm(pmod3, newdata = p4, var.terms= w_terms, cons.terms = cons.terms_c_twenty)
w3_5p <- predictFelm(pmod3, newdata = p5, var.terms= w_terms, cons.terms = cons.terms_c_twenty)

w4_0p <- predictFelm(pmod4, var.terms= w_terms, cons.terms = cons.terms_c_thirty)
w4_1p <- predictFelm(pmod4, newdata = p1, var.terms= w_terms, cons.terms = cons.terms_c_thirty)
w4_2p <- predictFelm(pmod4, newdata = p2, var.terms= w_terms, cons.terms = cons.terms_c_thirty)
w4_3p <- predictFelm(pmod4, newdata = p3, var.terms= w_terms, cons.terms = cons.terms_c_thirty)
w4_4p <- predictFelm(pmod4, newdata = p4, var.terms= w_terms, cons.terms = cons.terms_c_thirty)
w4_5p <- predictFelm(pmod4, newdata = p5, var.terms= w_terms, cons.terms = cons.terms_c_thirty)

# Total predicted revenue per acre
wa0 <- sum(exp(w1_0p$fit + w1_0p$res + w1_0p$effect) - 1)
wa1 <- sum(exp(w1_1p$fit + w1_1p$res + w1_1p$effect) - 1)
wa2 <- sum(exp(w1_2p$fit + w1_2p$res + w1_2p$effect) - 1) 
wa3 <- sum(exp(w1_3p$fit + w1_3p$res + w1_3p$effect) - 1) 
wa4 <- sum(exp(w1_4p$fit + w1_4p$res + w1_4p$effect) - 1) 
wa5 <- sum(exp(w1_5p$fit + w1_5p$res + w1_5p$effect) - 1) 

wb0 <- sum(exp(w2_0p$fit + w2_0p$res + w2_0p$effect) - 1)
wb1 <- sum(exp(w2_1p$fit + w2_1p$res + w2_1p$effect) - 1) 
wb2 <- sum(exp(w2_2p$fit + w2_2p$res + w2_2p$effect) - 1) 
wb3 <- sum(exp(w2_3p$fit + w2_3p$res + w2_3p$effect) - 1) 
wb4 <- sum(exp(w2_4p$fit + w2_4p$res + w2_4p$effect) - 1) 
wb5 <- sum(exp(w2_5p$fit + w2_5p$res + w2_5p$effect) - 1) 

wc0 <- sum(exp(w3_0p$fit + w3_0p$res + w3_0p$effect) - 1)
wc1 <- sum(exp(w3_1p$fit + w3_1p$res + w3_1p$effect) - 1) 
wc2 <- sum(exp(w3_2p$fit + w3_2p$res + w3_2p$effect) - 1) 
wc3 <- sum(exp(w3_3p$fit + w3_3p$res + w3_3p$effect) - 1) 
wc4 <- sum(exp(w3_4p$fit + w3_4p$res + w3_4p$effect) - 1) 
wc5 <- sum(exp(w3_5p$fit + w3_5p$res + w3_5p$effect) - 1) 

wd0 <- sum(exp(w4_0p$fit + w4_0p$res + w4_0p$effect) - 1)
wd1 <- sum(exp(w4_1p$fit + w4_1p$res + w4_1p$effect) - 1) 
wd2 <- sum(exp(w4_2p$fit + w4_2p$res + w4_2p$effect) - 1) 
wd3 <- sum(exp(w4_3p$fit + w4_3p$res + w4_3p$effect) - 1) 
wd4 <- sum(exp(w4_4p$fit + w4_4p$res + w4_4p$effect) - 1) 
wd5 <- sum(exp(w4_5p$fit + w4_5p$res + w4_5p$effect) - 1) 

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

c_terms_five <- c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five")

c_terms_ten <- c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten")

c_terms_twenty <- c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty")

c_terms_thirty <- c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty")

c_terms_sixty <- c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty")

# Get predictions for climate conditional on weather (restrict terms to climate)
c1_0p <- predictFelm(pmod1, var.terms= c_terms_five, cons.terms = cons.terms_w)
c1_1p <- predictFelm(pmod1, newdata = p1, var.terms= c_terms_five, cons.terms = cons.terms_w)
c1_2p <- predictFelm(pmod1, newdata = p2, var.terms= c_terms_five, cons.terms = cons.terms_w)
c1_3p <- predictFelm(pmod1, newdata = p3, var.terms= c_terms_five, cons.terms = cons.terms_w)
c1_4p <- predictFelm(pmod1, newdata = p4, var.terms= c_terms_five, cons.terms = cons.terms_w)
c1_5p <- predictFelm(pmod1, newdata = p5, var.terms= c_terms_five, cons.terms = cons.terms_w)

c2_0p <- predictFelm(pmod2, var.terms= c_terms_ten, cons.terms = cons.terms_w)
c2_1p <- predictFelm(pmod2, newdata = p1, var.terms= c_terms_ten, cons.terms = cons.terms_w)
c2_2p <- predictFelm(pmod2, newdata = p2, var.terms= c_terms_ten, cons.terms = cons.terms_w)
c2_3p <- predictFelm(pmod2, newdata = p3, var.terms= c_terms_ten, cons.terms = cons.terms_w)
c2_4p <- predictFelm(pmod2, newdata = p4, var.terms= c_terms_ten, cons.terms = cons.terms_w)
c2_5p <- predictFelm(pmod2, newdata = p5, var.terms= c_terms_ten, cons.terms = cons.terms_w)

c3_0p <- predictFelm(pmod3, var.terms= c_terms_twenty, cons.terms = cons.terms_w)
c3_1p <- predictFelm(pmod3, newdata = p1, var.terms= c_terms_twenty, cons.terms = cons.terms_w)
c3_2p <- predictFelm(pmod3, newdata = p2, var.terms= c_terms_twenty, cons.terms = cons.terms_w)
c3_3p <- predictFelm(pmod3, newdata = p3, var.terms= c_terms_twenty, cons.terms = cons.terms_w)
c3_4p <- predictFelm(pmod3, newdata = p4, var.terms= c_terms_twenty, cons.terms = cons.terms_w)
c3_5p <- predictFelm(pmod3, newdata = p5, var.terms= c_terms_twenty, cons.terms = cons.terms_w)

c4_0p <- predictFelm(pmod4, var.terms= c_terms_thirty, cons.terms = cons.terms_w)
c4_1p <- predictFelm(pmod4, newdata = p1, var.terms= c_terms_thirty, cons.terms = cons.terms_w)
c4_2p <- predictFelm(pmod4, newdata = p2, var.terms= c_terms_thirty, cons.terms = cons.terms_w)
c4_3p <- predictFelm(pmod4, newdata = p3, var.terms= c_terms_thirty, cons.terms = cons.terms_w)
c4_4p <- predictFelm(pmod4, newdata = p4, var.terms= c_terms_thirty, cons.terms = cons.terms_w)
c4_5p <- predictFelm(pmod4, newdata = p5, var.terms= c_terms_thirty, cons.terms = cons.terms_w)

c5_0p <- predictFelm(pmod5, var.terms= c_terms_sixty)
c5_1p <- predictFelm(pmod5, newdata = p1, var.terms= c_terms_sixty)
c5_2p <- predictFelm(pmod5, newdata = p2, var.terms= c_terms_sixty)
c5_3p <- predictFelm(pmod5, newdata = p3, var.terms= c_terms_sixty)
c5_4p <- predictFelm(pmod5, newdata = p4, var.terms= c_terms_sixty)
c5_5p <- predictFelm(pmod5, newdata = p5, var.terms= c_terms_sixty)

# Total predicted revenue per acre
ca0 <- sum(exp(c1_0p$fit + c1_0p$res + c1_0p$effect) - 1)
ca1 <- sum(exp(c1_1p$fit + c1_1p$res + c1_1p$effect) - 1) 
ca2 <- sum(exp(c1_2p$fit + c1_2p$res + c1_2p$effect) - 1) 
ca3 <- sum(exp(c1_3p$fit + c1_3p$res + c1_3p$effect) - 1) 
ca4 <- sum(exp(c1_4p$fit + c1_4p$res + c1_4p$effect) - 1) 
ca5 <- sum(exp(c1_5p$fit + c1_5p$res + c1_5p$effect) - 1)

cb0 <- sum(exp(c2_0p$fit + c2_0p$res + c2_0p$effect) - 1)
cb1 <- sum(exp(c2_1p$fit + c2_1p$res + c2_1p$effect) - 1) 
cb2 <- sum(exp(c2_2p$fit + c2_2p$res + c2_2p$effect) - 1) 
cb3 <- sum(exp(c2_3p$fit + c2_3p$res + c2_3p$effect) - 1) 
cb4 <- sum(exp(c2_4p$fit + c2_4p$res + c2_4p$effect) - 1) 
cb5 <- sum(exp(c2_5p$fit + c2_5p$res + c2_5p$effect) - 1)

cc0 <- sum(exp(c3_0p$fit + c3_0p$res + c3_0p$effect) - 1)
cc1 <- sum(exp(c3_1p$fit + c3_1p$res + c3_1p$effect) - 1) 
cc2 <- sum(exp(c3_2p$fit + c3_2p$res + c3_2p$effect) - 1) 
cc3 <- sum(exp(c3_3p$fit + c3_3p$res + c3_3p$effect) - 1) 
cc4 <- sum(exp(c3_4p$fit + c3_4p$res + c3_4p$effect) - 1) 
cc5 <- sum(exp(c3_5p$fit + c3_5p$res + c3_5p$effect) - 1)

cd0 <- sum(exp(c4_0p$fit + c4_0p$res + c4_0p$effect) - 1)
cd1 <- sum(exp(c4_1p$fit + c4_1p$res + c4_1p$effect) - 1) 
cd2 <- sum(exp(c4_2p$fit + c4_2p$res + c4_2p$effect) - 1) 
cd3 <- sum(exp(c4_3p$fit + c4_3p$res + c4_3p$effect) - 1) 
cd4 <- sum(exp(c4_4p$fit + c4_4p$res + c4_4p$effect) - 1) 
cd5 <- sum(exp(c4_5p$fit + c4_5p$res + c4_5p$effect) - 1)

ce0 <- sum(exp(c5_0p$fit + c5_0p$res + c5_0p$effect) - 1)
ce1 <- sum(exp(c5_1p$fit + c5_1p$res + c5_1p$effect) - 1) 
ce2 <- sum(exp(c5_2p$fit + c5_2p$res + c5_2p$effect) - 1) 
ce3 <- sum(exp(c5_3p$fit + c5_3p$res + c5_3p$effect) - 1) 
ce4 <- sum(exp(c5_4p$fit + c5_4p$res + c5_4p$effect) - 1) 
ce5 <- sum(exp(c5_5p$fit + c5_5p$res + c5_5p$effect) - 1)

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

#-----------------------------------------------------------------
# Total Effect


t_terms_five <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five")

t_terms_ten <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq","dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten")

t_terms_twenty <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq","dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty")

t_terms_thirty <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq","dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty")

t_terms_sixty <- c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty")

t1_0p <- predictFelm(felm.fit = pmod1, var.terms = t_terms_five)
t1_1p <- predictFelm(pmod1, newdata = p1, var.terms = t_terms_five)
t1_2p <- predictFelm(pmod1, newdata = p2, var.terms = t_terms_five)
t1_3p <- predictFelm(pmod1, newdata = p3, var.terms = t_terms_five)
t1_4p <- predictFelm(pmod1, newdata = p4, var.terms = t_terms_five)
t1_5p <- predictFelm(pmod1, newdata = p5, var.terms = t_terms_five)

t2_0p <- predictFelm(pmod2, var.terms = t_terms_ten)
t2_1p <- predictFelm(pmod2, newdata = p1, var.terms = t_terms_ten)
t2_2p <- predictFelm(pmod2, newdata = p2, var.terms = t_terms_ten)
t2_3p <- predictFelm(pmod2, newdata = p3, var.terms = t_terms_ten)
t2_4p <- predictFelm(pmod2, newdata = p4, var.terms = t_terms_ten)
t2_5p <- predictFelm(pmod2, newdata = p5, var.terms = t_terms_ten)

t3_0p <- predictFelm(pmod3, var.terms = t_terms_twenty)
t3_1p <- predictFelm(pmod3, newdata = p1, var.terms = t_terms_twenty)
t3_2p <- predictFelm(pmod3, newdata = p2, var.terms = t_terms_twenty)
t3_3p <- predictFelm(pmod3, newdata = p3, var.terms = t_terms_twenty)
t3_4p <- predictFelm(pmod3, newdata = p4, var.terms = t_terms_twenty)
t3_5p <- predictFelm(pmod3, newdata = p5, var.terms = t_terms_twenty)

t4_0p <- predictFelm(pmod4, var.terms = t_terms_thirty)
t4_1p <- predictFelm(pmod4, newdata = p1, var.terms = t_terms_thirty)
t4_2p <- predictFelm(pmod4, newdata = p2, var.terms = t_terms_thirty)
t4_3p <- predictFelm(pmod4, newdata = p3, var.terms = t_terms_thirty)
t4_4p <- predictFelm(pmod4, newdata = p4, var.terms = t_terms_thirty)
t4_5p <- predictFelm(pmod4, newdata = p5, var.terms = t_terms_thirty)

t5_0p <- predictFelm(pmod5, var.terms = t_terms_sixty)
t5_1p <- predictFelm(pmod5, newdata = p1, var.terms = t_terms_sixty)
t5_2p <- predictFelm(pmod5, newdata = p2, var.terms = t_terms_sixty)
t5_3p <- predictFelm(pmod5, newdata = p3, var.terms = t_terms_sixty)
t5_4p <- predictFelm(pmod5, newdata = p4, var.terms = t_terms_sixty)
t5_5p <- predictFelm(pmod5, newdata = p5, var.terms = t_terms_sixty)

# Total predicted revenue per acre
ta0_fit <- exp(t1_0p$fit + t1_0p$res + t1_0p$effect) - 1
ta1_fit <- exp(t1_1p$fit + t1_1p$res + t1_1p$effect) - 1 
ta2_fit <- exp(t1_2p$fit + t1_2p$res + t1_2p$effect) - 1 
ta3_fit <- exp(t1_3p$fit + t1_3p$res + t1_3p$effect) - 1 
ta4_fit <- exp(t1_4p$fit + t1_4p$res + t1_4p$effect) - 1 
ta5_fit <- exp(t1_5p$fit + t1_5p$res + t1_5p$effect) - 1

tb0_fit <- exp(t2_0p$fit + t2_0p$res + t2_0p$effect) - 1
tb1_fit <- exp(t2_1p$fit + t2_1p$res + t2_1p$effect) - 1 
tb2_fit <- exp(t2_2p$fit + t2_2p$res + t2_2p$effect) - 1 
tb3_fit <- exp(t2_3p$fit + t2_3p$res + t2_3p$effect) - 1 
tb4_fit <- exp(t2_4p$fit + t2_4p$res + t2_4p$effect) - 1 
tb5_fit <- exp(t2_5p$fit + t2_5p$res + t2_5p$effect) - 1

tc0_fit <- exp(t3_0p$fit + t3_0p$res + t3_0p$effect) - 1
tc1_fit <- exp(t3_1p$fit + t3_1p$res + t3_1p$effect) - 1 
tc2_fit <- exp(t3_2p$fit + t3_2p$res + t3_2p$effect) - 1 
tc3_fit <- exp(t3_3p$fit + t3_3p$res + t3_3p$effect) - 1 
tc4_fit <- exp(t3_4p$fit + t3_4p$res + t3_4p$effect) - 1 
tc5_fit <- exp(t3_5p$fit + t3_5p$res + t3_5p$effect) - 1

td0_fit <- exp(t4_0p$fit + t4_0p$res + t4_0p$effect) - 1
td1_fit <- exp(t4_1p$fit + t4_1p$res + t4_1p$effect) - 1 
td2_fit <- exp(t4_2p$fit + t4_2p$res + t4_2p$effect) - 1 
td3_fit <- exp(t4_3p$fit + t4_3p$res + t4_3p$effect) - 1 
td4_fit <- exp(t4_4p$fit + t4_4p$res + t4_4p$effect) - 1 
td5_fit <- exp(t4_5p$fit + t4_5p$res + t4_5p$effect) - 1

te0_fit <- exp(t5_0p$fit + t5_0p$res + t5_0p$effect) - 1
te1_fit <- exp(t5_1p$fit + t5_1p$res + t5_1p$effect) - 1 
te2_fit <- exp(t5_2p$fit + t5_2p$res + t5_2p$effect) - 1 
te3_fit <- exp(t5_3p$fit + t5_3p$res + t5_3p$effect) - 1 
te4_fit <- exp(t5_4p$fit + t5_4p$res + t5_4p$effect) - 1 
te5_fit <- exp(t5_5p$fit + t5_5p$res + t5_5p$effect) - 1

# Sum of total predicted revenue per acre
ta0 <- sum(exp(t1_0p$fit + t1_0p$res + t1_0p$effect) - 1)
ta1 <- sum(exp(t1_1p$fit + t1_1p$res + t1_1p$effect) - 1) 
ta2 <- sum(exp(t1_2p$fit + t1_2p$res + t1_2p$effect) - 1) 
ta3 <- sum(exp(t1_3p$fit + t1_3p$res + t1_3p$effect) - 1) 
ta4 <- sum(exp(t1_4p$fit + t1_4p$res + t1_4p$effect) - 1) 
ta5 <- sum(exp(t1_5p$fit + t1_5p$res + t1_5p$effect) - 1)

tb0 <- sum(exp(t2_0p$fit + t2_0p$res + t2_0p$effect) - 1)
tb1 <- sum(exp(t2_1p$fit + t2_1p$res + t2_1p$effect) - 1) 
tb2 <- sum(exp(t2_2p$fit + t2_2p$res + t2_2p$effect) - 1) 
tb3 <- sum(exp(t2_3p$fit + t2_3p$res + t2_3p$effect) - 1) 
tb4 <- sum(exp(t2_4p$fit + t2_4p$res + t2_4p$effect) - 1) 
tb5 <- sum(exp(t2_5p$fit + t2_5p$res + t2_5p$effect) - 1)

tc0 <- sum(exp(t3_0p$fit + t3_0p$res + t3_0p$effect) - 1)
tc1 <- sum(exp(t3_1p$fit + t3_1p$res + t3_1p$effect) - 1) 
tc2 <- sum(exp(t3_2p$fit + t3_2p$res + t3_2p$effect) - 1) 
tc3 <- sum(exp(t3_3p$fit + t3_3p$res + t3_3p$effect) - 1) 
tc4 <- sum(exp(t3_4p$fit + t3_4p$res + t3_4p$effect) - 1) 
tc5 <- sum(exp(t3_5p$fit + t3_5p$res + t3_5p$effect) - 1)

td0 <- sum(exp(t4_0p$fit + t4_0p$res + t4_0p$effect) - 1)
td1 <- sum(exp(t4_1p$fit + t4_1p$res + t4_1p$effect) - 1) 
td2 <- sum(exp(t4_2p$fit + t4_2p$res + t4_2p$effect) - 1) 
td3 <- sum(exp(t4_3p$fit + t4_3p$res + t4_3p$effect) - 1) 
td4 <- sum(exp(t4_4p$fit + t4_4p$res + t4_4p$effect) - 1) 
td5 <- sum(exp(t4_5p$fit + t4_5p$res + t4_5p$effect) - 1)

te0 <- sum(exp(t5_0p$fit + t5_0p$res + t5_0p$effect) - 1)
te1 <- sum(exp(t5_1p$fit + t5_1p$res + t5_1p$effect) - 1) 
te2 <- sum(exp(t5_2p$fit + t5_2p$res + t5_2p$effect) - 1) 
te3 <- sum(exp(t5_3p$fit + t5_3p$res + t5_3p$effect) - 1) 
te4 <- sum(exp(t5_4p$fit + t5_4p$res + t5_4p$effect) - 1) 
te5 <- sum(exp(t5_5p$fit + t5_5p$res + t5_5p$effect) - 1)

# Get standard errors of sum
ta0_ci <- sum(t1_0p$se.fit)*1.96
ta1_ci <- sum(t1_1p$se.fit)*1.96
ta2_ci <- sum(t1_2p$se.fit)*1.96
ta3_ci <- sum(t1_3p$se.fit)*1.96
ta4_ci <- sum(t1_4p$se.fit)*1.96
ta5_ci <- sum(t1_5p$se.fit)*1.96

tb0_ci <- sum(t2_0p$se.fit)*1.96
tb1_ci <- sum(t2_1p$se.fit)*1.96
tb2_ci <- sum(t2_2p$se.fit)*1.96
tb3_ci <- sum(t2_3p$se.fit)*1.96
tb4_ci <- sum(t2_4p$se.fit)*1.96
tb5_ci <- sum(t2_5p$se.fit)*1.96

tc0_ci <- sum(t3_0p$se.fit)*1.96
tc1_ci <- sum(t3_1p$se.fit)*1.96
tc2_ci <- sum(t3_2p$se.fit)*1.96
tc3_ci <- sum(t3_3p$se.fit)*1.96
tc4_ci <- sum(t3_4p$se.fit)*1.96
tc5_ci <- sum(t3_5p$se.fit)*1.96

td0_ci <- sum(t4_0p$se.fit)*1.96
td1_ci <- sum(t4_1p$se.fit)*1.96
td2_ci <- sum(t4_2p$se.fit)*1.96
td3_ci <- sum(t4_3p$se.fit)*1.96
td4_ci <- sum(t4_4p$se.fit)*1.96
td5_ci <- sum(t4_5p$se.fit)*1.96

te0_ci <- sum(t5_0p$se.fit)*1.96
te1_ci <- sum(t5_1p$se.fit)*1.96
te2_ci <- sum(t5_2p$se.fit)*1.96
te3_ci <- sum(t5_3p$se.fit)*1.96
te4_ci <- sum(t5_4p$se.fit)*1.96
te5_ci <- sum(t5_5p$se.fit)*1.96



pdat <- data.frame(effect = rep(c("Weather-effect", "Climate-effect", "Total-effect"), each = 30),
                   type = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), 3, each = 6),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 15),
                   sum = c(wa0, wa1, wa2, wa3, wa4, wa5, # Weather effect
                           wb0, wb1, wb2, wb3, wb4, wb5,
                           wc0, wc1, wc2, wc3, wc4, wc5,
                           wd0, wd1, wd2, wd3, wd4, wd5,
                           ce0, ce1, ce2, ce3, ce4, ce5, #Climate effect Same
                           ca0, ca1, ca2, ca3, ca4, ca5,
                           cb0, cb1, cb2, cb3, cb4, cb5,
                           cc0, cc1, cc2, cc3, cc4, cc5,
                           cd0, cd1, cd2, cd3, cd4, cd5,
                           ce0, ce1, ce2, ce3, ce4, ce5, # Same
                           ta0, ta1, ta2, ta3, ta4, ta5,
                           tb0, tb1, tb2, tb3, tb4, tb5,
                           tc0, tc1, tc2, tc3, tc4, tc5,
                           td0, td1, td2, td3, td4, td5,
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
                           ce0_ci, ce1_ci, ce2_ci, ce3, ce4_ci, ce5_ci,
                           ta0_ci, ta1_ci, ta2_ci, ta3_ci, ta4_ci, ta5_ci,
                           tb0_ci, tb1_ci, tb2_ci, tb3_ci, tb4_ci, tb5_ci,
                           tc0_ci, tc1_ci, tc2_ci, tc3_ci, tc4_ci, tc5_ci,
                           td0_ci, td1_ci, td2_ci, td3_ci, td4_ci, td5_ci,
                           ce0_ci, ce1_ci, ce2_ci, ce3, ce4_ci, ce5_ci))
pdat

adat <- data.frame(effect = "Total-effect",
                   fit = c(ta0_fit, ta1_fit, ta2_fit, ta3_fit, ta4_fit, ta5_fit,
                           tb0_fit, tb1_fit, tb2_fit, tb3_fit, tb4_fit, tb5_fit,
                           tc0_fit, tc1_fit, tc2_fit, tc3_fit, tc4_fit, tc5_fit,
                           td0_fit, td1_fit, td2_fit, td3_fit, td4_fit, td5_fit,
                           te0_fit, te1_fit, te2_fit, te3_fit, te4_fit, te5_fit),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 5, each = length(ta0_fit)),
                   type = rep(c("5-year", "10-year", "20-year", "30-year", "60-year"), each = length(ta0_fit)*6),
                   crop = "wheat")

head(adat)
saveRDS(adat, "data/rev_wheat_pred.rds")

#sixtypdat <- filter(pdat, type == "60-year")
# pdat <- filter(pdat, type != "60-year" | effect != "Weather-effect" & effect != "Total-effect")
pdat

pdat$effect <- factor(pdat$effect, levels = c("Weather-effect", "Climate-effect", "Total-effect"))
pdat <- pdat %>% 
  group_by(effect, type) %>% 
  mutate(change =  (sum - first(sum))/first(sum),
         change_min = ((sum - ci) - (first(sum) - first(ci)))/(first(sum) - first(ci)),
         change_max = ((sum + ci) - (first(sum) + first(ci)))/(first(sum) + first(ci)))
# sixtypdat <- sixtypdat %>% 
#   group_by(effect, type) %>% 
#   mutate(change =  (sum - first(sum))/first(sum),
#          change_min = ((sum - ci) - (first(sum) - first(ci)))/(first(sum) - first(ci)),
#          change_max = ((sum + ci) - (first(sum) + first(ci)))/(first(sum) + first(ci)))

pdat
pdat$change <- pdat$change*100
pdat$change_min <- pdat$change_min*100
pdat$change_max <- pdat$change_max*100

# sixtypdat$change <- sixtypdat$change*100
# sixtypdat$change_min <- sixtypdat$change_min*100
# sixtypdat$change_max <- sixtypdat$change_max*100



pdat$type <- factor(pdat$type, labels = c("5-year", "10-year", "20-year", "30-year", "60-year"))

pdat <- filter(pdat, type != "60-year" | (effect != "Weather-effect" & effect != "Total-effect"))

saveRDS(pdat, "data/rev_wheat_predictions.rds")

ggplot(pdat, aes(temp, change)) + 
  #geom_ribbon(aes(ymax = change_max, ymin = change_min, x = temp, fill = effect, group = effect), fill = "grey", alpha = 0.5 ) +
  geom_line(aes(color = effect)) +
  geom_point(aes(color = effect), size = 0.5) +
  #geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Revenue per acre \n (Wheat)") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = "top", 
       #legend.justification = c("left", "top"), 
       legend.box.background = element_rect(colour = "grey"), 
       legend.title = element_blank(), legend.key = element_blank()) +
  #theme(legend.position = c(.85,1), 
  #     legend.justification = c("left", "top"), 
  #     legend.box.background = element_rect(colour = "grey"), 
  #     legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~type, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5)

ggsave("figures/predicted_wheat.pdf", device = "pdf", width = 6, height = 4)
  
# ggplot(sixtypdat, aes(temp, change)) + geom_line()
#plot(c(c1, c2, c3, c4, c5))
#plot(c(ld1, ld2, ld3, ld4, ld5))
#plot(c(p1, p2, p3, p4, p5))


