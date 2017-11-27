library(tidyverse)
library(lfe)
library(ggthemes)

source("R/predictFelm.R")

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# Load models
csmod1 <- readRDS("models/cs_rev_fe.rds")
ldmod1 <- readRDS("models/ld_rev_fe.rds")
pmod1 <- readRDS("models/p_rev_fe.rds")

# Load changes in degree day data
cs1 <- readRDS("data/degree_day_changes/cross_section_regression_data_1C.rds")
cs2 <- readRDS("data/degree_day_changes/cross_section_regression_data_2C.rds")
cs3 <- readRDS("data/degree_day_changes/cross_section_regression_data_3C.rds")
cs4 <- readRDS("data/degree_day_changes/cross_section_regression_data_4C.rds")
cs5 <- readRDS("data/degree_day_changes/cross_section_regression_data_5C.rds")

p1 <- readRDS("data/degree_day_changes/panel_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_regression_data_5C.rds")

ld1 <- readRDS("data/degree_day_changes/diff_regression_data_1C.rds")
ld2 <- readRDS("data/degree_day_changes/diff_regression_data_2C.rds")
ld3 <- readRDS("data/degree_day_changes/diff_regression_data_3C.rds")
ld4 <- readRDS("data/degree_day_changes/diff_regression_data_4C.rds")
ld5 <- readRDS("data/degree_day_changes/diff_regression_data_5C.rds")

cs0_p <- predictFelm(csmod1)
cs1_p <- predictFelm(csmod1, newdata = cs1)
cs2_p <- predictFelm(csmod1, newdata = cs2)
cs3_p <- predictFelm(csmod1, newdata = cs3)
cs4_p <- predictFelm(csmod1, newdata = cs4)
cs5_p <- predictFelm(csmod1, newdata = cs5)

ld0_p <- predictFelm(ldmod1, newdata = ld1)
ld1_p <- predictFelm(ldmod1, newdata = ld1)
ld2_p <- predictFelm(ldmod1, newdata = ld2)
ld3_p <- predictFelm(ldmod1, newdata = ld3)
ld4_p <- predictFelm(ldmod1, newdata = ld4)
ld5_p <- predictFelm(ldmod1, newdata = ld5)

p0_p <- predictFelm(pmod1)
p1_p <- predictFelm(pmod1, newdata = p1)
p2_p <- predictFelm(pmod1, newdata = p2)
p3_p <- predictFelm(pmod1, newdata = p3)
p4_p <- predictFelm(pmod1, newdata = p4)
p5_p <- predictFelm(pmod1, newdata = p5)

# cbase <- sum(exp(cs0_p$fit))
# c1 <- (sum(exp(cs1_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
# c2 <- (sum(exp(cs2_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
# c3 <- (sum(exp(cs3_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
# c4 <- (sum(exp(cs4_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
# c5 <- (sum(exp(cs5_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
# 
# lbase <- sum(exp(ld0_p$fit))
# l1 <- (sum(exp(ld1_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
# l2 <- (sum(exp(ld2_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
# l3 <- (sum(exp(ld3_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
# l4 <- (sum(exp(ld4_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
# l5 <- (sum(exp(ld5_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
# 
# pbase <- sum(exp(p0_p$fit))
# pa1 <- (sum(exp(p1_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
# pa2 <- (sum(exp(p2_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
# pa3 <- (sum(exp(p3_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
# pa4 <- (sum(exp(p4_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
# pa5 <- (sum(exp(p5_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))

# Total predicted revenue per acre
c0 <- sum(exp(cs0_p$fit))
c1 <- sum(exp(cs1_p$fit))
c2 <- sum(exp(cs2_p$fit)) 
c3 <- sum(exp(cs3_p$fit)) 
c4 <- sum(exp(cs4_p$fit)) 
c5 <- sum(exp(cs5_p$fit)) 

l0 <- sum(exp(ld0_p$fit))
l1 <- sum(exp(ld1_p$fit)) 
l2 <- sum(exp(ld2_p$fit)) 
l3 <- sum(exp(ld3_p$fit)) 
l4 <- sum(exp(ld4_p$fit)) 
l5 <- sum(exp(ld5_p$fit)) 

pa0 <- sum(exp(p0_p$fit))
pa1 <- sum(exp(p1_p$fit)) 
pa2 <- sum(exp(p2_p$fit)) 
pa3 <- sum(exp(p3_p$fit)) 
pa4 <- sum(exp(p4_p$fit)) 
pa5 <- sum(exp(p5_p$fit))

# Get standard errors of sum
c0_ci <- sum(cs0_p$se.fit)*1.96
c1_ci <- sum(cs1_p$se.fit)*1.96
c2_ci <- sum(cs2_p$se.fit)*1.96
c3_ci <- sum(cs3_p$se.fit)*1.96
c4_ci <- sum(cs4_p$se.fit)*1.96
c5_ci <- sum(cs5_p$se.fit)*1.96

l0_ci <- sum(ld0_p$se.fit)*1.96
l1_ci <- sum(ld1_p$se.fit)*1.96
l2_ci <- sum(ld2_p$se.fit)*1.96
l3_ci <- sum(ld3_p$se.fit)*1.96
l4_ci <- sum(ld4_p$se.fit)*1.96
l5_ci <- sum(ld5_p$se.fit)*1.96

pa0_ci <- sum(p0_p$se.fit)*1.96
pa1_ci <- sum(p1_p$se.fit)*1.96
pa2_ci <- sum(p2_p$se.fit)*1.96
pa3_ci <- sum(p3_p$se.fit)*1.96
pa4_ci <- sum(p4_p$se.fit)*1.96
pa5_ci <- sum(p5_p$se.fit)*1.96

pdat <- data.frame(model = rep(c("cs", "ld", "p"), each = 6),
                   temp = rep(c(0, 1, 2, 3, 4, 5), 3),
                   sum = c(c0, c1, c2, c3, c4, c5,
                              l0, l1, l2, l3, l4, l5,
                              pa0, pa1, pa2, pa3, pa4, pa5),
                   ci = c(c0_ci, c1_ci, c2_ci, c3_ci, c4_ci, c5_ci,
                              l0_ci, l1_ci, l2_ci, l3_ci, l4_ci, l5_ci,
                              pa0_ci, pa1_ci, pa2_ci, pa3_ci, pa4_ci, pa5_ci))

pdat <- pdat %>% 
  group_by(model) %>% 
  mutate(change =  (sum - first(sum))/first(sum),
         change_min = ((sum - ci) - (first(sum) - first(ci)))/(first(sum) - first(ci)),
         change_max = ((sum + ci) - (first(sum) + first(ci)))/(first(sum) + first(ci)))

pdat$change <- pdat$change*100
pdat$change_min <- pdat$change_min*100
pdat$change_max <- pdat$change_max*100

pdat$model <- factor(pdat$model, labels = c("Cross-section", "Decade", "Panel"))

ggplot(pdat, aes(temp, change)) + 
  geom_ribbon(data = filter(pdat, model == "Cross-section"), 
              aes(ymax = change_max, ymin = change_min, x = temp), fill = "grey", alpha = 0.5 ) +
  geom_line(aes(color = model)) + 
  #geom_errorbar(aes(ymax = change_max, ymin = change_min), width = .1) +
  theme_tufte(base_size = 12) +
  ylab("% Change in Revenue per acre") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = c(.85,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.title = element_blank(), legend.key = element_blank())  
  

#plot(c(c1, c2, c3, c4, c5))
#plot(c(ld1, ld2, ld3, ld4, ld5))
#plot(c(p1, p2, p3, p4, p5))


