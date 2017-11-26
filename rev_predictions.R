source("R/predictFelm.R")

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

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

ld0_p <- predictFelm(ldmod1)
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

cbase <- sum(exp(cs0_p$fit))
c1 <- (sum(exp(cs1_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
c2 <- (sum(exp(cs2_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
c3 <- (sum(exp(cs3_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
c4 <- (sum(exp(cs4_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))
c5 <- (sum(exp(cs5_p$fit)) - sum(exp(cs0_p$fit)))/sum(exp(cs0_p$fit))

lbase <- sum(exp(ld0_p$fit))
l1 <- (sum(exp(ld1_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
l2 <- (sum(exp(ld2_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
l3 <- (sum(exp(ld3_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
l4 <- (sum(exp(ld4_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))
l5 <- (sum(exp(ld5_p$fit)) - sum(exp(ld0_p$fit)))/sum(exp(ld0_p$fit))

pbase <- sum(exp(p0_p$fit))
pa1 <- (sum(exp(p1_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
pa2 <- (sum(exp(p2_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
pa3 <- (sum(exp(p3_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
pa4 <- (sum(exp(p4_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))
pa5 <- (sum(exp(p5_p$fit)) - sum(exp(p0_p$fit)))/sum(exp(p0_p$fit))


pdat <- data.frame(model = rep(c("cs", "ld", "p"), each = 5),
                   temp = rep(c("+1C", "+2C", "+3C", "+4C", "+5C"), 3),
                   change = c(c1, c2, c3, c4, c5,
                              l1, l2, l3, l4, l5,
                              pa1, pa2, pa3, pa4, pa5))
pdat$model <- factor(model)

ggplot(pdat, aes(temp, change, color = model)) + geom_point()

plot(c(c1, c2, c3, c4, c5))
plot(c(ld1, ld2, ld3, ld4, ld5))
plot(c(p1, p2, p3, p4, p5))


