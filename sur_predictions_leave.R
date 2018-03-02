library(tidyverse)
library(lfe)
library(systemfit)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictSUR.R")
source("R/predictSUR.clean.R")
source("archive/predict_sur_leaveoneout.R")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
depdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a)

# Sur models
sur_ten <- readRDS("models/sur_share_model_ten.rds")
sur_twenty <- readRDS("models/sur_share_model_twenty.rds")
sur_thirty <- readRDS("models/sur_share_model_thirty.rds")

# Prediction data 0C-5C
p0 <- cropdat
p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")

# Get exact colnames
basedata <- cropdat
basedata_dm <- demeanlist(basedata, fl = list(fips = factor(cropdat$fips)))
basedat_means <- demeanlist(basedata, fl = list(fips = factor(cropdat$fips)), means = TRUE)

p0 <- cropdat
p0 <- select(p0, names(p1))

# Bind list
newdata_list <- list(p0 = p0,
                     p1 = p1,
                     p2 = p2,
                     p3 = p3,
                     p4 = p4,
                     p5 = p5)


# Demean data
p0_dm <- demeanlist(p0, fl = list(fips = factor(cropdat$fips)))
p0_means <- demeanlist(p0, fl = list(fips = factor(cropdat$fips)), means = TRUE)
head(p0_means)

p1_dm <- p1 - p0_means
p2_dm <- p2 - p0_means
p3_dm <- p3 - p0_means
p4_dm <- p4 - p0_means
p5_dm <- p5 - p0_means

newdata_list_dm <- list(p0 = p0_dm,
                     p1 = p1_dm,
                     p2 = p2_dm,
                     p3 = p3_dm,
                     p4 = p4_dm,
                     p5 = p5_dm)



# Equations 10-year
mod1 <- z_corn_a ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long- 1
 

mod3 <- z_hay_a ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- z_soybean_a ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long- 1


mod5 <- z_wheat_a ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long- 1


modlistten <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)

modlist <- modlistten
basedata <- basedata_dm

newdata <- p0_dm
basedata_means <- basedata_means


terms <- c("dday0_10_rm10", "dday10_30_rm10", "dday30_rm10", "prec_rm10", "prec_sq_rm10")

modlist = modlistten
basedata = basedata
newdata = p0_dm
basedata_means = p0_means
terms = terms

ten_0 <- predictSUR_leave(modlist = modlistten, basedata = basedata, basedata_means = p0_means, newdata = p0_dm, terms = terms, acres = cropdat$acres)
ten_1 <- predictSUR_leave(modlist = modlistten, basedata = basedata, basedata_means = p0_means, newdata = p1_dm, terms = terms, acres = cropdat$acres)
ten_2 <- predictSUR_leave(modlist = modlistten, basedata = basedata, basedata_means = p0_means, newdata = p2_dm, terms = terms, acres = cropdat$acres)
ten_3 <- predictSUR_leave(modlist = modlistten, basedata = basedata, basedata_means = p0_means, newdata = p3_dm, terms = terms, acres = cropdat$acres)
ten_4 <- predictSUR_leave(modlist = modlistten, basedata = basedata, basedata_means = p0_means, newdata = p4_dm, terms = terms, acres = cropdat$acres)
ten_5 <- predictSUR_leave(modlist = modlistten, basedata = basedata, basedata_means = p0_means, newdata = p5_dm, terms = terms, acres = cropdat$acres)


# Equations 20-year
mod1 <- z_corn_a ~ dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~ dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long- 1
 

mod3 <- z_hay_a ~ dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- z_soybean_a ~ dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod5 <- z_wheat_a ~ dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


modlisttwenty <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)


modlist <- modlisttwenty
newdata <- p0_dm
newdata_means <- p0_means
depdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a)

terms <- c("dday0_10_rm11", "dday10_30_rm11", "dday30_rm11", "prec_rm11", "prec_sq_rm11")

twenty_0 <- predictSUR_leave(modlist = modlisttwenty, basedata = basedata, basedata_means = p0_means, newdata = p0_dm, terms = terms, acres = cropdat$acres)
twenty_1 <- predictSUR_leave(modlist = modlisttwenty, basedata = basedata, basedata_means = p0_means, newdata = p1_dm, terms = terms, acres = cropdat$acres)
twenty_2 <- predictSUR_leave(modlist = modlisttwenty, basedata = basedata, basedata_means = p0_means, newdata = p2_dm, terms = terms, acres = cropdat$acres)
twenty_3 <- predictSUR_leave(modlist = modlisttwenty, basedata = basedata, basedata_means = p0_means, newdata = p3_dm, terms = terms, acres = cropdat$acres)
twenty_4 <- predictSUR_leave(modlist = modlisttwenty, basedata = basedata, basedata_means = p0_means, newdata = p4_dm, terms = terms, acres = cropdat$acres)
twenty_5 <- predictSUR_leave(modlist = modlisttwenty, basedata = basedata, basedata_means = p0_means, newdata = p5_dm, terms = terms, acres = cropdat$acres)


# Equations 30-year
mod1 <- z_corn_a ~ dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~ dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
 

mod3 <- z_hay_a ~ dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- z_soybean_a ~ dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod5 <- z_wheat_a ~ dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long- 1



modlistthirty <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)


modlist <- modlistthirty
newdata <- p0_dm
newdata_means <- p0_means
depdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a)

terms <- c("dday0_10_rm12", "dday10_30_rm12", "dday30_rm12", "prec_rm12", "prec_sq_rm12")

thirty_0 <- predictSUR_leave(modlist = modlistthirty, basedata = basedata, basedata_means = p0_means, newdata = p0_dm, terms = terms, acres = cropdat$acres)
thirty_1 <- predictSUR_leave(modlist = modlistthirty, basedata = basedata, basedata_means = p0_means, newdata = p1_dm, terms = terms, acres = cropdat$acres)
thirty_2 <- predictSUR_leave(modlist = modlistthirty, basedata = basedata, basedata_means = p0_means, newdata = p2_dm, terms = terms, acres = cropdat$acres)
thirty_3 <- predictSUR_leave(modlist = modlistthirty, basedata = basedata, basedata_means = p0_means, newdata = p3_dm, terms = terms, acres = cropdat$acres)
thirty_4 <- predictSUR_leave(modlist = modlistthirty, basedata = basedata, basedata_means = p0_means, newdata = p4_dm, terms = terms, acres = cropdat$acres)
thirty_5 <- predictSUR_leave(modlist = modlistthirty, basedata = basedata, basedata_means = p0_means, newdata = p5_dm, terms = terms, acres = cropdat$acres)




ten_dat <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), each = length(ten_0$corn_predict)),
                       corn.pred = c(ten_0$corn_predict,
                                ten_1$corn_predict,
                                ten_2$corn_predict,
                                ten_3$corn_predict,
                                ten_4$corn_predict,
                                ten_5$corn_predict),
                      cotton.pred = c(ten_0$cotton_predict,
                                ten_1$cotton_predict,
                                ten_2$cotton_predict,
                                ten_3$cotton_predict,
                                ten_4$cotton_predict,
                                ten_5$cotton_predict),
                      hay.pred = c(ten_0$hay_predict,
                                ten_1$hay_predict,
                                ten_2$hay_predict,
                                ten_3$hay_predict,
                                ten_4$hay_predict,
                                ten_5$hay_predict),
                      soybean.pred = c(ten_0$soybean_predict,
                                ten_1$soybean_predict,
                                ten_2$soybean_predict,
                                ten_3$soybean_predict,
                                ten_4$soybean_predict,
                                ten_5$soybean_predict),
                      wheat.pred = c(ten_0$wheat_predict,
                                ten_1$wheat_predict,
                                ten_2$wheat_predict,
                                ten_3$wheat_predict,
                                ten_4$wheat_predict,
                                ten_5$wheat_predict))
saveRDS(ten_dat, "data/cten.rds")
# 
pdat_ten <- gather(ten_dat, key = crop, value = sum, -temp)
# 
pdat_ten <- pdat_ten %>%
  group_by(crop, temp) %>%
  summarise_all(sum) %>%
  mutate(change = 100*(sum - first(sum))/first(sum))
head(pdat_ten)
# 
# ggplot(pdat_ten, aes(temp, change)) + geom_line() + facet_wrap(~crop, scales = "free")



twenty_dat <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), each = length(twenty_0$corn_predict)),
                       corn.pred = c(twenty_0$corn_predict,
                                twenty_1$corn_predict,
                                twenty_2$corn_predict,
                                twenty_3$corn_predict,
                                twenty_4$corn_predict,
                                twenty_5$corn_predict),
                      cotton.pred = c(twenty_0$cotton_predict,
                                twenty_1$cotton_predict,
                                twenty_2$cotton_predict,
                                twenty_3$cotton_predict,
                                twenty_4$cotton_predict,
                                twenty_5$cotton_predict),
                      hay.pred = c(twenty_0$hay_predict,
                                twenty_1$hay_predict,
                                twenty_2$hay_predict,
                                twenty_3$hay_predict,
                                twenty_4$hay_predict,
                                twenty_5$hay_predict),
                      soybean.pred = c(twenty_0$soybean_predict,
                                twenty_1$soybean_predict,
                                twenty_2$soybean_predict,
                                twenty_3$soybean_predict,
                                twenty_4$soybean_predict,
                                twenty_5$soybean_predict),
                      wheat.pred = c(twenty_0$wheat_predict,
                                twenty_1$wheat_predict,
                                twenty_2$wheat_predict,
                                twenty_3$wheat_predict,
                                twenty_4$wheat_predict,
                                twenty_5$wheat_predict))
saveRDS(twenty_dat, "data/ctwenty.rds")
pdat_twenty <- gather(twenty_dat, key = crop, value = sum, -temp)
# 
pdat_twenty <- pdat_twenty %>%
  group_by(crop, temp) %>%
  summarise_all(sum) %>%
  mutate(change = 100*(sum - first(sum))/first(sum))
head(pdat_twenty)
# 
# ggplot(pdat_twenty, aes(temp, change)) + geom_line() + facet_wrap(~crop, scales = "free")


thirty_dat <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), each = length(thirty_0$corn_predict)),
                       corn.pred = c(thirty_0$corn_predict,
                                thirty_1$corn_predict,
                                thirty_2$corn_predict,
                                thirty_3$corn_predict,
                                thirty_4$corn_predict,
                                thirty_5$corn_predict),
                      cotton.pred = c(thirty_0$cotton_predict,
                                thirty_1$cotton_predict,
                                thirty_2$cotton_predict,
                                thirty_3$cotton_predict,
                                thirty_4$cotton_predict,
                                thirty_5$cotton_predict),
                      hay.pred = c(thirty_0$hay_predict,
                                thirty_1$hay_predict,
                                thirty_2$hay_predict,
                                thirty_3$hay_predict,
                                thirty_4$hay_predict,
                                thirty_5$hay_predict),
                      soybean.pred = c(thirty_0$soybean_predict,
                                thirty_1$soybean_predict,
                                thirty_2$soybean_predict,
                                thirty_3$soybean_predict,
                                thirty_4$soybean_predict,
                                thirty_5$soybean_predict),
                      wheat.pred = c(thirty_0$wheat_predict,
                                thirty_1$wheat_predict,
                                thirty_2$wheat_predict,
                                thirty_3$wheat_predict,
                                thirty_4$wheat_predict,
                                thirty_5$wheat_predict))
saveRDS(thirty_dat, "data/cthirty.rds")
pdat_thirty <- gather(thirty_dat, key = crop, value = sum, -temp)
# 
pdat_thirty <- pdat_thirty %>%
  group_by(crop, temp) %>%
  summarise_all(sum) %>%
  mutate(change = 100*(sum - first(sum))/first(sum))
head(pdat_thirty)
# 
# ggplot(pdat_thirty, aes(temp, change)) + geom_line() + facet_wrap(~crop, scales = "free")



pdat <- rbind(pdat_ten, pdat_twenty, pdat_thirty)
pdat$interval <- rep(c('10-year', '20-year', '30-year'), each = nrow(pdat_ten))
head(pdat)

ggplot(pdat, aes(temp, change)) + 
  geom_line() + 
  geom_point() + 
  #geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
  theme_tufte(base_size = 12) +
  ylab("Change in Proportion of Crop Acres") +
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
  facet_wrap(interval~crop, ncol = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

# ppdat <- pdat %>%
#   # filter(effect == "Climate-effect") %>%
#   group_by(temp, type, effect) %>%
#   summarise(total = sum(sum)) %>%
#   group_by(type, effect) %>%
#   mutate(change = (total - first(total))/first(total))
# ppdat$change <- ppdat$change*100
# ppdat
# 
# ggplot(ppdat, aes(temp, change, color = type)) + 
#   geom_line() + 
#   geom_point(size = 0.5) + 
#   facet_wrap(~effect, ncol = 3) +
#   theme_tufte(base_size = 12) +
#   ylab("Change in Proportion of Total Crop Acres") +
#   xlab("Change in Temperature (C)") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
#   theme(legend.position = "top", 
#        #legend.justification = c("left", "top"), 
#        legend.box.background = element_rect(colour = "grey"), 
#        legend.title = element_blank(), legend.key = element_blank()) +
#   #theme(legend.position = c(.85,1), 
#   #     legend.justification = c("left", "top"), 
#   #     legend.box.background = element_rect(colour = "grey"), 
#   #     legend.title = element_blank(), legend.key = element_blank()) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
# 






