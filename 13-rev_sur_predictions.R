library(boot)
library(tidyverse)
library(ggthemes)

source("R/boot.strap.R")

cropdat <- readRDS("data/full_ag_data.rds")

# rev_crop_pred <- readRDS("data/rev_crop_predictions.rds")
rev_crop_pred <- readRDS("data/rev_crop_pred.rds")

# Revenue predictions
 # rev_corn_pred <- readRDS("data/rev_corn_pred.rds")
# rev_cotton_pred <- readRDS("data/rev_cotton_pred.rds")
# rev_hay_pred <- readRDS("data/rev_hay_pred.rds")
# rev_soybean_pred <- readRDS("data/rev_soybean_pred.rds")
# rev_wheat_pred <- readRDS("data/rev_wheat_pred.rds")

# Crop share predictions
# tten <- readRDS("data/tten.rds")
# ttwenty <- readRDS("data/ttwenty.rds")
# tthirty <- readRDS("data/tthirty.rds")

# tten$predictions$type <- "10-year"
# ttwenty$predictions$type <- "20-year"
# tthirty$predictions$type <- "30-year"

cten <- readRDS("data/cten.rds")
ctwenty <- readRDS("data/ctwenty.rds")
cthirty <- readRDS("data/cthirty.rds")

cten$predictions$type <- "10-year"
ctwenty$predictions$type <- "20-year"
cthirty$predictions$type <- "30-year"

dat <- cropdat %>% 
  select(year, fips, corn_grain_a, cotton_a, hay_a, wheat_a, soybean_a) %>% 
  filter(year >= 1950) %>% 
  group_by(fips) %>% 
  mutate(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE))


# tdat <- bind_rows(tfive$predictions, tten$predictions, ttwenty$predictions, tthirty$predictions, csixty$predictions)
# tdat$effect <- "Total-effect"

cdat <- bind_rows(cten$predictions, ctwenty$predictions, cthirty$predictions)
cdat$effect <- "Climate-effect"

ndat <- bind_rows(dat[, 3:7], dat[, 3:7], dat[, 3:7], dat[, 3:7], dat[, 3:7], dat[, 3:7])
# ndat <- bind_rows(dat[, 3:7])
# ndat <- bind_rows(ndat, ndat, ndat, ndat, ndat, ndat)
ndat$effect <- "No Crop-switching"


# # Total-effect
# tdat$corn_rev <- rev_corn_pred$fit
# tdat$cotton_rev <- rev_cotton_pred$fit
# tdat$hay_rev <- rev_hay_pred$fit
# tdat$soybean_rev <- rev_soybean_pred$fit
# tdat$wheat_rev <- rev_wheat_pred$fit
# 
# tdat$corn.pred <- tdat$corn.pred*cropdat$acres
# tdat$cotton.pred <- tdat$cotton.pred*cropdat$acres
# tdat$hay.pred <- tdat$hay.pred*cropdat$acres
# tdat$soybean.pred <- tdat$soybean.pred*cropdat$acres
# tdat$wheat.pred <- tdat$wheat.pred*cropdat$acres

# # Remove negative values from predictions
# tdat$corn_rev <- ifelse(tdat$corn_rev < 0, 0, tdat$corn_rev)
# tdat$cotton_rev <- ifelse(tdat$cotton_rev < 0, 0, tdat$cotton_rev)
# tdat$hay_rev <- ifelse(tdat$hay_rev < 0, 0, tdat$hay_rev)
# tdat$soybean_rev <- ifelse(tdat$soybean_rev < 0, 0, tdat$soybean_rev)
# tdat$wheat_rev <- ifelse(tdat$wheat_rev < 0, 0, tdat$wheat_rev)

# head(tdat)

# total_effect <- (tdat$corn.pred*tdat$corn_rev) + (tdat$cotton.pred*tdat$cotton_rev) + (tdat$hay.pred*tdat$hay_rev) +
#   (tdat$soybean.pred*tdat$soybean_rev) + (tdat$wheat.pred*tdat$wheat_rev)

# Climate effects
cdat$corn_rev <- rep(rev_crop_pred$corn_rev.pred, 3)
cdat$cotton_rev <- rep(rev_crop_pred$cotton_rev.pred, 3)
cdat$hay_rev <- rep(rev_crop_pred$hay_rev.pred, 3)
cdat$soybean_rev <- rep(rev_crop_pred$soybean_rev.pred, 3)
cdat$wheat_rev <- rep(rev_crop_pred$wheat_rev.pred, 3)

# Remove negative values from predictions
cdat$corn_rev <- ifelse(cdat$corn_rev < 0, 0, cdat$corn_rev)
cdat$cotton_rev <- ifelse(cdat$cotton_rev < 0, 0, cdat$cotton_rev)
cdat$hay_rev <- ifelse(cdat$hay_rev < 0, 0, cdat$hay_rev)
cdat$soybean_rev <- ifelse(cdat$soybean_rev < 0, 0, cdat$soybean_rev)
cdat$wheat_rev <- ifelse(cdat$wheat_rev < 0, 0, cdat$wheat_rev)

cdat$corn.pred <- cdat$corn.pred*cropdat$acres
cdat$cotton.pred <- cdat$cotton.pred*cropdat$acres
cdat$hay.pred <- cdat$hay.pred*cropdat$acres
cdat$soybean.pred <- cdat$soybean.pred*cropdat$acres
cdat$wheat.pred <- cdat$wheat.pred*cropdat$acres

head(cdat)

climate_effect <- (cdat$corn.pred*cdat$corn_rev) + (cdat$cotton.pred*cdat$cotton_rev) + (cdat$hay.pred*cdat$hay_rev) +
  (cdat$soybean.pred*cdat$soybean_rev) + (cdat$wheat.pred*cdat$wheat_rev)

# No crop-switching

# Remove negative values from predictions
# rev_corn_pred$fit <- ifelse(rev_corn_pred$fit < 0, 0, rev_corn_pred$fit)
# rev_cotton_pred$fit <- ifelse(rev_cotton_pred$fit < 0, 0, rev_cotton_pred$fit)
# rev_hay_pred$fit <- ifelse(rev_hay_pred$fit < 0, 0, rev_hay_pred$fit)
# rev_soybean_pred$fit <- ifelse(rev_soybean_pred$fit < 0, 0, rev_soybean_pred$fit)
# rev_wheat_pred$fit <- ifelse(rev_wheat_pred$fit < 0, 0, rev_wheat_pred$fit)

# ndat$corn.pred <- rev_corn_pred$fit*ndat$corn_grain_a
# ndat$cotton.pred <- rev_cotton_pred$fit*ndat$cotton_a
# ndat$hay.pred <- rev_hay_pred$fit*ndat$hay_a
# ndat$soybean.pred <- rev_soybean_pred$fit*ndat$soybean_a
# ndat$wheat.pred <- rev_wheat_pred$fit*ndat$wheat_a

ndat$corn.pred <- rev_crop_pred$corn_rev.pred*ndat$corn_grain_a
ndat$cotton.pred <- rev_crop_pred$cotton_rev.pred*ndat$cotton_a
ndat$hay.pred <- rev_crop_pred$hay_rev.pred*ndat$hay_a
ndat$soybean.pred <- rev_crop_pred$soybean_rev.pred*ndat$soybean_a
ndat$wheat.pred <- rev_crop_pred$wheat_rev.pred*ndat$wheat_a



head(ndat)


no_cs_effect <- (ndat$corn.pred) + (ndat$cotton.pred) + (ndat$hay.pred) +
  (ndat$soybean.pred) + (ndat$wheat.pred)

pdat <- data.frame(year = rep(cropdat$year, 3),
                   ten = rep(cropdat$ten, 3),
                   twenty = rep(cropdat$twenty, 3),
                   thirty = rep(cropdat$thirty, 3),
                   fips = rep(cropdat$fips, 3),
                   temp = cdat$temp,
                   type = cdat$type)

pdat$climate_effect <- climate_effect
# pdat$total_effect <- total_effect
pdat$no_cs_effect <- no_cs_effect
head(pdat)

# Bootstrap s.e. of sum
# head(test)
# test <- filter(pdat, temp == 0 & type == "5-year")
# pdat$cl <- ifelse(pdat$type == "5-year", "five", 0)
# boot.strap(test$climate_effect, rep = 5)

head(pdat)

bsum <- function(x,i) sum(x[i])
#bs <- system.time(boot(cropdat$ln_rev, bsum, R = 1000, strata = cropdat$five))

# pdat_se_five <- pdat %>% 
#   filter(type == "5-year") %>% 
#   group_by(temp, type) %>% 
#   summarise(climate_se_sum = sd(boot(climate_effect, bsum, R = 10, strata = five, parallel = "multicore", ncpus = 3)$t),
#             total_se_sum = sd(boot(total_effect, bsum, R = 10, strata = five, parallel = "multicore", ncpus = 3)$t),
#             no_cs_se_sum = sd(boot(no_cs_effect, bsum, R = 10, strata = five, parallel = "multicore", ncpus = 3)$t))
# 
# pdat_se_ten <- pdat %>% 
#   filter(type == "10-year") %>% 
#   group_by(temp, type) %>% 
#   summarise(climate_se_sum = sd(boot(climate_effect, bsum, R = 10, strata = ten, parallel = "multicore", ncpus = 3)$t),
#             total_se_sum = sd(boot(total_effect, bsum, R = 10, strata = ten, parallel = "multicore", ncpus = 3)$t),
#             no_cs_se_sum = sd(boot(no_cs_effect, bsum, R = 10, strata = ten, parallel = "multicore", ncpus = 3)$t))
# 
# pdat_se_twenty <- pdat %>% 
#   filter(type == "20-year") %>% 
#   group_by(temp, type) %>% 
#   summarise(climate_se_sum = sd(boot(climate_effect, bsum, R = 10, strata = twenty, parallel = "multicore", ncpus = 3)$t),
#             total_se_sum = sd(boot(total_effect, bsum, R = 10, strata = twenty, parallel = "multicore", ncpus = 3)$t),
#             no_cs_se_sum = sd(boot(no_cs_effect, bsum, R = 10, strata = twenty, parallel = "multicore", ncpus = 3)$t))
# 
# pdat_se_thirty <- pdat %>% 
#   filter(type == "30-year") %>% 
#   group_by(temp, type) %>% 
#   summarise(climate_se_sum = sd(boot(climate_effect, bsum, R = 10, strata = thirty, parallel = "multicore", ncpus = 3)$t),
#             total_se_sum = sd(boot(total_effect, bsum, R = 10, strata = thirty, parallel = "multicore", ncpus = 3)$t),
#             no_cs_se_sum = sd(boot(no_cs_effect, bsum, R = 10, strata = thirty, parallel = "multicore", ncpus = 3)$t))
# 
# pdat_se_sixty <- pdat %>% 
#   filter(type == "60-year") %>% 
#   group_by(temp, type) %>% 
#   summarise(climate_se_sum = sd(boot(climate_effect, bsum, R = 10, parallel = "multicore", ncpus = 3)$t),
#             total_se_sum = sd(boot(total_effect, bsum, R = 10, parallel = "multicore", ncpus = 3)$t),
#             no_cs_se_sum = sd(boot(no_cs_effect, bsum, R = 10, parallel = "multicore", ncpus = 3)$t))


pdat_se <- rbind(pdat_se_five, pdat_se_ten, pdat_se_twenty, pdat_se_thirty, pdat_se_sixty)
pdat_se

pdat <- pdat %>% 
  group_by(type, temp) %>% 
  summarise(climate_effect = sum(climate_effect),
            # total_effect = sum(total_effect),
            no_cs_effect = sum(no_cs_effect)) %>% 
  #left_join(pdat_se, by = c("type", "temp")) %>% 
  # mutate(climate_effect_min = climate_effect - 1.96*climate_se_sum,
  #        climate_effect_max = climate_effect + 1.96*climate_se_sum) %>% 
         # total_effect_min = total_effect - 1.96*total_se_sum,
         # total_effect_max = total_effect + 1.96*total_se_sum,
         # no_cs_effect_min = no_cs_effect - 1.96*no_cs_se_sum,
         # no_cs_effect_max = no_cs_effect + 1.96*no_cs_se_sum) %>% 
  group_by(type) %>% 
  mutate(change_climate_effect = 100*(climate_effect - first(climate_effect))/first(climate_effect),
         # change_total_effect = 100*(total_effect - first(total_effect))/first(total_effect),
         change_no_cs_effect = 100*(no_cs_effect - first(no_cs_effect))/first(no_cs_effect))
         
         # change_climate_effect_max = 100*(climate_effect_max - first(climate_effect_max))/first(climate_effect_max),
         # change_total_effect_max = 100*(total_effect_max - first(total_effect_max))/first(total_effect_max),
         # change_no_cs_effect_max = 100*(no_cs_effect_max - first(no_cs_effect_max))/first(no_cs_effect_max),
         # 
         # change_climate_effect_min = 100*(climate_effect_min - first(climate_effect_min))/first(climate_effect_min),
         # change_total_effect_min = 100*(total_effect_min - first(total_effect_min))/first(total_effect_min),
         # change_no_cs_effect_min = 100*(no_cs_effect_min - first(no_cs_effect_min))/first(no_cs_effect_min)) 
  
  

head(pdat)

pdat1 <- pdat %>% 
  select(type, temp, change_climate_effect, change_no_cs_effect) %>% 
  gather(key = effect, value = value, -type, -temp)

# pdat2 <- pdat %>% 
#     select(type, temp, change_climate_effect_max, change_total_effect_max, change_no_cs_effect_max) %>% 
#   gather(key = effect, value = value_max, -type, -temp)
# 
# pdat3 <- pdat %>% 
#     select(type, temp, change_climate_effect_min, change_total_effect_min, change_no_cs_effect_min) %>% 
#   gather(key = effect, value = value_min, -type, -temp)

# pdat1$value_max <- pdat2$value_max
# pdat1$value_min <- pdat3$value_min
# test <- left_join(pdat1, pdat2, by = c("type", "temp"))
# test
# pdat <- left_join(pdat, pdat3, by = c("type", "temp"))
head(pdat1)

# pdat1 <- filter(pdat1, type != "60-year" | effect != "change_total_effect")

pdat1$effect <- factor(pdat1$effect, levels = c("change_climate_effect", "change_no_cs_effect"),
                    labels = c("Climate-effect \n (w/ crop switching)", "Constant-effect \n (w/o crop switching)"))
pdat1$type <- factor(pdat1$type, levels = c("10-year", "20-year", "30-year"))


ggplot(pdat1, aes(temp, value, color = effect)) + geom_line() + facet_wrap(~type) +
  geom_point(aes(color = effect), size = 0.5) +
  ylim(-100, 0) +
  # geom_line(aes(y=value_max, temp, color = effect), linetype = "dashed", alpha = 0.5) +
  # geom_line(aes(y=value_min, temp, color = effect), linetype = "dashed", alpha = 0.5) +
  #geom_errorbar(aes(ymax = value_max, ymin = value_min, color = effect), width = .1) +
  #geom_ribbon((aes(ymax = value_max, ymin = value_min, color = effect)), fill = "grey") +
  theme_tufte(base_size = 10) +
  ylab("% Change in Total Revenue") +
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
  facet_wrap(~type) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5)
ggsave("figures/1-main_rev_plot.pdf", width = 6, height = 4)
# No adaptation 































# library(tidyverse)
# 
# setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")
# 
# ## Cropdata
# cropdat <- readRDS("data/full_ag_data.rds")
# 
# # Total revenue predictions
# rdat <- readRDS("data/rev_predictions.rds")
# 
# # Crop revenue predictions
# rcorn <- readRDS("data/rev_corn_predictions.rds")
# rcotton <- readRDS("data/rev_cotton_predictions.rds")
# rhay <- readRDS("data/rev_hay_predictions.rds")
# rsoybean <- readRDS("data/rev_soybean_predictions.rds")
# rwheat <- readRDS("data/rev_wheat_predictions.rds")
# 
# rcorn$crop <- "Corn"
# rcotton$crop <- "Cotton"
# rhay$crop <- "Hay"
# rsoybean$crop <- "Soybean"
# rwheat$crop <- "Wheat"
# rdat <- rbind(rcorn, rcotton, rhay, rsoybean, rwheat)
# 
# # Share acres (SUR)
# sdat <- readRDS("data/sur_predictions.rds")
# rdat <- select(rdat, names(sdat))
# 
# sdat <- arrange(sdat, type, effect, temp, crop)
# rdat <- arrange(rdat, type, effect, temp, crop)
# head(sdat)
# head(rdat)
# 
# pdat <- data.frame(temp = sdat$temp,
#                    crop = sdat$crop,
#                    type = sdat$type,
#                    effect = sdat$effect,
#                    rev = rdat$sum,
#                    acres = sdat$sum,
#                    rev_acres = rdat$sum*sdat$sum)
# 
# sacres <- cropdat %>% 
#   filter(year >= 1980) %>% 
#   summarise(corn_m = mean(corn_grain_a, na.rm = TRUE),
#             cotton_m = mean(cotton_a, na.rm = TRUE),
#             hay_m = mean(hay_a, na.rm = TRUE),
#             soybean_m = mean(soybean_a, na.rm = TRUE),
#             wheat_m = mean(wheat_a, na.rm = TRUE))
# 
# trev <- filter(pdat, effect == "Total-effect")
# climate_rev <- filter(pdat, effect == "Climate-effect" & type != "60-year")
# trev$avg_acres <- rep(c(sacres$corn_m, sacres$cotton_m, sacres$hay_m, sacres$soybean_m, sacres$wheat_m), 24)
# trev$climate_acres <- climate_rev$acres
# head(trev)
# climate_rev
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# pdat$no_adapt_acres <- rep(c(sacres$corn_m, sacres$cotton_m, sacres$hay_m, sacres$soybean_m, sacres$wheat_m), 78)
# pdat$no_adapt_rev_acres <-  pdat$rev*pdat$no_adapt_acres
# 
# head(pdat)
# #head(pdat)
# 
# pdat <- pdat %>% 
#   group_by(temp, type, effect) %>% 
#   summarise(rev_acres = sum(rev_acres),
#             no_adapt_rev_acres = sum(no_adapt_rev_acres)) %>% 
#   group_by(type, effect) %>% 
#   mutate(change_total = 100*(rev_acres - first(rev_acres))/first(rev_acres),
#          no_adapt_change_total = 100*(no_adapt_rev_acres - first(no_adapt_rev_acres))/first(no_adapt_rev_acres))
# 
# 
# pdat <- filter(pdat, effect != "Weather-effect")
# pdat <- filter(pdat, effect != "Total-effect")
# ggplot(pdat, aes(temp, change_total, color = effect)) + 
#   geom_line() + facet_wrap(~type, scales = "free") + ylab("% Change in Total Revenue") +
#   geom_line(aes(temp, no_adapt_change_total))
# 
# ggplot(pdat, aes(temp, no_adapt_change_total, coor = effect)) + geom_line() + facet_wrap(~type, scales = "free")






# newpdat <- pdat %>% 
#   group_by(type, )
# 
# 
# scorn <- filter(sdat, crop == "Corn")
# scotton <- filter(sdat, crop == "Cotton")
# shay <- filter(sdat, crop == "Hay")
# ssoybean <- filter(sdat, crop == "Soybean")
# swheat <- filter(sdat, crop == "Wheat")
# 
# sacres <- cropdat %>% 
#   filter(year >= 1980) %>% 
#   summarise(corn_m = mean(corn_grain_a, na.rm = TRUE),
#             cotton_m = mean(cotton_a, na.rm = TRUE),
#             hay_m = mean(hay_a, na.rm = TRUE),
#             soybean_m = mean(soybean_a, na.rm = TRUE),
#             wheat_m = mean(wheat_a, na.rm = TRUE))
# head(sacres)
# 
# cacres <- scorn
# 
# scorn <- arrange(scorn, type, effect, temp)
# rcorn <- arrange(rcorn, type, effect, temp)
# 
# scotton <- arrange(scotton, type, effect, temp)
# rcotton <- arrange(rcotton, type, effect, temp)
# 
# shay <- arrange(shay, type, effect, temp)
# rhay <- arrange(rhay, type, effect, temp)
# 
# ssoybean <- arrange(ssoybean, type, effect, temp)
# rsoybean <- arrange(rsoybean, type, effect, temp)
# 
# swheat <- arrange(swheat, type, effect, temp)
# rwheat <- arrange(rwheat, type, effect, temp)
# head(scorn)
# head(rcorn)
# 
# pdat <- data.frame(temp = scorn$temp,
#            type = scorn$type,
#            effect = scorn$effect,
#            rscorn = scorn$sum*rcorn$sum,
#            rscotton = scotton$sum*rcotton$sum,
#            rshay = shay$sum*rhay$sum,
#            rssoybean = ssoybean$sum*rsoybean$sum,
#            rswheat = swheat$sum*rwheat$sum)
# 
# head(pdat)
# pdat <- filter(pdat, effect != "Weather-effect")
# head(pdat)
# # No crop switching data
# # pdat <- filter(pdat, effect == "Total-effect")
# 
# 
# pdat$total <- rowSums(pdat[, 4:8])
# head(pdat)
# 
# pdat <- pdat %>% 
#   group_by(type, effect) %>% 
#   mutate(change_total = 100*(total - first(total))/first(total))
# 
# ggplot(pdat, aes(temp, change_total, color = effect)) + geom_line() + facet_wrap(~type, scales = "free") + ylab("% Change in Total Revenue")
# 
# mean(cropdat$acres)
# mean(c(cropdat$corn_grain_a, cropdat$cotton_a, cropdat$hay_a, cropdat$soybean_a, cropdat$wheat_a), na.rm = TRUE)
# # 
# # sdat <- sdat %>% 
# #   group_by(temp, type, effect) %>% 
# #   summarise(sum = sum(sum))
# # 
# # rdat
# # head(rdat)
# # head(sdat)
# # 
# # rdat <- select(rdat, temp, model, effect, sum)
# # names(rdat)[2] <- "type"
# # 
# # pdat <- data.frame(temp = sdat$temp,
# #                    type = sdat$type,
# #                    effect = sdat$effect,
# #                    a_sum = sdat$sum)
# # 
# # 
# # 
# # head(pdat)
# # pdat <- left_join(pdat, rdat, by = c("temp", "type", "effect"))
# # names(pdat)[5] <- "r_sum"
# #  
# switchdat <- filter(cropdat, year >= 1980)
# c_acres <- mean(switchdat$acres, na.rm = TRUE)
# 
# 
# cdat <- data.frame(temp = rep(unique(pdat$temp), 5),
#                    type = rep(unique(pdat$type), 6))
# 
# cdat
# 
# # cdat <- pdat
# cdat <- as.data.frame(filter(pdat, effect == "Total-effect"))
# newcdat <- as.data.frame(filter(pdat, effect == "Climate-effect"))
# newcdat <- filter(newcdat, type == "60-year")
# cdat <- rbind(cdat, newcdat)
# 
# cdat$effect <- "w/o Adaptation"
# cdat$a_sum <- c_acres
# 
# cdat$sum <- cdat$a_sum*cdat$r_sum
# 
# cdat <- cdat %>% 
#   group_by(type, effect) %>% 
#   mutate(change = (sum - first(sum))/first(sum))
# 
# cdat$change <- cdat$change*100
# # 
# # cdat$type <- rep(unique(pdat$type), 6)
# # cdat$a_sum <- c_acres
# # cdat$effect <- "w/o Adaptation"
# # cdat
# # 
# # pdat <- rbind(pdat, cdat)
# # pdat
# 
# 
# 
# 
# 
# pdat$sum <- pdat$a_sum*pdat$r_sum
# 
# pdat
# pdat <- pdat %>% 
#   group_by(type, effect) %>% 
#   mutate(change = (sum - first(sum))/first(sum))
# 
# pdat$change <- pdat$change*100
# pdat
# pdat <- rbind(pdat, cdat)
# 
# pdat$effect <- factor(pdat$effect, levels = c("Weather-effect", "Climate-effect", "Total-effect", "w/o Adaptation"))
# 
# ggplot(pdat, aes(temp, change, color = effect)) + 
#   geom_point(size = 1, alpha = 0.5) + 
#   geom_line() + 
#   facet_wrap(~type, ) +
#   theme_tufte(base_size = 12) +
#   ylab("% Change in Total Revenue") +
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
