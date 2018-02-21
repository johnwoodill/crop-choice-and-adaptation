# install.packages("dplyr")
# install.packages("tidyr")
#
# library(tidyverse)
# library(ggthemes)
library(cowplot)
library(boot)
library(dplyr)
library(ggthemes)
# library(doParallel)
library(tidyr)

# setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")
# setwd("/home/johnw/Projects/adaptation-and-crop-choice/")
# dir.create("data")
# 
#  download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1",
#                destfile = "data/full_ag_data.rds", method = "auto")
# 
# 
#  download.file("https://www.dropbox.com/s/0ou8valnkcy2g4w/cten.rds?raw=1",
#                destfile = "data/cten.rds", method = "auto")
# 
#  download.file("https://www.dropbox.com/s/tvn4efevmicqjes/ctwenty.rds?raw=1",
#                destfile = "data/ctwenty.rds", method = "auto")
# 
#  download.file("https://www.dropbox.com/s/b2sfagm24fvg64o/cthirty.rds?raw=1",
#                destfile = "data/cthirty.rds", method = "auto")
# 
#  download.file("https://www.dropbox.com/s/lz1m3chiok48l54/sur_rev_predictions.rds?raw=1",
#                destfile = "data/sur_rev_predictions.rds", method = "auto")
# 
#  download.file("https://www.dropbox.com/s/ktdboxifdcczpxu/rev_crop_predictions.rds?raw=1",
#                destfile = "data/rev_crop_predictions.rds", method = "auto")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

# Aggregate revenue per acre (Total-effect)
rev_crop_pred <- readRDS("data/rev_crop_predictions.rds")

# SUR Crop (Individual Crop revenue effect)
sur_rev <- readRDS("data/sur_rev_predictions.rds")

# Sur Share (Individual Climate Share effect)
cten <- readRDS("data/cten.rds")
ctwenty <- readRDS("data/ctwenty.rds")
cthirty <- readRDS("data/cthirty.rds")

# Get average crop since 1980
mdat <- cropdat %>% 
  select(year, fips, corn_grain_a, cotton_a, hay_a, wheat_a, soybean_a) %>% 
  filter(year >= 1980) %>%
  group_by(fips) %>% 
  summarise(avg_corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
         avg_cotton_a = mean(cotton_a, na.rm = TRUE),
         avg_hay_a = mean(hay_a, na.rm = TRUE),
         avg_wheat_a = mean(wheat_a, na.rm = TRUE),
         avg_soybean_a = mean(soybean_a, na.rm = TRUE))

nmdat <- select(cropdat, fips)
nmdat <- left_join(nmdat, mdat, by = "fips")
head(nmdat)

sur_rev$corn_rev <- ifelse(sur_rev$corn_rev < 0, 0, sur_rev$corn_rev)
sur_rev$cotton_rev <- ifelse(sur_rev$cotton_rev < 0, 0, sur_rev$cotton_rev)
sur_rev$hay_rev <- ifelse(sur_rev$hay_rev < 0, 0, sur_rev$hay_rev)
sur_rev$soybean_rev <- ifelse(sur_rev$soybean_rev < 0, 0, sur_rev$soybean_rev)
sur_rev$wheat_rev <- ifelse(sur_rev$wheat_rev < 0, 0, sur_rev$wheat_rev)


#----------------------------------------------------------------------------------------------
#Aggregate revenue per as.character
#
head(rev_crop_pred)
rev_crop_pred$fips <- cropdat$fips

rev_crop_pred$rev_max <- rev_crop_pred$rev.pred + 1.96*rev_crop_pred$rev.se
rev_crop_pred$rev_min <- rev_crop_pred$rev.pred - 1.96*rev_crop_pred$rev.se


pdat1 <- rev_crop_pred %>% 
  group_by(fips, effect, interval, temp) %>%
  summarise_all(mean) %>% 
  group_by(effect, interval, temp) %>% 
  summarise_all(sum) %>% 
  group_by(effect, interval) %>%
  mutate(change = 100*(rev.pred - first(rev.pred))/first(rev.pred),
         change_max = 100*(rev_max - first(rev.pred))/first(rev.pred),
         change_min = 100*(rev_min - first(rev.pred))/first(rev.pred)) %>% 
  select(temp, interval, effect, change) %>% 
  ungroup()

head(pdat1)



ggplot(pdat1, aes(temp, change, group = effect)) + 
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
  

# Weather effect from SUR rev model
head(sur_rev)
sur_rev$fips <- cropdat$fips
nsur_rev <- sur_rev %>% 
  group_by(fips, temp) %>% 
  summarise_all(mean) %>% 
  group_by(temp) %>% 
  summarise_all(sum) %>% 
  mutate(total_rev = corn_rev + cotton_rev + hay_rev + soybean_rev + wheat_rev)

# sur_rev$total_rev <- rowSums(sur_rev[, 2:6])

fchange <- function(x) 100*(x - first(x))/first(x)

sur_rev %>% 
  select(-fips) %>% 
  group_by(temp) %>% 
  summarise_all(sum) %>% 
  mutate_all(fchange)

pdat2 <- nsur_rev %>% 
  group_by(temp) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  mutate(change = 100*(total_rev - first(total_rev))/first(total_rev),
         effect = "Weather-effect") %>% 
  ungroup()

pdat2_1 <- pdat2; pdat2_1$interval = "10-year"
pdat2_2 <- pdat2; pdat2_2$interval = "20-year"
pdat2_3 <- pdat2; pdat2_3$interval = "30-year"

pdat2 <- rbind(pdat2_1, pdat2_2, pdat2_3)
pdat2 <- select(pdat2, temp, interval, effect, change)

ggplot(pdat2, aes(temp, change, color = interval)) + geom_line() 


# Weather-climate effects
sur_rev$fips <- cropdat$fips
sur_rev$corn_trev <- sur_rev$corn_rev*cropdat$corn_grain_a
sur_rev$cotton_trev <- sur_rev$cotton_rev*cropdat$cotton_a
sur_rev$hay_trev <- sur_rev$hay_rev*cropdat$hay_a
sur_rev$soybean_trev <- sur_rev$soybean_rev*cropdat$soybean_a
sur_rev$wheat_trev <- sur_rev$wheat_rev*cropdat$wheat_a

cten$predictions$fips <- cropdat$fips
ctwenty$predictions$fips <- cropdat$fips
cthirty$predictions$fips <- cropdat$fips

cten$predictions$corn_acres <- cten$predictions$corn.pred*cropdat$acres
cten$predictions$cotton_acres <- cten$predictions$cotton.pred*cropdat$acres
cten$predictions$hay_acres <- cten$predictions$hay.pred*cropdat$acres
cten$predictions$soybean_acres <- cten$predictions$soybean.pred*cropdat$acres
cten$predictions$wheat_acres <- cten$predictions$wheat.pred*cropdat$acres

ctwenty$predictions$corn_acres <- ctwenty$predictions$corn.pred*cropdat$acres
ctwenty$predictions$cotton_acres <- ctwenty$predictions$cotton.pred*cropdat$acres
ctwenty$predictions$hay_acres <- ctwenty$predictions$hay.pred*cropdat$acres
ctwenty$predictions$soybean_acres <- ctwenty$predictions$soybean.pred*cropdat$acres
ctwenty$predictions$wheat_acres <- ctwenty$predictions$wheat.pred*cropdat$acres

cthirty$predictions$corn_acres <- cthirty$predictions$corn.pred*cropdat$acres
cthirty$predictions$cotton_acres <- cthirty$predictions$cotton.pred*cropdat$acres
cthirty$predictions$hay_acres<- cthirty$predictions$hay.pred*cropdat$acres
cthirty$predictions$soybean_acres <- cthirty$predictions$soybean.pred*cropdat$acres
cthirty$predictions$wheat_acres <- cthirty$predictions$wheat.pred*cropdat$acres


rev <- sur_rev %>% 
  group_by(temp, fips) %>% 
  summarise(corn_rev = mean(corn_trev),
            cotton_rev = mean(cotton_trev),
            hay_rev = mean(hay_trev),
            soybean_rev = mean(soybean_trev),
            wheat_rev = mean(wheat_trev))

cten_acres <- cten$predictions %>% 
  group_by(temp, fips) %>% 
  summarise(corn_acres = mean(corn_acres),
            cotton_acres = mean(cotton_acres),
            hay_acres = mean(hay_acres),
            soybean_acres = mean(soybean_acres),
            wheat_acres = mean(wheat_acres))

ctwenty_acres <- ctwenty$predictions %>% 
  group_by(temp, fips) %>% 
  summarise(corn_acres = mean(corn_acres),
            cotton_acres = mean(cotton_acres),
            hay_acres = mean(hay_acres),
            soybean_acres = mean(soybean_acres),
            wheat_acres = mean(wheat_acres))

cthirty_acres <- cthirty$predictions %>% 
  group_by(temp, fips) %>% 
  summarise(corn_acres = mean(corn_acres),
            cotton_acres = mean(cotton_acres),
            hay_acres = mean(hay_acres),
            soybean_acres = mean(soybean_acres),
            wheat_acres = mean(wheat_acres))

cdat1 <- left_join(rev, cten_acres, by = c("temp", "fips"))
cdat2 <- left_join(rev, ctwenty_acres, by = c("temp", "fips"))
cdat3 <- left_join(rev, cthirty_acres, by = c("temp", "fips"))

cdat1 <- cdat1 %>% 
  group_by(temp) %>% 
  summarise_all(sum) %>% 
  mutate(corn = corn_rev/corn_acres,
         cotton = cotton_rev/cotton_acres,
         hay = hay_rev/hay_acres,
         soybean = soybean_rev/soybean_acres,
         wheat = wheat_rev/wheat_acres) %>% 
  select(temp, corn, cotton, hay, soybean, wheat) %>% 
  mutate(total = corn + cotton + hay + soybean + wheat) %>% 
  mutate(change = 100*(total - first(total))/first(total),
         interval = "10-year",
         effect = "Weather-climate-effect") %>% 
  select(temp, interval, effect, change) %>% 
  ungroup()
  
cdat2 <- cdat2 %>% 
  group_by(temp) %>% 
  summarise_all(sum) %>% 
  mutate(corn = corn_rev/corn_acres,
         cotton = cotton_rev/cotton_acres,
         hay = hay_rev/hay_acres,
         soybean = soybean_rev/soybean_acres,
         wheat = wheat_rev/wheat_acres) %>% 
  select(temp, corn, cotton, hay, soybean, wheat) %>% 
  mutate(total = corn + cotton + hay + soybean + wheat) %>% 
  mutate(change = 100*(total - first(total))/first(total),
         interval = "20-year",
         effect = "Weather-climate-effect") %>% 
  select(temp, interval, effect, change) %>% 
  ungroup()

cdat3 <- cdat3 %>% 
  group_by(temp) %>% 
  summarise_all(sum) %>% 
  mutate(corn = corn_rev/corn_acres,
         cotton = cotton_rev/cotton_acres,
         hay = hay_rev/hay_acres,
         soybean = soybean_rev/soybean_acres,
         wheat = wheat_rev/wheat_acres) %>% 
  select(temp, corn, cotton, hay, soybean, wheat) %>% 
  mutate(total = corn + cotton + hay + soybean + wheat) %>% 
  mutate(change = 100*(total - first(total))/first(total),
         interval = "30-year",
         effect = "Weather-climate-effect") %>% 
  select(temp, interval, effect, change) %>% 
  ungroup()

pdat3 <- rbind(cdat1, cdat2, cdat3)

ggplot(pdat3, aes(temp, change, color = interval)) + geom_line()

pdat1$panel = 1
pdat2$panel = 2
pdat3$panel = 2

pdat <- rbind(pdat1, pdat2, pdat3)


ggplot(pdat, aes(temp, change, color = effect)) + geom_line() + 
  geom_point(aes(color = effect), size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Total Revenue/Acre") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  # scale_y_continuous(breaks = seq(50,-50, by = -10), labels = c("50%", "40%", "30%", "20%", "10%", "0", "-10%", "-20%", "-30%", "-40%", "-50%")) +
  guides(color = guide_legend(keywidth = 1.5, keyheight = 1,
                                override.aes = list(linetype = c(1, 1),
                                                    size = 1.5,
                                                    shape = c(NA, NA)))) +
    facet_wrap(panel~interval, labeller = labeller(
      panel = c('1' = 'Aggregate Revenue', '2' = 'Disaggregated Revenue')), scales = "free") +
    theme(legend.position = "top",
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=8))
  facet_wrap(type~interval, scales = "free")


ggsave("figures/main_plot.pdf", width = 6, height = 4)






# 
# 
# # Multiply by acres to get Total rev, then weight by predicted share of acres
# sur_rev$corn_climate_ten <- (sur_rev$corn_rev*cropdat$corn_grain_a)/(cten$predictions$corn.pred*cropdat$acres)
# sur_rev$cotton_climate_ten <- (sur_rev$cotton_rev*cropdat$cotton_a)/(cten$predictions$cotton.pred*cropdat$acres)
# sur_rev$hay_climate_ten <- (sur_rev$hay_rev*cropdat$hay_a)/(cten$predictions$hay.pred*cropdat$acres)
# sur_rev$soybean_climate_ten <- (sur_rev$soybean_rev*cropdat$soybean_a)/(cten$predictions$soybean.pred*cropdat$acres)
# sur_rev$wheat_climate_ten <- (sur_rev$wheat_rev*cropdat$wheat_a)/(cten$predictions$wheat.pred*cropdat$acres)
# 
# sur_rev$corn_climate_twenty <- (sur_rev$corn_rev*cropdat$corn_grain_a)/(ctwenty$predictions$corn.pred*cropdat$acres)
# sur_rev$cotton_climate_twenty <- (sur_rev$cotton_rev*cropdat$cotton_a)/(ctwenty$predictions$cotton.pred*cropdat$acres)
# sur_rev$hay_climate_twenty <- (sur_rev$hay_rev*cropdat$hay_a)/(ctwenty$predictions$hay.pred*cropdat$acres)
# sur_rev$soybean_climate_twenty <- (sur_rev$soybean_rev*cropdat$soybean_a)/(ctwenty$predictions$soybean.pred*cropdat$acres)
# sur_rev$wheat_climate_twenty <- (sur_rev$wheat_rev*cropdat$wheat_a)/(ctwenty$predictions$wheat.pred*cropdat$acres)
# 
# sur_rev$corn_climate_thirty <- (sur_rev$corn_rev*cropdat$corn_grain_a)/(cthirty$predictions$corn.pred*cropdat$acres)
# sur_rev$cotton_climate_thirty <- (sur_rev$cotton_rev*cropdat$cotton_a)/(cthirty$predictions$cotton.pred*cropdat$acres)
# sur_rev$hay_climate_thirty <- (sur_rev$hay_rev*cropdat$hay_a)/(cthirty$predictions$hay.pred*cropdat$acres)
# sur_rev$soybean_climate_thirty <- (sur_rev$soybean_rev*cropdat$soybean_a)/(cthirty$predictions$soybean.pred*cropdat$acres)
# sur_rev$wheat_climate_thirty <- (sur_rev$wheat_rev*cropdat$wheat_a)/(cthirty$predictions$wheat.pred*cropdat$acres)
# 
# head((cten$predictions$corn.pred*cropdat$acres) + (cten$predictions$cotton.pred*cropdat$acres) +
#   (cten$predictions$hay.pred*cropdat$acres) + (cten$predictions$soybean.pred*cropdat$acres) + (cten$predictions$wheat.pred*cropdat$acres))
# head(cropdat$acres)
# 
# head(sur_rev)
# 
# sur_rev$corn_climate_ten <- ifelse(is.infinite(sur_rev$corn_climate_ten), 0, sur_rev$corn_climate_ten)
# sur_rev$cotton_climate_ten <- ifelse(is.infinite(sur_rev$cotton_climate_ten), 0, sur_rev$cotton_climate_ten)
# sur_rev$hay_climate_ten <- ifelse(is.infinite(sur_rev$hay_climate_ten), 0, sur_rev$hay_climate_ten)
# sur_rev$soybean_climate_ten <- ifelse(is.infinite(sur_rev$soybean_climate_ten), 0, sur_rev$soybean_climate_ten)
# sur_rev$wheat_climate_ten <- ifelse(is.infinite(sur_rev$wheat_climate_ten), 0, sur_rev$wheat_climate_ten)
# 
# sur_rev$corn_climate_ten <- ifelse(is.na(sur_rev$corn_climate_ten), 0, sur_rev$corn_climate_ten)
# sur_rev$cotton_climate_ten <- ifelse(is.na(sur_rev$cotton_climate_ten), 0, sur_rev$cotton_climate_ten)
# sur_rev$hay_climate_ten <- ifelse(is.na(sur_rev$hay_climate_ten), 0, sur_rev$hay_climate_ten)
# sur_rev$soybean_climate_ten <- ifelse(is.na(sur_rev$soybean_climate_ten), 0, sur_rev$soybean_climate_ten)
# sur_rev$wheat_climate_ten <- ifelse(is.na(sur_rev$wheat_climate_ten), 0, sur_rev$wheat_climate_ten)
# 
# test <- select(sur_rev, temp, corn_climate_ten, cotton_climate_ten, hay_climate_ten, soybean_climate_ten, wheat_climate_ten)
# test %>% 
#   group_by(temp) %>% 
#   summarise_all(sum) %>% 
#   mutate_all(fchange)
# 
# sur_rev$total_rev_ten <- rowSums(sur_rev[, c("corn_climate_ten", "cotton_climate_ten", "hay_climate_ten", "soybean_climate_ten", "wheat_climate_ten")], na.rm = TRUE)
# sur_rev$total_rev_twenty <- rowSums(sur_rev[, c("corn_climate_twenty", "cotton_climate_twenty", "hay_climate_twenty", "soybean_climate_twenty", "wheat_climate_twenty")], na.rm = TRUE)
# sur_rev$total_rev_thirty <- rowSums(sur_rev[, c("corn_climate_thirty", "cotton_climate_thirty", "hay_climate_thirty", "soybean_climate_thirty", "wheat_climate_thirty")], na.rm = TRUE)
# 
# sur_rev$total_rev_ten <- ifelse(is.infinite(sur_rev$total_rev_ten), NA, sur_rev$total_rev_ten)
# sur_rev$total_rev_twenty <- ifelse(is.infinite(sur_rev$total_rev_twenty), NA, sur_rev$total_rev_twenty)
# sur_rev$total_rev_thirty <- ifelse(is.infinite(sur_rev$total_rev_thirty), NA, sur_rev$total_rev_thirty)
# 
# pdat3 <- sur_rev %>% 
#   group_by(temp) %>% 
#   summarise(total_rev_ten = sum(total_rev_ten, na.rm = TRUE),
#             total_rev_twenty = sum(total_rev_twenty, na.rm = TRUE),
#             total_rev_thirty = sum(total_rev_thirty, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(change_ten = 100*(total_rev_ten - first(total_rev_ten))/first(total_rev_ten),
#          change_twenty = 100*(total_rev_twenty - first(total_rev_twenty))/first(total_rev_twenty),
#          change_thirty = 100*(total_rev_thirty - first(total_rev_thirty))/first(total_rev_thirty))
# 
# head(pdat3)
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
# 
# 
# 
# 
# 
# 
# 
# #-----------------------------------------------------------------------------------
# # Total-effect
# 
# # Remove negative values from predictions
# rev_crop_pred$modten_rev.pred <- ifelse(rev_crop_pred$modten_rev.pred < 0, 0, rev_crop_pred$modten_rev.pred)
# rev_crop_pred$modtwenty_rev.pred <- ifelse(rev_crop_pred$modtwenty_rev.pred < 0, 0, rev_crop_pred$modtwenty_rev.pred)
# rev_crop_pred$modthirty_rev.pred <- ifelse(rev_crop_pred$modthirty_rev.pred < 0, 0, rev_crop_pred$modthirty_rev.pred)
# 
# 
# 
# # w/ adaptation
# total_effect_w_adaptation_ten <- ((cten$predictions$corn.pred*cropdat$acres + 
#                                     cten$predictions$cotton.pred*cropdat$acres + 
#                                     cten$predictions$hay.pred*cropdat$acres + 
#                                     cten$predictions$soybean.pred*cropdat$acres + 
#                                     cten$predictions$wheat.pred*cropdat$acres)*rev_crop_pred$modten_rev.pred) 
#   
# total_effect_w_adaptation_twenty <- ((ctwenty$predictions$corn.pred*cropdat$acres + 
#                                     ctwenty$predictions$cotton.pred*cropdat$acres + 
#                                     ctwenty$predictions$hay.pred*cropdat$acres + 
#                                     ctwenty$predictions$soybean.pred*cropdat$acres + 
#                                     ctwenty$predictions$wheat.pred*cropdat$acres)*rev_crop_pred$modtwenty_rev.pred) 
# 
# total_effect_w_adaptation_thirty <- ((cthirty$predictions$corn.pred*cropdat$acres + 
#                                     cthirty$predictions$cotton.pred*cropdat$acres + 
#                                     cthirty$predictions$hay.pred*cropdat$acres + 
#                                     cthirty$predictions$soybean.pred*cropdat$acres + 
#                                     cthirty$predictions$wheat.pred*cropdat$acres)*rev_crop_pred$modthirty_rev.pred) 
# 
# # w/o adaptation
# total_effect_wo_adaptation_ten <- ((constant_acres)*rev_crop_pred$modten_rev.pred) 
#   
# total_effect_wo_adaptation_twenty <- ((constant_acres)*rev_crop_pred$modtwenty_rev.pred) 
# 
# total_effect_wo_adaptation_thirty <- ((constant_acres)*rev_crop_pred$modthirty_rev.pred) 
# 
# 
# tdat <- data.frame(temp = rev_crop_pred$temp,
#                    total_effect_w_adaptation_ten = total_effect_w_adaptation_ten,
#                    total_effect_w_adaptation_twenty = total_effect_w_adaptation_twenty,
#                    total_effect_w_adaptation_thirty = total_effect_w_adaptation_thirty,
#                    total_effect_wo_adaptation_ten = total_effect_wo_adaptation_ten,
#                    total_effect_wo_adaptation_twenty = total_effect_wo_adaptation_twenty,
#                    total_effect_wo_adaptation_thirty = total_effect_wo_adaptation_thirty)
# 
# bsum <- function(x,i) sum(x[i])
# # 
# 
# # tdat_se <- tdat %>%
# #  group_by(temp) %>%
# #  summarise(total_effect_w_adaptation_ten_se = sd(boot(tdat$total_effect_w_adaptation_ten, bsum, R = 2, parallel = "multicore", ncpus = 1)$t),
# #            total_effect_w_adaptation_twenty_se = sd(boot(tdat$total_effect_w_adaptation_twenty, bsum, R = 2, parallel = "multicore", ncpus = 1)$t),
# #            total_effect_w_adaptation_thirty_se = sd(boot(tdat$total_effect_w_adaptation_thirty, bsum, R = 2, parallel = "multicore", ncpus = 1)$t),
# #            total_effect_wo_adaptation_ten_se = sd(boot(tdat$total_effect_wo_adaptation_ten, bsum, R = 2, parallel = "multicore", ncpus = 1)$t),
# #            total_effect_wo_adaptation_twenty_se = sd(boot(tdat$total_effect_wo_adaptation_twenty, bsum, R = 2, parallel = "multicore", ncpus = 1)$t),
# #            total_effect_wo_adaptation_thirty_se = sd(boot(tdat$total_effect_wo_adaptation_thirty, bsum, R = 2, parallel = "multicore", ncpus = 1)$t))
# 
# tdat_se <- structure(list(temp = c(0, 1, 2, 3, 4, 5), total_effect_w_adaptation_ten_se = c(19073790202.906, 
# 19740204081.8862, 18962089673.5456, 19930084914.5478, 19356885005.4511, 
# 19551729107.7945), total_effect_w_adaptation_twenty_se = c(20288624647.0258, 
# 19995052402.5343, 20247284409.4748, 20377496305.5224, 20244882118.4208, 
# 20795093748.0419), total_effect_w_adaptation_thirty_se = c(29385105873.758, 
# 29183886072.3893, 29797870630.2517, 28306794237.1547, 29421155696.9885, 
# 28564991980.449), total_effect_wo_adaptation_ten_se = c(17328198656.3934, 
# 17858678788.8979, 17988467793.88, 17376515160.1466, 17296291309.0373, 
# 17600396659.6228), total_effect_wo_adaptation_twenty_se = c(18702940711.968, 
# 18093569361.8691, 18943875193.7337, 18309450542.6874, 18889964653.1173, 
# 18370925222.3326), total_effect_wo_adaptation_thirty_se = c(24637581936.2668, 
# 25525693012.456, 24798626986.7444, 25601753993.2751, 25108298730.2547, 
# 24808117215.4096)), class = c("tbl_df", "tbl", "data.frame"), .Names = c("temp", 
# "total_effect_w_adaptation_ten_se", "total_effect_w_adaptation_twenty_se", 
# "total_effect_w_adaptation_thirty_se", "total_effect_wo_adaptation_ten_se", 
# "total_effect_wo_adaptation_twenty_se", "total_effect_wo_adaptation_thirty_se"
# ), row.names = c(NA, -6L))
# 
# 
# tpdat <- tdat %>% 
#   group_by(temp) %>% 
#   summarise_all(sum) %>% 
#   ungroup() 
# 
# tpdat <- gather(tpdat, key = crop, value = value, -temp)
# tdat_se <- gather(tdat_se, key = crop, value = se, -temp)
# tdat_se$crop <- gsub("_se", "", tdat_se$crop)
# 
# tpdat <- left_join(tpdat, tdat_se, by = c("temp", "crop"))
# tpdat$min <- tpdat$value - tpdat$se*1.96
# tpdat$max <- tpdat$value + tpdat$se*1.96
# 
# tpdat <- tpdat %>% 
#   group_by(crop) %>% 
#   mutate(change = 100*(value - first(value))/first(value),
#              change_min = 100*(min - first(value))/first(value),
#              change_max = 100*(max - first(value))/first(value)) %>% 
#   ungroup()
# 
# 
# tpdat$interval <- rep(c("10-year", "20-year", "30-year"), by = 3, each = 6)
# tpdat$effect <- c(rep("Total effect (w/ adaptation)", 18), rep("Total effect (w/o adaptation)", 18))
# tpdat$crop <- NULL
# tpdat <- select(tpdat, temp, interval, effect, change, change_min, change_max)
# head(tpdat)
# 
# ggplot(tpdat, aes(temp, change, color = effect)) + geom_line() + 
#   geom_line(aes(temp, change_min), linetype = "dashed") +
#   geom_line(aes(temp, change_max), linetype = "dashed") + facet_wrap(~interval) 
# 
# 
# #----------------------------------------------------------------------------------------
# # Crop-effect
# 
# # Assign intervals
# cten$predictions$type <- "10-year"
# ctwenty$predictions$type <- "20-year"
# cthirty$predictions$type <- "30-year"
# 
# # Bind SUR climate data
# cdat <- bind_rows(cten$predictions, ctwenty$predictions, cthirty$predictions)
# cdat$effect <- "Crop-effect"
# sur_rev$temp <- NULL
# sur_rev <- rbind(sur_rev, sur_rev, sur_rev)
# cdat <- bind_cols(cdat, sur_rev)
# 
# # Remove negative values from predictions
# cdat$corn_rev <- ifelse(cdat$corn_rev < 0, 0, cdat$corn_rev)
# cdat$cotton_rev <- ifelse(cdat$cotton_rev < 0, 0, cdat$cotton_rev)
# cdat$hay_rev <- ifelse(cdat$hay_rev < 0, 0, cdat$hay_rev)
# cdat$soybean_rev <- ifelse(cdat$soybean_rev < 0, 0, cdat$soybean_rev)
# cdat$wheat_rev <- ifelse(cdat$wheat_rev < 0, 0, cdat$wheat_rev)
# 
# # Get crop acres by county
# cdat$corn.pred_ncs <- cdat$corn.pred*constant_acres
# cdat$cotton.pred_ncs <- cdat$cotton.pred*constant_acres
# cdat$hay.pred_ncs <- cdat$hay.pred*constant_acres
# cdat$soybean.pred_ncs <- cdat$soybean.pred*constant_acres
# cdat$wheat.pred_ncs <- cdat$wheat.pred*constant_acres
# 
# # Get crop acres by county
# cdat$corn.pred <- cdat$corn.pred*cropdat$acres
# cdat$cotton.pred <- cdat$cotton.pred*cropdat$acres
# cdat$hay.pred <- cdat$hay.pred*cropdat$acres
# cdat$soybean.pred <- cdat$soybean.pred*cropdat$acres
# cdat$wheat.pred <- cdat$wheat.pred*cropdat$acres
# 
# climate_effect <- (cdat$corn.pred*cdat$corn_rev) + (cdat$cotton.pred*cdat$cotton_rev) + (cdat$hay.pred*cdat$hay_rev) +
#   (cdat$soybean.pred*cdat$soybean_rev) + (cdat$wheat.pred*cdat$wheat_rev)
# 
# no_cs_effect <- (cdat$corn.pred_ncs*cdat$corn_rev) + (cdat$cotton.pred_ncs*cdat$cotton_rev) + (cdat$hay.pred_ncs*cdat$hay_rev) +
#   (cdat$soybean.pred_ncs*cdat$soybean_rev) + (cdat$wheat.pred_ncs*cdat$wheat_rev)
# 
# cpdat <-  data.frame(temp = cdat$temp,
#                      interval = cdat$type,
#                      climate_effect = climate_effect,
#                      no_cs_effect = no_cs_effect)
# head(cpdat)
# 
# 
# cpdat_se <- cpdat %>%
#  group_by(temp, interval) %>%
#  summarise(climate_effect_se = sd(boot(cpdat$climate_effect, bsum, R = 2000, parallel = "multicore", ncpus = 10)$t),
#            no_cs_effect_se = sd(boot(cpdat$no_cs_effect, bsum, R = 2000, parallel = "multicore", ncpus = 10)$t))
# 
# # cpdat_se <- cpdat %>%
# #  group_by(temp, interval) %>%
# #  summarise(climate_effect_se = sd(boot(cpdat$climate_effect, bsum, R = 2000)$t),
# #            no_cs_effect_se = sd(boot(cpdat$no_cs_effect, bsum, R = 2000)$t))
# 
# cpdat_se <- structure(list(temp = c(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4,
# 4, 4, 5, 5, 5), interval = structure(c(1L, 2L, 3L, 1L, 2L, 3L, 
# 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), .Label = c("10-year", 
# "20-year", "30-year"), class = "factor"), climate_effect_se = c(17162770787.7136, 
# 16511449418.8604, 16516497036.557, 17048234170.274, 16612557518.7988, 
# 16934512496.6512, 16881661827.3878, 16260316254.8786, 16836122180.2142, 
# 16668068685.4391, 16495741090.535, 16990070603.2612, 16750783296.0539, 
# 16932956463.6496, 16409738327.8969, 16899012963.3939, 16690644540.3352, 
# 17027740255.2907), no_cs_effect_se = c(16476036916.5247, 16112724262.157, 
# 16347586637.54, 16280438631.2917, 16564547977.1034, 16371055819.0225, 
# 16794426321.4232, 16461823595.627, 16179542557.4212, 16257916487.8651, 
# 16566092453.8316, 16003455775.3366, 16677241345.2355, 16493033566.4019, 
# 15979152798.4165, 16347460181.8961, 16421341245.4817, 16094963705.5546
# )), .Names = c("temp", "interval", "climate_effect_se", "no_cs_effect_se"
# ), row.names = c(NA, -18L), class = c("grouped_df", "tbl_df", 
# "tbl", "data.frame"), vars = "temp", drop = TRUE)
# 
# cpdat <- cpdat %>% 
#   group_by(temp, interval) %>% 
#   summarise_all(sum) %>% 
#   ungroup() 
# 
# cpdat <- gather(cpdat, key = effect, value = value, -temp, -interval)
# cpdat_se <- gather(cpdat_se, key = effect, value = se, -temp, -interval)
# cpdat_se$effect <- gsub("_se", "", cpdat_se$effect)
# 
# cpdat <- left_join(cpdat, cpdat_se, by = c("temp", "effect", "interval"))
# cpdat$min <- cpdat$value - cpdat$se*1.96
# cpdat$max <- cpdat$value + cpdat$se*1.96
# 
# 
# cpdat <- cpdat %>% 
#   group_by(interval, effect) %>% 
#   mutate(change = 100*(value - first(value))/first(value),
#          change_min = 100*(min - first(value))/first(value),
#          change_max = 100*(max - first(value))/first(value))
# 
#          
# head(cpdat)
# cpdat <- select(cpdat, temp, interval, effect, change, change_min, change_max)
# 
# 
# cpdat$effect <- factor(cpdat$effect, levels = c("no_cs_effect", "climate_effect"),
#                        labels = c("Crop effect (w/o adaptation)", "Crop effect (w/ adaptation)"))
# cpdat$panel <- 2
# tpdat$panel <- 1
# head(cpdat)
# head(tpdat)
# 
# pdat <- bind_rows(cpdat, tpdat)
# 
# pdat$effect <- factor(pdat$effect, levels = c("Total effect (w/ adaptation)", "Total effect (w/o adaptation)",
#                                               "Crop effect (w/ adaptation)", "Crop effect (w/o adaptation)")) 
# pdat
# ggplot(pdat, aes(x = temp, y = change, group = effect)) + 
#   geom_ribbon(aes(ymax = change_max, ymin = change_min), fill = "#C0CCD9", alpha = 0.5 ) +
#   geom_line(aes(color = effect)) + 
#   geom_point(aes(color = effect), size = 0.5) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
#   theme_tufte(base_size = 10) +
#   ylab("% Change in Total Revenue") +
#   xlab("Change in Temperature (C)") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
#   scale_y_continuous(breaks = seq(0,-50, by = -10), labels = c("0", "-10%", "-20%", "-30%", "-40%", "-50%")) +
#   guides(color = guide_legend(keywidth = 1, keyheight = 1,
#                                 override.aes = list(linetype = c(1, 1),
#                                                     size = 1.5,
#                                                     shape = c(NA, NA)))) +
#     facet_wrap(panel~interval, labeller = labeller(
#       panel = c('1' = '', '2' = ''))) +
#     theme(legend.position = "top", 
#         legend.box.background = element_rect(colour = "grey"), 
#         legend.title = element_blank(),
#         legend.key = element_rect(fill = NA, color = NA),
#         legend.text=element_text(size=6.3))
# 
# 
# ggsave("figures/1-main_rev_plot.pdf", width = 6, height = 5)
# 
# 
