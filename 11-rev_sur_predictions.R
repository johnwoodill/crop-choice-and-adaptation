# install.packages("dplyr")
# install.packages("tidyr")
#
library(cowplot)
library(boot)
library(dplyr)
library(ggthemes)
library(tidyr)

source("R/main_plot_bs.R")

# setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")
# setwd("/home/johnw/Projects/adaptation-and-crop-choice/")
# dir.create("data")
# 
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/full_ag_data.rds",
#               destfile = "data/full_ag_data.rds", method = "auto")
# 
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/cten.rds",
#               destfile = "data/cten.rds", method = "auto")
# 
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/ctwenty.rds",
#               destfile = "data/ctwenty.rds", method = "auto")
# 
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/cthirty.rds",
#               destfile = "data/cthirty.rds", method = "auto")
# 
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/rev_crop_predictions.rds",
#               destfile = "data/rev_crop_predictions.rds", method = "auto")
# 
# 
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/sur_rev_predictions.rds",
#               destfile = "data/sur_rev_predictions.rds", method = "auto")

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

# Merge avg crops
nmdat <- select(cropdat, fips)
nmdat <- left_join(nmdat, mdat, by = "fips")
head(nmdat)

# If predicted rev < 0 = 0
sur_rev$corn_rev <- ifelse(sur_rev$corn_rev < 0, 0, sur_rev$corn_rev)
sur_rev$cotton_rev <- ifelse(sur_rev$cotton_rev < 0, 0, sur_rev$cotton_rev)
sur_rev$hay_rev <- ifelse(sur_rev$hay_rev < 0, 0, sur_rev$hay_rev)
sur_rev$soybean_rev <- ifelse(sur_rev$soybean_rev < 0, 0, sur_rev$soybean_rev)
sur_rev$wheat_rev <- ifelse(sur_rev$wheat_rev < 0, 0, sur_rev$wheat_rev)


#----------------------------------------------------------------------------------------------
#Aggregate revenue per as.character
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
  select(temp, interval, effect, change, change_min, change_max) %>% 
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
         change_max = NA,
         change_min = NA,
         effect = "Weather-effect") %>% 
  ungroup()

pdat2_1 <- pdat2; pdat2_1$interval = "10-year"
pdat2_2 <- pdat2; pdat2_2$interval = "11-year"
pdat2_3 <- pdat2; pdat2_3$interval = "12-year"

pdat2 <- rbind(pdat2_1, pdat2_2, pdat2_3)
pdat2 <- select(pdat2, temp, interval, effect, change, change_min, change_max)

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

# Bootstrap se
# bs_se <- main_plot_bs(sur_rev, 
#                       cten$predictions, 
#                       ctwenty$predictions, 
#                       cthirty$predictions, 
#                       cropdat$year, 
#                       rep = 5, 
#                       cores = 1)

# dput from %dopar% run
bs_se <- structure(list(temp = c(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
4, 4, 5, 5, 5), interval = c("10-year", "11-year", "12-year", 
"10-year", "11-year", "12-year", "10-year", "11-year", "12-year", 
"10-year", "11-year", "12-year", "10-year", "11-year", "12-year", 
"10-year", "11-year", "12-year"), effect = c("Weather-climate-effect", 
"Weather-climate-effect", "Weather-climate-effect", "Weather-climate-effect", 
"Weather-climate-effect", "Weather-climate-effect", "Weather-climate-effect", 
"Weather-climate-effect", "Weather-climate-effect", "Weather-climate-effect", 
"Weather-climate-effect", "Weather-climate-effect", "Weather-climate-effect", 
"Weather-climate-effect", "Weather-climate-effect", "Weather-climate-effect", 
"Weather-climate-effect", "Weather-climate-effect"), se = c(1.10687426459601, 
1.10019542674658, 1.11087909255267, 0.883431284603084, 0.866155733534474, 
0.868790000057946, 0.849260420601032, 0.843845270909884, 0.867311791045779, 
0.936412501678389, 0.941687062622815, 0.977774469763472, 1.13404345458505, 
1.16723580604361, 1.20914286314339, 1.52255652793542, 1.49276437810352, 
1.47889152862132)), .Names = c("temp", "interval", "effect", 
"se"), class = c("grouped_df", "tbl_df", "tbl", "data.frame"), row.names = c(NA, 
-18L), vars = c("temp", "interval"), drop = TRUE)

head(bs_se)



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
  left_join(filter(bs_se, interval == "10-year"), by = "temp") %>% 
  mutate(change = 100*(total - first(total))/first(total),
         change_max = 100*((total + se*1.96)/first(total) - 1),
         change_min = 100*((total - se*1.96)/first(total) - 1),
         interval = "10-year",
         effect = "Weather-climate-effect") %>% 
  select(temp, interval, effect, change, change_min, change_max) %>% 
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
  left_join(filter(bs_se, interval == "11-year"), by = "temp") %>% 
  mutate(change = 100*(total - first(total))/first(total),
         change_max = 100*((total + se*1.96)/first(total) - 1),
         change_min = 100*((total - se*1.96)/first(total) - 1),
         interval = "11-year",
         effect = "Weather-climate-effect") %>% 
  select(temp, interval, effect, change, change_min, change_max) %>% 
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
  left_join(filter(bs_se, interval == "12-year"), by = "temp") %>% 
  mutate(change = 100*(total - first(total))/first(total),
         change_max = 100*((total + se*1.96)/first(total) - 1),
         change_min = 100*((total - se*1.96)/first(total) - 1),
         interval = "12-year",
         effect = "Weather-climate-effect") %>% 
  select(temp, interval, effect, change, change_min, change_max) %>% 
  ungroup()
pdat3 <- rbind(cdat1, cdat2, cdat3)



ggplot(pdat3, aes(temp, change, color = interval)) + geom_line()

pdat1$panel = 2
pdat2$panel = 1
pdat3$panel = 1

pdat <- rbind(pdat1, pdat2, pdat3)

# pdat$

ggplot(pdat, aes(temp, change, color = effect)) + geom_line() + 
  geom_point(aes(color = effect), size = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  geom_ribbon(aes(ymax = change_max, ymin = change_min, x = temp, linetype = NA), fill = "#C0CCD9", alpha = 0.5) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Revenue/Acre") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  #annotate("text", x = 3, y = 30, label = "asdf") +
  geom_text(data = filter(pdat, temp == 3 & effect == "Weather-climate-effect" & panel == 1), aes(label = effect), 
            vjust = -3, size = 2) + 
  geom_text(data = filter(pdat, temp == 3 & effect == "Weather-effect" & panel == 1), aes(label = effect), 
            vjust = 3, size = 2) + 
  geom_text(data = filter(pdat, temp == 3 & effect == "Weather-climate-effect" & panel == 2), aes(label = effect), 
            vjust = -1.5, size = 2, hjust = .3) + 
  geom_text(data = filter(pdat, temp == 3 & effect == "Weather-effect" & panel == 2), aes(label = effect), 
            vjust = 3, size = 2) + 
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  ylim(-60, 60) +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 1,
  #                              override.aes = list(linetype = c(1, 1),
  #                                                  size = 1.5,
  #                                                  shape = c(NA, NA)))) +
    facet_wrap(panel~interval, labeller = labeller(
      panel = c('1' = 'Disaggregate Revenue', '2' = 'Aggregate Revenue'))) +
    theme(legend.position = "none",
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=8))


ggsave("figures/main_plot.pdf", width = 6, height = 4)

