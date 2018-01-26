# install.packages("dplyr")
# install.packages("ggthemes")
# install.packages("doParallel")
# install.packages("tidyr")

library(tidyverse)
library(ggthemes)
library(boot)
library(dplyr)
library(doParallel)
library(tidyr)

# # Setup parallel for bootstrapping
# cl <- makeCluster(20)
# registerDoParallel(cl)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1",
#               destfile = "data/full_ag_data.rds", method = "auto")

# dir.create("data")
# download.file("https://www.dropbox.com/s/ec66n5lpnf93kx0/cten.rds?raw=1",
#               destfile = "data/cten.rds", method = "auto")
# download.file("https://www.dropbox.com/s/z76xz06mciubjs8/cthirty.rds?raw=1",
#               destfile = "data/cthirty.rds", method = "auto")
# download.file("https://www.dropbox.com/s/f39oeticdhasci3/ctwenty.rds?raw=1",
#               destfile = "data/ctwenty.rds", method = "auto")
# download.file("https://www.dropbox.com/s/t4e95ep1zhyenon/full_ag_data.rds?raw=1",
#               destfile = "data/full_ag_data.rds", method = "auto")
# download.file("https://www.dropbox.com/s/5m12yu2dxhudynb/rev_crop_pred.rds?raw=1",
#               destfile = "data/rev_crop_pred.rds", method = "auto")

# Aggregate revenue per acre (Total-effect)
rev_crop_pred <- readRDS("data/rev_crop_pred.rds")

# SUR Crop (Individual Crop revenue effect)
sur_rev <- readRDS("data/sur_rev_predictions.rds")

# Sur Share (Individual Climate Share effect)
cten <- readRDS("data/cten.rds")
ctwenty <- readRDS("data/ctwenty.rds")
cthirty <- readRDS("data/cthirty.rds")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

# Get average crop since 1980
mdat <- cropdat %>% 
  select(year, fips, corn_grain_a, cotton_a, hay_a, wheat_a, soybean_a) %>% 
  filter(year >= 1980) %>% 
  group_by(fips) %>% 
  summarise(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
         cotton_a = mean(cotton_a, na.rm = TRUE),
         hay_a = mean(hay_a, na.rm = TRUE),
         wheat_a = mean(wheat_a, na.rm = TRUE),
         soybean_a = mean(soybean_a, na.rm = TRUE))

dat <- cropdat
dat$corn_grain_a <- NULL
dat$cotton_a <- NULL
dat$hay_a <- NULL
dat$soybean_a <- NULL
dat$wheat_a <- NULL

dat <- left_join(dat, mdat, by = "fips")
constant_acres <- (dat$corn_grain_a + dat$cotton_a + dat$hay_a + dat$soybean_a + dat$wheat_a)
sum(constant_acres)
sum(cropdat$acres)

#-----------------------------------------------------------------------------------
# Total-effect

a <- cten$predictions$corn.pred*cropdat$acres + 
                                    cten$predictions$cotton.pred*cropdat$acres + 
                                    cten$predictions$hay.pred*cropdat$acres + 
                                    cten$predictions$soybean.pred*cropdat$acres + 
                                    cten$predictions$wheat.pred

head(a)
head(cropdat$acres*rev_crop_pred$modten_rev.pred)
head(total_effect_w_adaptation_ten)


# Remove negative values from predictions
rev_crop_pred$modten_rev.pred <- ifelse(rev_crop_pred$modten_rev.pred < 0, 0, rev_crop_pred$modten_rev.pred)
rev_crop_pred$modtwenty_rev.pred <- ifelse(rev_crop_pred$modtwenty_rev.pred < 0, 0, rev_crop_pred$modtwenty_rev.pred)
rev_crop_pred$modthirty_rev.pred <- ifelse(rev_crop_pred$modthirty_rev.pred < 0, 0, rev_crop_pred$modthirty_rev.pred)


# w/ adaptation
total_effect_w_adaptation_ten <- ((cten$predictions$corn.pred*cropdat$acres + 
                                    cten$predictions$cotton.pred*cropdat$acres + 
                                    cten$predictions$hay.pred*cropdat$acres + 
                                    cten$predictions$soybean.pred*cropdat$acres + 
                                    cten$predictions$wheat.pred*cropdat$acres)*rev_crop_pred$modten_rev.pred) 
  
total_effect_w_adaptation_twenty <- ((ctwenty$predictions$corn.pred*cropdat$acres + 
                                    ctwenty$predictions$cotton.pred*cropdat$acres + 
                                    ctwenty$predictions$hay.pred*cropdat$acres + 
                                    ctwenty$predictions$soybean.pred*cropdat$acres + 
                                    ctwenty$predictions$wheat.pred*cropdat$acres)*rev_crop_pred$modtwenty_rev.pred) 

total_effect_w_adaptation_thirty <- ((cthirty$predictions$corn.pred*cropdat$acres + 
                                    cthirty$predictions$cotton.pred*cropdat$acres + 
                                    cthirty$predictions$hay.pred*cropdat$acres + 
                                    cthirty$predictions$soybean.pred*cropdat$acres + 
                                    cthirty$predictions$wheat.pred*cropdat$acres)*rev_crop_pred$modthirty_rev.pred) 

# w/o adaptation
total_effect_wo_adaptation_ten <- ((cten$predictions$corn.pred*constant_acres + 
                                    cten$predictions$cotton.pred*constant_acres + 
                                    cten$predictions$hay.pred*constant_acres + 
                                    cten$predictions$soybean.pred*constant_acres + 
                                    cten$predictions$wheat.pred*constant_acres)*rev_crop_pred$modten_rev.pred) 
  
total_effect_wo_adaptation_twenty <- ((ctwenty$predictions$corn.pred*constant_acres + 
                                    ctwenty$predictions$cotton.pred*constant_acres + 
                                    ctwenty$predictions$hay.pred*constant_acres + 
                                    ctwenty$predictions$soybean.pred*constant_acres + 
                                    ctwenty$predictions$wheat.pred*constant_acres)*rev_crop_pred$modtwenty_rev.pred) 

total_effect_wo_adaptation_thirty <- ((cthirty$predictions$corn.pred*constant_acres + 
                                    cthirty$predictions$cotton.pred*constant_acres + 
                                    cthirty$predictions$hay.pred*constant_acres + 
                                    cthirty$predictions$soybean.pred*constant_acres + 
                                    cthirty$predictions$wheat.pred*constant_acres)*rev_crop_pred$modthirty_rev.pred) 


tdat <- data.frame(temp = rev_crop_pred$temp,
                   effect = rev_crop_pred$effect,
                   total_effect_w_adaptation_ten = total_effect_w_adaptation_ten,
                   total_effect_w_adaptation_twenty = total_effect_w_adaptation_twenty,
                   total_effect_w_adaptation_thirty = total_effect_w_adaptation_thirty,
                   total_effect_wo_adaptation_ten = total_effect_wo_adaptation_ten,
                   total_effect_wo_adaptation_twenty = total_effect_wo_adaptation_twenty,
                   total_effect_wo_adaptation_thirty = total_effect_wo_adaptation_thirty)

pc <- function(x){ 100*(x - first(x))/first(x)}

test <- tdat %>% 
  group_by(temp, effect) %>% 
  summarise_all(sum) %>% 
  ungroup() %>% 
  select(-temp) %>% 
  group_by(effect) %>% 
  mutate_all(pc)

test
test$temp <- c(0, 1, 2, 3, 4, 5)
test <- gather(test, key = crop, value = value, -temp, -effect)
test$interval <- rep(c("10-year", "20-year", "30-year"), each = 6)
test$type <- c(rep("Total effect w/ adaptation", 18), rep("Total effect w/o adaptation", 18))
head(test)
test
ggplot(test, aes(temp, value, color = type)) + geom_line() + facet_wrap(~interval, scales = "free") + ylim(-10, 100)




#--------------------------------------------------
# Individual crop effects

# Assign intervals
cten$predictions$type <- "10-year"
ctwenty$predictions$type <- "20-year"
cthirty$predictions$type <- "30-year"

# Bind SUR climate data
cdat <- bind_rows(cten$predictions, ctwenty$predictions, cthirty$predictions)
cdat$effect <- "Crop-effect"

# Assign rev to climate data
cdat$corn_rev <- rep(sur_rev$pred)


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

# Get crop acres by county
cdat$corn.pred <- cdat$corn.pred*cropdat$acres
cdat$cotton.pred <- cdat$cotton.pred*cropdat$acres
cdat$hay.pred <- cdat$hay.pred*cropdat$acres
cdat$soybean.pred <- cdat$soybean.pred*cropdat$acres
cdat$wheat.pred <- cdat$wheat.pred*cropdat$acres

head(cdat)

# Assign climate effects
climate_effect <- (cdat$corn.pred*cdat$corn_rev) + (cdat$cotton.pred*cdat$cotton_rev) + (cdat$hay.pred*cdat$hay_rev) +
  (cdat$soybean.pred*cdat$soybean_rev) + (cdat$wheat.pred*cdat$wheat_rev)

# No crop-switching

# Get no crop switching data
ndat$corn.pred <- rev_crop_pred$corn_rev.pred*ndat$corn_grain_a
ndat$cotton.pred <- rev_crop_pred$cotton_rev.pred*ndat$cotton_a
ndat$hay.pred <- rev_crop_pred$hay_rev.pred*ndat$hay_a
ndat$soybean.pred <- rev_crop_pred$soybean_rev.pred*ndat$soybean_a
ndat$wheat.pred <- rev_crop_pred$wheat_rev.pred*ndat$wheat_a

head(ndat)

# Get no crop switching effect
no_cs_effect <- (ndat$corn.pred) + (ndat$cotton.pred) + (ndat$hay.pred) + (ndat$soybean.pred) + (ndat$wheat.pred)


# Get plot data
pdat <- data.frame(year = rep(cropdat$year, 3),
                   ten = rep(cropdat$ten, 3),
                   twenty = rep(cropdat$twenty, 3),
                   thirty = rep(cropdat$thirty, 3),
                   fips = rep(cropdat$fips, 3),
                   temp = cdat$temp,
                   type = cdat$type)


# Assign climate and no crop switching effects
pdat$climate_effect <- climate_effect
pdat$no_cs_effect <- no_cs_effect



bsum <- function(x,i) sum(x[i])
# Test boot
## boot(cropdat$ln_rev, bsum, R = 2, strata = cropdat$five, parallel = "multicore", ncpus = 14)$t

# 
# pdat_se_ten <- pdat %>%
#   filter(type == "10-year") %>%
#   group_by(temp, type) %>%
#   summarise(climate_se_sum = sd(boot(pdat$climate_effect, bsum, R = 2000)$t),
#             no_cs_se_sum = sd(boot(pdat$no_cs_effect, bsum, R = 2000)$t))
# 
# pdat_se_twenty <- pdat %>%
#   filter(type == "20-year") %>%
#   group_by(temp, type) %>%
#   summarise(climate_se_sum = sd(boot(pdat$climate_effect, bsum, R = 2000)$t),
#             no_cs_se_sum = sd(boot(pdat$no_cs_effect, bsum, R = 2000)$t))
# 
# pdat_se_thirty <- pdat %>%
#   filter(type == "30-year") %>%
#   group_by(temp, type) %>%
#   summarise(climate_se_sum = sd(boot(pdat$climate_effect, bsum, R = 2000)$t),
#             no_cs_se_sum = sd(boot(pdat$no_cs_effect, bsum, R = 2000)$t))
# # #

# Results from AWS run
pdat_se_ten <- structure(list(temp = c(0, 1, 2, 3, 4, 5), type = structure(c(1L, 
1L, 1L, 1L, 1L, 1L), .Label = c("10-year", "20-year", "30-year"
), class = "factor"), climate_se_sum = c(5254676156.82528, 5178662535.38406, 
5304213056.50403, 5192633070.83388, 5292269833.13169, 5366307321.31913
), no_cs_se_sum = c(4247143411.80754, 4391272622.4693, 4313769731.86249, 
4331639487.54841, 4315580455.93608, 4438163581.27787)), .Names = c("temp", 
"type", "climate_se_sum", "no_cs_se_sum"), class = c("grouped_df", 
"tbl_df", "tbl", "data.frame"), row.names = c(NA, -6L), vars = "temp", drop = TRUE)


pdat_se_twenty <- structure(list(temp = c(0, 1, 2, 3, 4, 5), type = structure(c(2L, 
2L, 2L, 2L, 2L, 2L), .Label = c("10-year", "20-year", "30-year"
), class = "factor"), climate_se_sum = c(5244146160.07248, 5268045204.95006, 
5338659160.36829, 5265452635.0205, 5292947363.01195, 5316826463.30749
), no_cs_se_sum = c(4295090098.65422, 4282145462.36554, 4283873111.56688, 
4360165700.52113, 4295595698.79234, 4346001858.6149)), .Names = c("temp", 
"type", "climate_se_sum", "no_cs_se_sum"), class = c("grouped_df", 
"tbl_df", "tbl", "data.frame"), row.names = c(NA, -6L), vars = "temp", drop = TRUE)
  

pdat_se_thirty <- structure(list(temp = c(0, 1, 2, 3, 4, 5), type = structure(c(3L, 
3L, 3L, 3L, 3L, 3L), .Label = c("10-year", "20-year", "30-year"
), class = "factor"), climate_se_sum = c(5365321282.20105, 5197660903.35256, 
5404942357.1265, 5383358261.21039, 5273807976.11258, 5316758511.62023
), no_cs_se_sum = c(4314116091.74431, 4446948743.03167, 4513815813.85627, 
4410117210.59456, 4391835867.79902, 4388037066.66999)), .Names = c("temp", 
"type", "climate_se_sum", "no_cs_se_sum"), class = c("grouped_df", 
"tbl_df", "tbl", "data.frame"), row.names = c(NA, -6L), vars = "temp", drop = TRUE)



pdat_se <- rbind(pdat_se_ten, pdat_se_twenty, pdat_se_thirty)
# pdat_se

pdat <- pdat %>% 
  group_by(type, temp) %>% 
  summarise(climate_effect = sum(climate_effect),
            no_cs_effect = sum(no_cs_effect)) %>% 
  left_join(pdat_se, by = c("type", "temp")) %>%
  mutate(climate_effect_min = climate_effect - 1.96*climate_se_sum,
         climate_effect_max = climate_effect + 1.96*climate_se_sum,
         no_cs_effect_min = no_cs_effect - 1.96*no_cs_se_sum,
         no_cs_effect_max = no_cs_effect + 1.96*no_cs_se_sum) %>%
  group_by(type) %>% 
  mutate(change_climate_effect = 100*(climate_effect - first(climate_effect))/first(climate_effect),
         change_no_cs_effect = 100*(no_cs_effect - first(no_cs_effect))/first(no_cs_effect),
         change_climate_effect_max = 100*(climate_effect_max - first(climate_effect))/first(climate_effect),
         change_no_cs_effect_max = 100*(no_cs_effect_max - first(no_cs_effect))/first(no_cs_effect),
         change_climate_effect_min = 100*(climate_effect_min - first(climate_effect))/first(climate_effect),
         change_no_cs_effect_min = 100*(no_cs_effect_min - first(no_cs_effect))/first(no_cs_effect))



pdat1 <- pdat %>% 
  select(type, temp, change_climate_effect, change_no_cs_effect) %>% 
  gather(key = effect, value = value, -type, -temp)

pdat2 <- pdat %>%
    select(type, temp, change_climate_effect_max, change_no_cs_effect_max) %>%
  gather(key = effect, value = value_max, -type, -temp)

pdat3 <- pdat %>%
    select(type, temp, change_climate_effect_min, change_no_cs_effect_min) %>%
  gather(key = effect, value = value_min, -type, -temp)

pdat1$value_max <- pdat2$value_max
pdat1$value_min <- pdat3$value_min

pdat1$effect <- factor(pdat1$effect, levels = c("change_climate_effect", "change_no_cs_effect"),
                       labels = c("Climate acres \n (w/ crop switching)", "Constant acres \n (w/o crop switching)"))
pdat1$type <- factor(pdat1$type, levels = c("10-year", "20-year", "30-year"))

ggplot(pdat1, aes(x = temp, y = value, group = effect)) + 
  geom_ribbon(aes(ymax = value_max, ymin = value_min), fill = "#C0CCD9", alpha = 0.5 ) +
  geom_line(aes(color = effect)) + 
  geom_point(aes(color = effect), size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Total Revenue") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  scale_y_continuous(breaks = seq(0,-50, by = -10), labels = c("0", "-10%", "-20%", "-30%", "-40%", "-50%")) +
  guides(color = guide_legend(keywidth = 2, keyheight = 1,
                                override.aes = list(linetype = c(1, 1),
                                                    size = 1.5,
                                                    shape = c(NA, NA)))) +
    facet_wrap(~type) +
    theme(legend.position = "top", 
        legend.box.background = element_rect(colour = "grey"), 
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA))

ggsave("figures/1-main_rev_plot.pdf", width = 6, height = 4)

