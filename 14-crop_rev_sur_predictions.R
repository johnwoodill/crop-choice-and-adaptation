library(boot)
library(tidyverse)
library(ggthemes)

cropdat <- readRDS("data/full_ag_data.rds")

# Revenue predictions
rev_corn_pred <- readRDS("data/rev_corn_pred.rds")
rev_cotton_pred <- readRDS("data/rev_cotton_pred.rds")
rev_hay_pred <- readRDS("data/rev_hay_pred.rds")
rev_soybean_pred <- readRDS("data/rev_soybean_pred.rds")
rev_wheat_pred <- readRDS("data/rev_wheat_pred.rds")

# Crop share predictions
tfive <- readRDS("data/tfive.rds")
tten <- readRDS("data/tten.rds")
ttwenty <- readRDS("data/ttwenty.rds")
tthirty <- readRDS("data/tthirty.rds")

tfive$predictions$type <- "5-year"
tten$predictions$type <- "10-year"
ttwenty$predictions$type <- "20-year"
tthirty$predictions$type <- "30-year"

cfive <- readRDS("data/cfive.rds")
cten <- readRDS("data/cten.rds")
ctwenty <- readRDS("data/ctwenty.rds")
cthirty <- readRDS("data/cthirty.rds")
csixty <- readRDS("data/csixty.rds")

cfive$predictions$type <- "5-year"
cten$predictions$type <- "10-year"
ctwenty$predictions$type <- "20-year"
cthirty$predictions$type <- "30-year"
csixty$predictions$type <- "60-year"

dat <- cropdat %>% 
  select(year, fips, corn_grain_a, cotton_a, hay_a, wheat_a, soybean_a) %>% 
  filter(year >= 1950) %>% 
  group_by(fips) %>% 
  mutate(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE))


tdat <- bind_rows(tfive$predictions, tten$predictions, ttwenty$predictions, tthirty$predictions, csixty$predictions)
tdat$effect <- "Total-effect"

cdat <- bind_rows(cfive$predictions, cten$predictions, ctwenty$predictions, cthirty$predictions, csixty$predictions)
cdat$effect <- "Climate-effect"

ndat <- bind_rows(dat[, 3:7], dat[, 3:7], dat[, 3:7], dat[, 3:7], dat[, 3:7])
ndat <- bind_rows(ndat, ndat, ndat, ndat, ndat, ndat)
ndat$effect <- "No Crop-switching"


# Total-effect
tdat$corn_rev <- rev_corn_pred$fit
tdat$cotton_rev <- rev_cotton_pred$fit
tdat$hay_rev <- rev_hay_pred$fit
tdat$soybean_rev <- rev_soybean_pred$fit
tdat$wheat_rev <- rev_wheat_pred$fit

tdat$corn.pred <- tdat$corn.pred*cropdat$acres
tdat$cotton.pred <- tdat$cotton.pred*cropdat$acres
tdat$hay.pred <- tdat$hay.pred*cropdat$acres
tdat$soybean.pred <- tdat$soybean.pred*cropdat$acres
tdat$wheat.pred <- tdat$wheat.pred*cropdat$acres

# Remove negative values from predictions
tdat$corn_rev <- ifelse(tdat$corn_rev < 0, 0, tdat$corn_rev)
tdat$cotton_rev <- ifelse(tdat$cotton_rev < 0, 0, tdat$cotton_rev)
tdat$hay_rev <- ifelse(tdat$hay_rev < 0, 0, tdat$hay_rev)
tdat$soybean_rev <- ifelse(tdat$soybean_rev < 0, 0, tdat$soybean_rev)
tdat$wheat_rev <- ifelse(tdat$wheat_rev < 0, 0, tdat$wheat_rev)

tdat$corn_rev <- tdat$corn_rev*tdat$corn.pred
tdat$cotton_rev <- tdat$cotton_rev*tdat$cotton.pred
tdat$hay_rev <- tdat$hay_rev*tdat$hay.pred
tdat$soybean_rev <- tdat$soybean_rev*tdat$soybean.pred
tdat$wheat_rev <- tdat$wheat_rev*tdat$wheat.pred

head(tdat)
tdat <- select(tdat, temp, type, effect, corn_rev, cotton_rev, hay_rev, soybean_rev, wheat_rev)

total_effect <- (tdat$corn.pred*tdat$corn_rev) + (tdat$cotton.pred*tdat$cotton_rev) + (tdat$hay.pred*tdat$hay_rev) +
  (tdat$soybean.pred*tdat$soybean_rev) + (tdat$wheat.pred*tdat$wheat_rev)

# Climate effects
cdat$corn_rev <- rev_corn_pred$fit
cdat$cotton_rev <- rev_cotton_pred$fit
cdat$hay_rev <- rev_hay_pred$fit
cdat$soybean_rev <- rev_soybean_pred$fit
cdat$wheat_rev <- rev_wheat_pred$fit

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

cdat$corn_rev <- cdat$corn_rev*cdat$corn.pred
cdat$cotton_rev <- cdat$cotton_rev*cdat$cotton.pred
cdat$hay_rev <- cdat$hay_rev*cdat$hay.pred
cdat$soybean_rev <- cdat$soybean_rev*cdat$soybean.pred
cdat$wheat_rev <- cdat$wheat_rev*cdat$wheat.pred

head(cdat)
head(tdat)

cdat <- select(cdat, temp, type, effect, corn_rev, cotton_rev, hay_rev, soybean_rev, wheat_rev)

climate_effect <- (cdat$corn.pred*cdat$corn_rev) + (cdat$cotton.pred*cdat$cotton_rev) + (cdat$hay.pred*cdat$hay_rev) +
  (cdat$soybean.pred*cdat$soybean_rev) + (cdat$wheat.pred*cdat$wheat_rev)

# No crop-switching

# Remove negative values from predictions
rev_corn_pred$fit <- ifelse(rev_corn_pred$fit < 0, 0, rev_corn_pred$fit)
rev_cotton_pred$fit <- ifelse(rev_cotton_pred$fit < 0, 0, rev_cotton_pred$fit)
rev_hay_pred$fit <- ifelse(rev_hay_pred$fit < 0, 0, rev_hay_pred$fit)
rev_soybean_pred$fit <- ifelse(rev_soybean_pred$fit < 0, 0, rev_soybean_pred$fit)
rev_wheat_pred$fit <- ifelse(rev_wheat_pred$fit < 0, 0, rev_wheat_pred$fit)

ndat$corn_rev <- rev_corn_pred$fit*ndat$corn_grain_a
ndat$cotton_rev <- rev_cotton_pred$fit*ndat$cotton_a
ndat$hay_rev <- rev_hay_pred$fit*ndat$hay_a
ndat$soybean_rev <- rev_soybean_pred$fit*ndat$soybean_a
ndat$wheat_rev <- rev_wheat_pred$fit*ndat$wheat_a

ndat$temp <- cdat$temp
ndat$type <- cdat$type
head(ndat)

ndat <- select(ndat, temp, type, effect, corn_rev, cotton_rev, hay_rev, soybean_rev, wheat_rev)

pdat <- bind_rows(tdat, cdat, ndat)


# pdat <- data.frame(year = rep(cropdat$year, 5),
#                    five = rep(cropdat$five, 5),
#                    ten = rep(cropdat$ten, 5),
#                    twenty = rep(cropdat$twenty, 5),
#                    thirty = rep(cropdat$thirty, 5),
#                    fips = rep(cropdat$fips, 5),
#                    temp = cdat$temp,
#                    type = cdat$type)
# 
# pdat$climate_effect <- climate_effect
# pdat$total_effect <- total_effect
# pdat$no_cs_effect <- no_cs_effect
# head(pdat)

# Bootstrap s.e. of sum
# head(test)
# test <- filter(pdat, temp == 0 & type == "5-year")
# pdat$cl <- ifelse(pdat$type == "5-year", "five", 0)
# boot.strap(test$climate_effect, rep = 5)

head(pdat)

bsum <- function(x,i) sum(x[i])
#bs <- system.time(boot(cropdat$ln_rev, bsum, R = 1000, strata = cropdat$five))
# 
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
# 
# 
# pdat_se <- rbind(pdat_se_five, pdat_se_ten, pdat_se_twenty, pdat_se_thirty, pdat_se_sixty)
# pdat_se

pdat <- pdat %>% 
  group_by(type, temp, effect) %>% 
  summarise_all(sum) %>% 
  # summarise(climate_effect = sum(climate_effect),
  #           total_effect = sum(total_effect),
  #           no_cs_effect = sum(no_cs_effect)) %>% 
  #left_join(pdat_se, by = c("type", "temp")) %>% 
  # mutate(climate_effect_min = climate_effect - 1.96*climate_se_sum,
  #        climate_effect_max = climate_effect + 1.96*climate_se_sum,
  #        total_effect_min = total_effect - 1.96*total_se_sum,
  #        total_effect_max = total_effect + 1.96*total_se_sum,
  #        no_cs_effect_min = no_cs_effect - 1.96*no_cs_se_sum,
  #        no_cs_effect_max = no_cs_effect + 1.96*no_cs_se_sum) %>% 
  group_by(type, effect) %>% 
  mutate(corn_rev = 100*(corn_rev - first(corn_rev))/first(corn_rev),
         cotton_rev = 100*(cotton_rev - first(cotton_rev))/first(cotton_rev),
         hay_rev = 100*(hay_rev - first(hay_rev))/first(hay_rev),
         soybean_rev = 100*(soybean_rev - first(soybean_rev))/first(soybean_rev),
         wheat_rev = 100*(wheat_rev - first(wheat_rev))/first(wheat_rev))
         
pdat         
#change_climate_effect_max = 100*(climate_effect_max - first(climate_effect_max))/first(climate_effect_max),
         # change_total_effect_max = 100*(total_effect_max - first(total_effect_max))/first(total_effect_max),
         # change_no_cs_effect_max = 100*(no_cs_effect_max - first(no_cs_effect_max))/first(no_cs_effect_max),
         
         #change_climate_effect_min = 100*(climate_effect_min - first(climate_effect_min))/first(climate_effect_min),
         # change_total_effect_min = 100*(total_effect_min - first(total_effect_min))/first(total_effect_min),
         # change_no_cs_effect_min = 100*(no_cs_effect_min - first(no_cs_effect_min))/first(no_cs_effect_min)) 
  
  

# head(pdat)
# View(pdat)
# 
# pdat1 <- pdat %>% 
#   select(type, temp, change_climate_effect, change_total_effect, change_no_cs_effect) %>% 
#   gather(key = effect, value = value, -type, -temp)
# 
# pdat2 <- pdat %>% 
#     select(type, temp, change_climate_effect_max, change_total_effect_max, change_no_cs_effect_max) %>% 
#   gather(key = effect, value = value_max, -type, -temp)
# 
# pdat3 <- pdat %>% 
#     select(type, temp, change_climate_effect_min, change_total_effect_min, change_no_cs_effect_min) %>% 
#   gather(key = effect, value = value_min, -type, -temp)
# 
# pdat1$value_max <- pdat2$value_max
# pdat1$value_min <- pdat3$value_min
# test <- left_join(pdat1, pdat2, by = c("type", "temp"))
# test
# pdat <- left_join(pdat, pdat3, by = c("type", "temp"))
# head(pdat1)

# pdat <- filter(pdat, type != "60-year" | effect != "change_total_effect")
# 

pdat <- filter(pdat, type != "60-year" | effect != "Total-effect")

pdat <- gather(pdat, key = crop, value = value, -temp, -type, -effect)

pdat$effect <- factor(pdat$effect, levels = c("Total-effect", "Climate-effect", "No Crop-switching"),
                     labels = c("Climate-effect \n (w/ crop switching)", "Total-effect \n (w/ crop switching)", "Constant-effect \n (w/o crop switching)"))

pdat$type <- factor(pdat$type, levels = c("5-year", "10-year", "20-year", "30-year", "60-year"))
pdat$crop <- factor(pdat$crop, levels = c("corn_rev", "cotton_rev", "hay_rev", "soybean_rev", "wheat_rev"),
                    labels = c("Corn", "Cotton", "Hay", "Soybean", "Wheat"))

ggplot(pdat, aes(temp, value, color = effect)) + geom_line() + facet_wrap(type~crop) +
  geom_point(aes(color = effect), size = 0.5) +
  #geom_line(aes(y=value_max, temp, color = effect), linetype = "dashed", alpha = 0.5) +
  #geom_line(aes(y=value_min, temp, color = effect), linetype = "dashed", alpha = 0.5) +
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5)
ggsave("figures/2-main_crop_rev_plot.pdf", width = 6, height = 7)
# No adaptation 
