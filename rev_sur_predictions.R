library(tidyverse)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

## Cropdata
cropdat <- readRDS("data/full_ag_data.rds")

# Total revenue predictions
rdat <- readRDS("data/rev_predictions.rds")

# Crop revenue predictions
rcorn <- readRDS("data/rev_corn_predictions.rds")
rcotton <- readRDS("data/rev_cotton_predictions.rds")
rhay <- readRDS("data/rev_hay_predictions.rds")
rsoybean <- readRDS("data/rev_soybean_predictions.rds")
rwheat <- readRDS("data/rev_wheat_predictions.rds")

# Share acres (SUR)
sdat <- readRDS("data/sur_predictions.rds")

scorn <- filter(sdat, crop == "Corn")
scotton <- filter(sdat, crop == "Cotton")
shay <- filter(sdat, crop == "Hay")
ssoybean <- filter(sdat, crop == "Soybean")
swheat <- filter(sdat, crop == "Wheat")

sacres <- cropdat %>% 
  filter(year >= 1980) %>% 
  summarise(corn_m = mean(corn_grain_a, na.rm = TRUE),
            cotton_m = mean(cotton_a, na.rm = TRUE),
            hay_m = mean(hay_a, na.rm = TRUE),
            soybean_m = mean(soybean_a, na.rm = TRUE),
            wheat_m = mean(wheat_a, na.rm = TRUE))
cacres <- scorn

scorn <- arrange(scorn, type, effect, temp)
rcorn <- arrange(rcorn, type, effect, temp)

scotton <- arrange(scotton, type, effect, temp)
rcotton <- arrange(rcotton, type, effect, temp)

shay <- arrange(shay, type, effect, temp)
rhay <- arrange(rhay, type, effect, temp)

ssoybean <- arrange(ssoybean, type, effect, temp)
rsoybean <- arrange(rsoybean, type, effect, temp)

swheat <- arrange(swheat, type, effect, temp)
rwheat <- arrange(rwheat, type, effect, temp)
head(scorn)
head(rcorn)

pdat <- data.frame(temp = scorn$temp,
           type = scorn$type,
           effect = scorn$effect,
           rscorn = scorn$sum*rcorn$sum,
           rscotton = scotton$sum*rcotton$sum,
           rshay = shay$sum*rhay$sum,
           rssoybean = ssoybean$sum*rsoybean$sum,
           rswheat = swheat$sum*rwheat$sum)

# No crop switching data
# pdat <- filter(pdat, effect == "Total-effect")

pdat$total <- rowSums(pdat[, 4:8])
head(pdat)

pdat <- pdat %>% 
  group_by(type, effect) %>% 
  mutate(change_total = 100*(total - first(total))/first(total))

ggplot(filter(pdat, effect != "Weather-effect"), aes(temp, change_total, color = effect)) + geom_line() + facet_wrap(~type, scales = "free") + ylab("% Change in Total Revenue")

mean(cropdat$acres)
mean(c(cropdat$corn_grain_a, cropdat$cotton_a, cropdat$hay_a, cropdat$soybean_a, cropdat$wheat_a), na.rm = TRUE)
# 
# sdat <- sdat %>% 
#   group_by(temp, type, effect) %>% 
#   summarise(sum = sum(sum))
# 
# rdat
# head(rdat)
# head(sdat)
# 
# rdat <- select(rdat, temp, model, effect, sum)
# names(rdat)[2] <- "type"
# 
# pdat <- data.frame(temp = sdat$temp,
#                    type = sdat$type,
#                    effect = sdat$effect,
#                    a_sum = sdat$sum)
# 
# 
# 
# head(pdat)
# pdat <- left_join(pdat, rdat, by = c("temp", "type", "effect"))
# names(pdat)[5] <- "r_sum"
#  
switchdat <- filter(cropdat, year >= 1980)
c_acres <- mean(switchdat$acres, na.rm = TRUE)


cdat <- data.frame(temp = rep(unique(pdat$temp), 5),
                   type = rep(unique(pdat$type), 6))

cdat
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
