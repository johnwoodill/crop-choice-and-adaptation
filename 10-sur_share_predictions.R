library(tidyverse)
library(lfe)
library(systemfit)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictSUR.R")
source("R/predictSUR.clean.R")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$five <- factor(cropdat$five)
cropdat$ten <- factor(cropdat$ten)
cropdat$twenty <- factor(cropdat$twenty)
cropdat$thirty <- factor(cropdat$thirty)


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
p0 <- select(p0, names(p1))

# Bind list
newdata_list <- list(p0 = p0,
                     p1 = p1,
                     p2 = p2,
                     p3 = p3,
                     p4 = p4,
                     p5 = p5)



p0_ten <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         ten = factor(cropdat$ten)))
p0_ten_means <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         ten = factor(cropdat$ten)), means = TRUE)

p1_ten <- p1 - p0_ten_means
p2_ten <- p2 - p0_ten_means
p3_ten <- p3 - p0_ten_means
p4_ten <- p4 - p0_ten_means
p5_ten <- p5 - p0_ten_means

newdata_list_ten <- list(p0 = p0_ten,
                     p1 = p1_ten,
                     p2 = p2_ten,
                     p3 = p3_ten,
                     p4 = p4_ten,
                     p5 = p5_ten)

p0_twenty <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         twenty = factor(cropdat$twenty)))
p0_twenty_means <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         twenty = factor(cropdat$twenty)), means = TRUE)

p1_twenty <- p1 - p0_twenty_means
p2_twenty <- p2 - p0_twenty_means
p3_twenty <- p3 - p0_twenty_means
p4_twenty <- p4 - p0_twenty_means
p5_twenty <- p5 - p0_twenty_means

newdata_list_twenty <- list(p0 = p0_twenty,
                     p1 = p1_twenty,
                     p2 = p2_twenty,
                     p3 = p3_twenty,
                     p4 = p4_twenty,
                     p5 = p5_twenty)

p0_thirty <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         thirty = factor(cropdat$thirty)))
p0_thirty_means <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         thirty = factor(cropdat$thirty)), means = TRUE)

p1_thirty <- p1 - p0_thirty_means
p2_thirty <- p2 - p0_thirty_means
p3_thirty <- p3 - p0_thirty_means
p4_thirty <- p4 - p0_thirty_means
p5_thirty <- p5 - p0_thirty_means

newdata_list_thirty <- list(p0 = p0_thirty,
                     p1 = p1_thirty,
                     p2 = p2_thirty,
                     p3 = p3_thirty,
                     p4 = p4_thirty,
                     p5 = p5_thirty)

ten_climate_terms_v = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten")
twenty_climate_terms_v = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty")
thirty_climate_terms_v = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty")

#-------------------------------------
# Ten-year

# Climate-effect predictions

cten <- predictSUR.clean(mod = sur_ten, 
                         acres = cropdat$acres,  
                         fips = cropdat$fips,
                         newdata_list = newdata_list_ten,
                         var.terms = ten_climate_terms_v,
                         type = "10-year", 
                         effect = "Climate-effect")

#-------------------------------------
# Twenty-year

# Climate-effect predictions

ctwenty <- predictSUR.clean(mod = sur_twenty, 
                            acres = cropdat$acres,  
                            fips = cropdat$fips,
                            newdata_list = newdata_list_twenty, 
                            var.terms = twenty_climate_terms_v,
                            type = "20-year", 
                            effect = "Climate-effect")

#-------------------------------------
# Thirty-year

# Climate-effect predictions

cthirty <- predictSUR.clean(sur_thirty, 
                            acres = cropdat$acres,  
                            fips = cropdat$fips,
                            newdata_list = newdata_list_thirty,
                            var.terms = thirty_climate_terms_v,
                            type = "30-year", 
                            effect = "Climate-effect")

# Save predictions data
saveRDS(cten, "data/cten.rds")
saveRDS(ctwenty, "data/ctwenty.rds")
saveRDS(cthirty, "data/cthirty.rds")

pdat <- rbind(cten$agg_predictions, ctwenty$agg_predictions, cthirty$agg_predictions)

head(pdat)

ggplot(pdat, aes(temp, (sum/1000000), color = crop)) + geom_line() + 
  geom_hline(yintercept = 12073232296/1000000, linetype = "dashed", color = "grey") +
  theme_tufte(base_size = 10) +
  ylab("Total Acres \n (Millions)") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = "top", 
       #legend.justification = c("left", "top"), 
       legend.box.background = element_rect(colour = "grey"), 
       legend.title = element_blank(), legend.key = element_blank(),
       axis.text.x = element_text(angle = 45, hjust = 1)) +
  #theme(legend.position = c(.85,1), 
  #     legend.justification = c("left", "top"), 
  #     legend.box.background = element_rect(colour = "grey"), 
  #     legend.title = element_blank(), legend.key = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    facet_wrap(~type) 

pdat %>% group_by(temp, type) %>% summarise(nsum = sum(sum))

pdat <- pdat %>% 
  group_by(crop, type, effect) %>% 
  mutate(change = (sum - first(sum))/first(sum))
pdat
pdat$change <- 100*pdat$change

saveRDS(pdat, "data/sur_predictions.rds")

ggplot(pdat, aes(temp, change)) + 
  geom_line() + 
  geom_point(size = .5) + 
  # geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Proportion of Crop Acres") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = "top", 
       #legend.justification = c("left", "top"), 
       legend.box.background = element_rect(colour = "grey"), 
       legend.title = element_blank(), legend.key = element_blank(),
       axis.text.x = element_text(angle = 45, hjust = 1)) +
  #theme(legend.position = c(.85,1), 
  #     legend.justification = c("left", "top"), 
  #     legend.box.background = element_rect(colour = "grey"), 
  #     legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(type~crop, ncol = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

ggsave("figures/sur_crop_share_predictions.pdf", width = 6, height = 5)

# Average acres
# ppdat <- pdat %>%
#   # filter(effect == "Climate-effect") %>%
#   group_by(temp, type, effect) %>%
#   summarise(total = sum(sum)) %>%
#   group_by(type, effect) %>%
#   mutate(change = (total - first(total))/first(total))
# ppdat$change <- ppdat$change*100
# ppdat
# 
#  ggplot(ppdat, aes(temp, change, color = type)) +
#    geom_line() +
#    geom_point(size = 0.5) +
#    facet_wrap(~effect, ncol = 3) +
#    theme_tufte(base_size = 12) +
#    ylab("Change in Proportion of Total Crop Acres") +
#    xlab("Change in Temperature (C)") +
#    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#    scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
#    theme(legend.position = "top",
#         #legend.justification = c("left", "top"),
#         legend.box.background = element_rect(colour = "grey"),
#         legend.title = element_blank(), legend.key = element_blank()) +
#    #theme(legend.position = c(.85,1),
#    #     legend.justification = c("left", "top"),
#    #     legend.box.background = element_rect(colour = "grey"),
#    #     legend.title = element_blank(), legend.key = element_blank()) +
#    geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
# 
# 
# 
# 
# 
# 
# 