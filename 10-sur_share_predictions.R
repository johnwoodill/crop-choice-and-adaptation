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



p0_dm <- demeanlist(p0, fl = list(fips = factor(cropdat$fips)))
p0_means <- demeanlist(p0, fl = list(fips = factor(cropdat$fips)), means = TRUE)

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


ten_climate_terms_v = c("dday0_10_rm10", "dday10_30_rm10", "dday30_rm10", 
                        "prec_rm10", "prec_sq_rm10")
twenty_climate_terms_v = c("dday0_10_rm11", "dday10_30_rm11", "dday30_rm11", 
                           "prec_rm11", "prec_sq_rm11")
thirty_climate_terms_v = c("dday0_10_rm12", "dday10_30_rm12", "dday30_rm12", 
                           "prec_rm12", "prec_sq_rm12")

terms <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq")

#-------------------------------------
# Ten-year

# Climate-effect predictions
# 

# mod = sur_ten
# acres = cropdat$acres
# fips = cropdat$fips
# newdata_list = newdata_list_dm
# var.terms = ten_climate_terms_v
# # cons.terms = terms
# type = "10-year"
# effect = "Climate-effect"

cten <- predictSUR.clean(mod = sur_ten, 
                         acres = cropdat$acres,  
                         fips = cropdat$fips,
                         newdata_list = newdata_list_dm,
                         var.terms = ten_climate_terms_v,
                         # cons.terms = terms,
                         type = "10-year", 
                         effect = "Climate-effect")

# head(cten$agg_predictions)
# View(cten$agg_predictions)

test <- cten$agg_predictions %>% 
  group_by(crop) %>% 
  mutate(change = 100*(sum - first(sum))/(first(sum)))
test

View(test)
#  cten_rs <- rowSums(cten$predictions)
#  length(which((cten_rs != 1) == FALSE))
# 
# cten <- predictSUR.clean(mod = sur_ten, acres = cropdat$acres, newdata_list = newdata_list_dm,type = "10-year", 
                         # effect = "Climate-effect")
# head(cten$predictions)
#-------------------------------------
# Twenty-year

# Climate-effect predictions

ctwenty <- predictSUR.clean(mod = sur_twenty, 
                            acres = cropdat$acres,  
                            fips = cropdat$fips,
                            newdata_list = newdata_list_dm, 
                            var.terms = twenty_climate_terms_v,
                            # cons.terms = terms,
                            type = "11-year", 
                            effect = "Climate-effect")

# ctwenty <- predictSUR.clean(mod = sur_twenty, acres = cropdat$acres, newdata_list = newdata_list_dm,type = "20-year", 
                         # effect = "Climate-effect")

#-------------------------------------
# Thirty-year

# Climate-effect predictions

cthirty <- predictSUR.clean(mod = sur_thirty, 
                            acres = cropdat$acres,  
                            fips = cropdat$fips,
                            newdata_list = newdata_list_dm,
                            var.terms = thirty_climate_terms_v,
                            # cons.terms = terms,
                            type = "12-year", 
                            effect = "Climate-effect")

cthirty$predictions %>% 
 group_by(temp) %>% 
 summarise_all(mean)

# cthirty <- predictSUR.clean(mod = sur_thirty, acres = cropdat$acres, newdata_list = newdata_list_dm,type = "30-year", 
                         # effect = "Climate-effect")

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

# pdat %>% group_by(temp, type) %>% summarise(nsum = sum(sum))

# pdat <- pdat %>% 
#   group_by(crop, type, effect) %>% 
#   mutate(change = (sum - first(sum))/first(sum))
# pdat
# pdat$change <- 100*pdat$change
# 
# saveRDS(pdat, "data/sur_predictions.rds")
# # 
# ggplot(pdat, aes(temp, change)) +
#   geom_line() +
#   geom_point(size = .5) +
#   # geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
#   theme_tufte(base_size = 10) +
#   ylab("% Change in Proportion of Crop Acres") +
#   xlab("Change in Temperature (C)") +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
#   theme(legend.position = "top",
#        #legend.justification = c("left", "top"),
#        legend.box.background = element_rect(colour = "grey"),
#        legend.title = element_blank(), legend.key = element_blank(),
#        axis.text.x = element_text(angle = 45, hjust = 1)) +
#   #theme(legend.position = c(.85,1),
#   #     legend.justification = c("left", "top"),
#   #     legend.box.background = element_rect(colour = "grey"),
#   #     legend.title = element_blank(), legend.key = element_blank()) +
#   facet_wrap(type~crop, ncol = 5, scales = "free") +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
# 
# ggsave("figures/sur_crop_share_predictions.pdf", width = 6, height = 5)
# # 
# # # Average acres
# #  ppdat <- pdat %>%
# #    # filter(effect == "Climate-effect") %>%
# #    group_by(temp, type, effect) %>%
# #    summarise(total = sum(sum)) %>%
# #    group_by(type, effect) %>%
# #    mutate(change = (total - first(total))/first(total))
# #  ppdat$change <- ppdat$change*100
# #  ppdat
# # 
# #   ggplot(ppdat, aes(temp, change, color = type)) +
# #     geom_line() +
# #     geom_point(size = 0.5) +
# #     facet_wrap(~effect, ncol = 3) +
# #     theme_tufte(base_size = 12) +
# #     ylab("Change in Proportion of Total Crop Acres") +
# #     xlab("Change in Temperature (C)") +
# #     annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
# #     annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
# #     scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
# #     theme(legend.position = "top",
# #          #legend.justification = c("left", "top"),
# #          legend.box.background = element_rect(colour = "grey"),
# #          legend.title = element_blank(), legend.key = element_blank()) +
# #     #theme(legend.position = c(.85,1),
# #     #     legend.justification = c("left", "top"),
# #     #     legend.box.background = element_rect(colour = "grey"),
# #     #     legend.title = element_blank(), legend.key = element_blank()) +
# #     geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
# # 
# #  
# #  
# #  
# #  
# #  
# #  
