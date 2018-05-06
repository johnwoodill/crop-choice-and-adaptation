library(tidyverse)
library(lfe)
library(systemfit)
library(ggthemes)


#####################################################
#####################################################
#####################################################
#####################################################

# NEED TO FIX PREDICTIONS BECAUSE THEY ARE NOT PREDICTING CORRECTLY
# AFTER WEIGHTING REGRESSION
# NEED:
# PREDICTION + MEAN + FIXED-EFFECT + RESIDUALS
# PREDICTION FROM PREDICTSUR
# NEED TO INCLUDE MEAN FOR DEPENDENT VARIABLE (MEAN)
# FIXED-EFFECT FROM FELM WHICH IS THE SAME AS SUR
# CAN'T FIND RESIDUALS = (LN_REV_CORN - (PREDICTION + MEAN + FIXED-EFFECT))

#####################################################
#####################################################
#####################################################
#####################################################

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictSUR.R")
# source("R/predictSUR.clean.R")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)

sur_rev <- readRDS("models/sur_rev_model.rds")

# acres_climate_iv <- readRDS("models/acres_climate_iv.rds")

# Prediction data 0C-5C
p0 <- cropdat
p0 <- cbind(cropdat, acres_climate_iv)

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

newdata_list <- list(p0 = p0_dm,
                     p1 = p1_dm,
                     p2 = p2_dm,
                     p3 = p3_dm,
                     p4 = p4_dm,
                     p5 = p5_dm)

terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq")

# Predictions
prev0 <- predictSUR(sur_rev, newdata = p0_dm, var.terms = terms, IV = FALSE)
prev1 <- predictSUR(sur_rev, newdata = p1_dm, var.terms = terms, IV = FALSE)
prev2 <- predictSUR(sur_rev, newdata = p2_dm, var.terms = terms, IV = FALSE)
prev3 <- predictSUR(sur_rev, newdata = p3_dm, var.terms = terms, IV = FALSE)
prev4 <- predictSUR(sur_rev, newdata = p4_dm, var.terms = terms, IV = FALSE)
prev5 <- predictSUR(sur_rev, newdata = p5_dm, var.terms = terms, IV = FALSE)

# Sum of 
# Corn rev changes and predictions
corn_p0 <- sum(exp(prev0$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1)
corn_p1 <- sum(exp(prev1$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1)
corn_p2 <- sum(exp(prev2$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1)
corn_p3 <- sum(exp(prev3$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1)
corn_p4 <- sum(exp(prev4$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1)
corn_p5 <- sum(exp(prev5$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1)

corn_p0_fit <- exp(prev0$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1
corn_p1_fit <- exp(prev1$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1
corn_p2_fit <- exp(prev2$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1
corn_p3_fit <- exp(prev3$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1
corn_p4_fit <- exp(prev4$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1
corn_p5_fit <- exp(prev5$corn_pred + sur_rev$effects$ln_corn.effect + resid(sur_rev)[[1]]) - 1

# cotton rev changes
cotton_p0 <- sum(exp(prev0$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1)
cotton_p1 <- sum(exp(prev1$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1)
cotton_p2 <- sum(exp(prev2$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1)
cotton_p3 <- sum(exp(prev3$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1)
cotton_p4 <- sum(exp(prev4$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1)
cotton_p5 <- sum(exp(prev5$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1)

cotton_p0_fit <- exp(prev0$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1
cotton_p1_fit <- exp(prev1$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1
cotton_p2_fit <- exp(prev2$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1
cotton_p3_fit <- exp(prev3$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1
cotton_p4_fit <- exp(prev4$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1
cotton_p5_fit <- exp(prev5$cotton_pred + sur_rev$effects$ln_cotton.effect + resid(sur_rev)[[2]]) - 1


# hay rev changes
hay_p0 <- sum(exp(prev0$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1)
hay_p1 <- sum(exp(prev1$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1)
hay_p2 <- sum(exp(prev2$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1)
hay_p3 <- sum(exp(prev3$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1)
hay_p4 <- sum(exp(prev4$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1)
hay_p5 <- sum(exp(prev5$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1)

hay_p0_fit <- exp(prev0$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1
hay_p1_fit <- exp(prev1$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1
hay_p2_fit <- exp(prev2$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1
hay_p3_fit <- exp(prev3$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1
hay_p4_fit <- exp(prev4$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1
hay_p5_fit <- exp(prev5$hay_pred + sur_rev$effects$ln_hay.effect + resid(sur_rev)[[3]]) - 1

# soybean rev changes
soybean_p0 <- sum(exp(prev0$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1)
soybean_p1 <- sum(exp(prev1$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1)
soybean_p2 <- sum(exp(prev2$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1)
soybean_p3 <- sum(exp(prev3$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1)
soybean_p4 <- sum(exp(prev4$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1)
soybean_p5 <- sum(exp(prev5$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1)

soybean_p0_fit <- exp(prev0$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1
soybean_p1_fit <- exp(prev1$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1
soybean_p2_fit <- exp(prev2$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1
soybean_p3_fit <- exp(prev3$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1
soybean_p4_fit <- exp(prev4$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1
soybean_p5_fit <- exp(prev5$soybean_pred + sur_rev$effects$ln_soybean.effect + resid(sur_rev)[[4]]) - 1


# wheat rev changes
wheat_p0 <- sum(exp(prev0$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1)
wheat_p1 <- sum(exp(prev1$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1)
wheat_p2 <- sum(exp(prev2$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1)
wheat_p3 <- sum(exp(prev3$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1)
wheat_p4 <- sum(exp(prev4$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1)
wheat_p5 <- sum(exp(prev5$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1)

wheat_p0_fit <- exp(prev0$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1
wheat_p1_fit <- exp(prev1$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1
wheat_p2_fit <- exp(prev2$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1
wheat_p3_fit <- exp(prev3$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1
wheat_p4_fit <- exp(prev4$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1
wheat_p5_fit <- exp(prev5$wheat_pred + sur_rev$effects$ln_wheat.effect + resid(sur_rev)[[5]]) - 1

pred_sur_rev <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), each = length(corn_p0_fit)),
                           # crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), each = 6),
                           corn_rev = c(corn_p0_fit, corn_p1_fit, corn_p2_fit, corn_p3_fit, corn_p4_fit, corn_p5_fit),
                           cotton_rev = c(cotton_p0_fit, cotton_p1_fit, cotton_p2_fit, cotton_p3_fit, cotton_p4_fit, cotton_p5_fit),
                           hay_rev = c(hay_p0_fit, hay_p1_fit, hay_p2_fit, hay_p3_fit, hay_p4_fit, hay_p5_fit),
                           soybean_rev = c(soybean_p0_fit, soybean_p1_fit, soybean_p2_fit, soybean_p3_fit, soybean_p4_fit, soybean_p5_fit),
                           wheat_rev = c(wheat_p0_fit, wheat_p1_fit, wheat_p2_fit, wheat_p3_fit, wheat_p4_fit, wheat_p5_fit))
head(pred_sur_rev)                           
nrow(pred_sur_rev)/6

pdat_sur_rev <- data.frame(temp = rep(c(0, 1, 2, 3, 4, 5), 5),
                           crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), each = 6),
                           sum = c(corn_p0, corn_p1, corn_p2, corn_p3, corn_p4, corn_p5,
                                   cotton_p0, cotton_p1, cotton_p2, cotton_p3, cotton_p4, cotton_p5,
                                   hay_p0, hay_p1, hay_p2, hay_p3, hay_p4, hay_p5,
                                   soybean_p0, soybean_p1, soybean_p2, soybean_p3, soybean_p4, soybean_p5,
                                   wheat_p0, wheat_p1, wheat_p2, wheat_p3, wheat_p4, wheat_p5))



# pdat_sur_rev


pdat_sur_rev <- pdat_sur_rev %>% 
  group_by(crop) %>% 
  mutate(change = (sum - first(sum))/first(sum))
pdat_sur_rev
pdat_sur_rev$change <- 100*pdat_sur_rev$change

saveRDS(pdat_sur_rev, "data/sur_rev_agg_predictions.rds")
saveRDS(pred_sur_rev, "data/sur_rev_predictions.rds")

ggplot(pdat_sur_rev, aes(temp, change)) + 
  geom_line() + 
  geom_point(size = .5) + 
  # geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
  theme_tufte(base_size = 10) +
  ylab("% Change in Revenue per acre") +
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
  facet_wrap(~crop, ncol = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

ggsave("figures/sur_crop_share_predictions.pdf", width = 6, height = 5)

head(pred_sur_rev)

# Average acres
# ppdat <- pdat %>% 
#   filter(effect == "Climate-effect") %>% 
#   group_by(temp, type, effect) %>% 
#   summarise(total = sum(sum)) %>%
#   group_by(type, effect) %>% 
#   mutate(change = (total - first(total))/first(total))
# ppdat$change <- ppdat$change*100
# ppdat

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







