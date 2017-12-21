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
sur_five <- readRDS("models/sur_model_five.rds")
sur_ten <- readRDS("models/sur_model_ten.rds")
sur_twenty <- readRDS("models/sur_model_twenty.rds")
sur_thirty <- readRDS("models/sur_model_thirty.rds")
sur_sixty <- readRDS("models/sur_model_sixty.rds")


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


# Demean data
p0_five <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         five = factor(cropdat$five)))
p0_five_means <- demeanlist(p0, fl = list(fips = factor(cropdat$fips),
                                         five = factor(cropdat$five)), means = TRUE)

# Different predictions data from baseline means
p1_five <- p1 - p0_five_means
p2_five <- p2 - p0_five_means
p3_five <- p3 - p0_five_means
p4_five <- p4 - p0_five_means
p5_five <- p5 - p0_five_means

# Bind list
newdata_list_five <- list(p0 = p0_five,
                     p1 = p1_five,
                     p2 = p2_five,
                     p3 = p3_five,
                     p4 = p4_five,
                     p5 = p5_five)



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

p0_sixty <- demeanlist(p0, fl = list(state = factor(cropdat$state)))
p0_sixty_means <- demeanlist(p0, fl = list(state = factor(cropdat$state)), means = TRUE)

p1_sixty <- p1 - p0_sixty_means
p2_sixty <- p2 - p0_sixty_means
p3_sixty <- p3 - p0_sixty_means
p4_sixty <- p4 - p0_sixty_means
p5_sixty <- p5 - p0_sixty_means

newdata_list_sixty <- list(p0 = p0_sixty,
                     p1 = p1_sixty,
                     p2 = p2_sixty,
                     p3 = p3_sixty,
                     p4 = p4_sixty,
                     p5 = p5_sixty)


cons.terms_w <- c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq")
cons.terms_c_five <- c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five")
cons.terms_c_ten <- c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten")
cons.terms_c_twenty <- c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty")
cons.terms_c_thirty <- c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty")


#-------------------------------------
# Five-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", 
                  "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five", 
                  "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")


# test <- predictSUR(sur_five, newdata = cropdat, var.terms = c("dday30"), cons.terms = c("dday0_10_five"), intercept = FALSE)
# head(test)
# 
# wfive <- predictSUR.clean(mod = sur_five, 
#                           newdata_list = newdata_list_five, 
#                           var.terms = c("dday30"),
#                           cons.terms = c("dday0_10_five"),
#                           type = "5-year", 
#                           effect = "Weather-effect")
# head(wfive)

wfive <- predictSUR.clean(mod = sur_five, newdata_list = newdata_list_five, var.terms = weather_terms, cons.terms = cons.terms_c_five, type = "5-year", effect = "Weather-effect")
cfive <- predictSUR.clean(mod = sur_five, newdata_list = newdata_list_five, var.terms = climate_terms, cons.terms = cons.terms_w, type = "5-year", effect = "Climate-effect")
tfive <- predictSUR.clean(mod = sur_five, newdata_list = newdata_list_five, type = "5-year", effect = "Total-effect")

#-------------------------------------
# Ten-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten", 
                  "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")

wten <- predictSUR.clean(sur_ten, newdata_list = newdata_list_ten, var.terms = weather_terms, cons.terms = cons.terms_c_ten, type = "10-year", effect = "Weather-effect")
cten <- predictSUR.clean(sur_ten, newdata_list = newdata_list_ten, var.terms = climate_terms, cons.terms = cons.terms_w, type = "10-year", effect = "Climate-effect")
tten <- predictSUR.clean(sur_ten, newdata_list = newdata_list_ten, type = "10-year", effect = "Total-effect")

#-------------------------------------
# Twenty-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq",
                  "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty", 
                  "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")

wtwenty <- predictSUR.clean(sur_twenty, newdata_list = newdata_list_twenty, var.terms = weather_terms, cons.terms = cons.terms_c_twenty, type = "20-year", effect = "Weather-effect")
ctwenty <- predictSUR.clean(sur_twenty, newdata_list = newdata_list_twenty, var.terms = climate_terms, cons.terms = cons.terms_w, type = "20-year", effect = "Climate-effect")
ttwenty <- predictSUR.clean(sur_twenty, newdata_list = newdata_list_twenty, type = "20-year", effect = "Total-effect", var.terms = NULL)

#-------------------------------------
# Thirty-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", 
                  "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty", 
                  "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")

wthirty <- predictSUR.clean(sur_thirty, newdata_list = newdata_list_thirty, var.terms = weather_terms, cons.terms = cons.terms_c_thirty, type = "30-year", effect = "Weather-effect")
cthirty <- predictSUR.clean(sur_thirty, newdata_list = newdata_list_thirty, var.terms = climate_terms, cons.terms = cons.terms_w, type = "30-year", effect = "Climate-effect")
tthirty <- predictSUR.clean(sur_thirty, newdata_list = newdata_list_thirty, type = "30-year", effect = "Total-effect", var.terms = NULL)

#-------------------------------------
# Sixty-year

# Climate-effect predictions
climate_terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty")

csixty <- predictSUR.clean(sur_sixty, newdata_list = newdata_list_sixty, type = "60-year", effect = "Climate-effect")



pdat <- rbind(wfive, cfive, tfive,
              wten, cten, tten,
              wtwenty, ctwenty, ttwenty,
              wthirty, cthirty, tthirty,
              csixty)

pdat <- pdat %>% 
  group_by(crop, type, effect) %>% 
  mutate(change = (sum - first(sum))/first(sum))
pdat
pdat$change <- 100*pdat$change

pdat <- filter(pdat, type != "60-year" | effect != "Weather-effect" & effect != "Total-effect")

saveRDS(pdat, "data/sur_predictions.rds")

ggplot(pdat, aes(temp, change, color = effect)) + 
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
  facet_wrap(type~crop, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

ppdat <- pdat %>% 
  #filter(effect == "Climate-effect") %>% 
  group_by(temp, type, effect) %>% 
  summarise(total = sum(sum)) %>%
  group_by(type, effect) %>% 
  mutate(change = (total - first(total))/first(total))
ppdat$change <- ppdat$change*100
ppdat

ggplot(ppdat, aes(temp, change, color = type)) + 
  geom_line() + 
  geom_point(size = 0.5) + 
  facet_wrap(~effect, ncol = 3) +
  theme_tufte(base_size = 12) +
  ylab("Change in Proportion of Total Crop Acres") +
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")







