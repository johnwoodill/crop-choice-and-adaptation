library(tidyverse)
library(lfe)
library(systemfit)
library(ggthemes)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictSUR.R")
source("R/predictSUR.clean.R")

cropdat <- readRDS("data/full_ag_data.rds")


# Get SUR out files
# sur_five <- readRDS("data/sur_out_five.rds")
# sur_ten <- readRDS("data/sur_out_ten.rds")
# sur_twenty <- readRDS("data/sur_out_twenty.rds")
# sur_thirty <- readRDS("data/sur_out_thirty.rds")

sur_five <- readRDS("models/sur_model_five.rds")
sur_ten <- readRDS("models/sur_model_ten.rds")
sur_twenty <- readRDS("models/sur_model_twenty.rds")
sur_thirty <- readRDS("models/sur_model_thirty.rds")
sur_sixty <- readRDS("models/sur_model_sixty.rds")

# 
# # Residuals
# mod_res <- readRDS("data/mod_residuals.rds")

# msd <- select(sur_thirty, crop_mean_m, crop_sd_sd)

# sur_thirty <- sur_thirty[, 2:62]
# names(sur_thirty) <- substr(names(sur_thirty), 1, nchar(names(sur_thirty))-2)

# mod_matrix <- model.matrix(~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#                dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
#                factor(state) + factor(thirty) +trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#                trend2_va + trend2_wi - 1, data = cropdat)

p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")

newdata_list <- list(p0 = cropdat,
                     p1 = p1,
                     p2 = p2,
                     p3 = p3,
                     p4 = p4,
                     p5 = p5)

#-------------------------------------
# Five-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "factor(state)", 
                  "factor(five)", "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_five", "dday10_30_five", "dday30_five", "prec_five", "prec_sq_five", 
                  "factor(state)", "factor(five)", "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")

wfive <- predictSURSUR.clean(sur_five, newdata_list = newdata_list, terms = weather_terms, type = "5-year", effect = "Weather-effect")
cfive <- predictSURSUR.clean(sur_five, newdata_list = newdata_list, terms = climate_terms, type = "5-year", effect = "Climate-effect")
tfive <- predictSURSUR.clean(sur_five, newdata_list = newdata_list, type = "5-year", effect = "Total-effect")
#-------------------------------------
# Ten-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "factor(state)", 
                  "factor(ten)", "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_ten", "dday10_30_ten", "dday30_ten", "prec_ten", "prec_sq_ten", 
                  "factor(state)", "factor(ten)", "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")

wten <- predictSURSUR.clean(sur_ten, newdata_list = newdata_list, terms = weather_terms, type = "10-year", effect = "Weather-effect")
cten <- predictSURSUR.clean(sur_ten, newdata_list = newdata_list, terms = climate_terms, type = "10-year", effect = "Climate-effect")
tten <- predictSURSUR.clean(sur_ten, newdata_list = newdata_list, type = "10-year", effect = "Total-effect")

#-------------------------------------
# Twenty-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "factor(state)", 
                  "factor(twenty)", "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_twenty", "dday10_30_twenty", "dday30_twenty", "prec_twenty", "prec_sq_twenty", 
                  "factor(state)", "factor(twenty)", "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")

wtwenty <- predictSURSUR.clean(sur_twenty, newdata_list = newdata_list, terms = weather_terms, type = "20-year", effect = "Weather-effect")
ctwenty <- predictSURSUR.clean(sur_twenty, newdata_list = newdata_list, terms = climate_terms, type = "20-year", effect = "Climate-effect")
ttwenty <- predictSURSUR.clean(sur_twenty, newdata_list = newdata_list, type = "20-year", effect = "Total-effect")

#-------------------------------------
# Thirty-year

# Weather-effect predictions
weather_terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "factor(state)", 
                  "factor(thirty)", "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", 
                  "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", 
                  "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", 
                  "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", 
                  "trend2_va", "trend2_wi")

# Climate-effect predictions
climate_terms = c("dday0_10_thirty", "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty", 
                  "factor(state)", "factor(thirty)", "trend2_al", "trend2_ar", "trend2_de", 
                  "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
                  "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", 
                  "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok", "trend2_sc", 
                  "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi")

wthirty <- predictSURSUR.clean(sur_thirty, newdata_list = newdata_list, terms = weather_terms, type = "30-year", effect = "Weather-effect")
cthirty <- predictSURSUR.clean(sur_thirty, newdata_list = newdata_list, terms = climate_terms, type = "30-year", effect = "Climate-effect")
tthirty <- predictSURSUR.clean(sur_thirty, newdata_list = newdata_list, type = "30-year", effect = "Total-effect")

#-------------------------------------
# Sixty-year

# Climate-effect predictions
climate_terms = c("dday0_10_sixty", "dday10_30_sixty", "dday30_sixty", "prec_sixty", "prec_sq_sixty", 
                  "factor(state)")

csixty <- predictSURSUR.clean(sur_sixty, newdata_list = newdata_list, terms = climate_terms, type = "60-year", effect = "Climate-effect")



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

saveRDS(pdat, "data/sur_predictions.rds")

ggplot(pdat, aes(temp, change, color = effect)) + 
  geom_line() + 
  geom_point() + 
  #geom_errorbar(aes(ymax = change_max, ymin = change_min, color = effect), width = .1) +
  theme_tufte(base_size = 12) +
  ylab("% Change in Crop Acres") +
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
  filter(effect == "Climate-effect") %>% 
  group_by(temp, type, effect) %>% 
  summarise(total = sum(sum)) %>%
  group_by(type, effect) %>% 
  mutate(change = (total - first(total))/first(total))
ppdat$change <- ppdat$change*100
ppdat

ggplot(ppdat, aes(temp, change, color = type)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~effect, ncol = 3) +
  theme_tufte(base_size = 12) +
  ylab("% Change in Total Crop Acres") +
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







