library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# Crop data
regdat <- readRDS("data/full_ag_data.rds")
regdat <- as.data.frame(regdat)
regdat$fips <- factor(regdat$fips)
regdat$state <- factor(regdat$state)
#---------------------------------------------------------------------------------------------

# Regressions with state fe and trends

trends <- regdat[, c(grep("trend1", names(regdat)), grep("trend2", names(regdat)))]        

paste(names(trends), collapse = " + ")

# "trend1_al", "trend1_ar", "trend1_ga", "trend1_ia", "trend1_il" "trend1_in" "trend1_ks" "trend1_ky"
# "trend1_md", "trend1_mi", "trend1_mn", "trend1_mo", "trend1_ms" "trend1_mt" "trend1_nc" "trend1_nd"
# "trend1_ne", "trend1_oh", "trend1_ok", "trend1_sc", "trend1_sd" "trend1_tn" "trend1_tx" "trend1_va"
# "trend1_wi", "trend1_wv", "trend2_al", "trend2_ar", "trend2_ga" "trend2_ia" "trend2_il" "trend2_in"
# "trend2_ks", "trend2_ky", "trend2_md", "trend2_mi", "trend2_mn" "trend2_mo" "trend2_ms" "trend2_mt"
# "trend2_nc", "trend2_nd", "trend2_ne", "trend2_oh", "trend2_ok" "trend2_sc" "trend2_sd" "trend2_tn"
# "trend2_tx", "trend2_va", "trend2_wi", "trend2_wv"

# Ten year differences 1950-1980 & 1980-2010

#-------------------------------------------------------------
mod_corn <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv 
            | fips | 0 | state, 
            data = regdat, weights = (1 + regdat$corn_w), psdef = FALSE)

summary(mod_corn)

mod_cotton <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv 
            | fips | 0 | state, 
            data = regdat, weights = (1 + regdat$cotton_w), psdef = FALSE)

summary(mod_cotton)

mod_hay <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv 
            | fips | 0 | state, 
            data = regdat, weights = (1 + regdat$hay_w), psdef = FALSE)

summary(mod_hay)

mod_soybean <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv 
            | fips | 0 | state, 
            data = regdat, weights = (1 + regdat$soybean_w), psdef = FALSE)

summary(mod_soybean)

mod_wheat <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv 
            | fips | 0 | state, 
            data = regdat, weights = (1 + regdat$wheat_w), psdef = FALSE)

summary(mod_wheat)

saveRDS(mod_corn, "models/mod_corn.rds")
saveRDS(mod_cotton, "models/mod_cotton.rds")
saveRDS(mod_hay, "models/mod_hay.rds")
saveRDS(mod_soybean, "models/mod_soybean.rds")
saveRDS(mod_wheat, "models/mod_wheat.rds")


#-------------------------------------------------------------
# # Build up regression results

# Corn
corn_base_1 <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$corn_w)

corn_base_2 <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq  | fips | 0 | 0,
            data = regdat, weights = regdat$corn_w)

corn_base_3 <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv 
            | fips  | 0 | 0,
            data = regdat, weights = regdat$corn_w)

corn_base_4 <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
              trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
              trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
              trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
              trend2_va + trend2_wi + trend2_wv
            | fips  | 0 | 0,
            data = regdat, weights = regdat$corn_w)

saveRDS(corn_base_1, "models/mod_corn_base_1.rds")
saveRDS(corn_base_2, "models/mod_corn_base_2.rds")
saveRDS(corn_base_3, "models/mod_corn_base_3.rds")
saveRDS(corn_base_3, "models/mod_corn_base_4.rds")


# Cotton
cotton_base_1 <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$cotton_w)

cotton_base_2 <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq  | fips | 0 | 0,
            data = regdat, weights = regdat$cotton_w)

cotton_base_3 <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv 
            | fips  | 0 | 0,
            data = regdat, weights = regdat$cotton_w)

cotton_base_4 <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
              trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
              trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
              trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
              trend2_va + trend2_wi + trend2_wv
            | fips  | 0 | 0,
            data = regdat, weights = regdat$cotton_w)

saveRDS(cotton_base_1, "models/mod_cotton_base_1.rds")
saveRDS(cotton_base_2, "models/mod_cotton_base_2.rds")
saveRDS(cotton_base_3, "models/mod_cotton_base_3.rds")
saveRDS(cotton_base_3, "models/mod_cotton_base_4.rds")


# hay
hay_base_1 <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$hay_w)

hay_base_2 <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq  | fips | 0 | 0,
            data = regdat, weights = regdat$hay_w)

hay_base_3 <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv 
            | fips  | 0 | 0,
            data = regdat, weights = regdat$hay_w)

hay_base_4 <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
              trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
              trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
              trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
              trend2_va + trend2_wi + trend2_wv
            | fips  | 0 | 0,
            data = regdat, weights = regdat$hay_w)

saveRDS(hay_base_1, "models/mod_hay_base_1.rds")
saveRDS(hay_base_2, "models/mod_hay_base_2.rds")
saveRDS(hay_base_3, "models/mod_hay_base_3.rds")
saveRDS(hay_base_3, "models/mod_hay_base_4.rds")


# soybean
soybean_base_1 <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$soybean_w)

soybean_base_2 <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq  | fips | 0 | 0,
            data = regdat, weights = regdat$soybean_w)

soybean_base_3 <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv 
            | fips  | 0 | 0,
            data = regdat, weights = regdat$soybean_w)

soybean_base_4 <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
              trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
              trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
              trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
              trend2_va + trend2_wi + trend2_wv
            | fips  | 0 | 0,
            data = regdat, weights = regdat$soybean_w)

saveRDS(soybean_base_1, "models/mod_soybean_base_1.rds")
saveRDS(soybean_base_2, "models/mod_soybean_base_2.rds")
saveRDS(soybean_base_3, "models/mod_soybean_base_3.rds")
saveRDS(soybean_base_3, "models/mod_soybean_base_4.rds")


# wheat
wheat_base_1 <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$wheat_w)

wheat_base_2 <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq  | fips | 0 | 0,
            data = regdat, weights = regdat$wheat_w)

wheat_base_3 <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv 
            | fips  | 0 | 0,
            data = regdat, weights = regdat$wheat_w)

wheat_base_4 <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
              trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
              trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
              trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
              trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
              trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
              trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
              trend2_va + trend2_wi + trend2_wv
            | fips  | 0 | 0,
            data = regdat, weights = regdat$wheat_w)

saveRDS(wheat_base_1, "models/mod_wheat_base_1.rds")
saveRDS(wheat_base_2, "models/mod_wheat_base_2.rds")
saveRDS(wheat_base_3, "models/mod_wheat_base_3.rds")
saveRDS(wheat_base_3, "models/mod_wheat_base_4.rds")




