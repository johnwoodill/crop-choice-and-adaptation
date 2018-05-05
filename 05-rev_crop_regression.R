library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# Crop data

regdat <- readRDS("data/full_ag_data.rds")

# trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
#   trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
#   trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
#   trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
#   trend1_wv

#---------------------------------------------------------------------------------------------
# Regressions with state fe and trends

# Ten year differences 1950-1980 & 1980-2010

modten_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm + dday10_30_rm + dday30_rm + prec_rm + prec_sq_rm +
              trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv   | fips  | 0 | state, 
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modten_1)
saveRDS(modten_1, "models/rev_crop_modten.rds")

summary(fit)
# Twenty year differences 1950-1980 & 1980-2010
modtwenty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
              trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | state, 
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modtwenty_1)

saveRDS(modtwenty_1, "models/rev_crop_modtwenty.rds")

# Thirty year differences 1950-1980 & 1980-2010

modthirty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
              trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | state, 
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modthirty_1)

saveRDS(modthirty_1, "models/rev_crop_modthirty.rds")

# Build up regression

mod_base_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10,
            data = regdat, weights = regdat$w)

mod_base_2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 | fips | 0 | 0,
            data = regdat, weights = regdat$w)

mod_base_3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
                trend + trend_sq | fips | 0 | 0,
            data = regdat, weights = regdat$w)

saveRDS(mod_base_1, "models/rev_crop_mod_base_1.rds")
saveRDS(mod_base_2, "models/rev_crop_mod_base_2.rds")
saveRDS(mod_base_3, "models/rev_crop_mod_base_3.rds")
