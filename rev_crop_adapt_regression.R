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

# Five year differences 1950-1980 & 1980-2010
modfive_corn <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + five| 0 | state, 
            data = regdat, weights = (1 + regdat$corn_w), psdef = FALSE)

sum(modfive_corn$coefficients[1:10])
summary(modfive_corn)

modfive_cotton <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + five | 0 | state, 
            data = regdat, weights = (1 + regdat$cotton_w), psdef = FALSE)

sum(modfive_cotton$coefficients[1:10])
summary(modfive_cotton)

modfive_hay <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + five | 0 | state, 
            data = regdat, weights = (1 + regdat$hay_w), psdef = FALSE)

sum(modfive_hay$coefficients[1:10])
summary(modfive_hay)

modfive_soybean <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + five | 0 | state, 
            data = regdat, weights = (1 + regdat$soybean_w), psdef = FALSE)

sum(modfive_soybean$coefficients[1:10])
summary(modfive_soybean)

modfive_wheat <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + five | 0 | state, 
            data = regdat, weights = (1 + regdat$wheat_w), psdef = FALSE)

sum(modfive_wheat$coefficients)
summary(modfive_wheat)

saveRDS(modfive_corn, "models/modfive_corn.rds")
saveRDS(modfive_cotton, "models/modfive_cotton.rds")
saveRDS(modfive_hay, "models/modfive_hay.rds")
saveRDS(modfive_soybean, "models/modfive_soybean.rds")
saveRDS(modfive_wheat, "models/modfive_wheat.rds")

# Ten year differences 1950-1980 & 1980-2010


# ten year differences 1950-1980 & 1980-2010
modten_corn <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + ten | 0 | state, 
            data = regdat, weights = (1 + regdat$corn_w), psdef = FALSE)

summary(modten_corn)

modten_cotton <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + ten | 0 | state, 
            data = regdat, weights = (1 + regdat$cotton_w), psdef = FALSE)

summary(modten_cotton)

modten_hay <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + ten | 0 | state, 
            data = regdat, weights = (1 + regdat$hay_w), psdef = FALSE)

summary(modten_hay)

modten_soybean <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + ten | 0 | state, 
            data = regdat, weights = (1 + regdat$soybean_w), psdef = FALSE)

summary(modten_soybean)

modten_wheat <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + ten | 0 | state, 
            data = regdat, weights = (1 + regdat$wheat_w), psdef = FALSE)

summary(modten_wheat)

saveRDS(modten_corn, "models/modten_corn.rds")
saveRDS(modten_cotton, "models/modten_cotton.rds")
saveRDS(modten_hay, "models/modten_hay.rds")
saveRDS(modten_soybean, "models/modten_soybean.rds")
saveRDS(modten_wheat, "models/modten_wheat.rds")


# twenty year differences 1950-1980 & 1980-2010
modtwenty_corn <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + twenty | 0 | state, 
            data = regdat, weights = (1 + regdat$corn_w))

sum(modtwenty_corn$coefficients[1:10])
summary(modtwenty_corn)

modtwenty_cotton <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + twenty | 0 | state, 
            data = regdat, weights = (1 + regdat$cotton_w), psdef = FALSE)

sum(modtwenty_cotton$coefficients[1:10])
summary(modtwenty_cotton)

modtwenty_hay <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + twenty  | 0 | state, 
            data = regdat, weights = (1 + regdat$hay_w), psdef = FALSE)

sum(modtwenty_hay$coefficients)
summary(modtwenty_hay)

modtwenty_soybean <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + twenty | 0 | state, 
            data = regdat, weights = (1 +regdat$soybean_w), psdef = FALSE)

summary(modtwenty_soybean)

modtwenty_wheat <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + twenty | 0 | state, 
            data = regdat, weights = (1 + regdat$wheat_w), psdef = FALSE)

summary(modtwenty_wheat)

saveRDS(modtwenty_corn, "models/modtwenty_corn.rds")
saveRDS(modtwenty_cotton, "models/modtwenty_cotton.rds")
saveRDS(modtwenty_hay, "models/modtwenty_hay.rds")
saveRDS(modtwenty_soybean, "models/modtwenty_soybean.rds")
saveRDS(modtwenty_wheat, "models/modtwenty_wheat.rds")


# thirty year differences 1950-1980 & 1980-2010
modthirty_corn <- felm(ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + thirty | 0 | state, 
            data = regdat, weights = (1 + regdat$corn_w), psdef = FALSE)

summary(modthirty_corn)

modthirty_cotton <- felm(ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + thirty | 0 | state, 
            data = regdat, weights = (1 + regdat$cotton_w), psdef = FALSE)

summary(modthirty_cotton)

modthirty_hay <- felm(ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + thirty | 0 | state, 
            data = regdat, weights = (1 + regdat$hay_w), psdef = FALSE)

summary(modthirty_hay)

modthirty_soybean <- felm(ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + thirty | 0 | state, 
            data = regdat, weights = (1 + regdat$soybean_w), psdef = FALSE)

summary(modthirty_soybean)

modthirty_wheat <- felm(ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty + 
              trend_al +trend_ar  +trend_ga + trend_ia  +         
              trend_il +trend_in + trend_ks + trend_ky + trend_md + trend_mi +         
              trend_mn+ trend_mo + trend_ms + trend_mt + trend_nc + trend_nd +         
              trend_ne +trend_oh + trend_ok +  trend_sc + trend_sd + trend_tn +         
              trend_va + trend_wi +
              trend2_al +trend2_ar  +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi 
            | fips + thirty | 0 | state, 
            data = regdat, weights = (1 + regdat$wheat_w), psdef = FALSE)

summary(modthirty_wheat)

saveRDS(modthirty_corn, "models/modthirty_corn.rds")
saveRDS(modthirty_cotton, "models/modthirty_cotton.rds")
saveRDS(modthirty_hay, "models/modthirty_hay.rds")
saveRDS(modthirty_soybean, "models/modthirty_soybean.rds")
saveRDS(modthirty_wheat, "models/modthirty_wheat.rds")



# sixty year differences 1950-1980 & 1980-2010
modsixty_corn <- felm(ln_rev_corn ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty  
            | state | 0 | state, 
            data = regdat, weights = (1 + regdat$corn_w), psdef = FALSE)

summary(modsixty_corn)

modsixty_cotton <- felm(ln_rev_cotton ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty  
            | state | 0 | state, 
            data = regdat, weights = (1 + regdat$cotton_w), psdef = FALSE)

summary(modsixty_cotton)

modsixty_hay <- felm(ln_rev_hay ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty  
            | state | 0 | state, 
            data = regdat, weights = (1 + regdat$hay_w), psdef = FALSE)

summary(modsixty_hay)

modsixty_soybean <- felm(ln_rev_soybean ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty  
            | state | 0 | state, 
            data = regdat, weights = (1 + regdat$soybean_w), psdef = FALSE)

summary(modsixty_soybean)

modsixty_wheat <- felm(ln_rev_wheat ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty 
            | state | 0 | state, 
            data = regdat, weights = (1 + regdat$wheat_w), psdef = FALSE)

summary(modsixty_wheat)

saveRDS(modsixty_corn, "models/modsixty_corn.rds")
saveRDS(modsixty_cotton, "models/modsixty_cotton.rds")
saveRDS(modsixty_hay, "models/modsixty_hay.rds")
saveRDS(modsixty_soybean, "models/modsixty_soybean.rds")
saveRDS(modsixty_wheat, "models/modsixty_wheat.rds")

# 
# # # Build up regression
# mod_base_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq, data = regdat, weights = regdat$w)
# 
# mod_base_2 <- felm(ln_rev ~ dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty,
#             data = regdat, weights = regdat$w)
# 
# mod_base_3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#               dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty,
#             data = regdat, weights = regdat$w)
# 
# mod_base_4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#               dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
#               trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#               trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#               trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#               trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#               trend2_va + trend2_wi
#             | state  | 0 | 0,
#             data = regdat, weights = regdat$w)
# 
# saveRDS(mod_base_1, "models/mod_base_1.rds")
# saveRDS(mod_base_2, "models/mod_base_2.rds")
# saveRDS(mod_base_3, "models/mod_base_3.rds")
# saveRDS(mod_base_4, "models/mod_base_4.rds")
