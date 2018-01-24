#-----------------------------------------------------
# Warning: This script includes bootstrapping methods
# for calculating standard errors for each SUR equation.
# As a result, this script will take a while to run.
#------------------------------
# Time to complete: 24-hours
# Cores: 20  RAM: 200GB
#------------------------------


library(systemfit)
library(tidyverse)
library(lfe)
library(doParallel)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")
setwd("/home/johnw/")

# Setup parallel for bootstrapping
# cl <- makeCluster(20)
# registerDoParallel(cl)

# # Crop data
download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1",
              destfile = "data/full_ag_data.rds", method = "auto")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$ten <- factor(cropdat$ten)
cropdat$twenty <- factor(cropdat$twenty)
cropdat$thirty <- factor(cropdat$thirty)


#-----------------------------------------------------------------------------------
dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat, 
                dday0_10, dday10_30, dday30, prec, prec_sq,
trend1_al , trend1_ar , trend1_ga , trend1_ia , trend1_il , trend1_in , trend1_ks , 
  trend1_ky , trend1_md , trend1_mi , trend1_mn , trend1_mo , trend1_ms , trend1_mt , 
  trend1_nc , trend1_nd , trend1_ne , trend1_oh , trend1_ok , trend1_sc , trend1_sd , 
  trend1_tn , trend1_tx , trend1_va , trend1_wi , trend1_wv , trend2_al , trend2_ar , 
  trend2_ga , trend2_ia , trend2_il , trend2_in , trend2_ks , trend2_ky , trend2_md , 
  trend2_mi , trend2_mn , trend2_mo , trend2_ms , trend2_mt , trend2_nc , trend2_nd , 
  trend2_ne , trend2_oh , trend2_ok , trend2_sc , trend2_sd , trend2_tn , trend2_tx , 
  trend2_va , trend2_wi , trend2_wv)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
  trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod2 <- ln_rev_cotton ~    dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
  trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1
 

mod3 <- ln_rev_hay ~   dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
  trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
  trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
  trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1

mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

summary(ten_mod)
sum(ten_mod$coefficients)

mod$effects <- list(ln_corn.effect = cropdat_means$ln_rev_corn,
                    ln_cotton.effect = cropdat_means$ln_rev_cotton,
                    ln_hay.effect = cropdat_means$ln_rev_hay,
                    ln_soybean.effect = cropdat_means$ln_rev_soybean,
                    ln_wheat.effect = cropdat_means$ln_rev_wheat)

# Bootstrap standard errors
# bs_cropdat_dm <- cropdat_dm
# 
# se_dat <- data.frame()
# 
# d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
#   # Resample within interval
#   regdat <- bs_cropdat_dm %>% 
#     sample_frac(1, replace = TRUE)
#   
#   bsmod <- systemfit(list(corn = mod1, 
#                        cotton = mod2, 
#                        hay = mod3, 
#                        soybean = mod4,
#                        wheat = mod5), data = regdat, method = "SUR")
#   
#   mdat <- as.data.frame(t(bsmod$coefficients))
#   names(mdat) <- names(bsmod$coefficients)
#   mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
#   mdat
# }
# 
# mod$bs.se <- as.data.frame(apply(d, 2, sd))
saveRDS(mod, "models/sur_rev_model.rds")


# stopCluster(cl)
