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

# Setup parallel for bootstrapping
cl <- makeCluster(20)
registerDoParallel(cl)

# # Crop data
# download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1",
#               destfile = "data/full_ag_data.rds", method = "auto")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$five <- factor(cropdat$five)
cropdat$ten <- factor(cropdat$ten)
cropdat$twenty <- factor(cropdat$twenty)
cropdat$thirty <- factor(cropdat$thirty)


#-----------------------------------------------------------------------------------
# ten-year


testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10_ten , dday10_30_ten , dday30_ten , prec_ten , prec_sq_ten, 
trend1_al , trend1_ar , trend1_ga , trend1_ia , trend1_il , trend1_in , trend1_ks , 
  trend1_ky , trend1_md , trend1_mi , trend1_mn , trend1_mo , trend1_ms , trend1_mt , 
  trend1_nc , trend1_nd , trend1_ne , trend1_oh , trend1_ok , trend1_sc , trend1_sd , 
  trend1_tn , trend1_tx , trend1_va , trend1_wi , trend1_wv , trend2_al , trend2_ar , 
  trend2_ga , trend2_ia , trend2_il , trend2_in , trend2_ks , trend2_ky , trend2_md , 
  trend2_mi , trend2_mn , trend2_mo , trend2_ms , trend2_mt , trend2_nc , trend2_nd , 
  trend2_ne , trend2_oh , trend2_ok , trend2_sc , trend2_sd , trend2_tn , trend2_tx , 
  trend2_va , trend2_wi , trend2_wv)

cropdat_dm <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      ten = factor(cropdat$ten)))

cropdat_means <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      ten = factor(cropdat$ten)), means = TRUE)


mod1 <- z_corn_a ~ dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod2 <- z_cotton_a ~    dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1
 

mod3 <- z_hay_a ~   dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod4 <- z_soybean_a ~ dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv - 1


mod5 <- z_wheat_a ~ dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1

mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

summary(mod)
sum(mod$coefficients)

mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                    cotton.effect = cropdat_means$z_cotton_a,
                    hay.effect = cropdat_means$z_hay_a,
                    soybean.effect = cropdat_means$z_soybean_a,
                    wheat.effect = cropdat_means$z_wheat_a)

# Bootstrap standard errors
bs_cropdat_dm <- cropdat_dm
bs_cropdat_dm$ten <- cropdat$ten

se_dat <- data.frame()

d <- foreach(i = 1:2, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
  # Resample within interval
  regdat <- bs_cropdat_dm %>% 
    group_by(ten) %>% 
    sample_frac(1, replace = TRUE)
  
  bsmod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = regdat, method = "SUR")
  
  mdat <- as.data.frame(t(bsmod$coefficients))
  names(mdat) <- names(bsmod$coefficients)
  mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
  mdat
}

mod$bs.se <- as.data.frame(apply(d, 2, sd))
saveRDS(mod, "models/sur_model_ten.rds")




#-----------------------------------------------------------------------------------
# twenty-year


testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10_twenty , dday10_30_twenty , dday30_twenty , prec_twenty , prec_sq_twenty, 
trend1_al , trend1_ar , trend1_ga , trend1_ia , trend1_il , trend1_in , trend1_ks , 
  trend1_ky , trend1_md , trend1_mi , trend1_mn , trend1_mo , trend1_ms , trend1_mt , 
  trend1_nc , trend1_nd , trend1_ne , trend1_oh , trend1_ok , trend1_sc , trend1_sd , 
  trend1_tn , trend1_tx , trend1_va , trend1_wi , trend1_wv , trend2_al , trend2_ar , 
  trend2_ga , trend2_ia , trend2_il , trend2_in , trend2_ks , trend2_ky , trend2_md , 
  trend2_mi , trend2_mn , trend2_mo , trend2_ms , trend2_mt , trend2_nc , trend2_nd , 
  trend2_ne , trend2_oh , trend2_ok , trend2_sc , trend2_sd , trend2_tn , trend2_tx , 
  trend2_va , trend2_wi , trend2_wv)

cropdat_dm <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      twenty = factor(cropdat$twenty)))

cropdat_means <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      twenty = factor(cropdat$twenty)), means = TRUE)



mod1 <- z_corn_a ~ dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod2 <- z_cotton_a ~   dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
  trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1
 

mod3 <- z_hay_a ~   dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod4 <- z_soybean_a ~   dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1

mod5 <- z_wheat_a ~   dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1

mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

summary(mod)
sum(mod$coefficients)

mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                    cotton.effect = cropdat_means$z_cotton_a,
                    hay.effect = cropdat_means$z_hay_a,
                    soybean.effect = cropdat_means$z_soybean_a,
                    wheat.effect = cropdat_means$z_wheat_a)

# Bootstrap standard errors
bs_cropdat_dm <- cropdat_dm
bs_cropdat_dm$twenty <- cropdat$twenty

se_dat <- data.frame()

d <- foreach(i = 1:2, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
  # Resample within interval
  regdat <- bs_cropdat_dm %>% 
    group_by(twenty) %>% 
    sample_frac(1, replace = TRUE)
  
  bsmod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = regdat, method = "SUR")
  
  mdat <- as.data.frame(t(bsmod$coefficients))
  names(mdat) <- names(bsmod$coefficients)
  mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
  mdat
}

mod$bs.se <- as.data.frame(apply(d, 2, sd))


saveRDS(mod, "models/sur_model_twenty.rds")



#-----------------------------------------------------------------------------------
# Thirty-year

testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10_thirty , dday10_30_thirty , dday30_thirty , prec_thirty , prec_sq_thirty, 
trend1_al , trend1_ar , trend1_ga , trend1_ia , trend1_il , trend1_in , trend1_ks , 
  trend1_ky , trend1_md , trend1_mi , trend1_mn , trend1_mo , trend1_ms , trend1_mt , 
  trend1_nc , trend1_nd , trend1_ne , trend1_oh , trend1_ok , trend1_sc , trend1_sd , 
  trend1_tn , trend1_tx , trend1_va , trend1_wi , trend1_wv , trend2_al , trend2_ar , 
  trend2_ga , trend2_ia , trend2_il , trend2_in , trend2_ks , trend2_ky , trend2_md , 
  trend2_mi , trend2_mn , trend2_mo , trend2_ms , trend2_mt , trend2_nc , trend2_nd , 
  trend2_ne , trend2_oh , trend2_ok , trend2_sc , trend2_sd , trend2_tn , trend2_tx , 
  trend2_va , trend2_wi , trend2_wv)

cropdat_dm <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      thirty = factor(cropdat$thirty)))

cropdat_means <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      thirty = factor(cropdat$thirty)), means = TRUE)



mod1 <- z_corn_a ~   dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod2 <- z_cotton_a ~   dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1
 

mod3 <- z_hay_a ~   dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod4 <- z_soybean_a ~  dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod5 <- z_wheat_a ~  dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
  trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
  trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
  trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv + trend2_al + trend2_ar + 
  trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
  trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
  trend2_va + trend2_wi + trend2_wv  - 1


mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

summary(mod)
sum(mod$coefficients)

mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                    cotton.effect = cropdat_means$z_cotton_a,
                    hay.effect = cropdat_means$z_hay_a,
                    soybean.effect = cropdat_means$z_soybean_a,
                    wheat.effect = cropdat_means$z_wheat_a)

# Bootstrap standard errors
bs_cropdat_dm <- cropdat_dm
bs_cropdat_dm$thirty <- cropdat$thirty

se_dat <- data.frame()

d <- foreach(i = 1:2, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
  # Resample within interval
  regdat <- bs_cropdat_dm %>% 
    group_by(thirty) %>% 
    sample_frac(1, replace = TRUE)
  
  bsmod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = regdat, method = "SUR")
  
  mdat <- as.data.frame(t(bsmod$coefficients))
  names(mdat) <- names(bsmod$coefficients)
  mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
  mdat
}

mod$bs.se <- as.data.frame(apply(d, 2, sd))


saveRDS(mod, "models/sur_model_thirty.rds")

stopCluster(cl)
