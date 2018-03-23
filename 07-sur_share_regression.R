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

# Setup parallel for bootstrapping
# cl <- makeCluster(14)
# registerDoParallel(cl)

# Crop data
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/full_ag_data.rds",
#               destfile = "data/full_ag_data.rds", method = "auto")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)


#-----------------------------------------------------------------------------------
# ten-year


dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                dday0_10_rm10 , dday10_30_rm10 , dday30_rm10 , prec_rm10 , prec_sq_rm10, 
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- z_corn_a ~ 
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~   
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod3 <- z_hay_a ~  
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod4 <- z_soybean_a ~  
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1



mod5 <- z_wheat_a ~  
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1

ten_mod <- systemfit(list(corn = mod1, 
                          cotton = mod2, 
                          hay = mod3, 
                          soybean = mod4,
                          wheat = mod5), data = cropdat_dm, method = "SUR")

summary(ten_mod)

ten_mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                        cotton.effect = cropdat_means$z_cotton_a,
                        hay.effect = cropdat_means$z_hay_a,
                        soybean.effect = cropdat_means$z_soybean_a,
                        wheat.effect = cropdat_means$z_wheat_a)

# Bootstrap standard errors
# bs_cropdat_dm <- cropdat_dm
# bs_cropdat_dm$year <- cropdat$year
# bs_cropdat_dm$state <- cropdat$state
# # 
# se_dat <- data.frame()
# # 
# d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
#   # Resample within interval
#   regdat <- bs_cropdat_dm %>%
#     group_by(state) %>%
#     sample_frac(1, replace = TRUE)
#   
#   bsmod <- systemfit(list(corn = mod1,
#                           cotton = mod2,
#                           hay = mod3,
#                           soybean = mod4,
#                           wheat = mod5), data = regdat, method = "SUR")
#   
#   mdat <- as.data.frame(t(bsmod$coefficients))
#   names(mdat) <- names(bsmod$coefficients)
#   mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
#   mdat
# }
# 
# ten_mod$bs.se <- as.data.frame(apply(d, 2, sd))


# From parallel run
ten_mod$bs.se <- structure(list(`apply(d, 2, sd)` = c(0.000210053668428398, 0.000107530699104682, 
0.000397022695507217, 0.0102568110764039, 0.000165599333645509, 
0.000104969425904618, 6.64406137620295e-05, 0.000314516531319913, 
0.00811069924659747, 0.000132947721802708, 0.000454872867061025, 
0.00012692268319596, 0.000370290745404606, 0.0105870162002529, 
0.000166387808780784, 0.000145279593843557, 7.57207015703651e-05, 
0.000307839141863906, 0.00839506994054773, 0.000138631062933204, 
0.000368774360036379, 0.000101345408309346, 0.000338149177903847, 
0.00875299228715998, 0.000135284361482097)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10_rm10", 
"corn_dday10_30_rm10", "corn_dday30_rm10", "corn_prec_rm10", 
"corn_prec_sq_rm10", "cotton_dday0_10_rm10", "cotton_dday10_30_rm10", 
"cotton_dday30_rm10", "cotton_prec_rm10", "cotton_prec_sq_rm10", 
"hay_dday0_10_rm10", "hay_dday10_30_rm10", "hay_dday30_rm10", 
"hay_prec_rm10", "hay_prec_sq_rm10", "soybean_dday0_10_rm10", 
"soybean_dday10_30_rm10", "soybean_dday30_rm10", "soybean_prec_rm10", 
"soybean_prec_sq_rm10", "wheat_dday0_10_rm10", "wheat_dday10_30_rm10", 
"wheat_dday30_rm10", "wheat_prec_rm10", "wheat_prec_sq_rm10"), class = "data.frame")

saveRDS(ten_mod, "models/sur_share_model_ten.rds")


#-----------------------------------------------------------------------------------
# twenty-year

dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq,
                dday0_10_rm11 , dday10_30_rm11 , dday30_rm11 , prec_rm11 , prec_sq_rm11, 
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- z_corn_a ~  
  dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod2 <- z_cotton_a ~  
  dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod3 <- z_hay_a ~   
  dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod4 <- z_soybean_a ~   
  dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1

mod5 <- z_wheat_a ~  
  dday0_10_rm11 + dday10_30_rm11 + dday30_rm11 + prec_rm11 + prec_sq_rm11 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1

twenty_mod <- systemfit(list(corn = mod1, 
                             cotton = mod2, 
                             hay = mod3, 
                             soybean = mod4,
                             wheat = mod5), data = cropdat_dm, method = "SUR")

summary(twenty_mod)
sum(twenty_mod$coefficients)

twenty_mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                           cotton.effect = cropdat_means$z_cotton_a,
                           hay.effect = cropdat_means$z_hay_a,
                           soybean.effect = cropdat_means$z_soybean_a,
                           wheat.effect = cropdat_means$z_wheat_a)

# Bootstrap standard errors
# bs_cropdat_dm <- cropdat_dm
# bs_cropdat_dm$year <- cropdat$year
# bs_cropdat_dm$state <- cropdat$state
# se_dat <- data.frame()
# 
# d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
#   # Resample within interval
#   regdat <- bs_cropdat_dm %>%
#     group_by(state) %>%
#     sample_frac(1, replace = TRUE)
#   
#   bsmod <- systemfit(list(corn = mod1,
#                           cotton = mod2,
#                           hay = mod3,
#                           soybean = mod4,
#                           wheat = mod5), data = regdat, method = "SUR")
#   
#   mdat <- as.data.frame(t(bsmod$coefficients))
#   names(mdat) <- names(bsmod$coefficients)
#   mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
#   mdat
# }
# 
# twenty_mod$bs.se <- as.data.frame(apply(d, 2, sd))


# From parallel run
twenty_mod$bs.se <- structure(list(`apply(d, 2, sd)` = c(0.000214515010841551, 0.000111288235521924, 
0.0004250061960508, 0.0110061571943664, 0.000176771923145687, 
0.000104547761363737, 7.06154508449778e-05, 0.000317895652354432, 
0.00844896585257001, 0.000139620296196725, 0.000477770656483317, 
0.000132895073435956, 0.000395290898020977, 0.0117937285984362, 
0.000185917739094665, 0.00014846588242155, 7.50342350314522e-05, 
0.000308381561140622, 0.00878894972114166, 0.000145176495969591, 
0.000357682204859115, 0.000104597636369948, 0.000355361871020709, 
0.00938895998780093, 0.000144774530017206)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10_rm11", 
"corn_dday10_30_rm11", "corn_dday30_rm11", "corn_prec_rm11", 
"corn_prec_sq_rm11", "cotton_dday0_10_rm11", "cotton_dday10_30_rm11", 
"cotton_dday30_rm11", "cotton_prec_rm11", "cotton_prec_sq_rm11", 
"hay_dday0_10_rm11", "hay_dday10_30_rm11", "hay_dday30_rm11", 
"hay_prec_rm11", "hay_prec_sq_rm11", "soybean_dday0_10_rm11", 
"soybean_dday10_30_rm11", "soybean_dday30_rm11", "soybean_prec_rm11", 
"soybean_prec_sq_rm11", "wheat_dday0_10_rm11", "wheat_dday10_30_rm11", 
"wheat_dday30_rm11", "wheat_prec_rm11", "wheat_prec_sq_rm11"), class = "data.frame")


saveRDS(twenty_mod, "models/sur_share_model_twenty.rds")



#-----------------------------------------------------------------------------------
# Thirty-year
dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq,
                dday0_10_rm12 , dday10_30_rm12 , dday30_rm12 , prec_rm12 , prec_sq_rm12, 
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- z_corn_a ~  
  dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod2 <- z_cotton_a ~   
  dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod3 <- z_hay_a ~  
  dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod4 <- z_soybean_a ~  
  dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1

mod5 <- z_wheat_a ~  
  dday0_10_rm12 + dday10_30_rm12 + dday30_rm12 + prec_rm12 + prec_sq_rm12 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1

thirty_mod <- systemfit(list(corn = mod1, 
                             cotton = mod2, 
                             hay = mod3, 
                             soybean = mod4,
                             wheat = mod5), data = cropdat_dm, method = "SUR")

summary(thirty_mod)
sum(thirty_mod$coefficients)

thirty_mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                           cotton.effect = cropdat_means$z_cotton_a,
                           hay.effect = cropdat_means$z_hay_a,
                           soybean.effect = cropdat_means$z_soybean_a,
                           wheat.effect = cropdat_means$z_wheat_a)

# Bootstrap standard errors
# bs_cropdat_dm <- cropdat_dm
# bs_cropdat_dm$year <- cropdat$year
# bs_cropdat_dm$state <- cropdat$state
# 
# se_dat <- data.frame()
# 
# d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
#   # Resample within interval
#   regdat <- bs_cropdat_dm %>%
#     group_by(state) %>% 
#     sample_frac(1, replace = TRUE)
#   
#   bsmod <- systemfit(list(corn = mod1,
#                           cotton = mod2,
#                           hay = mod3,
#                           soybean = mod4,
#                           wheat = mod5), data = regdat, method = "SUR")
#   
#   mdat <- as.data.frame(t(bsmod$coefficients))
#   names(mdat) <- names(bsmod$coefficients)
#   mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
#   mdat
# }
# # 
# thirty_mod$bs.se <- as.data.frame(apply(d, 2, sd))

# From parallel run
thirty_mod$bs.se <- structure(list(`apply(d, 2, sd)` = c(0.000224505455506903, 0.00011581865455546, 
0.000440301826908184, 0.0117698404958115, 0.000189268785370906, 
0.00010375779135024, 7.17926661797334e-05, 0.000343537030172129, 
0.00890964890766199, 0.000146997716834564, 0.000514030618515879, 
0.000143698566866247, 0.000414112490631873, 0.0122947883267809, 
0.000193519369827894, 0.000155930658676112, 8.06094634957661e-05, 
0.000345663010257987, 0.00929757149138735, 0.000152694802193697, 
0.000315406809601693, 0.000101821296927337, 0.000347191181049796, 
0.01018084946953, 0.000159471118881751)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10_rm12", 
"corn_dday10_30_rm12", "corn_dday30_rm12", "corn_prec_rm12", 
"corn_prec_sq_rm12", "cotton_dday0_10_rm12", "cotton_dday10_30_rm12", 
"cotton_dday30_rm12", "cotton_prec_rm12", "cotton_prec_sq_rm12", 
"hay_dday0_10_rm12", "hay_dday10_30_rm12", "hay_dday30_rm12", 
"hay_prec_rm12", "hay_prec_sq_rm12", "soybean_dday0_10_rm12", 
"soybean_dday10_30_rm12", "soybean_dday30_rm12", "soybean_prec_rm12", 
"soybean_prec_sq_rm12", "wheat_dday0_10_rm12", "wheat_dday10_30_rm12", 
"wheat_dday30_rm12", "wheat_prec_rm12", "wheat_prec_sq_rm12"), class = "data.frame")



saveRDS(thirty_mod, "models/sur_share_model_thirty.rds")

stopCluster(cl)
