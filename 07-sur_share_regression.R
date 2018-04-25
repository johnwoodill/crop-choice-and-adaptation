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

source("R/clse_systemfit.R")
source("R/predictSUR.R")

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
# ten_mod$bs_se <- as.data.frame(apply(d, 2, sd))


# From parallel run
ten_mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(0.000210053668428398, 0.000107530699104682, 
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


ten_mod$cl_se <- clse_systemfit(ten_mod, cropdat$state)

saveRDS(ten_mod, "models/sur_share_model_ten.rds")

# Get predictions as instrument for revenue per acre
# pdat <- predictSUR(ten_mod, newdata = cropdat_dm, fips = cropdat$fips, 
#                    var.terms = c("dday0_10_rm10", "dday10_30_rm10", "dday30_rm10", "prec_rm10", "prec_sq_rm10"))

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
# twenty_mod$bs_se <- as.data.frame(apply(d, 2, sd))


# From parallel run
twenty_mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(0.000214515010841551, 0.000111288235521924, 
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


twenty_mod$cl_se <- clse_systemfit(twenty_mod, cropdat$state)

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
# thirty_mod$bs_se <- as.data.frame(apply(d, 2, sd))

# From parallel run
thirty_mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(0.000224505455506903, 0.00011581865455546, 
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

thirty_mod$cl_se <- clse_systemfit(thirty_mod, cropdat$state)

saveRDS(thirty_mod, "models/sur_share_model_thirty.rds")

stopCluster(cl)

#-------------------------------------------------------------------------------------------
# Build up regressions (No fe or trends)
mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq


mod2 <- ln_rev_cotton ~      dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10


mod3 <- ln_rev_hay ~     dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10


mod4 <- ln_rev_soybean ~   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10


mod5 <- ln_rev_wheat ~  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10

mod <- systemfit(list(corn = mod1,
                       cotton = mod2,
                       hay = mod3,
                       soybean = mod4,
                       wheat = mod5), data = cropdat, method = "SUR")

summary(mod)
sum(mod$coefficients)

mod$effects <- list(ln_corn.effect = cropdat_means$ln_rev_corn,
                    ln_cotton.effect = cropdat_means$ln_rev_cotton,
                    ln_hay.effect = cropdat_means$ln_rev_hay,
                    ln_soybean.effect = cropdat_means$ln_rev_soybean,
                    ln_wheat.effect = cropdat_means$ln_rev_wheat)

# # Bootstrap standard errors
# # bs_cropdat <- cropdat
# # bs_cropdat$state <- cropdat$state
# # 
# # se_dat <- data.frame()
# # 
# # d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
# #   # Resample within interval
# #   regdat <- bs_cropdat %>%
# #     group_by(state) %>%
# #     sample_frac(1, replace = TRUE)
# # 
# #   bsmod <- systemfit(list(corn = mod1,
# #                        cotton = mod2,
# #                        hay = mod3,
# #                        soybean = mod4,
# #                        wheat = mod5), data = regdat, method = "SUR")
# # 
# #   mdat <- as.data.frame(t(bsmod$coefficients))
# #   names(mdat) <- names(bsmod$coefficients)
# #   mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
# #   mdat
# # }
# 
# # mod$bs.se <- as.data.frame(apply(d, 2, sd))
# 
# # Results from bs parallel run
mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(0.086668023204253, 5.99956578574945e-05,
2.34765963521534e-05, 0.00014309112648562, 0.0024199566484305, 
3.84614396221551e-05, 0.152797055121084, 0.000111165979133228, 
4.48111448037873e-05, 0.000272673713516631, 0.00582461565551548, 
9.83986513054354e-05, 0.174434572023923, 0.000127921168228789, 
5.10443246254796e-05, 0.000295945548216875, 0.00741517182820161, 
0.000127604113086435, 0.189859055275208, 0.000143751478006533, 
5.36394189230595e-05, 0.000320326636986507, 0.00783664307036346, 
0.000130470071115274, 0.171964433621587, 0.000126654536547728, 
4.66307387151972e-05, 0.000259521479152859, 0.00629047402567867, 
0.000106656375003973)), .Names = "apply(d, 2, sd)", row.names = c("corn_(Intercept)", 
"corn_dday0_10", "corn_dday10_30", "corn_dday30", "corn_prec", 
"corn_prec_sq", "cotton_(Intercept)", "cotton_dday0_10_rm10", 
"cotton_dday10_30_rm10", "cotton_dday30_rm10", "cotton_prec_rm10", 
"cotton_prec_sq_rm10", "hay_(Intercept)", "hay_dday0_10_rm10", 
"hay_dday10_30_rm10", "hay_dday30_rm10", "hay_prec_rm10", "hay_prec_sq_rm10", 
"soybean_(Intercept)", "soybean_dday0_10_rm10", "soybean_dday10_30_rm10", 
"soybean_dday30_rm10", "soybean_prec_rm10", "soybean_prec_sq_rm10", 
"wheat_(Intercept)", "wheat_dday0_10_rm10", "wheat_dday10_30_rm10", 
"wheat_dday30_rm10", "wheat_prec_rm10", "wheat_prec_sq_rm10"), class = "data.frame")
# 
#   
saveRDS(mod, "models/sur_share_ten_model1.rds")
# 
# 
# 
# #-------------------------------------------------------------------------------------------
# # Build up regressions (Fe no trends)
# 
dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat,
                dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10, trend, trend_sq,
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- ln_rev_corn ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  - 1


mod2 <- ln_rev_cotton ~   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  - 1


mod3 <- ln_rev_hay ~   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  - 1


mod4 <- ln_rev_soybean ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  - 1


mod5 <- ln_rev_wheat ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  - 1

mod <- systemfit(list(corn = mod1,
                       cotton = mod2,
                       hay = mod3,
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

summary(mod)
sum(mod$coefficients)

mod$effects <- list(ln_corn.effect = cropdat_means$ln_rev_corn,
                    ln_cotton.effect = cropdat_means$ln_rev_cotton,
                    ln_hay.effect = cropdat_means$ln_rev_hay,
                    ln_soybean.effect = cropdat_means$ln_rev_soybean,
                    ln_wheat.effect = cropdat_means$ln_rev_wheat)

# # Bootstrap standard errors
# # bs_cropdat_dm <- cropdat_dm
# # bs_cropdat_dm$state <- cropdat$state
# # 
# # se_dat <- data.frame()
# # 
# # d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
# #   # Resample within interval
# #   regdat <- bs_cropdat_dm %>%
# #     group_by(state) %>%
# #     sample_frac(1, replace = TRUE)
# # 
# #   bsmod <- systemfit(list(corn = mod1,
# #                        cotton = mod2,
# #                        hay = mod3,
# #                        soybean = mod4,
# #                        wheat = mod5), data = regdat, method = "SUR")
# # 
# #   mdat <- as.data.frame(t(bsmod$coefficients))
# #   names(mdat) <- names(bsmod$coefficients)
# #   mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
# #   mdat
# # }
# 
# # mod$bs.se <- as.data.frame(apply(d, 2, sd))
# 
# # Results from bs parallel run
mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(0.000230915566884734, 0.00010767330002322,
0.000416417162828298, 0.0103858155148637, 0.000169428490576251, 
0.000189028130347619, 8.77217464261196e-05, 0.000354976351161394, 
0.00946139558538302, 0.000153721952033332, 0.00021366400038945, 
0.000105489973300803, 0.000365643511259002, 0.0138459120563272, 
0.000213584460572255, 0.00049203827665667, 0.000126437699501528, 
0.000441863233613712, 0.0132367055387859, 0.000212841375728833, 
0.0002454550627407, 0.000123942855268034, 0.000461619000272569, 
0.014830493608128, 0.000234968077186706)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10_rm10", 
"corn_dday10_30_rm10", "corn_dday30_rm10", "corn_prec_rm10", 
"corn_prec_sq_rm10", "cotton_dday0_10_rm10", "cotton_dday10_30_rm10", 
"cotton_dday30_rm10", "cotton_prec_rm10", "cotton_prec_sq_rm10", 
"hay_dday0_10_rm10", "hay_dday10_30_rm10", "hay_dday30_rm10", 
"hay_prec_rm10", "hay_prec_sq_rm10", "soybean_dday0_10_rm10", 
"soybean_dday10_30_rm10", "soybean_dday30_rm10", "soybean_prec_rm10", 
"soybean_prec_sq_rm10", "wheat_dday0_10_rm10", "wheat_dday10_30_rm10", 
"wheat_dday30_rm10", "wheat_prec_rm10", "wheat_prec_sq_rm10"), class = "data.frame")
# 
#   
saveRDS(mod, "models/sur_share_ten_model2.rds")
# 
# 
# 
# #-------------------------------------------------------------------------------------------
# # Build up regressions (Fe with linear trends)
# 
dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat,
                dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10, trend, trend_sq,
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- ln_rev_corn ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  +
  trend_lat  + trend_long - 1


mod2 <- ln_rev_cotton ~    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  +
  trend_lat  + trend_long - 1

mod3 <- ln_rev_hay ~   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  +
  trend_lat  + trend_long - 1


mod4 <- ln_rev_soybean ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  +
  trend_lat  + trend_long - 1


mod5 <- ln_rev_wheat ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10  +
  trend_lat  + trend_long - 1

mod <- systemfit(list(corn = mod1,
                       cotton = mod2,
                       hay = mod3,
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

# summary(mod)
# sum(mod$coefficients)
# 
mod$effects <- list(ln_corn.effect = cropdat_means$ln_rev_corn,
                    ln_cotton.effect = cropdat_means$ln_rev_cotton,
                    ln_hay.effect = cropdat_means$ln_rev_hay,
                    ln_soybean.effect = cropdat_means$ln_rev_soybean,
                    ln_wheat.effect = cropdat_means$ln_rev_wheat)

# # Bootstrap standard errors
# # bs_cropdat_dm <- cropdat_dm
# # bs_cropdat_dm$state <- cropdat$state
# # 
# # se_dat <- data.frame()
# 
# # d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
# #   # Resample within interval
# #   regdat <- bs_cropdat_dm %>%
# #     group_by(state) %>%
# #     sample_frac(1, replace = TRUE)
# # 
# #   bsmod <- systemfit(list(corn = mod1,
# #                        cotton = mod2,
# #                        hay = mod3,
# #                        soybean = mod4,
# #                        wheat = mod5), data = regdat, method = "SUR")
# # 
# #   mdat <- as.data.frame(t(bsmod$coefficients))
# #   names(mdat) <- names(bsmod$coefficients)
# #   mdat <- select(mdat, -one_of(grep("trend", names(bsmod$coefficients), value = TRUE)))
# #   mdat
# # }
# 
# # mod$bs.se <- as.data.frame(apply(d, 2, sd))
# 
# # Results from bs parallel run
mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(0.000198548503793061, 0.000107655372002748,
0.000426858339263312, 0.0106263571753012, 0.00017194358996077, 
0.000137886035213758, 8.19861368750322e-05, 0.000371996051499081, 
0.00935243606097949, 0.000149735628298107, 0.000285676436117594, 
0.000118820560920806, 0.000385594946986713, 0.0135850336279963, 
0.000212634351837248, 0.000249503484142623, 0.000112747565280507, 
0.000467578809132548, 0.0123225159924996, 0.000200190848248454, 
0.000268734321823227, 0.000134273449737008, 0.000474278479831807, 
0.0138924675120672, 0.000220053684356792)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10_rm10", 
"corn_dday10_30_rm10", "corn_dday30_rm10", "corn_prec_rm10", 
"corn_prec_sq_rm10", "cotton_dday0_10_rm10", "cotton_dday10_30_rm10", 
"cotton_dday30_rm10", "cotton_prec_rm10", "cotton_prec_sq_rm10", 
"hay_dday0_10_rm10", "hay_dday10_30_rm10", "hay_dday30_rm10", 
"hay_prec_rm10", "hay_prec_sq_rm10", "soybean_dday0_10_rm10", 
"soybean_dday10_30_rm10", "soybean_dday30_rm10", "soybean_prec_rm10", 
"soybean_prec_sq_rm10", "wheat_dday0_10_rm10", "wheat_dday10_30_rm10", 
"wheat_dday30_rm10", "wheat_prec_rm10", "wheat_prec_sq_rm10"), class = "data.frame")
# 
#   
saveRDS(mod, "models/sur_share_ten_model3.rds")
# 
# 
