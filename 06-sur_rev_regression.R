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

# setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")
# setwd("/home/johnw/")

# Setup parallel for bootstrapping
# cl <- makeCluster(14)
# registerDoParallel(cl)

# # Crop data
# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/full_ag_data.rds",
#               destfile = "data/full_ag_data.rds", method = "auto")


cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)

# Predictions from 10-sur_share_predictions.R
# cten <- readRDS("data/cten.rds")
# ctwenty <- readRDS("data/ctwenty.rds")
# cthirty <- readRDS("data/cthirty.rds")
# 
# cten <- filter(cten$predictions, temp == 0)
# ctwenty <- filter(ctwenty$predictions, temp == 0)
# cthirty <- filter(cthirty$predictions, temp == 0)


#---------------------------------------------------------------------------------------
# Instrument crop acreages and climate

dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                dday0_10_rm10 , dday10_30_rm10 , dday30_rm10 , prec_rm10 , prec_sq_rm10, 
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +  
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1


mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1



mod5 <- z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long  - 1

ten_mod <- systemfit(list(corn = mod1, 
                          cotton = mod2, 
                          hay = mod3, 
                          soybean = mod4,
                          wheat = mod5), data = cropdat_dm, method = "SUR")

summary(ten_mod)
pdat <- predict(ten_mod)

ten_mod$effects <- list(z_corn_a = cropdat_means$z_corn_a,
                        z_cotton_a = cropdat_means$z_cotton_a,
                        z_hay_a = cropdat_means$z_hay_a,
                        z_soybean_a = cropdat_means$z_soybean_a,
                        z_wheat_a = cropdat_means$z_wheat_a)

pdat$corn.pred <- pdat$corn.pred + ten_mod$effects$z_corn_a 
pdat$cotton.pred <- pdat$cotton.pred + ten_mod$effects$z_cotton_a
pdat$hay.pred <- pdat$hay.pred + ten_mod$effects$z_hay_a
pdat$soybean.pred <- pdat$soybean.pred + ten_mod$effects$z_soybean_a
pdat$wheat.pred <- pdat$wheat.pred + ten_mod$effects$z_wheat_a

pdat$corn.pred <- pnorm((pdat$corn.pred)*1.00101 - 0.001)
pdat$cotton.pred <- pnorm((pdat$cotton.pred)*1.00101 - 0.001)
pdat$hay.pred <- pnorm((pdat$hay.pred)*1.00101 - 0.001)
pdat$soybean.pred <- pnorm((pdat$soybean.pred)*1.00101 - 0.001)
pdat$wheat.pred <- pnorm((pdat$wheat.pred)*1.00101 - 0.001)

saveRDS(pdat, "models/acres_climate_iv.rds")

# 10-year revenue per acre SUR model
#-----------------------------------------------------------------------------------
dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

# Add predicted values
dmdat <- cbind(dmdat, pdat)


cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))


cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- ln_rev_corn ~ corn.pred + dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- ln_rev_cotton ~ cotton.pred + dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
 

mod3 <- ln_rev_hay ~ hay.pred + dday0_10 + dday10_30 + dday30 + prec + prec_sq +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- ln_rev_soybean ~ soybean.pred + dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod5 <- ln_rev_wheat ~ wheat.pred + dday0_10 + dday10_30 + dday30 + prec + prec_sq +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1

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

# Bootstrap standard errors
# bs_cropdat_dm <- cropdat_dm
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

# Results from bs parallel run
mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(6.21686620327522e-05, 3.09294962076834e-05, 
0.00015386340078004, 0.00233875977992693, 3.63037589084633e-05, 
4.04562215231435e-05, 2.43775931459578e-05, 0.000134358482338491, 
0.00199193811953618, 3.14259015478427e-05, 8.00367060093982e-05, 
3.97581563691865e-05, 0.000147157568118606, 0.00261789434302336, 
4.02996790173258e-05, 7.49666399755784e-05, 3.47828136271543e-05, 
0.000161462449923296, 0.00276043302774276, 4.30779725144458e-05, 
8.62344756938277e-05, 3.98075078314174e-05, 0.000157969668281536, 
0.00259016201082209, 3.93366212924019e-05)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10", 
"corn_dday10_30", "corn_dday30", "corn_prec", "corn_prec_sq", 
"cotton_dday0_10", "cotton_dday10_30", "cotton_dday30", "cotton_prec", 
"cotton_prec_sq", "hay_dday0_10", "hay_dday10_30", "hay_dday30", 
"hay_prec", "hay_prec_sq", "soybean_dday0_10", "soybean_dday10_30", 
"soybean_dday30", "soybean_prec", "soybean_prec_sq", "wheat_dday0_10", 
"wheat_dday10_30", "wheat_dday30", "wheat_prec", "wheat_prec_sq"
), class = "data.frame")


# Clustered s.e.
mod$cl_se <- clse_systemfit(mod, cropdat$state)

saveRDS(mod, "models/sur_rev_model.rds")


# stopCluster(cl)



#-------------------------------------------------------------------------------------------
# Build up regressions (No fe or trends)
# mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq 
# 
# 
# mod2 <- ln_rev_cotton ~    dday0_10 + dday10_30 + dday30 +  prec + prec_sq 
#  
# 
# mod3 <- ln_rev_hay ~   dday0_10 + dday10_30 + dday30 +  prec + prec_sq 
# 
# 
# mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq 
# 
# 
# mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq 
# 
# mod <- systemfit(list(corn = mod1, 
#                        cotton = mod2, 
#                        hay = mod3, 
#                        soybean = mod4,
#                        wheat = mod5), data = cropdat, method = "SUR")
# 
# summary(mod)
# sum(mod$coefficients)
# 
# mod$effects <- list(ln_corn.effect = cropdat_means$ln_rev_corn,
#                     ln_cotton.effect = cropdat_means$ln_rev_cotton,
#                     ln_hay.effect = cropdat_means$ln_rev_hay,
#                     ln_soybean.effect = cropdat_means$ln_rev_soybean,
#                     ln_wheat.effect = cropdat_means$ln_rev_wheat)
# 
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
# mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(0.098754679362128, 6.87986547405508e-05, 
# 2.60906341745628e-05, 0.000152831922322258, 0.0030487462644062, 
# 4.87023845859277e-05, 0.0869500729784675, 6.19487988623548e-05, 
# 2.61913104251386e-05, 0.000165452364793888, 0.00290461104562241, 
# 4.70310455447994e-05, 0.123981723430193, 8.74112577151539e-05, 
# 3.31372294768009e-05, 0.000191311402653008, 0.00354760101232905, 
# 5.5170539297928e-05, 0.14848202097551, 0.000101415617711999, 
# 3.76487010604183e-05, 0.000206098858645426, 0.00392639436546341, 
# 6.06157615696101e-05, 0.124781499485469, 8.65970945294311e-05, 
# 3.17328101960169e-05, 0.000168636435655749, 0.00333697443467231, 
# 5.22639410825415e-05)), .Names = "apply(d, 2, sd)", row.names = c("corn_(Intercept)", 
# "corn_dday0_10", "corn_dday10_30", "corn_dday30", "corn_prec", 
# "corn_prec_sq", "cotton_(Intercept)", "cotton_dday0_10", "cotton_dday10_30", 
# "cotton_dday30", "cotton_prec", "cotton_prec_sq", "hay_(Intercept)", 
# "hay_dday0_10", "hay_dday10_30", "hay_dday30", "hay_prec", "hay_prec_sq", 
# "soybean_(Intercept)", "soybean_dday0_10", "soybean_dday10_30", 
# "soybean_dday30", "soybean_prec", "soybean_prec_sq", "wheat_(Intercept)", 
# "wheat_dday0_10", "wheat_dday10_30", "wheat_dday30", "wheat_prec", 
# "wheat_prec_sq"), class = "data.frame")
# 
#   
# saveRDS(mod, "models/sur_rev_model1.rds")
# 
# 
# 
# #-------------------------------------------------------------------------------------------
# # Build up regressions (Fe no trends)
# 
# dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat, 
#                 dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
#                 trend_lat, trend_long, trend_sq_lat, trend_sq_long)
# 
# cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))
# 
# cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)
# 
# 
# mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq - 1 
# 
# 
# mod2 <- ln_rev_cotton ~    dday0_10 + dday10_30 + dday30 +  prec + prec_sq - 1
#  
# 
# mod3 <- ln_rev_hay ~   dday0_10 + dday10_30 + dday30 +  prec + prec_sq - 1
# 
# 
# mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq - 1
# 
# 
# mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq - 1
# 
# mod <- systemfit(list(corn = mod1, 
#                        cotton = mod2, 
#                        hay = mod3, 
#                        soybean = mod4,
#                        wheat = mod5), data = cropdat_dm, method = "SUR")
# 
# summary(mod)
# sum(mod$coefficients)
# 
# mod$effects <- list(ln_corn.effect = cropdat_means$ln_rev_corn,
#                     ln_cotton.effect = cropdat_means$ln_rev_cotton,
#                     ln_hay.effect = cropdat_means$ln_rev_hay,
#                     ln_soybean.effect = cropdat_means$ln_rev_soybean,
#                     ln_wheat.effect = cropdat_means$ln_rev_wheat)
# 
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
# mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(6.08896577861203e-05, 3.17664723051653e-05, 
# 0.000167911945369795, 0.0025428195885901, 3.95586330456138e-05, 
# 4.20847351912836e-05, 2.63091522547214e-05, 0.000144899107738895, 
# 0.00219831891744955, 3.49925168707657e-05, 8.68624356588107e-05, 
# 4.32477664023313e-05, 0.000162171496627272, 0.00281441724506906, 
# 4.23672173527379e-05, 8.08447813273105e-05, 3.66168120222692e-05, 
# 0.000170304771829901, 0.0029318679529337, 4.58101212029642e-05, 
# 9.9595555920073e-05, 4.76309133948677e-05, 0.000180538937725483, 
# 0.003287065430537, 5.14462032652725e-05)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10", 
# "corn_dday10_30", "corn_dday30", "corn_prec", "corn_prec_sq", 
# "cotton_dday0_10", "cotton_dday10_30", "cotton_dday30", "cotton_prec", 
# "cotton_prec_sq", "hay_dday0_10", "hay_dday10_30", "hay_dday30", 
# "hay_prec", "hay_prec_sq", "soybean_dday0_10", "soybean_dday10_30", 
# "soybean_dday30", "soybean_prec", "soybean_prec_sq", "wheat_dday0_10", 
# "wheat_dday10_30", "wheat_dday30", "wheat_prec", "wheat_prec_sq"
# ), class = "data.frame")
# 
#   
# saveRDS(mod, "models/sur_rev_model2.rds")
# 
# 
# 
# #-------------------------------------------------------------------------------------------
# # Build up regressions (Fe with linear trends)
# 
# dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat, 
#                 dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
#                 trend_lat, trend_long, trend_sq_lat, trend_sq_long)
# 
# cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))
# 
# cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)
# 
# 
# mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
#   trend_lat  + trend_long - 1 
# 
# 
# mod2 <- ln_rev_cotton ~    dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
#   trend_lat  + trend_long - 1
#  
# mod3 <- ln_rev_hay ~   dday0_10 + dday10_30 + dday30 +  prec + prec_sq + 
#   trend_lat  + trend_long - 1
# 
# 
# mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
#   trend_lat  + trend_long - 1
# 
# 
# mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#   trend_lat  + trend_long - 1
# 
# mod <- systemfit(list(corn = mod1, 
#                        cotton = mod2, 
#                        hay = mod3, 
#                        soybean = mod4,
#                        wheat = mod5), data = cropdat_dm, method = "SUR")
# 
# summary(mod)
# sum(mod$coefficients)
# 
# mod$effects <- list(ln_corn.effect = cropdat_means$ln_rev_corn,
#                     ln_cotton.effect = cropdat_means$ln_rev_cotton,
#                     ln_hay.effect = cropdat_means$ln_rev_hay,
#                     ln_soybean.effect = cropdat_means$ln_rev_soybean,
#                     ln_wheat.effect = cropdat_means$ln_rev_wheat)
# 
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
# mod$bs_se <- structure(list(`apply(d, 2, sd)` = c(6.24003247152985e-05, 3.19907571567475e-05, 
# 0.000163453914076656, 0.00253393095286869, 3.98416674259845e-05, 
# 4.17832883899501e-05, 2.48602736413876e-05, 0.000141463050013138, 
# 0.00205794364508551, 3.22916422011257e-05, 8.61571820146261e-05, 
# 4.33477241008873e-05, 0.000153364439620213, 0.00271480650423957, 
# 4.12433937270758e-05, 7.43724003883409e-05, 3.51977759179165e-05, 
# 0.000165948539096522, 0.00306004967847258, 4.82903215692476e-05, 
# 9.48068523922326e-05, 4.55773703467458e-05, 0.00019063293003371, 
# 0.00322880104287666, 5.01930749834195e-05)), .Names = "apply(d, 2, sd)", row.names = c("corn_dday0_10", 
# "corn_dday10_30", "corn_dday30", "corn_prec", "corn_prec_sq", 
# "cotton_dday0_10", "cotton_dday10_30", "cotton_dday30", "cotton_prec", 
# "cotton_prec_sq", "hay_dday0_10", "hay_dday10_30", "hay_dday30", 
# "hay_prec", "hay_prec_sq", "soybean_dday0_10", "soybean_dday10_30", 
# "soybean_dday30", "soybean_prec", "soybean_prec_sq", "wheat_dday0_10", 
# "wheat_dday10_30", "wheat_dday30", "wheat_prec", "wheat_prec_sq"
# ), class = "data.frame")
# 
#   
# saveRDS(mod, "models/sur_rev_model3.rds")
# 
# 
