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
# setwd("/home/johnw/Projects/adaptation-and-crop-choice/")

# Setup parallel for bootstrapping
# cl <- makeCluster(25)
# registerDoParallel(cl)

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


dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                dday0_10_rm_ten , dday10_30_rm_ten , dday30_rm_ten , prec_rm_ten , prec_sq_rm_ten, 
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- z_corn_a ~ dday0_10_rm_ten + dday10_30_rm_ten + dday30_rm_ten + prec_rm_ten + prec_sq_rm_ten +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~  dday0_10_rm_ten + dday10_30_rm_ten + dday30_rm_ten + prec_rm_ten + prec_sq_rm_ten +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
 

mod3 <- z_hay_a ~ dday0_10_rm_ten + dday10_30_rm_ten + dday30_rm_ten + prec_rm_ten + prec_sq_rm_ten +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- z_soybean_a ~ dday0_10_rm_ten + dday10_30_rm_ten + dday30_rm_ten + prec_rm_ten + prec_sq_rm_ten +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1



mod5 <- z_wheat_a ~ dday0_10_rm_ten + dday10_30_rm_ten + dday30_rm_ten + prec_rm_ten + prec_sq_rm_ten +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1

ten_mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

summary(ten_mod)
sum(ten_mod$coefficients)

ten_mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                    cotton.effect = cropdat_means$z_cotton_a,
                    hay.effect = cropdat_means$z_hay_a,
                    soybean.effect = cropdat_means$z_soybean_a,
                    wheat.effect = cropdat_means$z_wheat_a)

# Bootstrap standard errors
# bs_cropdat_dm <- cropdat_dm
# bs_cropdat_dm$ten <- cropdat$ten
# 
# se_dat <- data.frame()
# 
# d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
#   # Resample within interval
#   regdat <- bs_cropdat_dm %>%
#     group_by(ten) %>%
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
# tne_mod$bs.se <- as.data.frame(apply(d, 2, sd))

saveRDS(ten_mod, "models/sur_share_model_ten.rds")


#-----------------------------------------------------------------------------------
# twenty-year

dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                dday0_10_rm_twenty , dday10_30_rm_twenty , dday30_rm_twenty , prec_rm_twenty , prec_sq_rm_twenty, 
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- z_corn_a ~ dday0_10_rm_twenty + dday10_30_rm_twenty + dday30_rm_twenty + prec_rm_twenty + prec_sq_rm_twenty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~  dday0_10_rm_twenty + dday10_30_rm_twenty + dday30_rm_twenty + prec_rm_twenty + prec_sq_rm_twenty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
 

mod3 <- z_hay_a ~ dday0_10_rm_twenty + dday10_30_rm_twenty + dday30_rm_twenty + prec_rm_twenty + prec_sq_rm_twenty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- z_soybean_a ~ dday0_10_rm_twenty + dday10_30_rm_twenty + dday30_rm_twenty + prec_rm_twenty + prec_sq_rm_twenty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1



mod5 <- z_wheat_a ~ dday0_10_rm_twenty + dday10_30_rm_twenty + dday30_rm_twenty + prec_rm_twenty + prec_sq_rm_twenty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1

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
# bs_cropdat_dm$twenty <- cropdat$twenty
# 
# se_dat <- data.frame()
# 
# d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
#   # Resample within interval
#   regdat <- bs_cropdat_dm %>%
#     group_by(twenty) %>%
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
# twenty_mod$bs.se <- as.data.frame(apply(d, 2, sd))


saveRDS(twenty_mod, "models/sur_share_model_twenty.rds")



#-----------------------------------------------------------------------------------
# Thirty-year
dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                dday0_10_rm_thirty , dday10_30_rm_thirty , dday30_rm_thirty , prec_rm_thirty , prec_sq_rm_thirty, 
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- z_corn_a ~ dday0_10_rm_thirty + dday10_30_rm_thirty + dday30_rm_thirty + prec_rm_thirty + prec_sq_rm_thirty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- z_cotton_a ~  dday0_10_rm_thirty + dday10_30_rm_thirty + dday30_rm_thirty + prec_rm_thirty + prec_sq_rm_thirty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
 

mod3 <- z_hay_a ~ dday0_10_rm_thirty + dday10_30_rm_thirty + dday30_rm_thirty + prec_rm_thirty + prec_sq_rm_thirty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- z_soybean_a ~ dday0_10_rm_thirty + dday10_30_rm_thirty + dday30_rm_thirty + prec_rm_thirty + prec_sq_rm_thirty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1



mod5 <- z_wheat_a ~ dday0_10_rm_thirty + dday10_30_rm_thirty + dday30_rm_thirty + prec_rm_thirty + prec_sq_rm_thirty +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1

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
# bs_cropdat_dm$thirty <- cropdat$thirty
# 
# se_dat <- data.frame()
# 
# d <- foreach(i = 1:2000, .combine = rbind, .packages = c("dplyr", "systemfit")) %dopar% {
#   # Resample within interval
#   regdat <- bs_cropdat_dm %>%
#     group_by(thirty) %>%
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
# thirty_mod$bs.se <- as.data.frame(apply(d, 2, sd))


saveRDS(thirty_mod, "models/sur_share_model_thirty.rds")

stopCluster(cl)
