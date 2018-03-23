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

#-----------------------------------------------------------------------------------
dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                trend_lat, trend_long, trend_sq_lat, trend_sq_long)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)


mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- ln_rev_cotton ~    dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
 

mod3 <- ln_rev_hay ~   dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
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
mod$bs.se <- structure(list(`apply(d, 2, sd)` = c(6.21686620327522e-05, 3.09294962076834e-05, 
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


saveRDS(mod, "models/sur_rev_model.rds")


# stopCluster(cl)