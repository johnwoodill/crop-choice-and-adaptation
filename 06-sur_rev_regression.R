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
# setwd("/home/johnw/")

# Setup parallel for bootstrapping
# cl <- makeCluster(20)
# registerDoParallel(cl)

# # Crop data
# download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1",
              # destfile = "data/full_ag_data.rds", method = "auto")

cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$ten <- factor(cropdat$ten)
cropdat$twenty <- factor(cropdat$twenty)
cropdat$thirty <- factor(cropdat$thirty)


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
