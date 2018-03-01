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

# res_terms <- c("corn_dday0_10_rm_ten  +   corn_dday10_30_rm_ten   + corn_dday30_rm_ten   +   corn_prec_rm_ten    +     corn_prec_sq_rm_ten    +  corn_trend_lat       +   corn_trend_long     +     corn_trend_sq_lat       + corn_trend_sq_long + cotton_dday0_10_rm_ten  + cotton_dday10_30_rm_ten  +cotton_dday30_rm_ten  +   cotton_prec_rm_ten    +   cotton_prec_sq_rm_ten  +  cotton_trend_lat     +  cotton_trend_long    +    cotton_trend_sq_lat    +  cotton_trend_sq_long  +   hay_dday0_10_rm_ten   +   hay_dday10_30_rm_ten  +   hay_dday30_rm_ten     +   hay_prec_rm_ten     +     hay_prec_sq_rm_ten    +   hay_trend_lat         +  hay_trend_long       +    hay_trend_sq_lat      +   hay_trend_sq_long       + soybean_dday0_10_rm_ten + soybean_dday10_30_rm_ten +soybean_dday30_rm_ten  +  soybean_prec_rm_ten   +   soybean_prec_sq_rm_ten +  soybean_trend_lat       + soybean_trend_long    +   soybean_trend_sq_lat   +  soybean_trend_sq_long   + wheat_dday0_10_rm_ten +   wheat_dday10_30_rm_ten +  wheat_dday30_rm_ten     + wheat_prec_rm_ten     +   wheat_prec_sq_rm_ten   +  wheat_trend_lat        +  wheat_trend_long      +   wheat_trend_sq_lat     +  wheat_trend_sq_long = 0")
# 
# res_terms <- c("corn_dday0_10_rm_ten + corn_dday10_30_rm_ten +  corn_dday30_rm_ten+  corn_prec_rm_ten    
#                +     corn_prec_sq_rm_ten   +  corn_trend_lat + corn_trend_long        +       corn_trend_sq    +      
#                cotton_dday0_10_rm_ten  + cotton_dday10_30_rm_ten + cotton_dday30_rm_ten  +   
#                cotton_prec_rm_ten    +   cotton_prec_sq_rm_ten +   cotton_trend        +     
#                cotton_trend_sq       +  hay_dday0_10_rm_ten   +   hay_dday10_30_rm_ten  +   
#                hay_dday30_rm_ten    +    hay_prec_rm_ten    +      hay_prec_sq_rm_ten   +   
#                hay_trend          +      hay_trend_sq       +   soybean_dday0_10_rm_ten + 
#                soybean_dday10_30_rm_ten + soybean_dday30_rm_ten   + soybean_prec_rm_ten   +  
#                soybean_prec_sq_rm_ten + soybean_trend       +    soybean_trend_sq     +   
#                wheat_dday0_10_rm_ten  +  wheat_dday10_30_rm_ten  + wheat_dday30_rm_ten   + 
#                wheat_prec_rm_ten     +   wheat_prec_sq_rm_ten +   wheat_trend          +    wheat_trend_sq = 0" )

# res_terms <- c("corn_dday0_10  +             corn_dday10_30     +         corn_dday30         +  corn_prec      +             corn_prec_sq              +  corn_dday0_10_rm_ten    + corn_dday10_30_rm_ten  +  corn_dday30_rm_ten     +  corn_prec_rm_ten      +  corn_prec_sq_rm_ten   +   corn_trend_lat            +  corn_trend_long          +  corn_trend_sq_lat      +     corn_trend_sq_long        +  cotton_dday0_10           + cotton_dday10_30      +      cotton_dday30             +  cotton_prec             +   cotton_prec_sq        +      cotton_dday0_10_rm_ten +  cotton_dday10_30_rm_ten +cotton_dday30_rm_ten +    cotton_prec_rm_ten     +  cotton_prec_sq_rm_ten   +cotton_trend_lat       +     cotton_trend_long         +  cotton_trend_sq_lat        +cotton_trend_sq_long   +     hay_dday0_10              +  hay_dday10_30              +hay_dday30             +     hay_prec                  +  hay_prec_sq               + hay_dday0_10_rm_ten  +    hay_dday10_30_rm_ten   +  hay_dday30_rm_ten      + hay_prec_rm_ten      +    hay_prec_sq_rm_ten     +  hay_trend_lat             + hay_trend_long          +    hay_trend_sq_lat          +  hay_trend_sq_long         + soybean_dday0_10        +    soybean_dday10_30         +  soybean_dday30            + soybean_prec             +   soybean_prec_sq           +  soybean_dday0_10_rm_ten +soybean_dday10_30_rm_ten +soybean_dday30_rm_ten  +  soybean_prec_rm_ten    + soybean_prec_sq_rm_ten   +soybean_trend_lat         +  soybean_trend_long        + soybean_trend_sq_lat        +soybean_trend_sq_long    +   wheat_dday0_10            + wheat_dday10_30             +wheat_dday30              +  wheat_prec                + wheat_prec_sq               +wheat_dday0_10_rm_ten  +  wheat_dday10_30_rm_ten + wheat_dday30_rm_ten      +wheat_prec_rm_ten      +  wheat_prec_sq_rm_ten   + wheat_trend_lat             +wheat_trend_long          +  wheat_trend_sq_lat      +   wheat_trend_sq_long = 0") 

ten_mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

sum(ten_mod$coefficients) 


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

# "corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq + cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq + hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq + soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq + wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq

# res_terms <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq + corn_dday0_10_rm_twenty + corn_dday10_30_rm_twenty +  corn_dday30_rm_twenty+  corn_prec_rm_twenty    +     corn_prec_sq_rm_twenty   +  corn_trend_lat + corn_trend_long       +       corn_trend_sq    +  cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq +   cotton_dday0_10_rm_twenty  + cotton_dday10_30_rm_twenty + cotton_dday30_rm_twenty  +   cotton_prec_rm_twenty    +   cotton_prec_sq_rm_twenty +   cotton_trend        +     cotton_trend_sq + hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq      +  hay_dday0_10_rm_twenty   +   hay_dday10_30_rm_twenty  +   hay_dday30_rm_twenty    +    hay_prec_rm_twenty    +      hay_prec_sq_rm_twenty   +    hay_trend          +      hay_trend_sq   + soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq    +   soybean_dday0_10_rm_twenty + soybean_dday10_30_rm_twenty + soybean_dday30_rm_twenty   + soybean_prec_rm_twenty   +   soybean_prec_sq_rm_twenty + soybean_trend       +    soybean_trend_sq  +  wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq   +    wheat_dday0_10_rm_twenty  +  wheat_dday10_30_rm_twenty  + wheat_dday30_rm_twenty   +   wheat_prec_rm_twenty     +   wheat_prec_sq_rm_twenty +   wheat_trend          +    wheat_trend_sq = 0" )

# res_terms <- c(" corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq + corn_dday0_10_rm_twenty  +   corn_dday10_30_rm_twenty   + corn_dday30_rm_twenty   +   corn_prec_rm_twenty    +     corn_prec_sq_rm_twenty    +  corn_trend_lat       +   corn_trend_long     +     corn_trend_sq_lat       + corn_trend_sq_long   + cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq +  cotton_dday0_10_rm_twenty  + cotton_dday10_30_rm_twenty  +cotton_dday30_rm_twenty  +   cotton_prec_rm_twenty    +   cotton_prec_sq_rm_twenty  +  cotton_trend_lat     +  cotton_trend_long    +    cotton_trend_sq_lat    +  cotton_trend_sq_long  + hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq +  hay_dday0_10_rm_twenty   +   hay_dday10_30_rm_twenty  +   hay_dday30_rm_twenty     +   hay_prec_rm_twenty     +     hay_prec_sq_rm_twenty    +   hay_trend_lat         +  hay_trend_long       +    hay_trend_sq_lat      +   hay_trend_sq_long   + soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq    + soybean_dday0_10_rm_twenty + soybean_dday10_30_rm_twenty +soybean_dday30_rm_twenty  +  soybean_prec_rm_twenty   +   soybean_prec_sq_rm_twenty +  soybean_trend_lat       + soybean_trend_long    +   soybean_trend_sq_lat   +  soybean_trend_sq_long  + wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq + wheat_dday0_10_rm_twenty +   wheat_dday10_30_rm_twenty +  wheat_dday30_rm_twenty     + wheat_prec_rm_twenty     +   wheat_prec_sq_rm_twenty   +  wheat_trend_lat        +  wheat_trend_long      +   wheat_trend_sq_lat     +  wheat_trend_sq_long = 0")
# 
# # with weather terms
# res_terms <- c("corn_dday0_10  +             corn_dday10_30     +         corn_dday30         +       
# corn_prec      +             corn_prec_sq              +  corn_dday0_10_rm_twenty    +
# corn_dday10_30_rm_twenty  +  corn_dday30_rm_twenty     +  corn_prec_rm_twenty      +  
# corn_prec_sq_rm_twenty   +   corn_trend_lat            +  corn_trend_long          +  
# corn_trend_sq_lat      +     corn_trend_sq_long        +  cotton_dday0_10           + 
# cotton_dday10_30      +      cotton_dday30             +  cotton_prec             +   
# cotton_prec_sq        +      cotton_dday0_10_rm_twenty +  cotton_dday10_30_rm_twenty +
# cotton_dday30_rm_twenty +    cotton_prec_rm_twenty     +  cotton_prec_sq_rm_twenty   +
# cotton_trend_lat       +     cotton_trend_long         +  cotton_trend_sq_lat        +
# cotton_trend_sq_long   +     hay_dday0_10              +  hay_dday10_30              +
# hay_dday30             +     hay_prec                  +  hay_prec_sq               + 
# hay_dday0_10_rm_twenty  +    hay_dday10_30_rm_twenty   +  hay_dday30_rm_twenty      + 
# hay_prec_rm_twenty      +    hay_prec_sq_rm_twenty     +  hay_trend_lat             + 
# hay_trend_long          +    hay_trend_sq_lat          +  hay_trend_sq_long         + 
# soybean_dday0_10        +    soybean_dday10_30         +  soybean_dday30            + 
# soybean_prec             +   soybean_prec_sq           +  soybean_dday0_10_rm_twenty +
# soybean_dday10_30_rm_twenty +soybean_dday30_rm_twenty  +  soybean_prec_rm_twenty    + 
# soybean_prec_sq_rm_twenty   +soybean_trend_lat         +  soybean_trend_long        + 
# soybean_trend_sq_lat        +soybean_trend_sq_long    +   wheat_dday0_10            + 
# wheat_dday10_30             +wheat_dday30              +  wheat_prec                + 
# wheat_prec_sq               +wheat_dday0_10_rm_twenty  +  wheat_dday10_30_rm_twenty + 
# wheat_dday30_rm_twenty      +wheat_prec_rm_twenty      +  wheat_prec_sq_rm_twenty   + 
# wheat_trend_lat             +wheat_trend_long          +  wheat_trend_sq_lat      +   
# wheat_trend_sq_long = 0") 


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

# res_terms <- c("corn_dday0_10_rm_thirty + corn_dday10_30_rm_thirty +  corn_dday30_rm_thirty+  corn_prec_rm_thirty    +     corn_prec_sq_rm_thirty   +  corn_trend_lat + corn_trend_long       +       corn_trend_sq    +      cotton_dday0_10_rm_thirty  + cotton_dday10_30_rm_thirty + cotton_dday30_rm_thirty  +   cotton_prec_rm_thirty    +   cotton_prec_sq_rm_thirty +   cotton_trend        +     cotton_trend_sq       +  hay_dday0_10_rm_thirty   +   hay_dday10_30_rm_thirty  +   hay_dday30_rm_thirty    +    hay_prec_rm_thirty    +      hay_prec_sq_rm_thirty   +    hay_trend          +      hay_trend_sq       +   soybean_dday0_10_rm_thirty + soybean_dday10_30_rm_thirty + soybean_dday30_rm_thirty   + soybean_prec_rm_thirty   +   soybean_prec_sq_rm_thirty + soybean_trend       +    soybean_trend_sq     +    wheat_dday0_10_rm_thirty  +  wheat_dday10_30_rm_thirty  + wheat_dday30_rm_thirty   +   wheat_prec_rm_thirty     +   wheat_prec_sq_rm_thirty +   wheat_trend          +    wheat_trend_sq = 0" )
# 
# res_terms <- c("corn_dday0_10_rm_thirty  +   corn_dday10_30_rm_thirty   + corn_dday30_rm_thirty   +   corn_prec_rm_thirty    +     corn_prec_sq_rm_thirty    +  corn_trend_lat       +   corn_trend_long     +     corn_trend_sq_lat       + corn_trend_sq_long   +   cotton_dday0_10_rm_thirty  + cotton_dday10_30_rm_thirty  +cotton_dday30_rm_thirty  +   cotton_prec_rm_thirty    +   cotton_prec_sq_rm_thirty  +  cotton_trend_lat     +  cotton_trend_long    +    cotton_trend_sq_lat    +  cotton_trend_sq_long  +   hay_dday0_10_rm_thirty   +   hay_dday10_30_rm_thirty  +   hay_dday30_rm_thirty     +   hay_prec_rm_thirty     +     hay_prec_sq_rm_thirty    +   hay_trend_lat         +  hay_trend_long       +    hay_trend_sq_lat      +   hay_trend_sq_long       + soybean_dday0_10_rm_thirty + soybean_dday10_30_rm_thirty +soybean_dday30_rm_thirty  +  soybean_prec_rm_thirty   +   soybean_prec_sq_rm_thirty +  soybean_trend_lat       + soybean_trend_long    +   soybean_trend_sq_lat   +  soybean_trend_sq_long   + wheat_dday0_10_rm_thirty +   wheat_dday10_30_rm_thirty +  wheat_dday30_rm_thirty     + wheat_prec_rm_thirty     +   wheat_prec_sq_rm_thirty   +  wheat_trend_lat        +  wheat_trend_long      +   wheat_trend_sq_lat     +  wheat_trend_sq_long = 0")
# 
# res_terms <- c("corn_dday0_10  +             corn_dday10_30     +         corn_dday30         +       
# corn_prec      +             corn_prec_sq              +  corn_dday0_10_rm_thirty    +
# corn_dday10_30_rm_thirty  +  corn_dday30_rm_thirty     +  corn_prec_rm_thirty      +  
# corn_prec_sq_rm_thirty   +   corn_trend_lat            +  corn_trend_long          +  
# corn_trend_sq_lat      +     corn_trend_sq_long        +  cotton_dday0_10           + 
# cotton_dday10_30      +      cotton_dday30             +  cotton_prec             +   
# cotton_prec_sq        +      cotton_dday0_10_rm_thirty +  cotton_dday10_30_rm_thirty +
# cotton_dday30_rm_thirty +    cotton_prec_rm_thirty     +  cotton_prec_sq_rm_thirty   +
# cotton_trend_lat       +     cotton_trend_long         +  cotton_trend_sq_lat        +
# cotton_trend_sq_long   +     hay_dday0_10              +  hay_dday10_30              +
# hay_dday30             +     hay_prec                  +  hay_prec_sq               + 
# hay_dday0_10_rm_thirty  +    hay_dday10_30_rm_thirty   +  hay_dday30_rm_thirty      + 
# hay_prec_rm_thirty      +    hay_prec_sq_rm_thirty     +  hay_trend_lat             + 
# hay_trend_long          +    hay_trend_sq_lat          +  hay_trend_sq_long         + 
# soybean_dday0_10        +    soybean_dday10_30         +  soybean_dday30            + 
# soybean_prec             +   soybean_prec_sq           +  soybean_dday0_10_rm_thirty +
# soybean_dday10_30_rm_thirty +soybean_dday30_rm_thirty  +  soybean_prec_rm_thirty    + 
# soybean_prec_sq_rm_thirty   +soybean_trend_lat         +  soybean_trend_long        + 
# soybean_trend_sq_lat        +soybean_trend_sq_long    +   wheat_dday0_10            + 
# wheat_dday10_30             +wheat_dday30              +  wheat_prec                + 
# wheat_prec_sq               +wheat_dday0_10_rm_thirty  +  wheat_dday10_30_rm_thirty + 
# wheat_dday30_rm_thirty      +wheat_prec_rm_thirty      +  wheat_prec_sq_rm_thirty   + 
# wheat_trend_lat             +wheat_trend_long          +  wheat_trend_sq_lat      +   
# wheat_trend_sq_long = 0") 

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
