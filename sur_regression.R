library(systemfit)
library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")


# Crop data
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- as.data.frame(cropdat)
cropdat$fips <- factor(cropdat$fips)
cropdat$state <- factor(cropdat$state)
cropdat$five <- factor(cropdat$five)
cropdat$ten <- factor(cropdat$ten)
cropdat$twenty <- factor(cropdat$twenty)
cropdat$thirty <- factor(cropdat$thirty)


# five-year

testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10 , dday10_30 , dday30 , prec , prec_sq , 
                dday0_10_five , dday10_30_five , dday30_five , prec_five , prec_sq_five, 
                trend2_al ,trend2_ar , trend2_de ,trend2_ga , trend2_ia  ,         
                trend2_il ,trend2_in , trend2_ks , trend2_ky , trend2_md , trend2_mi ,         
                trend2_mn, trend2_mo , trend2_ms ,  trend2_mt , trend2_nc , trend2_nd ,         
                trend2_ne ,trend2_oh , trend2_ok ,  trend2_sc , trend2_sd , trend2_tn ,         
                trend2_va , trend2_wi)

cropdat_dm <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                            five = factor(cropdat$five)))

cropdat_means <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                              five = factor(cropdat$five)), means = TRUE)

mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1
 

mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod5 <- z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq +
               corn_dday0_10_five + corn_dday10_30_five + corn_dday30_five + corn_prec_five + corn_prec_sq_five +
              corn_trend2_al + corn_trend2_ar + corn_trend2_de + corn_trend2_ga + corn_trend2_ia  +  
              corn_trend2_il +corn_trend2_in + corn_trend2_ks + corn_trend2_ky + corn_trend2_md + corn_trend2_mi +         
                corn_trend2_mn+ corn_trend2_mo + corn_trend2_ms +  corn_trend2_mt + corn_trend2_nc + corn_trend2_nd +         
                corn_trend2_ne +corn_trend2_oh + corn_trend2_ok +  corn_trend2_sc + corn_trend2_sd + corn_trend2_tn +         
                corn_trend2_va + corn_trend2_wi +
               
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq +
               cotton_dday0_10_five + cotton_dday10_30_five + cotton_dday30_five + cotton_prec_five + cotton_prec_sq_five +
cotton_trend2_al + cotton_trend2_ar + cotton_trend2_de + cotton_trend2_ga + cotton_trend2_ia  +                  
cotton_trend2_il +cotton_trend2_in + cotton_trend2_ks + cotton_trend2_ky + cotton_trend2_md + cotton_trend2_mi +         
                cotton_trend2_mn+ cotton_trend2_mo + cotton_trend2_ms +  cotton_trend2_mt + cotton_trend2_nc + cotton_trend2_nd +         
                cotton_trend2_ne +cotton_trend2_oh + cotton_trend2_ok +  cotton_trend2_sc + cotton_trend2_sd + cotton_trend2_tn +         
                cotton_trend2_va + cotton_trend2_wi +

               hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq +
               hay_dday0_10_five + hay_dday10_30_five + hay_dday30_five + hay_prec_five + hay_prec_sq_five +
hay_trend2_al + hay_trend2_ar + hay_trend2_de + hay_trend2_ga + hay_trend2_ia  +                 
 hay_trend2_il +hay_trend2_in + hay_trend2_ks + hay_trend2_ky + hay_trend2_md + hay_trend2_mi +         
                hay_trend2_mn+ hay_trend2_mo + hay_trend2_ms +  hay_trend2_mt + hay_trend2_nc + hay_trend2_nd +         
                hay_trend2_ne +hay_trend2_oh + hay_trend2_ok +  hay_trend2_sc + hay_trend2_sd + hay_trend2_tn +         
                hay_trend2_va + hay_trend2_wi +
 
               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq +
               soybean_dday0_10_five + soybean_dday10_30_five + soybean_dday30_five + soybean_prec_five + soybean_prec_sq_five +
soybean_trend2_al + soybean_trend2_ar + soybean_trend2_de + soybean_trend2_ga + soybean_trend2_ia  +                  
soybean_trend2_il +soybean_trend2_in + soybean_trend2_ks + soybean_trend2_ky + soybean_trend2_md + soybean_trend2_mi +         
                soybean_trend2_mn+ soybean_trend2_mo + soybean_trend2_ms +  soybean_trend2_mt + soybean_trend2_nc + soybean_trend2_nd +         
                soybean_trend2_ne +soybean_trend2_oh + soybean_trend2_ok +  soybean_trend2_sc + soybean_trend2_sd + soybean_trend2_tn +         
                soybean_trend2_va + soybean_trend2_wi +

               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq +
               wheat_dday0_10_five + wheat_dday10_30_five + wheat_dday30_five + wheat_prec_five + wheat_prec_sq_five +
wheat_trend2_al + wheat_trend2_ar + wheat_trend2_de + wheat_trend2_ga + wheat_trend2_ia  +  
                wheat_trend2_il +wheat_trend2_in + wheat_trend2_ks + wheat_trend2_ky + wheat_trend2_md + wheat_trend2_mi +         
                wheat_trend2_mn+ wheat_trend2_mo + wheat_trend2_ms +  wheat_trend2_mt + wheat_trend2_nc + wheat_trend2_nd +         
                wheat_trend2_ne +wheat_trend2_oh + wheat_trend2_ok +  wheat_trend2_sc + wheat_trend2_sd + wheat_trend2_tn +         
                wheat_trend2_va + wheat_trend2_wi = 0")

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


saveRDS(mod, "models/sur_model_five.rds")



# ten-year


testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10 , dday10_30 , dday30 , prec , prec_sq , 
                dday0_10_ten , dday10_30_ten , dday30_ten , prec_ten , prec_sq_ten, 
                trend2_al ,trend2_ar , trend2_de ,trend2_ga , trend2_ia  ,         
                trend2_il ,trend2_in , trend2_ks , trend2_ky , trend2_md , trend2_mi ,         
                trend2_mn, trend2_mo , trend2_ms ,  trend2_mt , trend2_nc , trend2_nd ,         
                trend2_ne ,trend2_oh , trend2_ok ,  trend2_sc , trend2_sd , trend2_tn ,         
                trend2_va , trend2_wi)

cropdat_dm <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      ten = factor(cropdat$ten)))

cropdat_means <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      ten = factor(cropdat$ten)), means = TRUE)


mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1
 

mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod5 <- z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq +
               corn_dday0_10_ten + corn_dday10_30_ten + corn_dday30_ten + corn_prec_ten + corn_prec_sq_ten +
              corn_trend2_al + corn_trend2_ar + corn_trend2_de + corn_trend2_ga + corn_trend2_ia  +  
              corn_trend2_il +corn_trend2_in + corn_trend2_ks + corn_trend2_ky + corn_trend2_md + corn_trend2_mi +         
                corn_trend2_mn+ corn_trend2_mo + corn_trend2_ms +  corn_trend2_mt + corn_trend2_nc + corn_trend2_nd +         
                corn_trend2_ne +corn_trend2_oh + corn_trend2_ok +  corn_trend2_sc + corn_trend2_sd + corn_trend2_tn +         
                corn_trend2_va + corn_trend2_wi +
               
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq +
               cotton_dday0_10_ten + cotton_dday10_30_ten + cotton_dday30_ten + cotton_prec_ten + cotton_prec_sq_ten +
cotton_trend2_al + cotton_trend2_ar + cotton_trend2_de + cotton_trend2_ga + cotton_trend2_ia  +                  
cotton_trend2_il +cotton_trend2_in + cotton_trend2_ks + cotton_trend2_ky + cotton_trend2_md + cotton_trend2_mi +         
                cotton_trend2_mn+ cotton_trend2_mo + cotton_trend2_ms +  cotton_trend2_mt + cotton_trend2_nc + cotton_trend2_nd +         
                cotton_trend2_ne +cotton_trend2_oh + cotton_trend2_ok +  cotton_trend2_sc + cotton_trend2_sd + cotton_trend2_tn +         
                cotton_trend2_va + cotton_trend2_wi +

               hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq +
               hay_dday0_10_ten + hay_dday10_30_ten + hay_dday30_ten + hay_prec_ten + hay_prec_sq_ten +
hay_trend2_al + hay_trend2_ar + hay_trend2_de + hay_trend2_ga + hay_trend2_ia  +                 
 hay_trend2_il +hay_trend2_in + hay_trend2_ks + hay_trend2_ky + hay_trend2_md + hay_trend2_mi +         
                hay_trend2_mn+ hay_trend2_mo + hay_trend2_ms +  hay_trend2_mt + hay_trend2_nc + hay_trend2_nd +         
                hay_trend2_ne +hay_trend2_oh + hay_trend2_ok +  hay_trend2_sc + hay_trend2_sd + hay_trend2_tn +         
                hay_trend2_va + hay_trend2_wi +
 
               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq +
               soybean_dday0_10_ten + soybean_dday10_30_ten + soybean_dday30_ten + soybean_prec_ten + soybean_prec_sq_ten +
soybean_trend2_al + soybean_trend2_ar + soybean_trend2_de + soybean_trend2_ga + soybean_trend2_ia  +                  
soybean_trend2_il +soybean_trend2_in + soybean_trend2_ks + soybean_trend2_ky + soybean_trend2_md + soybean_trend2_mi +         
                soybean_trend2_mn+ soybean_trend2_mo + soybean_trend2_ms +  soybean_trend2_mt + soybean_trend2_nc + soybean_trend2_nd +         
                soybean_trend2_ne +soybean_trend2_oh + soybean_trend2_ok +  soybean_trend2_sc + soybean_trend2_sd + soybean_trend2_tn +         
                soybean_trend2_va + soybean_trend2_wi +

               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq +
               wheat_dday0_10_ten + wheat_dday10_30_ten + wheat_dday30_ten + wheat_prec_ten + wheat_prec_sq_ten +
wheat_trend2_al + wheat_trend2_ar + wheat_trend2_de + wheat_trend2_ga + wheat_trend2_ia  +  
                wheat_trend2_il +wheat_trend2_in + wheat_trend2_ks + wheat_trend2_ky + wheat_trend2_md + wheat_trend2_mi +         
                wheat_trend2_mn+ wheat_trend2_mo + wheat_trend2_ms +  wheat_trend2_mt + wheat_trend2_nc + wheat_trend2_nd +         
                wheat_trend2_ne +wheat_trend2_oh + wheat_trend2_ok +  wheat_trend2_sc + wheat_trend2_sd + wheat_trend2_tn +         
                wheat_trend2_va + wheat_trend2_wi = 0")

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


saveRDS(mod, "models/sur_model_ten.rds")





# twenty-year


testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10 , dday10_30 , dday30 , prec , prec_sq , 
                dday0_10_twenty , dday10_30_twenty , dday30_twenty , prec_twenty , prec_sq_twenty, 
                trend2_al ,trend2_ar , trend2_de ,trend2_ga , trend2_ia  ,         
                trend2_il ,trend2_in , trend2_ks , trend2_ky , trend2_md , trend2_mi ,         
                trend2_mn, trend2_mo , trend2_ms ,  trend2_mt , trend2_nc , trend2_nd ,         
                trend2_ne ,trend2_oh , trend2_ok ,  trend2_sc , trend2_sd , trend2_tn ,         
                trend2_va , trend2_wi)

cropdat_dm <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      twenty = factor(cropdat$twenty)))

cropdat_means <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      twenty = factor(cropdat$twenty)), means = TRUE)



mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1
 

mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod5 <- z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq +
               corn_dday0_10_twenty + corn_dday10_30_twenty + corn_dday30_twenty + corn_prec_twenty + corn_prec_sq_twenty +
              corn_trend2_al + corn_trend2_ar + corn_trend2_de + corn_trend2_ga + corn_trend2_ia  +  
              corn_trend2_il +corn_trend2_in + corn_trend2_ks + corn_trend2_ky + corn_trend2_md + corn_trend2_mi +         
                corn_trend2_mn+ corn_trend2_mo + corn_trend2_ms +  corn_trend2_mt + corn_trend2_nc + corn_trend2_nd +         
                corn_trend2_ne +corn_trend2_oh + corn_trend2_ok +  corn_trend2_sc + corn_trend2_sd + corn_trend2_tn +         
                corn_trend2_va + corn_trend2_wi +
               
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq +
               cotton_dday0_10_twenty + cotton_dday10_30_twenty + cotton_dday30_twenty + cotton_prec_twenty + cotton_prec_sq_twenty +
cotton_trend2_al + cotton_trend2_ar + cotton_trend2_de + cotton_trend2_ga + cotton_trend2_ia  +                  
cotton_trend2_il +cotton_trend2_in + cotton_trend2_ks + cotton_trend2_ky + cotton_trend2_md + cotton_trend2_mi +         
                cotton_trend2_mn+ cotton_trend2_mo + cotton_trend2_ms +  cotton_trend2_mt + cotton_trend2_nc + cotton_trend2_nd +         
                cotton_trend2_ne +cotton_trend2_oh + cotton_trend2_ok +  cotton_trend2_sc + cotton_trend2_sd + cotton_trend2_tn +         
                cotton_trend2_va + cotton_trend2_wi +

               hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq +
               hay_dday0_10_twenty + hay_dday10_30_twenty + hay_dday30_twenty + hay_prec_twenty + hay_prec_sq_twenty +
hay_trend2_al + hay_trend2_ar + hay_trend2_de + hay_trend2_ga + hay_trend2_ia  +                 
 hay_trend2_il +hay_trend2_in + hay_trend2_ks + hay_trend2_ky + hay_trend2_md + hay_trend2_mi +         
                hay_trend2_mn+ hay_trend2_mo + hay_trend2_ms +  hay_trend2_mt + hay_trend2_nc + hay_trend2_nd +         
                hay_trend2_ne +hay_trend2_oh + hay_trend2_ok +  hay_trend2_sc + hay_trend2_sd + hay_trend2_tn +         
                hay_trend2_va + hay_trend2_wi +
 
               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq +
               soybean_dday0_10_twenty + soybean_dday10_30_twenty + soybean_dday30_twenty + soybean_prec_twenty + soybean_prec_sq_twenty +
soybean_trend2_al + soybean_trend2_ar + soybean_trend2_de + soybean_trend2_ga + soybean_trend2_ia  +                  
soybean_trend2_il +soybean_trend2_in + soybean_trend2_ks + soybean_trend2_ky + soybean_trend2_md + soybean_trend2_mi +         
                soybean_trend2_mn+ soybean_trend2_mo + soybean_trend2_ms +  soybean_trend2_mt + soybean_trend2_nc + soybean_trend2_nd +         
                soybean_trend2_ne +soybean_trend2_oh + soybean_trend2_ok +  soybean_trend2_sc + soybean_trend2_sd + soybean_trend2_tn +         
                soybean_trend2_va + soybean_trend2_wi +

               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq +
               wheat_dday0_10_twenty + wheat_dday10_30_twenty + wheat_dday30_twenty + wheat_prec_twenty + wheat_prec_sq_twenty +
wheat_trend2_al + wheat_trend2_ar + wheat_trend2_de + wheat_trend2_ga + wheat_trend2_ia  +  
                wheat_trend2_il +wheat_trend2_in + wheat_trend2_ks + wheat_trend2_ky + wheat_trend2_md + wheat_trend2_mi +         
                wheat_trend2_mn+ wheat_trend2_mo + wheat_trend2_ms +  wheat_trend2_mt + wheat_trend2_nc + wheat_trend2_nd +         
                wheat_trend2_ne +wheat_trend2_oh + wheat_trend2_ok +  wheat_trend2_sc + wheat_trend2_sd + wheat_trend2_tn +         
                wheat_trend2_va + wheat_trend2_wi = 0")

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


saveRDS(mod, "models/sur_model_twenty.rds")




# Thirty-year

testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10 , dday10_30 , dday30 , prec , prec_sq , 
                dday0_10_thirty , dday10_30_thirty , dday30_thirty , prec_thirty , prec_sq_thirty, 
                trend2_al ,trend2_ar , trend2_de ,trend2_ga , trend2_ia  ,         
                trend2_il ,trend2_in , trend2_ks , trend2_ky , trend2_md , trend2_mi ,         
                trend2_mn, trend2_mo , trend2_ms ,  trend2_mt , trend2_nc , trend2_nd ,         
                trend2_ne ,trend2_oh , trend2_ok ,  trend2_sc , trend2_sd , trend2_tn ,         
                trend2_va , trend2_wi)

cropdat_dm <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      thirty = factor(cropdat$thirty)))

cropdat_means <- demeanlist(testdat, fl = list(fips = factor(cropdat$fips),
                                      thirty = factor(cropdat$thirty)), means = TRUE)



mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1
 

mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi- 1


mod5 <- z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
                trend2_va + trend2_wi - 1


restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq +
               corn_dday0_10_thirty + corn_dday10_30_thirty + corn_dday30_thirty + corn_prec_thirty + corn_prec_sq_thirty +
              corn_trend2_al + corn_trend2_ar + corn_trend2_de + corn_trend2_ga + corn_trend2_ia  +  
              corn_trend2_il +corn_trend2_in + corn_trend2_ks + corn_trend2_ky + corn_trend2_md + corn_trend2_mi +         
                corn_trend2_mn+ corn_trend2_mo + corn_trend2_ms +  corn_trend2_mt + corn_trend2_nc + corn_trend2_nd +         
                corn_trend2_ne +corn_trend2_oh + corn_trend2_ok +  corn_trend2_sc + corn_trend2_sd + corn_trend2_tn +         
                corn_trend2_va + corn_trend2_wi +
               
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq +
               cotton_dday0_10_thirty + cotton_dday10_30_thirty + cotton_dday30_thirty + cotton_prec_thirty + cotton_prec_sq_thirty +
cotton_trend2_al + cotton_trend2_ar + cotton_trend2_de + cotton_trend2_ga + cotton_trend2_ia  +                  
cotton_trend2_il +cotton_trend2_in + cotton_trend2_ks + cotton_trend2_ky + cotton_trend2_md + cotton_trend2_mi +         
                cotton_trend2_mn+ cotton_trend2_mo + cotton_trend2_ms +  cotton_trend2_mt + cotton_trend2_nc + cotton_trend2_nd +         
                cotton_trend2_ne +cotton_trend2_oh + cotton_trend2_ok +  cotton_trend2_sc + cotton_trend2_sd + cotton_trend2_tn +         
                cotton_trend2_va + cotton_trend2_wi +

               hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq +
               hay_dday0_10_thirty + hay_dday10_30_thirty + hay_dday30_thirty + hay_prec_thirty + hay_prec_sq_thirty +
hay_trend2_al + hay_trend2_ar + hay_trend2_de + hay_trend2_ga + hay_trend2_ia  +                 
 hay_trend2_il +hay_trend2_in + hay_trend2_ks + hay_trend2_ky + hay_trend2_md + hay_trend2_mi +         
                hay_trend2_mn+ hay_trend2_mo + hay_trend2_ms +  hay_trend2_mt + hay_trend2_nc + hay_trend2_nd +         
                hay_trend2_ne +hay_trend2_oh + hay_trend2_ok +  hay_trend2_sc + hay_trend2_sd + hay_trend2_tn +         
                hay_trend2_va + hay_trend2_wi +
 
               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq +
               soybean_dday0_10_thirty + soybean_dday10_30_thirty + soybean_dday30_thirty + soybean_prec_thirty + soybean_prec_sq_thirty +
soybean_trend2_al + soybean_trend2_ar + soybean_trend2_de + soybean_trend2_ga + soybean_trend2_ia  +                  
soybean_trend2_il +soybean_trend2_in + soybean_trend2_ks + soybean_trend2_ky + soybean_trend2_md + soybean_trend2_mi +         
                soybean_trend2_mn+ soybean_trend2_mo + soybean_trend2_ms +  soybean_trend2_mt + soybean_trend2_nc + soybean_trend2_nd +         
                soybean_trend2_ne +soybean_trend2_oh + soybean_trend2_ok +  soybean_trend2_sc + soybean_trend2_sd + soybean_trend2_tn +         
                soybean_trend2_va + soybean_trend2_wi +

               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq +
               wheat_dday0_10_thirty + wheat_dday10_30_thirty + wheat_dday30_thirty + wheat_prec_thirty + wheat_prec_sq_thirty +
wheat_trend2_al + wheat_trend2_ar + wheat_trend2_de + wheat_trend2_ga + wheat_trend2_ia  +  
                wheat_trend2_il +wheat_trend2_in + wheat_trend2_ks + wheat_trend2_ky + wheat_trend2_md + wheat_trend2_mi +         
                wheat_trend2_mn+ wheat_trend2_mo + wheat_trend2_ms +  wheat_trend2_mt + wheat_trend2_nc + wheat_trend2_nd +         
                wheat_trend2_ne +wheat_trend2_oh + wheat_trend2_ok +  wheat_trend2_sc + wheat_trend2_sd + wheat_trend2_tn +         
                wheat_trend2_va + wheat_trend2_wi = 0")

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


saveRDS(mod, "models/sur_model_thirty.rds")


testdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10_sixty , dday10_30_sixty , dday30_sixty , prec_sixty , prec_sq_sixty)

cropdat_dm <- demeanlist(testdat, fl = list(state = cropdat$state))

cropdat_means <- demeanlist(testdat, fl = list(state = cropdat$state), means = TRUE)



mod1 <- z_corn_a ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty - 1



mod2 <- z_cotton_a ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty - 1
 

mod3 <- z_hay_a ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty - 1


mod4 <- z_soybean_a ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty - 1


mod5 <- z_wheat_a ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty - 1


 restrict <- c("
               corn_dday0_10_sixty + corn_dday10_30_sixty + corn_dday30_sixty + corn_prec_sixty + corn_prec_sq_sixty +

               
               cotton_dday0_10_sixty + cotton_dday10_30_sixty + cotton_dday30_sixty + cotton_prec_sixty + cotton_prec_sq_sixty +

              
               hay_dday0_10_sixty + hay_dday10_30_sixty + hay_dday30_sixty + hay_prec_sixty + hay_prec_sq_sixty +

             
               soybean_dday0_10_sixty + soybean_dday10_30_sixty + soybean_dday30_sixty + soybean_prec_sixty + soybean_prec_sq_sixty +

               
               wheat_dday0_10_sixty + wheat_dday10_30_sixty + wheat_dday30_sixty + wheat_prec_sixty + wheat_prec_sq_sixty = 0")


mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

mod$effects <- list(corn.effect = cropdat_means$z_corn_a,
                    cotton.effect = cropdat_means$z_cotton_a,
                    hay.effect = cropdat_means$z_hay_a,
                    soybean.effect = cropdat_means$z_soybean_a,
                    wheat.effect = cropdat_means$z_wheat_a)


saveRDS(mod, "models/sur_model_sixty.rds")


##---------------------------------------------------------------------
# 
# # coefnames <- c("corn_dday0_10","corn_dday10_30", "corn_dday30",           
# # "corn_prec", "corn_prec_sq", "corn_dday0_10_five",    
# # "corn_dday10_30_five", "corn_dday30_five", "corn_prec_five",        
# # "corn_prec_sq_five", "cotton_dday0_10", "cotton_dday10_30",      
# # "cotton_dday30", "cotton_prec", "cotton_prec_sq",        
# # "cotton_dday0_10_five", "cotton_dday10_30_five", "cotton_dday30_five",    
# # "cotton_prec_five", "cotton_prec_sq_five", "hay_dday0_10",          
# # "hay_dday10_30", "hay_dday30", "hay_prec",              
# # "hay_prec_sq", "hay_dday0_10_five", "hay_dday10_30_five",    
# # "hay_dday30_five", "hay_prec_five", "hay_prec_sq_five",      
# # "soybean_dday0_10", "soybean_dday10_30", "soybean_dday30",        
# # "soybean_prec", "soybean_prec_sq", "soybean_dday0_10_five", 
# # "soybean_dday10_30_five", "soybean_dday30_five", "soybean_prec_five",     
# # "soybean_prec_sq_five" )
# 
# # Skeleton Model
# # Models
# # skmod1 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + - 1   
# # 
# # skmod2 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five   - 1
# # 
# # skmod3 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five   - 1
# # 
# # skmod4 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  - 1
# # 
# # skmod5 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five   - 1
# # 
# # skelmod <- systemfit(list(corn = skmod1, 
# #                       cotton = skmod2, 
# #                       hay = skmod3, 
# #                       soybean = skmod4,
# #                       wheat = skmod5), data = cropdat, method = "SUR")
# # 
# # skelmod$coefficients
# # 
# # modmat <- model.matrix(~dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  + 
# #                 factor(state) - 1, data = cropdat)
# # modmat
# 
# # Convert to z-scores for linear regression
# 
# # First estimate between zero and 1
# cropdat$p_corn_a <- (cropdat$p_corn_a + .0001)/1.0002
# cropdat$p_cotton_a <- (cropdat$p_cotton_a + .0001)/1.0002
# cropdat$p_hay_a <- (cropdat$p_hay_a + .0001)/1.0002
# cropdat$p_soybean_a <- (cropdat$p_soybean_a + .0001)/1.0002
# cropdat$p_wheat_a <- (cropdat$p_wheat_a + .0001)/1.0002
# 
# 
# # Calc z-scores
# cropdat$z_corn_a <- qnorm(cropdat$p_corn_a)
# cropdat$z_cotton_a <- qnorm(cropdat$p_cotton_a)
# cropdat$z_hay_a <- qnorm(cropdat$p_hay_a)
# cropdat$z_soybean_a <- qnorm(cropdat$p_soybean_a)
# cropdat$z_wheat_a <- qnorm(cropdat$p_wheat_a)
# 
# 
# 
# # Boot-strapping regression to get coefficients and standard errors
# cropdat <- as.data.frame(cropdat)
# outdat <- data.frame()
# coefmat <- data.frame()
# x <- cropdat
# 
# bs_rep <- function(x){
#   indat <- data.frame()
#   for (j in 1:5){
#   # Resample based on group
#   bsdat <- x %>%
#     group_by(twenty) %>%
#     sample_frac(1, replace = TRUE)
#   
#   # Twenty-year regression
#   cmod <- felm(z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#                 dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
#                 trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#                 trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#                 trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#                 trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#                 trend2_va + trend2_wi
#               | fips + twenty  | 0 | 0, 
#               data = bsdat)
#   
#   comod <- felm(z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#                 dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
#                 trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#                 trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#                 trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#                 trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#                 trend2_va + trend2_wi
#               | fips + twenty  | 0 | 0, 
#               data = bsdat)
#   
#   hmod <- felm(z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#                 dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
#                 trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#                 trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#                 trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#                 trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#                 trend2_va + trend2_wi
#               | fips + twenty  | 0 | 0, 
#               data = bsdat)
#   
#   smod <- felm(z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#                 dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
#                 trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#                 trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#                 trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#                 trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#                 trend2_va + trend2_wi
#               | fips + twenty  | 0 | 0, 
#               data = bsdat)
#   
#   wmod <- felm(z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#                 dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
#                 trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#                 trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#                 trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#                 trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#                 trend2_va + trend2_wi
#               | fips + twenty  | 0 | 0, 
#               data = bsdat)
#   
#   corncoef <- c(1, i,  t(cmod$coefficients), as.numeric(t(getfe(cmod)[1])))
#   cottoncoef <- c(2, i, t(comod$coefficients), as.numeric(t(getfe(comod)[1])))
#   haycoef <- c(3, i, t(hmod$coefficients), as.numeric(t(getfe(hmod)[1])))
#   soybeancoef <- c(4, i, t(smod$coefficients), as.numeric(t(getfe(smod)[1])))
#   wheatcoef <- c(5, i, t(wmod$coefficients), as.numeric(t(getfe(wmod)[1])))
#   
#   
# #     crop bs_run      dday0_10     dday10_30        dday30
# # 1    1      1 -6.428279e-04  5.105858e-04 -0.0030368703
# 
# #     crop bs_run      dday0_10     dday10_30        dday30
# # 1    1      1 -7.083741e-04  5.601832e-04 -0.0029606977
# 
#   coefrun <- as.data.frame(rbind(corncoef, cottoncoef, haycoef, soybeancoef, wheatcoef))
#   rownames(coefrun) <- NULL
#   colnames(coefrun) <- c("crop", "bs_run", rownames(cmod$coefficients), t(getfe(cmod)[5]))
#   coefrun <- filter(coefrun, crop != j)
#   eq5 <- as.numeric(as.data.frame(t(c(j, i, colSums(coefrun[,3:ncol(coefrun)])*-1))))
#   coefrun <- rbind(coefrun, eq5)
#   rownames(coefrun) <- NULL
#   indat <- rbind(indat, coefrun)
#   }
#   
#   # Average coefficients from leave-one-out
#   lo <- indat %>% 
#     group_by(crop) %>% 
#     summarise_all(mean)
#   lo
#   outdat <- rbind(outdat, lo)
#   return(outdat)
# }
# 
# 
# 
# 
# 
# 
# 
# # # Models
# #   
# # mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
# # 
# # mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
# # 
# # mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
# # 
# # mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
# # 
# # mod5 <- z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
# #               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
# # 
# # restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq +
# #               corn_dday0_10_five + corn_dday10_30_five + corn_dday30_five + corn_prec_five + corn_prec_sq_five +
# #               
# #               cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq +
# #               cotton_dday0_10_five + cotton_dday10_30_five + cotton_dday30_five + cotton_prec_five + cotton_prec_sq_five +
# # 
# #               hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq +
# #               hay_dday0_10_five + hay_dday10_30_five + hay_dday30_five + hay_prec_five + hay_prec_sq_five +
# # 
# #               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq +
# #               soybean_dday0_10_five + soybean_dday10_30_five + soybean_dday30_five + soybean_prec_five + soybean_prec_sq_five +
# # 
# #               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq +
# #               wheat_dday0_10_five + wheat_dday10_30_five + wheat_dday30_five + wheat_prec_five + wheat_prec_sq_five = 0")
# # 
# # mod <- systemfit(list(corn = mod1, 
# #                       cotton = mod2, 
# #                       hay = mod3, 
# #                       soybean = mod4,
# #                       wheat = mod5), data = cropdat, method = "SUR")
# # 
# # , restrict.matrix = restrict)
# # 
# # summary(mod)
# # mod$coefCov
# # mod$coefficients
# # sum(mod$coefficients)
# # 
# # 
# # 
# # # (n) coefficients
# # ncoef <- length(mod$coefficients)
# # ncoef4 <- length(mod$coefficients)/4
# # 
# # # Keep main coefficients
# # coeff <- mod$coefficients
# # 
# # # Build coefmatrix
# # coefmat <- as.data.frame(matrix(mod$coefficients, ncol = ncoef4, nrow = 4, byrow = TRUE))
# # 
# # # Solve for 5th equation
# # eq5 <- as.data.frame(colSums(coefmat)*-1)
# # coefmat <- rbind(coefmat, t(eq5))
# # rownames(coefmat) <- NULL
# # rownames(coefmat) <- c("Corn", "Cotton", "Hay", "Soybean", "Wheat")
# # names(coefmat) <- names(mod$coefficients)[1:ncoef4]
# # coefmat
# # names(coefmat) <- substring(names(coefmat), 6)
# # coefmat
# # colSums(coefmat)
# # sum(coefmat)  
# # rowSums(coefmat)
# # 
# # # test <- as.numeric(c(coefmat[1, 1:ncoef4], coefmat[2, 1:ncoef4], coefmat[3, 1:ncoef4], coefmat[4, 1:ncoef4], coefmat[5, 1:ncoef4]))
# # # names(test) <- names(skelmod$coefficients)
# # # skelmod$coefficients <- test
# # # 
# # # pmod <- predict(skelmod)
# # # pmod
# # # summary(skelmod)
# # # 
# # # pmod[, 1] <- (pmod[,1]*corn_sd) + corn_m
# # # pmod[, 2] <- (pmod[,2]*cotton_sd) + cotton_m
# # # pmod[, 3] <- (pmod[,3]*hay_sd) + hay_m
# # # pmod[, 4] <- (pmod[,4]*soybean_sd) + soybean_m
# # # 
# # # pmod[, 1] <- (pmod[,1]*1.02) - 0.01
# # # pmod[, 2] <- (pmod[,2]*1.02) - 0.01
# # # pmod[, 3] <- (pmod[,3]*1.02) - 0.01
# # # pmod[, 4] <- (pmod[,4]*1.02) - 0.01
# # # 
# # # rowSums(pmod[, 1:4])
# # # which(rowSums(pmod[, 1:4]) > 1)
# # # 
# # # # Convert back to proportions
# # # coefmat[1, 1:10] <- (coefmat[1, 1:10]*corn_sd) + corn_m
# # # coefmat[2, 1:10] <- (coefmat[2, 1:10]*cotton_sd) + cotton_m
# # # coefmat[3, 1:10] <- (coefmat[3, 1:10]*hay_sd) + hay_m
# # # coefmat[4, 1:10] <- (coefmat[4, 1:10]*soybean_sd) + soybean_m
# # # 
# # # # Transform back to original acres
# # # coefmat[1, 1:10] <- (coefmat[1, 1:10]*1.02) - 0.01
# # # coefmat[2, 1:10] <- (coefmat[2, 1:10]*1.02) - 0.01
# # # coefmat[3, 1:10] <- (coefmat[3, 1:10]*1.02) - 0.01
# # # coefmat[4, 1:10] <- (coefmat[4, 1:10]*1.02) - 0.01
# # # 
# # # coefmat
# # # sum(coefmat$dday0_10)
# # 
# # predmat <- data.frame(corn = modmat %*% t(coefmat[1, 1:ncoef4]),
# #                        cotton = modmat %*% t(coefmat[2, 1:ncoef4]),
# #                        hay = modmat %*% t(coefmat[3, 1:ncoef4]),
# #                        soybean = modmat %*% t(coefmat[4, 1:ncoef4]))
# # predmat$Wheat <- 1 - rowSums(predmat[, 1:4])
# # 
# # rowSums(predmat[, 1:5])
# # 
# # 
# # ,
# #                        wheat = modmat %*% t(coefmat[5, 1:ncoef4]))
# # 
# # coefmat[1, 1:ncoef4] %*% modmat
# # 
# # modmat %*% t(coefmat[1, 1:ncoef4]
# # predmat
# # 
# # # Convert back to proportions
# # predmat$Corn <- (predmat$Corn*corn_sd) + corn_m
# # predmat$Cotton <- (predmat$Cotton*cotton_sd) + cotton_m
# # predmat$Hay <- (predmat$Hay*hay_sd) + hay_m
# # predmat$Soybean <- (predmat$Soybean*soybean_sd) + soybean_m
# # predmat$Wheat <- (predmat$Wheat*wheat_sd) + wheat_m
# # 
# # rowSums(predmat[, 1:5])
# # 
# # # Transform back to original acres
# # predmat$Corn <- (predmat$Corn*1.02) - 0.01
# # predmat$Cotton <- (predmat$Cotton*1.02) - 0.01
# # predmat$Hay <- (predmat$Hay*1.02) - 0.01
# # predmat$Soybean <- (predmat$Soybean*1.02) - 0.01
# # predmat$Wheat <- (predmat$Wheat*1.02) - 0.01
# # 
# # rowSums(predmat[, 1:5])
# # range(predmat[, 1:5])
# # 
# # # Convert back to proportions
# # # cropdat$p_corn_a <- (cropdat$z_corn_a*corn_sd) + corn_m
# # # cropdat$p_cotton_a <- (cropdat$z_cotton_a*cotton_sd) + cotton_m
# # # cropdat$p_hay_a <- (cropdat$z_hay_a*hay_sd) + hay_m
# # # cropdat$p_soybean_a <- (cropdat$z_soybean_a*soybean_sd) + soybean_m
# # 
# # 
# # # Transform back to original acres
# # # cropdat$p_corn_a <- (cropdat$p_corn_a*1.02) - 0.01
# # # cropdat$p_cotton_a <- (cropdat$p_cotton_a*1.02) - 0.01
# # # cropdat$p_hay_a <- (cropdat$p_hay_a*1.02) - 0.01
# # # cropdat$p_soybean_a <- (cropdat$p_soybean_a*1.02) - 0.01
# # 
# # 
# # # 
# # # sum(mod$coefficients)
# # # test <- predict(mod)[1]
# # # rowSums(test[, 1:5])
# # # lm(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq , data = surdat)
# # # 
# # # mod1b_fe <- felm(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state, data = surdat)
# # # mod1fe <- getfe(mod1b_fe)
# # # mod1fe <- mod1fe[, c(1, 5)]
# # # rownames(mod1fe) <- NULL
# # # names(mod1fe) <- c("effect", "state")
# # # mod1fee <- data.frame(state = csdat$state)
# # # mod1fee <- left_join(mod1fee, mod1fe, by = "state")
# # # mod1fee <- cbind(mod1fee, test)
# # # mod1fee$fit <- mod1fee$test + mod1fee$effect
# # # summary(mod1fee$fit)
# # # 
# # # 
# # # 
# # # y <- select(csdat, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
# # # X <- select(csdat, dday0_10, dday10_30, dday30C, prec, prec_sq)
# # # 
# # # fm <- fmlogit(y, X, maxit = 1000)
# # # summary(fm)
# # # 
# # # 
# # # # Doesn't work stacking varaibles because of unbal panel not implemented
# # # csdat$latlong <- csdat$lat*csdat$long
# # # surdat <- select(csdat, fips, state, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a,
# # #                  dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
# # # 
# # # surdat <- gather(surdat, key = crops, value = value, -fips, -state, -dday0_10, -dday10_30, -dday30C, -prec, -prec_sq,
# # #                  -lat, -long, -latlong)
# # # 
# # # surdat <- pdata.frame(surdat, c("state"))
# # # 
# # # surdat <- make.pbalanced(surdat)
# # # 
# # # 
# # # mod <- systemfit(value ~ dday0_10 + dday10_30 + dday30C + prec , data = surdat, method = "SUR",
# # #                  pooled = TRUE)
# # # summary(mod)
# # # 
# # # # Demean for fixed effects
# # # femoddat <- as.data.frame(moddat)
# # # femoddat <- select(femoddat, dday0_10, dday10_30, dday30C, prec, prec_sq)
# # # 
# # # femoddat$fips <- factor(femoddat$fips)
# # # class(femoddat)
# # # femoddat <- demeanlist(femoddat, fl = moddat$fips)
# # # femoddat
# # # 
# # # mod1b <- p_corn_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# # # 
# # # mod2b <- p_cotton_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# # # 
# # # mod3b <- p_hay_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# # # 
# # # mod4b <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq  + lat + long + latlong -1
# # # 
# # # mod5b <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# # # 
# # # restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30C + corn_prec + corn_prec_sq + corn_lat + 
# # #               corn_long + corn_latlong +              
# # #               cotton_dday0_10 + cotton_dday10_30 + cotton_dday30C + cotton_prec + cotton_prec_sq  + 
# # #               cotton_lat + cotton_long + cotton_latlong + 
# # #               hay_dday0_10 + hay_dday10_30 + hay_dday30C + hay_prec + hay_prec_sq + hay_lat + 
# # #               hay_long + hay_latlong + 
# # #               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30C + soybean_prec + 
# # #               soybean_prec_sq  + soybean_lat + soybean_long + soybean_latlong + 
# # #               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30C + wheat_prec + wheat_prec_sq + 
# # #               wheat_lat + wheat_long + wheat_latlong = 0")
# # # 
# # # 
# # # 
# # # mod <- systemfit(list(corn = mod1b, 
# # #                       cotton = mod2b, 
# # #                       hay = mod3b, 
# # #                       soybean = mod4b,
# # #                       wheat = mod5b), data = csdat, method = "SUR", restrict.matrix = restrict)
# # # 
# # # predict(mod)
# # # 
# # # , restrict.matrix = restrict)
# # # summary(mod)
# # # predict(mod)
# # # ,
# # #                  restrict.matrix = restrict)
