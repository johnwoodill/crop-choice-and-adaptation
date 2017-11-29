library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

cropdat <- filter(cropdat, year < 2010)
  
cropdat$corn_rprice <- mean(cropdat$corn_rprice, na.rm = TRUE)
cropdat$cotton_rprice <- mean(cropdat$cotton_rprice, na.rm = TRUE)
cropdat$hay_rprice <- mean(cropdat$hay_rprice, na.rm = TRUE)
cropdat$wheat_rprice <- mean(cropdat$wheat_rprice, na.rm = TRUE)
cropdat$soybean_rprice <- mean(cropdat$soybean_rprice, na.rm = TRUE)

cropdat <- cropdat %>% 
   group_by(fips) %>% 
   mutate(avg_corn_a = mean(corn_grain_a, na.rm = TRUE),
          avg_cotton_a = mean(cotton_a, na.rm = TRUE),
          avg_hay_a = mean(hay_a, na.rm = TRUE),
          avg_soybean_a = mean(soybean_a, na.rm = TRUE),
          avg_wheat_a = mean(wheat_a, na.rm = TRUE))

# Total Activity
cropdat$corn <- cropdat$corn_yield*cropdat$corn_rprice
cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_rprice
cropdat$hay <- cropdat$hay_yield*cropdat$hay_rprice
cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_rprice
cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_rprice

# Set acres to zero
cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Variables
cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
cropdat$acres <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)

cropdat$rev <- ifelse(is.na(cropdat$rev), 0, cropdat$rev)

cropdat$ln_rev <- log(1 + cropdat$rev)

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

# Spline through acres to smooth out weights
cropdat <- cropdat %>% 
  group_by(fips) %>% 
  arrange(year) %>% 
  mutate(w = loess(acres ~ year)$fitted)
cropdat$w <- ifelse(cropdat$w < 0 , 0, cropdat$w)

cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$dday30 <- cropdat$dday30C
cropdat$prec_sq <- cropdat$prec^2

# 60 year intervals
regdat <- cropdat %>% 
  group_by(fips) %>% 
  mutate(dday0_10_sixty = mean(dday0_10, na.rm = TRUE),
         dday10_30_sixty = mean(dday10_30, na.rm = TRUE),
         dday30_sixty = mean(dday30, na.rm = TRUE),
         prec_sixty = mean(prec, na.rm = TRUE),
         prec_sq_sixty = prec^2)

# 30 year intervals
regdat$thirty <- regdat$year - (regdat$year %% 30)

regdat <- regdat %>% 
  group_by(fips, thirty) %>% 
  mutate(dday0_10_thirty = mean(dday0_10, na.rm = TRUE),
         dday10_30_thirty = mean(dday10_30, na.rm = TRUE),
         dday30_thirty = mean(dday30, na.rm = TRUE),
         prec_thirty = mean(prec, na.rm = TRUE),
         prec_sq_thirty = prec_thirty^2)

# 20 year intervals
regdat$twenty <- 0
regdat$twenty <- ifelse(regdat$year %in% seq(1950, 1969, 1), 1950, regdat$twenty)
regdat$twenty <- ifelse(regdat$year %in% seq(1970, 1989, 1), 1970, regdat$twenty)
regdat$twenty <- ifelse(regdat$year %in% seq(1990, 2009, 1), 1990, regdat$twenty)

regdat <- regdat %>% 
  group_by(fips, twenty) %>% 
  mutate(dday0_10_twenty = mean(dday0_10, na.rm = TRUE),
         dday10_30_twenty = mean(dday10_30, na.rm = TRUE),
         dday30_twenty = mean(dday30, na.rm = TRUE),
         prec_twenty = mean(prec, na.rm = TRUE),
         prec_sq_twenty = prec_twenty^2)

# 10 year intervals
regdat$ten <- regdat$year - (regdat$year %% 10)

regdat <- regdat %>% 
  group_by(fips, ten) %>% 
  mutate(dday0_10_ten = mean(dday0_10, na.rm = TRUE),
         dday10_30_ten = mean(dday10_30, na.rm = TRUE),
         dday30_ten = mean(dday30, na.rm = TRUE),
         prec_ten = mean(prec, na.rm = TRUE),
         prec_sq_ten = prec_ten^2)

# 5 year intervals
regdat$five <- regdat$year - (regdat$year %% 5)

regdat <- regdat %>% 
  group_by(fips, five) %>% 
  mutate(dday0_10_five = mean(dday0_10, na.rm = TRUE),
         dday10_30_five = mean(dday10_30, na.rm = TRUE),
         dday30_five = mean(dday30, na.rm = TRUE),
         prec_five = mean(prec, na.rm = TRUE),
         prec_sq_five = prec_five^2)

#---------------------------------------------------------------------------------------------
# Regressions

# Five year differences 1950-1980 & 1980-2010
modfive1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
            |0 | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modfive1)

modfive2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
            |state | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modfive2)

modfive3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
            |fips | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modfive3)

modfive4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
            |state + five | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modfive4)

modfive5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
            | state + five | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modfive5)


# Ten year differences 1950-1980 & 1980-2010
modten1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten
            |0 | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modten1)

modten2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten
            |state | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modten2)

modten3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten
            |fips | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modten3)

modten4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten
            |state + ten | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modten4)

modten5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten
            | state + ten | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modten5)


# Twenty year differences 1950-1980 & 1980-2010
modtwenty1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty
            |0 | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modtwenty1)

modtwenty2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty
            |state | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modtwenty2)

modtwenty3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty
            |fips | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modtwenty3)

modtwenty4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty
            |state + twenty | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modtwenty4)

modtwenty5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty
            | state + twenty | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modtwenty5)

# Thirty year differences 1950-1980 & 1980-2010
modthirty1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            |0 | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modthirty1)

modthirty2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            |state | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modthirty2)

modthirty3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            |fips | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modthirty3)

modthirty4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            |state + thirty | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modthirty4)

modthirty5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            | state + thirty | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modthirty5)

# 60-year
modsixty1 <- felm(ln_rev ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty
            |0 | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modsixty1)

modsixty2 <- felm(ln_rev ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty
            |state | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modsixty2)

modsixty3 <- felm(ln_rev ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty
            |fips | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modsixty3)

mod4sixty4 <- felm(ln_rev ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty
            |state | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(mod4sixty4)

modsixty5 <- felm(ln_rev ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty
            | state | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modsixty5)



# Save robust model (5)

saveRDS(modfive5, "models/modfive5.rds")
saveRDS(modten5, "models/modten5.rds")
saveRDS(modtwenty5, "models/modtwenty5.rds")
saveRDS(modthirty5, "models/modthirty5.rds")
saveRDS(modsixty5, "models/modsixty5.rds")
