library(tidyverse)
library(lfe)
library(zoo)
library(noncensus)
library(RcppRoll)
setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")
depvar <- cropdat[, c("z_corn_a", "z_cotton_a", "z_hay_a", "z_soybean_a", "z_wheat_a")]
cropdat$acres <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)
cropdat <- select(cropdat, year, fips, state, acres)

# IV predictions
acres_climate_iv <- readRDS("models/acres_climate_iv.rds")

data(zip_codes)
zip_codes <- select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("fips", "lat", "long")

zip_codes <- zip_codes %>% 
  group_by(fips) %>% 
  summarise(lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE))

# > sum(cropdat$prec)
# [1] 3496644

# Process data
dd_temp <- function(x, prec){
  pdat <- left_join(x, prec, by = c("fips", "year", "month"))
  pdat <- filter(pdat, fips %in% unique(cropdat$fips))
  
  pdat <- pdat %>%
    filter(month >= 3 & month <= 10) %>%
    group_by(fips, year) %>%
    summarise(tavg = mean(tavg, na.rm = TRUE),
              prec = sum(prec, na.rm = TRUE),
              dday0C = sum(dday0C, na.rm = TRUE),
              dday10C = sum(dday10C, na.rm = TRUE),
              dday30C = sum(dday30C, na.rm = TRUE)) %>% 
    left_join(cropdat, by = c("fips", "year"))  %>% 
    ungroup()

  pdat$dday0_10 <- pdat$dday0C - pdat$dday10C
  pdat$dday10_30 <- pdat$dday10C - pdat$dday30C
  pdat$dday30 <- pdat$dday30C
  pdat$prec_sq <- pdat$prec^2
  
  pdat <- left_join(pdat, zip_codes, by = "fips")
  pdat <- as.data.frame(pdat)
  
  #--------------------------------------------------
# Roll.mean intervals

# Lag one so current year is not included
  pdat <- pdat %>% 
    group_by(fips) %>% 
    arrange(-year) %>% 
    mutate(dday0_10_lag1 = lag(dday0_10),
           dday10_30_lag1 = lag(dday10_30),
           dday30_lag1 = lag(dday30),
           prec_lag1 = lag(prec))
  
  pdat <- pdat %>% 
    group_by(fips) %>% 
    arrange(year) %>% 
    mutate(dday0_10_rm10 = roll_mean(dday0_10_lag1, 10, align = "right", fill = "NA"),
           dday10_30_rm10 = roll_mean(dday10_30_lag1, 10, align = "right", fill = "NA"),
           dday30_rm10 = roll_mean(dday30_lag1, 10, align = "right", fill = "NA"),
           prec_rm10 = roll_mean(prec_lag1, 10, align = "right", fill = "NA"),
           prec_sq_rm10 = prec_rm10^2,
           
           dday0_10_rm11 = roll_mean(dday0_10_lag1, 11, align = "right", fill = "NA"),
           dday10_30_rm11 = roll_mean(dday10_30_lag1, 11, align = "right", fill = "NA"),
           dday30_rm11 = roll_mean(dday30_lag1, 11, align = "right", fill = "NA"),
           prec_rm11 = roll_mean(prec_lag1, 11, align = "right", fill = "NA"),
           prec_sq_rm11 = prec_rm11^2,
           
           dday0_10_rm12 = roll_mean(dday0_10_lag1, 12, align = "right", fill = "NA"),
           dday10_30_rm12 = roll_mean(dday10_30_lag1, 12, align = "right", fill = "NA"),
           dday30_rm12 = roll_mean(dday30_lag1, 12, align = "right", fill = "NA"),
           prec_rm12 = roll_mean(prec_lag1, 12, align = "right", fill = "NA"),
           prec_sq_rm12 = prec_rm12^2) %>% 
    ungroup()



  pdat <- filter(pdat, year >= 1950 & year <= 2010)
  pdat$trend <- pdat$year - (min(pdat$year) - 1)
  pdat$trend_sq <- pdat$trend^2
  pdat$trend_lat <- pdat$trend*pdat$lat
  pdat$trend_long <- pdat$trend*pdat$long
  pdat$trend_sq_long <- pdat$trend_sq*pdat$long
  pdat$trend_sq_lat <- pdat$trend_sq*pdat$lat
  
  pdat <- as.data.frame(pdat)
  pdat <- cbind(pdat, depvar)
  
  # Select columns
  pdat <- select(pdat, fips, year, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a,
                 dday0_10, dday10_30, dday30, prec, prec_sq, 
                 dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10,
                 dday0_10_rm11, dday10_30_rm11, dday30_rm11, prec_rm11, prec_sq_rm11,
                 dday0_10_rm12, dday10_30_rm12, dday30_rm12, prec_rm12, prec_sq_rm12,
                trend_lat, trend_sq_lat, trend_long, trend_sq_long)
  pdat <- cbind(pdat, acres_climate_iv)
  
  return(pdat)
  
}



# Degree day changes 1-5C

# Load data
dd1c <- readRDS("data/degree_day_changes/fips_degree_days_1C_1900-2013.rds")
dd2c <- readRDS("data/degree_day_changes/fips_degree_days_2C_1900-2013.rds")
dd3c <- readRDS("data/degree_day_changes/fips_degree_days_3C_1900-2013.rds")
dd4c <- readRDS("data/degree_day_changes/fips_degree_days_4C_1900-2013.rds")
dd5c <- readRDS("data/degree_day_changes/fips_degree_days_5C_1900-2013.rds")

prec <- read_csv("data/fips_precipitation_1900-2013.csv")
prec <- as.data.frame(prec)
names(prec)[4] <- "prec"

p1 <- dd_temp(dd1c, prec)
p2 <- dd_temp(dd2c, prec)
p3 <- dd_temp(dd3c, prec)
p4 <- dd_temp(dd4c, prec)
p5 <- dd_temp(dd5c, prec)


saveRDS(p1, "data/degree_day_changes/panel_adapt_regression_data_1C.rds")
saveRDS(p2, "data/degree_day_changes/panel_adapt_regression_data_2C.rds")
saveRDS(p3, "data/degree_day_changes/panel_adapt_regression_data_3C.rds")
saveRDS(p4, "data/degree_day_changes/panel_adapt_regression_data_4C.rds")
saveRDS(p5, "data/degree_day_changes/panel_adapt_regression_data_5C.rds")
