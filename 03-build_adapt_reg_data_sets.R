library(tidyverse)
library(lfe)
library(zoo)
library(noncensus)
setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

allFipsRM = function(dat, varName, len){
  do.call(rbind, lapply(split(dat, dat$fips), function(x) {
    all.rm <- as.data.frame(sapply(len, function(l) c(rollmean(x[,varName], l), rep(NA, l-1))))
    colnames(all.rm) <- paste0("rm", len)
    cbind(data.frame(fips=x$fips[1]), all.rm, data.frame(year=seq_len(nrow(x))-1))
  }))
}

dummyCreator <- function(invec, prefix = NULL) {
     L <- length(invec)
     ColNames <- sort(unique(invec))
     M <- matrix(0L, ncol = length(ColNames), nrow = L,
                 dimnames = list(NULL, ColNames))
     M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
     if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
     M
} 

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

# rm data
rm_dat <- readRDS("data/full_rollmean_lag_variables.rds")
rm_dat <- select(rm_dat, year, fips, dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10,
                 dday0_10_rm11, dday10_30_rm11, dday30_rm11, prec_rm11, prec_sq_rm11,
                 dday0_10_rm12, dday10_30_rm12, dday30_rm12, prec_rm12, prec_sq_rm12)
# Degree day changes 1-5C

# Load data
dd1c <- readRDS("data/degree_day_changes/fips_degree_days_1C_1900-2013.rds")
dd2c <- readRDS("data/degree_day_changes/fips_degree_days_2C_1900-2013.rds")
dd3c <- readRDS("data/degree_day_changes/fips_degree_days_3C_1900-2013.rds")
dd4c <- readRDS("data/degree_day_changes/fips_degree_days_4C_1900-2013.rds")
dd5c <- readRDS("data/degree_day_changes/fips_degree_days_5C_1900-2013.rds")


data(zip_codes)
zip_codes <- select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("fips", "lat", "long")

zip_codes <- zip_codes %>% 
  group_by(fips) %>% 
  summarise(lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE))



# x <- dd1c
prec <- read_csv("data/fips_precipitation_1900-2013.csv")
names(prec)[4] <- "prec"

# Get acres
# cropdat <- readRDS("data/full_ag_data.rds")
depvar <- cropdat[, c("z_corn_a", "z_cotton_a", "z_hay_a", "z_soybean_a", "z_wheat_a")]
cropdat$acres <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)
cropdat <- select(cropdat, year, fips, state, acres)

# Panel function
p_dat <- function(x, prec){
  
  
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
  left_join(cropdat, by = c("fips", "year")) 
    
  pdat <- pdat %>% 
    group_by(fips) %>% 
    mutate(acres = mean(acres, na.rm = TRUE))
  
  pdat$dday0_10 <- pdat$dday0C - pdat$dday10C
  pdat$dday10_30 <- pdat$dday10C - pdat$dday30C
  pdat$dday30 <- pdat$dday30C
  pdat$prec_sq <- pdat$prec^2
  
  pdat <- left_join(pdat, zip_codes, by = "fips")
  
  #--------------------------------------------------
# Roll.mean intervals
pdat <- left_join(pdat, rm_dat, by = c("year", "fips"))
  
  # pdat <- pdat %>% 
  #   arrange(year) %>% 
  #   group_by(fips) %>% 
  #   mutate(dday0_10_rm10 = rollmean(dday0_10, 10, align = "left", fill = "NA"),
  #          dday10_30_rm10 = rollmean(dday10_30, 10, align = "left", fill = "NA"),
  #          dday30_rm10 = rollmean(dday30, 10, align = "left", fill = "NA"),
  #          prec_rm10 = rollmean(prec, 10, align = "left", fill = "NA"),
  #          prec_sq_rm10 = prec_rm10^2,
  #          
  #          dday0_10_rm11 = rollmean(dday0_10, 11, align = "left", fill = "NA"),
  #          dday10_30_rm11 = rollmean(dday10_30, 11, align = "left", fill = "NA"),
  #          dday30_rm11 = rollmean(dday30, 11, align = "left", fill = "NA"),
  #          prec_rm11 = rollmean(prec, 11, align = "left", fill = "NA"),
  #          prec_sq_rm11 = prec_rm11^2,
  #          
  #          dday0_10_rm12 = rollmean(dday0_10, 12, align = "left", fill = "NA"),
  #          dday10_30_rm12 = rollmean(dday10_30, 12, align = "left", fill = "NA"),
  #          dday30_rm12 = rollmean(dday30, 12, align = "left", fill = "NA"),
  #          prec_rm12 = rollmean(prec, 12, align = "left", fill = "NA"),
  #          prec_sq_rm12 = prec_rm12^2) %>% 
  #   ungroup()

  pdat <- filter(pdat, year >= 1960 & year <= 2010)
  pdat$trend <- pdat$year - (min(pdat$year) - 1)
  pdat$trend_sq <- pdat$trend^2
  pdat$trend_lat <- pdat$trend*pdat$lat
  pdat$trend_long <- pdat$trend*pdat$long
  pdat$trend_sq_long <- pdat$trend_sq*pdat$long
  pdat$trend_sq_lat <- pdat$trend_sq*pdat$lat
  
  # pdat <- cbind(pdat, depvar)
  
  # Select columns
  pdat <- select(pdat, #z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a,
                 dday0_10, dday10_30, dday30, prec, prec_sq, 
                 dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10,
                 dday0_10_rm11, dday10_30_rm11, dday30_rm11, prec_rm11, prec_sq_rm11,
                 dday0_10_rm12, dday10_30_rm12, dday30_rm12, prec_rm12, prec_sq_rm12,
                trend_lat, trend_sq_lat, trend_long, trend_sq_long)
  
  # pdat <- cbind(pdat, depvar)
  
  return(pdat)
  
}

# Clean up data for new temp data
p1 <- p_dat(x = dd1c, prec = prec)
p2 <- p_dat(dd2c, prec)
p3 <- p_dat(dd3c, prec)
p4 <- p_dat(dd4c, prec)
p5 <- p_dat(dd5c, prec)

saveRDS(p1, "data/degree_day_changes/panel_adapt_regression_data_1C.rds")
saveRDS(p2, "data/degree_day_changes/panel_adapt_regression_data_2C.rds")
saveRDS(p3, "data/degree_day_changes/panel_adapt_regression_data_3C.rds")
saveRDS(p4, "data/degree_day_changes/panel_adapt_regression_data_4C.rds")
saveRDS(p5, "data/degree_day_changes/panel_adapt_regression_data_5C.rds")

