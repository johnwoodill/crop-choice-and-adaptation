library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

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


  #------------------------------------------------------------------------
# Cross-section data setup
csdat <- cropdat

csdat <- csdat %>%
  group_by(year) %>%
  mutate(dm_ln_rev = ln_rev - mean(ln_rev, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(state, fips) %>%
  summarise(ln_rev = mean(ln_rev, na.rm = TRUE),
            acres = mean(acres, na.rm = TRUE),
            tavg = mean(tavg, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE),
            dday0C = mean(dday0C, na.rm = TRUE),
            dday10C = mean(dday10C, na.rm = TRUE),
            dday30C = mean(dday30C, na.rm = TRUE),
            lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE),
            w = mean(w, na.rm = TRUE),
            corn_a = mean(corn_grain_a, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE)) %>% 
  ungroup() 

csdat$dday0_10 <- csdat$dday0C - csdat$dday10C
csdat$dday10_30 <- csdat$dday10C - csdat$dday30C
csdat$dday30 <- csdat$dday30C
csdat$prec_sq <- csdat$prec^2

csdat$tacres <- rowSums(csdat[, c("corn_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)

csdat$p_corn_a <- csdat$corn_a/csdat$tacres
csdat$p_cotton_a <- csdat$cotton_a/csdat$tacres
csdat$p_hay_a <- csdat$hay_a/csdat$tacres
csdat$p_soybean_a <- csdat$soybean_a/csdat$tacres
csdat$p_wheat_a <- csdat$wheat_a/csdat$tacres

mod <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state | 0 | state, 
            data = csdat, weights = csdat$w)
summary(mod)

saveRDS(csdat, "data/cross_section_regression_data.rds")

#----------------------------------------------------------
# Long-difference

lddat <- cropdat

# Long-difference (Burke)
# 
# lddat1950 <- filter(cropdat, year >= 1950 & year <= 1959)
# lddat1980 <- filter(cropdat, year >= 2000 & year <= 2009)
# 

# # 
# lddat1950 <- lddat1950 %>%
#   group_by(state, fips) %>%
#   summarise(rev_1950 = mean(rev, na.rm = TRUE),
#             ln_rev_1950 = mean(ln_rev, na.rm = TRUE),
#             dday0C_1950 = mean(dday0C, na.rm = TRUE),
#             dday10C_1950 = mean(dday10C, na.rm = TRUE),
#             dday30C_1950 = mean(dday30C, na.rm = TRUE),
#             prec_1950 = mean(prec, na.rm = TRUE))
# 
# lddat1980 <- lddat1980 %>%
#   group_by(state, fips) %>%
#   summarise(rev_1980 = mean(rev, na.rm = TRUE),
#             ln_rev_1980 = mean(ln_rev, na.rm = TRUE),
#             dday0C_1980 = mean(dday0C, na.rm = TRUE),
#             dday10C_1980 = mean(dday10C, na.rm = TRUE),
#             dday30C_1980 = mean(dday30C, na.rm = TRUE),
#             prec_1980 = mean(prec, na.rm = TRUE))
# 
# # 30-year intervals (1980-1950 & 2009 - 1980)
# 
# lddat <- left_join(lddat1950, lddat1980, by = c("fips", "state"))
# lddat <- left_join(lddat, ldweights, by = c("fips"))
# 
# lddat$ln_rev_diff <- log(1 + lddat$ln_rev_1980) - log(1 + lddat$ln_rev_1950)
# lddat$dday0_10 <- (lddat$dday0C_1980 - lddat$dday10C_1980) - (lddat$dday0C_1950 - lddat$dday10C_1950)
# lddat$dday10_30 <- (lddat$dday10C_1980 - lddat$dday30C_1980) - (lddat$dday10C_1950 - lddat$dday30C_1950)
# lddat$dday0_30 <- (lddat$dday0C_1980 - lddat$dday30C_1980) - (lddat$dday0C_1950 - lddat$dday30C_1950)
# lddat$dday30 <- (lddat$dday30C_1980 - lddat$dday30C_1950)
# lddat$prec <- lddat$prec_1980 - lddat$prec_1950
# lddat$prec_sq <-lddat$prec_1980^2 - lddat$prec_1950^2

# Demean by decade
lddat$decade <- substr(lddat$year, 3,3)
lddat$decade <- ifelse(lddat$decade == 0, 10, lddat$decade)

# lddat1960 <- filter(lddat, year >= 1960 & year < 1970)
# lddat1970 <- filter(lddat, year >= 1970 & year < 1980)
# lddat1980 <- filter(lddat, year >= 1980 & year < 1990)
# lddat1990 <- filter(lddat, year >= 1990 & year < 2000)
#lddat2000 <- filter(lddat, year >= 2000 & year < 2010)

mergdat <- data.frame()
for (i in unique(lddat$decade)){
  intdat <- filter(lddat, decade == i)
  intdat$ln_rev <- intdat$ln_rev - mean(intdat$ln_rev, na.rm = TRUE)
  intdat <- intdat %>%
  #group_by(decade) %>%
  #mutate(ln_rev = ln_rev - mean(ln_rev, na.rm = TRUE)) %>%
  #ungroup() %>%
  group_by(state, fips) %>%
  summarise(ln_rev = mean(ln_rev, na.rm = TRUE),
            dday0C = mean(dday0C, na.rm = TRUE),
            dday10C = mean(dday10C, na.rm = TRUE),
            dday30C = mean(dday30C, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE)) %>%
  ungroup()
  intdat$decade <- i
  mergdat <- rbind(mergdat, intdat)
}
head(mergdat)
lddat <- mergdat

# # # Weights (1950-2010 Average acres)
ldweights <- cropdat %>%
  filter(year <= 1959) %>%
  group_by(fips, state) %>%
  summarise(w = mean(acres, na.rm = TRUE)) %>%
ungroup()
ldweights$state <- NULL
lddat <- left_join(lddat, ldweights, by = "fips")


# 
# 


lddat$dday0_10 <- lddat$dday0C - lddat$dday10C
lddat$dday10_30 <- lddat$dday10C - lddat$dday30C
lddat$dday30 <- lddat$dday30C
lddat$prec_sq <- lddat$prec^2

mod <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | state + decade | 0 | state, 
            data = lddat, weights = lddat$w)
summary(mod)

saveRDS(lddat, "data/long_difference_regression_data.rds")


#-----------------------------------------
# Panel 

pdat <- cropdat


pdat$dday0_10 <- pdat$dday0C - pdat$dday10C
pdat$dday10_30 <- pdat$dday10C - pdat$dday30C
pdat$dday30 <- pdat$dday30C
pdat$prec_sq <- pdat$prec^2

pdat <- pdat %>% 
   group_by(fips) %>% 
   mutate(w = mean(acres, na.rm = TRUE))

mod <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
            data = pdat, weights = pdat$w)
summary(mod)


saveRDS(pdat, "data/panel_regression_data.rds")


#-------------------------------------------
# Degree day changes 1-5C

# Load data
dd1c <- readRDS("data/degree_day_changes/fips_degree_days_1C_1900-2013.rds")
dd2c <- readRDS("data/degree_day_changes/fips_degree_days_2C_1900-2013.rds")
dd3c <- readRDS("data/degree_day_changes/fips_degree_days_3C_1900-2013.rds")
dd4c <- readRDS("data/degree_day_changes/fips_degree_days_4C_1900-2013.rds")
dd5c <- readRDS("data/degree_day_changes/fips_degree_days_5C_1900-2013.rds")

# x <- dd1c
prec <- read_csv("data/fips_precipitation_1900-2013.csv")
names(prec)[4] <- "prec"

# Get acres
cropdat <- readRDS("data/full_ag_data.rds")
cropdat$acres <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)
cropdat <- select(cropdat, year, fips, state, acres)

# Cross-sectional data
cs_dat <- function(x, prec){
  csdat <- filter(x, year >= 1950 & year <= 2009)
  prec <- filter(prec, year >= 1950 & year <= 2009)
  csdat <- left_join(csdat, prec, by = c("fips", "year", "month"))
  csdat <- filter(csdat, fips %in% unique(cropdat$fips))

  csdat <- csdat %>%
    filter(month >= 3 & month <= 10) %>%
    group_by(fips, year) %>%
    summarise(tavg = mean(tavg, na.rm = TRUE),
            prec = sum(prec, na.rm = TRUE),
            dday0C = sum(dday0C, na.rm = TRUE),
            dday10C = sum(dday10C, na.rm = TRUE),
            dday30C = sum(dday30C, na.rm = TRUE)) %>% 
    left_join(cropdat, by = c("fips", "year")) %>% 
    group_by(fips) %>%
    summarise(tavg = mean(tavg, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE),
            dday0C = mean(dday0C, na.rm = TRUE),
            dday10C = mean(dday10C, na.rm = TRUE),
            dday30C = mean(dday30C, na.rm = TRUE),
            acres = mean(acres, na.rm = TRUE)) %>%
  ungroup()

  csdat$dday0_10 <- csdat$dday0C - csdat$dday10C
  csdat$dday10_30 <- csdat$dday10C - csdat$dday30C
  csdat$dday30 <- csdat$dday30C
  csdat$prec_sq <- csdat$prec^2
  return(csdat)
  }

# Long-difference

ld_dat <- function(x, prec) {
  
  
  lddat <- filter(x, year >= 1950 & year <= 2009)
  prec <- filter(prec, year >= 1950 & year <= 2009)
  lddat <- left_join(lddat, prec, by = c("fips", "year", "month"))
  lddat <- filter(lddat, fips %in% unique(cropdat$fips))
                  
  lddat <- lddat %>%
    filter(month >= 3 & month <= 10) %>%
    group_by(fips, year) %>%
    summarise(tavg = mean(tavg, na.rm = TRUE),
            prec = sum(prec, na.rm = TRUE),
            dday0C = sum(dday0C, na.rm = TRUE),
            dday10C = sum(dday10C, na.rm = TRUE),
            dday30C = sum(dday30C, na.rm = TRUE))
  
  # Long-difference
  # lddat1950 <- filter(lddat, year >= 1950 & year <= 1959)
  # lddat1980 <- filter(lddat, year >= 2000 & year <= 2009)
  # 
  # lddat1950 <- lddat1950 %>%
  #   group_by(fips) %>%
  #   summarise(dday0C_1950 = mean(dday0C, na.rm = TRUE),
  #             dday10C_1950 = mean(dday10C, na.rm = TRUE),
  #             dday30C_1950 = mean(dday30C, na.rm = TRUE),
  #             prec_1950 = mean(prec, na.rm = TRUE))
  # 
  # lddat1980 <- lddat1980 %>%
  #   group_by(fips) %>%
  #   summarise(dday0C_1980 = mean(dday0C, na.rm = TRUE),
  #             dday10C_1980 = mean(dday10C, na.rm = TRUE),
  #             dday30C_1980 = mean(dday30C, na.rm = TRUE),
  #             prec_1980 = mean(prec, na.rm = TRUE))
  # 
  # lddat <- left_join(lddat1950, lddat1980, by = c("fips"))
  # 
  # ldweights <- cropdat %>%
  #   filter(year <= 1959) %>%
  #   group_by(fips) %>%
  #   summarise(acres = mean(acres, na.rm = TRUE)) %>%
  #   ungroup()
  # ldweights$state <- NULL
  # 
  # lddat <- left_join(lddat, ldweights, by = c("fips"))
  
  # lddat$dday0_10 <- (lddat$dday0C_1980 - lddat$dday10C_1980) - (lddat$dday0C_1950 - lddat$dday10C_1950)
  # lddat$dday10_30 <- (lddat$dday10C_1980 - lddat$dday30C_1980) - (lddat$dday10C_1950 - lddat$dday30C_1950)
  # lddat$dday0_30 <- (lddat$dday0C_1980 - lddat$dday30C_1980) - (lddat$dday0C_1950 - lddat$dday30C_1950)
  # lddat$dday30 <- (lddat$dday30C_1980 - lddat$dday30C_1950)
  # lddat$prec <- lddat$prec_1980 - lddat$prec_1950
  # lddat$prec_sq <-lddat$prec_1980^2 - lddat$prec_1950^2
  
  # Demean decade 
  # Demean by decade
  lddat$decade <- substr(lddat$year, 3,3)
  lddat$decade <- ifelse(lddat$decade == 0, 10, lddat$decade)
  
  # lddat1960 <- filter(lddat, year >= 1960 & year < 1970)
  # lddat1970 <- filter(lddat, year >= 1970 & year < 1980)
  # lddat1980 <- filter(lddat, year >= 1980 & year < 1990)
  # lddat1990 <- filter(lddat, year >= 1990 & year < 2000)
  #lddat2000 <- filter(lddat, year >= 2000 & year < 2010)

  mergdat <- data.frame()
  for (i in unique(lddat$decade)){
    intdat <- filter(lddat, decade == i)
    intdat <- intdat %>%
    #group_by(decade) %>%
    #mutate(ln_rev = ln_rev - mean(ln_rev, na.rm = TRUE)) %>%
    #ungroup() %>%
    group_by(fips) %>%
    summarise(dday0C = mean(dday0C, na.rm = TRUE),
              dday10C = mean(dday10C, na.rm = TRUE),
              dday30C = mean(dday30C, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE)) %>%
    ungroup()
    intdat$decade <- i
    
    mergdat <- rbind(mergdat, intdat)
  }
  head(mergdat)
  lddat <- mergdat
  
  lddat$dday0_10 <- lddat$dday0C - lddat$dday10C
  lddat$dday10_30 <- lddat$dday10C - lddat$dday30C
  lddat$dday30 <- lddat$dday30C
  lddat$prec_sq <- lddat$prec^2
  return(lddat) 
}


# Panel

p_dat <- function(x, prec){
  pdat <- filter(x, year >= 1950 & year <= 2009)
  prec <- filter(prec, year >= 1950 & year <= 2009)
  pdat <- left_join(pdat, prec, by = c("fips", "year", "month"))
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
  return(pdat)

}

# Clean up data for new temp data
cs1 <- cs_dat(dd1c, prec)
cs2 <- cs_dat(dd2c, prec)
cs3 <- cs_dat(dd3c, prec)
cs4 <- cs_dat(dd4c, prec)
cs5 <- cs_dat(dd5c, prec)

ld1 <- ld_dat(dd1c, prec)
ld2 <- ld_dat(dd2c, prec)
ld3 <- ld_dat(dd3c, prec)
ld4 <- ld_dat(dd4c, prec)
ld5 <- ld_dat(dd5c, prec)

p1 <- p_dat(dd1c, prec)
p2 <- p_dat(dd2c, prec)
p3 <- p_dat(dd3c, prec)
p4 <- p_dat(dd4c, prec)
p5 <- p_dat(dd5c, prec)

saveRDS(cs1, "data/degree_day_changes/cross_section_regression_data_1C.rds")
saveRDS(cs2, "data/degree_day_changes/cross_section_regression_data_2C.rds")
saveRDS(cs3, "data/degree_day_changes/cross_section_regression_data_3C.rds")
saveRDS(cs4, "data/degree_day_changes/cross_section_regression_data_4C.rds")
saveRDS(cs5, "data/degree_day_changes/cross_section_regression_data_5C.rds")

saveRDS(ld1, "data/degree_day_changes/diff_regression_data_1C.rds")
saveRDS(ld2, "data/degree_day_changes/diff_regression_data_2C.rds")
saveRDS(ld3, "data/degree_day_changes/diff_regression_data_3C.rds")
saveRDS(ld4, "data/degree_day_changes/diff_regression_data_4C.rds")
saveRDS(ld5, "data/degree_day_changes/diff_regression_data_5C.rds")

saveRDS(p1, "data/degree_day_changes/panel_regression_data_1C.rds")
saveRDS(p2, "data/degree_day_changes/panel_regression_data_2C.rds")
saveRDS(p3, "data/degree_day_changes/panel_regression_data_3C.rds")
saveRDS(p4, "data/degree_day_changes/panel_regression_data_4C.rds")
saveRDS(p5, "data/degree_day_changes/panel_regression_data_5C.rds")

