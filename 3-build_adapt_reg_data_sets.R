library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

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

# Panel function
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

  # 60 year intervals
  pdat <- pdat %>% 
    group_by(fips) %>% 
    mutate(dday0_10_sixty = mean(dday0_10, na.rm = TRUE),
           dday10_30_sixty = mean(dday10_30, na.rm = TRUE),
           dday30_sixty = mean(dday30, na.rm = TRUE),
           prec_sixty = mean(prec, na.rm = TRUE),
           prec_sq_sixty = prec^2)
  
  # 30 year intervals
  pdat$thirty <- pdat$year - (pdat$year %% 30)
  
  pdat <- pdat %>% 
    group_by(fips, thirty) %>% 
    mutate(dday0_10_thirty = mean(dday0_10, na.rm = TRUE),
           dday10_30_thirty = mean(dday10_30, na.rm = TRUE),
           dday30_thirty = mean(dday30, na.rm = TRUE),
           prec_thirty = mean(prec, na.rm = TRUE),
           prec_sq_thirty = prec_thirty^2)
  
  # 20 year intervals
  pdat$twenty <- 0
  pdat$twenty <- ifelse(pdat$year %in% seq(1950, 1969, 1), 1950, pdat$twenty)
  pdat$twenty <- ifelse(pdat$year %in% seq(1970, 1989, 1), 1970, pdat$twenty)
  pdat$twenty <- ifelse(pdat$year %in% seq(1990, 2009, 1), 1990, pdat$twenty)
  
  pdat <- pdat %>% 
    group_by(fips, twenty) %>% 
    mutate(dday0_10_twenty = mean(dday0_10, na.rm = TRUE),
           dday10_30_twenty = mean(dday10_30, na.rm = TRUE),
           dday30_twenty = mean(dday30, na.rm = TRUE),
           prec_twenty = mean(prec, na.rm = TRUE),
           prec_sq_twenty = prec_twenty^2)
  
  # 10 year intervals
  pdat$ten <- pdat$year - (pdat$year %% 10)
  
  pdat <- pdat %>% 
    group_by(fips, ten) %>% 
    mutate(dday0_10_ten = mean(dday0_10, na.rm = TRUE),
           dday10_30_ten = mean(dday10_30, na.rm = TRUE),
           dday30_ten = mean(dday30, na.rm = TRUE),
           prec_ten = mean(prec, na.rm = TRUE),
           prec_sq_ten = prec_ten^2)
  
  # 5 year intervals
  pdat$five <- pdat$year - (pdat$year %% 5)
  
  pdat <- pdat %>% 
    group_by(fips, five) %>% 
    mutate(dday0_10_five = mean(dday0_10, na.rm = TRUE),
           dday10_30_five = mean(dday10_30, na.rm = TRUE),
           dday30_five = mean(dday30, na.rm = TRUE),
           prec_five = mean(prec, na.rm = TRUE),
           prec_sq_five = prec_five^2)
  
  pdat$trend <- pdat$year - 1949
  pdat <- as.data.frame(pdat)
  state_trends <- as.data.frame(dummyCreator(pdat$state, "trend2"))
  state_trends$trend_sq <- pdat$trend^2
  state_trends <- state_trends[, 1:25]*state_trends$trend_sq
  pdat <- cbind(pdat, state_trends)
  pdat <- select(pdat, dday0_10, dday10_30, dday30, prec, prec_sq, 
                 dday0_10_five, dday10_30_five, dday30_five, prec_five, prec_sq_five,
                 dday0_10_ten, dday10_30_ten, dday30_ten, prec_ten, prec_sq_ten,
                 dday0_10_twenty, dday10_30_twenty, dday30_twenty, prec_twenty, prec_sq_twenty,
                 dday0_10_thirty, dday10_30_thirty, dday30_thirty, prec_thirty, prec_sq_thirty,
                dday0_10_sixty, dday10_30_sixty, dday30_sixty, prec_sixty, prec_sq_sixty,
                  trend2_al, trend2_ar, trend2_de, trend2_ga, trend2_ia, 
                  trend2_il, trend2_in, trend2_ks, trend2_ky, trend2_md, trend2_mi, 
                  trend2_mn, trend2_mo, trend2_ms, trend2_mt, trend2_nc, trend2_nd, 
                  trend2_ne, trend2_oh, trend2_ok, trend2_sc, trend2_sd, trend2_tn, 
                  trend2_va, trend2_wi)
  return(pdat)
  
}

# Clean up data for new temp data
p1 <- p_dat(dd1c, prec)
p2 <- p_dat(dd2c, prec)
p3 <- p_dat(dd3c, prec)
p4 <- p_dat(dd4c, prec)
p5 <- p_dat(dd5c, prec)

saveRDS(p1, "data/degree_day_changes/panel_adapt_regression_data_1C.rds")
saveRDS(p2, "data/degree_day_changes/panel_adapt_regression_data_2C.rds")
saveRDS(p3, "data/degree_day_changes/panel_adapt_regression_data_3C.rds")
saveRDS(p4, "data/degree_day_changes/panel_adapt_regression_data_4C.rds")
saveRDS(p5, "data/degree_day_changes/panel_adapt_regression_data_5C.rds")

