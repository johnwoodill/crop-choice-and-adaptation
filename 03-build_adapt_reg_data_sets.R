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

#40-year
pdat <- pdat %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(dday0_10_rm_fifty = rollmean(dday0_10, k = 50, align = "right", fill = "NA"),
         dday10_30_rm_fifty = rollmean(dday10_30, k = 50, align = "right", fill = "NA"),
         dday30_rm_fifty = rollmean(dday30, k = 50, align = "right", fill = "NA"),
         prec_rm_fifty = rollmean(prec, k = 50, align = "right", fill = "NA"),
         prec_sq_rm_fifty = prec_rm_fifty^2)

#40-year
pdat <- pdat %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(dday0_10_rm_fourty = rollmean(dday0_10, k = 40, align = "right", fill = "NA"),
         dday10_30_rm_fourty = rollmean(dday10_30, k = 40, align = "right", fill = "NA"),
         dday30_rm_fourty = rollmean(dday30, k = 40, align = "right", fill = "NA"),
         prec_rm_fourty = rollmean(prec, k = 40, align = "right", fill = "NA"),
         prec_sq_rm_fourty = prec_rm_fourty^2)

#30-year
pdat <- pdat %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(dday0_10_rm_thirty = rollmean(dday0_10, k = 30, align = "right", fill = "NA"),
         dday10_30_rm_thirty = rollmean(dday10_30, k = 30, align = "right", fill = "NA"),
         dday30_rm_thirty = rollmean(dday30, k = 30, align = "right", fill = "NA"),
         prec_rm_thirty = rollmean(prec, k = 30, align = "right", fill = "NA"),
         prec_sq_rm_thirty = prec_rm_thirty^2)

# 20 year intervals
pdat <- pdat %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(dday0_10_rm_twenty = rollmean(dday0_10, k = 20, align = "right", fill = "NA"),
         dday10_30_rm_twenty = rollmean(dday10_30, k = 20, align = "right", fill = "NA"),
         dday30_rm_twenty = rollmean(dday30, k = 20, align = "right", fill = "NA"),
         prec_rm_twenty = rollmean(prec, k = 20, align = "right", fill = "NA"),
         prec_sq_rm_twenty = prec_rm_twenty^2)

# 10-year interval
pdat <- pdat %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(dday0_10_rm_ten = rollmean(dday0_10, k = 10, align = "right", fill = "NA"),
         dday10_30_rm_ten = rollmean(dday10_30, k = 10, align = "right", fill = "NA"),
         dday30_rm_ten = rollmean(dday30, k = 10, align = "right", fill = "NA"),
         prec_rm_ten = rollmean(prec, k = 10, align = "right", fill = "NA"),
         prec_sq_rm_ten = prec_rm_ten^2)
  
  pdat <- filter(pdat, year >= 1950 & year <= 2010)


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
  pdat$trend_sq <- pdat$trend^2
  pdat <- as.data.frame(pdat)
  
  # State-level time trends
  # Linear
  state_trends <- as.data.frame(dummyCreator(pdat$state, "trend1"))
  state_trends$trend <- pdat$trend
  state_trends <- state_trends[, 1:length(state_trends)]*state_trends$trend
  state_trends$trend <- NULL
  
  # Quadratic
  state_trends_sq <- as.data.frame(dummyCreator(pdat$state, "trend2"))
  state_trends_sq$trend_sq <- pdat$trend^2
  state_trends_sq <- state_trends_sq[, 1:length(state_trends_sq)]*state_trends_sq$trend_sq
  state_trends_sq$trend_sq <- NULL
  
  pdat <- cbind(pdat, state_trends, state_trends_sq)
  
  # State-level interval trend 
  ten_trend <- as.data.frame(dummyCreator(pdat$state, "ten_trend1"))
  twenty_trend <- as.data.frame(dummyCreator(pdat$state, "twenty_trend1"))
  thirty_trend <- as.data.frame(dummyCreator(pdat$state, "thirty_trend1"))
  
  ten_trend$ten <- ifelse(pdat$ten == 1950, 1, ifelse(pdat$ten == 1960, 2, ifelse(pdat$ten == 1970, 3, ifelse(pdat$ten == 1980, 4, 
  ifelse(pdat$ten == 1990, 5, 6)))))
  
  twenty_trend$twenty <- ifelse(pdat$twenty == 1950, 1, ifelse(pdat$twenty == 1970, 2, 3))
  
  thirty_trend$thirty <- ifelse(pdat$thirty == 1950, 1, 2)
  
  ten_trend <- ten_trend[, 1:length(ten_trend)]*ten_trend$ten
  twenty_trend <- twenty_trend[, 1:length(twenty_trend)]*twenty_trend$twenty
  thirty_trend <- thirty_trend[, 1:length(thirty_trend)]*thirty_trend$thirty
  
  
  # Quadratic
  ten_trend_sq <- as.data.frame(dummyCreator(pdat$state, "ten_trend2"))
  twenty_trend_sq <- as.data.frame(dummyCreator(pdat$state, "twenty_trend2"))
  thirty_trend_sq <- as.data.frame(dummyCreator(pdat$state, "thirty_trend2"))
  
  ten_trend$ten <- ifelse(pdat$ten == 1950, 1, ifelse(pdat$ten == 1960, 2, ifelse(pdat$ten == 1970, 3, ifelse(pdat$ten == 1980, 4, 
  ifelse(pdat$ten == 1990, 5, 6)))))
  
  twenty_trend$twenty <- ifelse(pdat$twenty == 1950, 1, ifelse(pdat$twenty == 1970, 2, 3))
  
  thirty_trend$thirty <- ifelse(pdat$thirty == 1950, 1, 2)
  
  ten_trend_sq$ten_sq <- ten_trend$ten^2
  twenty_trend_sq$twenty_sq <- twenty_trend$twenty^2
  thirty_trend_sq$thirty_sq <- thirty_trend$thirty^2
  
  ten_trend_sq <- ten_trend_sq[, 1:length(ten_trend_sq)]*ten_trend_sq$ten_sq
  twenty_trend_sq <- twenty_trend_sq[, 1:length(twenty_trend_sq)]*twenty_trend_sq$twenty_sq
  thirty_trend_sq <- thirty_trend_sq[, 1:length(thirty_trend_sq)]*thirty_trend_sq$thirty_sq
  
  ten_trend$ten <- NULL
  twenty_trend$twenty <- NULL
  thirty_trend$thirty <- NULL
  ten_trend$ten_sq <- NULL
  twenty_trend$twenty_sq <- NULL
  thirty_trend$thirty_sq <- NULL
  
  # pdat <- cbind(pdat, depvar)
  
  # Select columns
  pdat <- select(pdat, #z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a,
                 dday0_10, dday10_30, dday30, prec, prec_sq, 
                 dday0_10_five, dday10_30_five, dday30_five, prec_five, prec_sq_five,
                 
                 dday0_10_ten, dday10_30_ten, dday30_ten, prec_ten, prec_sq_ten,
                 dday0_10_rm_ten, dday10_30_rm_ten, dday30_rm_ten, prec_rm_ten, prec_sq_rm_ten,
                 
                 dday0_10_twenty, dday10_30_twenty, dday30_twenty, prec_twenty, prec_sq_twenty,
                 dday0_10_rm_twenty, dday10_30_rm_twenty, dday30_rm_twenty, prec_rm_twenty, prec_sq_rm_twenty,
                 
                 dday0_10_thirty, dday10_30_thirty, dday30_thirty, prec_thirty, prec_sq_thirty,
                 dday0_10_rm_thirty, dday10_30_rm_thirty, dday30_rm_thirty, prec_rm_thirty, prec_sq_rm_thirty,
                 
                 dday0_10_rm_fourty, dday10_30_rm_fourty, dday30_rm_fourty, prec_rm_fourty, prec_sq_rm_fourty,
                 
                 dday0_10_rm_fifty, dday10_30_rm_fifty, dday30_rm_fifty, prec_rm_fifty, prec_sq_rm_fifty,
                 
                dday0_10_sixty, dday10_30_sixty, dday30_sixty, prec_sixty, prec_sq_sixty, trend, trend_sq, lat, long)
  
  pdat <- cbind(pdat, depvar)
  pdat <- cbind(pdat, state_trends, state_trends_sq)
  pdat <- cbind(pdat, ten_trend, twenty_trend, thirty_trend, ten_trend_sq, twenty_trend_sq, thirty_trend_sq)
  
  
                  # trend1_al, trend1_ar, trend1_ga, trend1_ia, 
                  # trend1_il, trend1_in, trend1_ks, trend1_ky, trend1_md, trend1_mi, 
                  # trend1_mn, trend1_mo, trend1_ms, trend1_mt, trend1_nc, trend1_nd, 
                  # trend1_ne, trend1_oh, trend1_ok, trend1_sc, trend1_sd, trend1_tn, 
                  # trend1_va, trend1_wi,
                  # trend2_al, trend2_ar, trend2_ga, trend2_ia, 
                  # trend2_il, trend2_in, trend2_ks, trend2_ky, trend2_md, trend2_mi, 
                  # trend2_mn, trend2_mo, trend2_ms, trend2_mt, trend2_nc, trend2_nd, 
                  # trend2_ne, trend2_oh, trend2_ok, trend2_sc, trend2_sd, trend2_tn, 
                  # trend2_va, trend2_wi)
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

