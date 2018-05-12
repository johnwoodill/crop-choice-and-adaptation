library(tidyverse)
library(RcppRoll)
library(rms)
library(noncensus)
library(maps)
library(lubridate)
library(stringr)
library(lfe)
library(foreign)
library(haven)
library(zoo)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")
source("R/iv_temp.R")

dummyCreator <- function(invec, prefix = NULL) {
     L <- length(invec)
     ColNames <- sort(unique(invec))
     M <- matrix(0L, ncol = length(ColNames), nrow = L,
                 dimnames = list(NULL, ColNames))
     M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
     if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
     M
}

# Function to extract data


data(county.fips) 

county.fips$state <- sapply(str_split(county.fips$polyname, ","),'[',1)

county.fips$county <- sapply(str_split(county.fips$polyname, ","),'[',2)
county.fips <- select(county.fips, fips, county, state)


data(zip_codes)
zip_codes <- select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("county", "lat", "long")
zip_codes <- zip_codes %>% 
  group_by(county) %>% 
  summarise(lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE))


# Function to extract data
# Extract NASS crop data at state level
extract_d_state <- function(x){
  names(x) <- tolower(names(x))
  crop <- x
  crop$state <- tolower(crop$state)
  crop <- left_join(crop, stateabb, by = c("state"="name"))
  crop$state <- NULL
  names(crop) <- c("year", "data_item", "value", "state")
  crop <- select(crop, year, state, data_item, value)
  crop$row <- 1:nrow(crop)   # unique identifer
  crop <- spread(crop, data_item, value = value, fill = NA)
  crop$row <- NULL
  return(crop)
}

# Extract NASS crop data at county level
extract_d_county <- function(x){
  names(x) <- c("year", "state", "county", "data_item", "value")
  crop <- x
  crop$state <- tolower(crop$state)
  crop$county <- tolower(crop$county)
  crop$row <- 1:nrow(crop)   # unique identifer
  crop <- spread(crop, data_item, value = value, fill = NA)
  crop$row <- NULL
  #crop <- filter(crop, year >= 1900)
  #crop <- crop[,c(1,2,4,7,10,13,16)]
  crop <- crop %>% 
    group_by(state, county, year) %>% 
    summarise_each(funs(sum(., na.rm=TRUE))) 
  return(crop)
}

# Get all combinations of years and states
data(states)
stateabb <- select(states, state, name)
stateabb$state <- tolower(stateabb$state)
stateabb$name <- tolower(stateabb$name)

# Import crop prices from get_NASS_data.R
crop_prices <- readRDS("data/crop_statelevel_prices.RDS")

# Remove cotton prices from USDA and us national prices
# No upland cotton prices east of 100th before 1964
cotton_prices_lower <- select(crop_prices, year, state) %>% select(year, state) %>% filter(year < 1964) 
cotton_prices_upper <- select(crop_prices, year, state, cotton_nprice) %>% filter(year >= 1964) 
crop_prices$cotton_nprice <- NULL

# USDA national cotton prices
cotton_prices <- read_csv("data/US_cotton_prices.csv")
cotton_prices_lower <- left_join(cotton_prices_lower, cotton_prices, by = "year") 
cotton_prices_merge <- rbind(cotton_prices_lower, cotton_prices_upper)

crop_prices <- left_join(crop_prices, cotton_prices_merge, by = c("year", "state"))

# Full grid
newgrid <- expand.grid(tolower(states$state), 1900:2016, stringsAsFactors = FALSE)
names(newgrid) <- c("state", "year")

# Join grid to prices
crop_prices <- left_join(newgrid, crop_prices, by = c("year", "state"))

crop_prices <- filter(crop_prices, year >= 1900)

# NASS data description
# corn_price: $/bu corn_p: bu
# cotton_price: $/lb   cotton_p: 480lb bales
# hay_price: $/ton    hay_p: tons
# wheat_price: $/bu   wheat_p: bu
# soybean_price: $/bu   soybean_p: bu

# Adjust cotton price
# crop_prices$cotton_nprice <- crop_prices$cotton_nprice*480

# Nominal to Real prices using GDP product index deflator (base=2010)
def <- read.csv("data/gdp_def_base2009.csv")

# Adjust prices to 2010 levels
def$gdp_price_def <- def$gdp_price_def/1.01221

# Merge crop prices and deflator data
crop_prices <- left_join(crop_prices, def, by = "year")

# Adjust prices to real
crop_prices$corn_rprice <- (crop_prices$corn_nprice*crop_prices$gdp_price_def/100)
crop_prices$cotton_rprice <- (crop_prices$cotton_nprice*crop_prices$gdp_price_def/100)
crop_prices$hay_rprice <- (crop_prices$hay_nprice*crop_prices$gdp_price_def/100)
crop_prices$wheat_rprice <- (crop_prices$wheat_nprice*crop_prices$gdp_price_def/100)
crop_prices$soybean_rprice <- (crop_prices$soybean_nprice*crop_prices$gdp_price_def/100)

# Select variables
crop_prices <- as.data.frame(crop_prices)
crop_prices <- select(crop_prices, year, state, gdp_price_def, wheat_nprice, wheat_rprice, corn_nprice, corn_rprice, 
                      hay_nprice, hay_rprice, soybean_nprice, soybean_rprice, cotton_nprice, cotton_rprice)


# Aggregate county level ag data ------------------------------------------

# Load crop data (balanced years 1927-2007)
corn <- read_csv("data/corn_1910-2016.csv")
corn$state <- tolower(corn$state)
corn$fips <- as.integer(corn$fips)

cotton <- read_csv("data/cotton_1919-2016.csv")
cotton$state <- tolower(cotton$state)
cotton$fips <- as.integer(cotton$fips)

hay <- read_csv("data/hay_1918-2008.csv")
hay$state <- tolower(hay$state)
hay$fips <- as.integer(hay$fips)

wheat <- read_csv("data/wheat_1909-2007.csv")
# wheat <- read_csv("data/wheat_1909-2007_spring.csv")
wheat$state <- tolower(wheat$state)
wheat$fips <- as.integer(wheat$fips)

soybean <- read_csv("data/soybean_1927-2016.csv")
soybean$state <- tolower(soybean$state)
soybean$fips <- as.integer(soybean$fips)

# Get all combinations of years and states
newgrid <- expand.grid(county.fips$fips, 1900:2016)#
mergdat <- data.frame(county = county.fips$fips, name = county.fips$state)
statedat <- select(states, state, name)
statedat$state <- tolower(statedat$state)
statedat$name <- tolower(statedat$name)
mergdat <- left_join(mergdat, statedat, by = c("name"))
mergdat <- select(mergdat, county, state)
names(newgrid) <- c("county", "year")
newgrid <- left_join(newgrid, mergdat, by = "county")
newgrid <- left_join(newgrid, zip_codes, by = "county")
newgrid <- newgrid[!duplicated(newgrid[,1:3]),]
newgrid$county <- as.character(newgrid$county)
names(newgrid) <- c("fips", "year", "state", "lat", "long")
newgrid$fips <- as.integer(newgrid$fips)

# Merge crop data
cropdat <- left_join(newgrid, corn, by = c("year", "state", "fips"))
cropdat <- left_join(cropdat, cotton, by = c("state", "fips", "year"))
cropdat <- left_join(cropdat, hay, by = c("state", "fips", "year"))
cropdat <- left_join(cropdat, wheat, by = c("state", "fips", "year"))
cropdat <- left_join(cropdat, soybean, by = c("state", "fips", "year"))

# Extract NASS crop data at county level
extract_d_county <- function(x){
  x <- select(x, Year, fips, `Data Item`, Value)
  names(x) <- c("year", "fips", "data_item", "value")
  crop <- x
  # crop$state <- tolower(crop$state)
  # crop$county <- tolower(crop$county)
  crop$fips <- as.numeric(crop$fips)
  crop$row <- 1:nrow(crop)   # unique identifer
  crop <- spread(crop, data_item, value = value, fill = NA)
  crop$row <- NULL
  #crop <- filter(crop, year >= 1900)
  #crop <- crop[,c(1,2,4,7,10,13,16)]
  crop <- crop %>% 
    group_by(fips, year) %>% 
    summarise_all(funs(sum(., na.rm=TRUE))) 
  return(crop)
}


# dat <- read_csv("data/1950_acres.csv")
# dat$fips <- paste(dat$`State ANSI`, dat$`County ANSI`, sep = "")
# head(dat)
# test <- extract_d_county(dat)
# names(test) <- c("fips", "year", "corn", "cotton", "hay", "soybean", "wheat")
# head(test)
# test$acres <- rowSums(test[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
# head(test)
# test <- select(test, fips, acres)
# names(test) <- c("region", "value")
# test$region <- as.numeric(test$region)
# head(test)
# test <- as.data.frame(test)
# fipss <- test$region
# # cropdat <- filter(cropdat, fips %in% fipss)

#-----------------------------------------------------
# # Merge historical Haines data
# hdat <- read_dta("data/DustBowl_All_base1910.dta")
# hdat <- select(hdat, year, fips, corn_grain_a, corn_grain_y, cotton_a, cotton_y, hay_a, hay_y, wheat_a, wheat_y)
# 
# #hains_dat <- select(hains_dat, year, fips, cropland_harvested)
# hdat$year <- as.integer(hdat$year)
# hdat$fips <- as.integer(hdat$fips)
# 
# # expand grid data
# fips <- unique(cropdat$fips)
# years <- 1910:2012
# mdat <- expand.grid(years, fips)
# names(mdat) <- c("year", "fips")
# 
# # Merge historical with current census data
#  mdat <- left_join(mdat, hdat, by = c("fips", "year"))
#  head(mdat)
# 
#  names(mdat)[3:10] <- c("corn_grain_a", "corn_grain_p", "cotton_a", "cotton_p", "hay_a", "hay_p", "wheat_a", "wheat_p")
# 
# 
#  mdat <- mdat %>%
#    group_by(fips) %>%
#    arrange(year) %>%
#    mutate(corn_grain_a = na.approx(corn_grain_a, na.rm = FALSE),
#         corn_grain_p = na.approx(corn_grain_p, na.rm = FALSE),
#           cotton_a = na.approx(cotton_a, na.rm = FALSE),
#           cotton_p= na.approx(cotton_p, na.rm = FALSE),
#           hay_a = na.approx(hay_a, na.rm = FALSE),
#         hay_p = na.approx(hay_p, na.rm = FALSE),
#           wheat_a = na.approx(wheat_a, na.rm = FALSE),
#           wheat_p = na.approx(wheat_p, na.rm = FALSE)) %>%
#     ungroup()
#  head(mdat)
# 
# cropdat <- left_join(cropdat, mdat, by = c("fips", "year")) %>%
#    mutate(corn_grain_a = ifelse(is.na(corn_grain_a.x), corn_grain_a.y, corn_grain_a.x),
#           corn_grain_p = ifelse(is.na(corn_grain_p.x), corn_grain_p.y, corn_grain_p.x),
#           cotton_a = ifelse(is.na(cotton_a.x), cotton_a.y, cotton_a.x),
#           cotton_p = ifelse(is.na(cotton_p.x), cotton_p.y, cotton_p.x),
#           hay_a = ifelse(is.na(hay_a.x), hay_a.y, hay_a.x),
#           hay_p = ifelse(is.na(hay_p.x), hay_p.y, hay_p.x),
#           wheat_a = ifelse(is.na(wheat_a.x), wheat_a.y, wheat_a.x),
#           wheat_p = ifelse(is.na(wheat_p.x), wheat_p.y, wheat_p.x)) %>%
#    select(-corn_grain_a.x, -corn_grain_a.y, -corn_grain_p.x, -corn_grain_p.y,
#           -cotton_a.x, -cotton_a.y, -cotton_p.x, -cotton_p.y,
#           -hay_a.x, -hay_a.y, -hay_p.x, -hay_p.y,
#           -wheat_a.x, -wheat_a.y, -wheat_p.x, -wheat_p.y)
#  head(cropdat)
# # # Interpolated historical data and new data
#  cropdat <- cropdat %>%
#    group_by(fips) %>%
#    arrange(year) %>%
#    mutate(corn_grain_a = na.approx(corn_grain_a, na.rm = FALSE),
#           corn_grain_p = na.approx(corn_grain_p, na.rm = FALSE),
#           cotton_a = na.approx(cotton_a, na.rm = FALSE),
#           cotton_p= na.approx(cotton_p, na.rm = FALSE),
#           hay_a = na.approx(hay_a, na.rm = FALSE),
#           hay_p = na.approx(hay_p, na.rm = FALSE),
#           wheat_a = na.approx(wheat_a, na.rm = FALSE),
#           wheat_p = na.approx(wheat_p, na.rm = FALSE),
#           soybean_a = na.approx(soybean_a, na.rm = FALSE),
#           soybean_p = na.approx(soybean_p, na.rm = FALSE)) %>%
#     ungroup()
#  head(cropdat)
#-----------------------------------------------------

# Aggregate county-level degree days -----------------------------------------------

# Full data
# dd <- read_csv("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/data/fips_degree_days_1900-2013.csv")
# dd <- dplyr::select(dd, fips, year, month, dday0C, dday10C, dday30C, tavg)

dd <- readRDS('data/sub_fips_degree_days_1900-2013.rds')

prec <- read_csv("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/data/fips_precipitation_1900-2013.csv")

 dd$year <- as.integer(dd$year)
 dd$fips <- as.integer(dd$fips)
 dd_dat <- left_join(dd, prec, by = c("fips", "year", "month"))
 dd_dat <- filter(dd_dat, month >= 3 & month <= 9)

 dd_dat$X1 <- NULL

 dd_dat <- dd_dat %>%
   group_by(year, fips) %>%
   summarise(dday0C = sum(dday0C),
            dday10C = sum(dday10C),
            dday30C = sum(dday30C),
            prec = sum(ppt),
            tavg = mean(tavg))


# Schlenker and Roberts data
 # dd_dat <- read_dta("data/ddayOverAgAreaByMonth.dta")
 #  dd_dat$year <- as.integer(dd_dat$year)
 #  dd_dat$fips <- as.integer(dd_dat$fips)
 #  dd_dat <- dd_dat %>% 
 #    group_by(year, fips) %>% 
 #    summarise(dday0C = sum(dday0C),
 #               dday8C = sum(dday8C),
 #              dday10C = sum(dday10C),
 #              dday30C = sum(dday30C),
 #              dday32C = sum(dday32C),
 #              dday34C = sum(dday34C),
 #              prec = sum(prec),
 #              tavg = mean(tAvg))
 
 
 # Schlenker data 1900-2016
 # dd_dat <- read_dta("data/FULL_ddayByYearandFips_cropAreaWeighted.dta")
 #  dd_dat$year <- as.integer(dd_dat$year)
 #  dd_dat$fips <- as.integer(dd_dat$fips)
 #  dd_dat <- dd_dat %>%
 #    group_by(year, fips) %>%
 #    summarise(dday0C = sum(dday0C),
 #              dday10C = sum(dday10C),
 #              dday30C = sum(dday30C),
 #              prec = sum(prec),
 #              tavg = mean(tavg))
 # # 

dd_dat$dday0_10 <- dd_dat$dday0C - dd_dat$dday10C
dd_dat$dday10_30 <- dd_dat$dday10C - dd_dat$dday30C
dd_dat$dday30 <- dd_dat$dday30C
dd_dat$prec_sq <- dd_dat$prec^2

dd_dat <- select(dd_dat, year, fips, dday0_10, dday10_30, dday30, prec, prec_sq, tavg)

#--------------------------------------------------
# Roll.mean intervals

# Lag one so current year is not included
dd_dat <- dd_dat %>% 
  group_by(fips) %>% 
  arrange(year) %>% 
  mutate(dday0_10_lag1 = lag(dday0_10),
         dday10_30_lag1 = lag(dday10_30),
         dday30_lag1 = lag(dday30),
         prec_lag1 = lag(prec))

dd_dat <- dd_dat %>% 
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
         prec_sq_rm12 = prec_rm12^2,
         
         dday0_10_rm = roll_mean(dday0_10_lag1, 30, align = "right", fill = "NA"),
         dday10_30_rm = roll_mean(dday10_30_lag1, 10, align = "right", fill = "NA"),
         dday30_rm = roll_mean(dday30_lag1, 7, align = "right", fill = "NA"),
         prec_rm = roll_mean(prec_lag1, 9, align = "right", fill = "NA"),
         prec_sq_rm = prec_rm^2) %>%  
  
  ungroup()

# Lag variables
# for (n in 1:30){
#   lab1 <- paste0("dday0_10_lag", n)
#   lab2 <- paste0("dday10_30_lag", n)
#   lab3 <- paste0("dday30_lag", n)
#   lab4 <- paste0("prec_lag", n)
#   lab5 <- paste0("prec_sq_lag", n)
# 
# 
#   dd_dat <- dd_dat %>%
#     group_by(fips) %>%
#     arrange(year) %>%
#     mutate(!!lab1 := lag(dday0_10, n),
#            !!lab2 := lag(dday10_30, n),
#            !!lab3 := lag(dday30, n),
#            !!lab4 := lag(prec, n),
#            !!lab5 := lag(prec, n)^2)
# }

# Decade averages
dd_dat$ten <- dd_dat$year - (dd_dat$year %% 10)

dd_dat <- dd_dat %>% 
  group_by(fips, ten) %>% 
  mutate(dday0_10_davg = mean(dday0_10, na.rm = TRUE),
         dday10_30_davg = mean(dday10_30, na.rm = TRUE),
         dday30_davg = mean(dday30, na.rm = TRUE),
         prec_davg = mean(prec, na.rm = TRUE),
         prec_sq_davg = prec_davg^2)

# Merge ag prices, ag crop data, and degree day data ----------------------

fulldat <- right_join(cropdat, crop_prices, by = c("year", "state"))
 
# Import region data file
region_dat <- read_csv("data/ResourceRegionCRDfips.csv")
names(region_dat) <- c("fips", "ers_region", "crd")
fulldat <- left_join(fulldat, region_dat, by = "fips")

# Yield
fulldat$corn_yield <- fulldat$corn_grain_p/fulldat$corn_grain_a
fulldat$cotton_yield <- (fulldat$cotton_p*480)/fulldat$cotton_a
fulldat$hay_yield <- fulldat$hay_p/fulldat$hay_a
fulldat$wheat_yield <- fulldat$wheat_p/fulldat$wheat_a
fulldat$soybean_yield <- fulldat$soybean_p/fulldat$soybean_a

# Set revenue equal to zero if NA
fulldat$corn_yield <- ifelse(is.na(fulldat$corn_yield), 0, fulldat$corn_yield)
fulldat$cotton_yield <- ifelse(is.na(fulldat$cotton_yield), 0, fulldat$cotton_yield)
fulldat$hay_yield <- ifelse(is.na(fulldat$hay_yield), 0, fulldat$hay_yield)
fulldat$soybean_yield <- ifelse(is.na(fulldat$soybean_yield), 0, fulldat$soybean_yield)
fulldat$wheat_yield <- ifelse(is.na(fulldat$wheat_yield), 0, fulldat$wheat_yield)

# Real mean revenue per acre constant price
fulldat$corn_mrev <- (fulldat$corn_grain_p*mean(fulldat$corn_rprice, na.rm = TRUE))/fulldat$corn_grain_a
fulldat$cotton_mrev <- (fulldat$cotton_p*480)*mean(fulldat$cotton_rprice, na.rm = TRUE)/fulldat$cotton_a
fulldat$hay_mrev <- (fulldat$hay_p*mean(fulldat$hay_rprice, na.rm = TRUE))/fulldat$hay_a
fulldat$wheat_mrev <- (fulldat$wheat_p*mean(fulldat$wheat_rprice, na.rm = TRUE))/fulldat$wheat_a
fulldat$soybean_mrev <- (fulldat$soybean_p*mean(fulldat$soybean_rprice, na.rm = TRUE))/fulldat$soybean_a

# Organize before degree day merge
fulldat <- select(fulldat, year, state, fips, ers_region, crd, lat, long, gdp_price_def, 
                  corn_grain_a, corn_grain_p, corn_yield, corn_nprice, corn_rprice, corn_mrev, 
                  cotton_a, cotton_p, cotton_yield, cotton_nprice, cotton_rprice, cotton_mrev, 
                  hay_a, hay_p, hay_yield, hay_nprice, hay_rprice, hay_mrev,
                  wheat_a, wheat_p, wheat_yield, wheat_nprice, wheat_rprice, wheat_mrev, 
                  soybean_a, soybean_p, soybean_yield, soybean_nprice, soybean_rprice, soybean_mrev)


# Merge degree days
fulldat <- left_join(fulldat, dd_dat, by = c("year", "fips"))

# Convert inf to NA
fulldat <- do.call(data.frame,lapply(fulldat, function(x) replace(x, is.infinite(x),NA)))


states <-  c("al","ar","ct","dc", "de", "fl","ga","il","in","ia","ks","ky","la","me","md","ma","mi","mn","ms","mo","mt",
"ne","nh","nj","ny","nc","nd","oh","ok","pa","ri","sc","sd","tn","tx","vt","va","wv","wi")

data <- filter(fulldat, state %in% states)

# Keep only those counties with acres in 1950
data$acres <- rowSums(data[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)
check <- filter(data, year == 1950)
check <- filter(check, acres > 0)
check$state <- factor(check$state)
length(unique(check$fips))
unique(check$state)
fipss <- unique(check$fips)
data <- filter(data, fips %in% fipss)
data <- filter(data, year >= unique(check$year))

# Build data set for regression estimates
cropdat <- filter(data, year <= 2010)

cropdat$corn_mrev <- ifelse(is.na(cropdat$corn_mrev), 0, cropdat$corn_mrev)
cropdat$cotton_mrev <- ifelse(is.na(cropdat$cotton_mrev), 0, cropdat$cotton_mrev)
cropdat$hay_mrev <- ifelse(is.na(cropdat$hay_mrev), 0, cropdat$hay_mrev)
cropdat$soybean_mrev <- ifelse(is.na(cropdat$soybean_mrev), 0, cropdat$soybean_mrev)
cropdat$wheat_mrev <- ifelse(is.na(cropdat$wheat_mrev), 0, cropdat$wheat_mrev)

# Log crop revenue
cropdat$ln_rev_corn <- log(1 + cropdat$corn_mrev)
cropdat$ln_rev_cotton <- log(1 + cropdat$cotton_mrev)
cropdat$ln_rev_hay <- log(1 + cropdat$hay_mrev)
cropdat$ln_rev_soybean <- log(1 + cropdat$soybean_mrev)
cropdat$ln_rev_wheat <- log(1 + cropdat$wheat_mrev)

# Set acres to zero
cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)

# Variables
cropdat$rev <- rowSums(cropdat[, c("corn_mrev", "cotton_mrev", "hay_mrev", "soybean_mrev", "wheat_mrev")], na.rm = TRUE)
cropdat$acres <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)

cropdat$ln_rev <- log(1 + cropdat$rev)

# Remove inf to na
is.na(cropdat) <- do.call(cbind, lapply(cropdat, is.infinite))

# Rolling mean through acres to smooth out weights
cropdat <- cropdat %>% 
  group_by(fips) %>% 
  arrange(year) %>% 
  # mutate(w = mean(rev, na.rm = TRUE))
  mutate(w = rollapply(acres, 2, mean, partial = TRUE))

# Build trends
cropdat$trend <- cropdat$year - (min(cropdat$year) - 1)
cropdat$trend_sq <- cropdat$trend^2

# Crop acres as percentage of total
cropdat$p_corn_a <- cropdat$corn_grain_a/cropdat$acres
cropdat$p_cotton_a <- cropdat$cotton_a/cropdat$acres
cropdat$p_hay_a <- cropdat$hay_a/cropdat$acres
cropdat$p_soybean_a <- cropdat$soybean_a/cropdat$acres
cropdat$p_wheat_a <- cropdat$wheat_a/cropdat$acres

# Set acres to zero
cropdat$p_corn_a <- ifelse(is.na(cropdat$p_corn_a), 0, cropdat$p_corn_a)
cropdat$p_cotton_a <- ifelse(is.na(cropdat$p_cotton_a), 0, cropdat$p_cotton_a)
cropdat$p_hay_a <- ifelse(is.na(cropdat$p_hay_a), 0, cropdat$p_hay_a)
cropdat$p_wheat_a <- ifelse(is.na(cropdat$p_wheat_a), 0, cropdat$p_wheat_a)
cropdat$p_soybean_a <- ifelse(is.na(cropdat$p_soybean_a), 0, cropdat$p_soybean_a)

# First estimate between zero and 1
cropdat$cp_corn_a <- (cropdat$p_corn_a + .001)/1.00101
cropdat$cp_cotton_a <- (cropdat$p_cotton_a + .001)/1.00101
cropdat$cp_hay_a <- (cropdat$p_hay_a + .001)/1.00101
cropdat$cp_soybean_a <- (cropdat$p_soybean_a + .001)/1.00101
cropdat$cp_wheat_a <- (cropdat$p_wheat_a + .001)/1.00101

# Calc z-scores
cropdat$z_corn_a <- qnorm(cropdat$cp_corn_a)
cropdat$z_cotton_a <- qnorm(cropdat$cp_cotton_a)
cropdat$z_hay_a <- qnorm(cropdat$cp_hay_a)
cropdat$z_soybean_a <- qnorm(cropdat$cp_soybean_a)
cropdat$z_wheat_a <- qnorm(cropdat$cp_wheat_a)

cropdat <- as.data.frame(cropdat)

# Remove de because 180 total obs.
# cropdat <- filter(cropdat, state != "de")
# cropdat <- filter(cropdat, state != "dc")
# cropdat <- filter(cropdat, state != "me")
# cropdat <- filter(cropdat, state != "nh")
# cropdat <- filter(cropdat, state != "ri")
# cropdat <- filter(cropdat, state != "vt")
# cropdat <- filter(cropdat, state != "nj")
# cropdat <- filter(cropdat, state != "ma")
# cropdat <- filter(cropdat, state != "ct")

cropdat$trend_lat <- cropdat$trend*cropdat$lat
cropdat$trend_long <- cropdat$trend*cropdat$long
cropdat$trend_sq_long <- cropdat$trend_long^2
cropdat$trend_sq_lat <- cropdat$trend_lat^2

# Save cropdat

cropdat <- filter(cropdat, abs(long) <= 100 )
cropdat$state <- factor(cropdat$state)

# Instrument variable weather using climate
# source('R/iv_temp.R')
# 
# dday0_10_fit_iv <- iv_temp('dday0_10', 27, cropdat)
# dday10_30_fit_iv <- iv_temp('dday10_30', 21, cropdat)
# dday30_fit_iv <- iv_temp('dday30', 20, cropdat)
# prec_fit_iv <- iv_temp('prec', 16, cropdat)
# 
# dday0_10_fit_10 <- iv_temp('dday0_10', 10, cropdat)
# dday10_30_fit_10 <- iv_temp('dday10_30', 10, cropdat)
# dday30_fit_10 <- iv_temp('dday30', 10, cropdat)
# prec_fit_10 <- iv_temp('prec', 10, cropdat)
# 
# dday0_10_fit_20 <- iv_temp('dday0_10', 20, cropdat)
# dday10_30_fit_20 <- iv_temp('dday10_30', 20, cropdat)
# dday30_fit_20 <- iv_temp('dday30', 20, cropdat)
# prec_fit_20 <- iv_temp('prec', 20, cropdat)
# 
# dday0_10_fit_30 <- iv_temp('dday0_10', 30, cropdat)
# dday10_30_fit_30 <- iv_temp('dday10_30', 30, cropdat)
# dday30_fit_30 <- iv_temp('dday30', 30, cropdat)
# prec_fit_30 <- iv_temp('prec', 30, cropdat)

# Add IV to data.frame
# cropdat$dday0_10_iv <- dday0_10_fit_iv
# cropdat$dday10_30_iv <- dday10_30_fit_iv
# cropdat$dday30_iv <- dday30_fit_iv
# cropdat$prec_iv <- prec_fit_iv
# cropdat$prec_sq_iv <- prec_fit_iv^2
# 
# cropdat$dday0_10_iv10 <- dday0_10_fit_10
# cropdat$dday10_30_iv10 <- dday10_30_fit_10
# cropdat$dday30_iv10 <- dday30_fit_10
# cropdat$prec_iv10 <- prec_fit_10
# cropdat$prec_sq_iv10 <- prec_fit_10^2
# 
# cropdat$dday0_10_iv20 <- dday0_10_fit_20
# cropdat$dday10_30_iv20 <- dday10_30_fit_20
# cropdat$dday30_iv20 <- dday30_fit_20
# cropdat$prec_iv20 <- prec_fit_20
# cropdat$prec_sq_iv20 <- prec_fit_20^2
# 
# cropdat$dday0_10_iv30 <- dday0_10_fit_30
# cropdat$dday10_30_iv30 <- dday10_30_fit_30
# cropdat$dday30_iv30 <- dday30_fit_30
# cropdat$prec_iv30 <- prec_fit_30
# cropdat$prec_sq_iv30 <- prec_fit_30^2

# Remove lag columns
# cropdat <- cropdat[, -grep('lag', colnames(cropdat))]

# Fix outliers
cropdat$cotton_yield[which((cropdat$cotton_yield)>3000)] <- 366

# Quadratic State-by-year time trends
# Linear
state_trends <- as.data.frame(dummyCreator(cropdat$state, "trend1"))
state_trends$trend <- cropdat$trend
state_trends <- state_trends[, 1:length(state_trends)]*state_trends$trend
state_trends$trend <- NULL

# Quadratic
state_trends_sq <- as.data.frame(dummyCreator(cropdat$state, "trend2"))
state_trends_sq$trend_sq <- cropdat$trend^2
state_trends_sq <- state_trends_sq[, 1:length(state_trends_sq)]*state_trends_sq$trend_sq
state_trends_sq$trend_sq <- NULL

cropdat <- cbind(cropdat, state_trends, state_trends_sq)

trends <- cropdat[, c(grep("trend1", names(cropdat)), grep("trend2", names(cropdat)))]        

paste(names(trends), collapse = " + ")

cropdat <- cropdat %>%
  group_by(year) %>%
  mutate(acres_w = acres/mean(acres, na.rm = TRUE))

cropdat$dday0_10w <- cropdat$dday0_10_rm*cropdat$acres_w
cropdat$dday10_30w <- cropdat$dday10_30_rm*cropdat$acres_w
cropdat$dday30w <- cropdat$dday30_rm*cropdat$acres_w
cropdat$precw <- cropdat$prec*cropdat$acres_w
cropdat$prec_sqw <- cropdat$precw^2

cropdat$dday0_10_rmw <- cropdat$dday0_10_rm*cropdat$acres_w
cropdat$dday10_30_rmw <- cropdat$dday10_30_rm*cropdat$acres_w
cropdat$dday30_rmw <- cropdat$dday30_rm*cropdat$acres_w
cropdat$prec_rmw <- cropdat$prec_rm*cropdat$acres_w
cropdat$prec_sq_rmw <- cropdat$prec_rmw^2

saveRDS(cropdat, "data/full_ag_data.rds")
fulldat <- readRDS("data/full_ag_data.rds")

cropdat <- fulldat




fit <- felm(ln_rev ~ dday0_10w + dday10_30w + dday30w + prec + prec_sq + 
              state:trend +
             dday0_10_rmw + dday10_30_rmw + dday30_rmw + prec_rm + prec_sq_rm
           | fips | 0 | state, data = cropdat)
summary(fit)

sqrt(mean(fit$residuals^2))


test2 <- test %>% 
  group_by(fips) %>% 
  mutate(dday30_rm10 - mean(dday30_rm10, na.rm = TRUE))
fit2 <- felm(ln_rev ~ trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips, data = cropdat)
fit3 <- cropdat$ln_rev - fit2$residuals

ggplot(cropdat, aes(y=fit3, x=dday30_rm10)) + geom_point() + geom_smooth(method  = 'lm')
