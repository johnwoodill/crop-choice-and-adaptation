library(tidyverse)
library(rms)
library(noncensus)
library(maps)
library(lubridate)
library(stringr)
library(foreign)
library(haven)
library(zoo)

setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")


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
#crop_prices$`COTTON, UPLAND - PRICE RECEIVED, MEASURED IN $ / LB` <- crop_prices$`COTTON, UPLAND - PRICE RECEIVED, MEASURED IN $ / LB`*480

crop_prices$cotton_nprice <- crop_prices$cotton_nprice*480

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
#wheat <- read_csv("data/wheat_1909-2007_spring.csv")
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

# Get harvested cropland acres
cropland <- read_csv("data/census_harv_cropland_a_1997-2012.csv")
cropland$fips <- as.numeric(cropland$fips)
cropland$state <- NULL
head(cropland)


# # Merge historical Haines data
hains_dat <- read_dta("data/DustBowl_All_base1910.dta")
hains_dat <- select(hains_dat, year, fips, cropland_harvested)
hains_dat$year <- as.integer(hains_dat$year)
hains_dat$fips <- as.integer(hains_dat$fips)
names(hains_dat) <- c("year", "fips", "harvested_cropland_a")
head(hains_dat)

# expand grid data
fips <- unique(cropdat$fips)
years <- 1910:2012

mdat <- expand.grid(years, fips)
names(mdat) <- c("year", "fips")

# Merge historical with current census data
mdat <- left_join(mdat, hains_dat, by = c("fips", "year")) %>% 
  left_join(cropland, by = c("fips", "year")) %>% 
  mutate(harvested_cropland_a = ifelse(is.na(harvested_cropland_a.x), harvested_cropland_a.y, harvested_cropland_a.x)) %>% 
  select(-harvested_cropland_a.x, -harvested_cropland_a.y)

mdatt <- mdat %>%   
   group_by(fips) %>% 
   arrange(year) %>% 
   mutate(harvested_cropland_a = na.approx(harvested_cropland_a, na.rm = FALSE)) %>% 
  ungroup()
head(mdatt)

cropdat <- left_join(cropdat, mdatt, by = c("fips", "year"))
head(cropdat)

# 
# newdat <- select(dat, year, state, fips, corn_grain_a, corn_grain_y, 
#                  cotton_a, cotton_y,
#                  hay_a, hay_y,
#                  wheat_a, wheat_y)
# names(newdat)[4:11] <- c("corn_grain_a", "corn_grain_p", "cotton_a", "cotton_p", "hay_a", "hay_p", "wheat_a", "wheat_p")
# 
# newdat <- newdat[rowSums(is.na(newdat[,c(4,6,8,10)]))!=4, ]
# fips <- unique(newdat$fips)
# years <- 1910:1997
# 
# mdat <- expand.grid(years, fips)
# names(mdat) <- c("year", "fips")
# mdat <- left_join(mdat, newdat, by = c("year", "fips"))
# 
# mdatt <- mdat %>%   
#   group_by(fips) %>% 
#   arrange(year) %>% 
#   mutate(corn_grain_a = na.approx(corn_grain_a, na.rm = FALSE),
#          corn_grain_p = na.approx(corn_grain_p, na.rm = FALSE),
#          cotton_a = na.approx(cotton_a, na.rm = FALSE),
#          cotton_p= na.approx(cotton_p, na.rm = FALSE),
#          hay_a = na.approx(hay_a, na.rm = FALSE),
#          hay_p = na.approx(hay_p, na.rm = FALSE),
#          wheat_a = na.approx(wheat_a, na.rm = FALSE),
#          wheat_p = na.approx(wheat_p, na.rm = FALSE)) %>% 
#    ungroup()
# mdatt$state <- NULL
# 
# cropdat <- left_join(cropdat, mdatt, by = c("fips", "year")) %>% 
#   mutate(corn_grain_a = ifelse(is.na(corn_grain_a.x), corn_grain_a.y, corn_grain_a.x),
#          corn_grain_p = ifelse(is.na(corn_grain_p.x), corn_grain_p.y, corn_grain_p.x),
#          cotton_a = ifelse(is.na(cotton_a.x), cotton_a.y, cotton_a.x),
#          cotton_p = ifelse(is.na(cotton_p.x), cotton_p.y, cotton_p.x),
#          hay_a = ifelse(is.na(hay_a.x), hay_a.y, hay_a.x),
#          hay_p = ifelse(is.na(hay_p.x), hay_p.y, hay_p.x),
#          wheat_a = ifelse(is.na(wheat_a.x), wheat_a.y, wheat_a.x),
#          wheat_p = ifelse(is.na(wheat_p.x), wheat_p.y, wheat_p.x)) %>% 
#   select(-corn_grain_a.x, -corn_grain_a.y, -corn_grain_p.x, -corn_grain_p.y,
#          -cotton_a.x, -cotton_a.y, -cotton_p.x, -cotton_p.y,
#          -hay_a.x, -hay_a.y, -hay_p.x, -hay_p.y,
#          -wheat_a.x, -wheat_a.y, -wheat_p.x, -wheat_p.y)
# 
# # Interpolated historical data and new data
# cropdat <- cropdat %>%   
#   group_by(fips) %>% 
#   arrange(year) %>% 
#   mutate(corn_grain_a = na.approx(corn_grain_a, na.rm = FALSE),
#          corn_grain_p = na.approx(corn_grain_p, na.rm = FALSE),
#          cotton_a = na.approx(cotton_a, na.rm = FALSE),
#          cotton_p= na.approx(cotton_p, na.rm = FALSE),
#          hay_a = na.approx(hay_a, na.rm = FALSE),
#          hay_p = na.approx(hay_p, na.rm = FALSE),
#          wheat_a = na.approx(wheat_a, na.rm = FALSE),
#          wheat_p = na.approx(wheat_p, na.rm = FALSE),
#          soybean_a = na.approx(soybean_a, na.rm = FALSE),
#          soybean_p = na.approx(soybean_p, na.rm = FALSE)) %>% 
#    ungroup()
# #head(testdat)

# Aggregate county-level degree days -----------------------------------------------

 dd <- read_csv("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/data/fips_degree_days_1900-2013.csv")
 prec <- read_csv("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/data/fips_precipitation_1900-2013.csv")

 dd$year <- as.integer(dd$year)
 dd$fips <- as.integer(dd$fips)
 dd_dat <- left_join(dd, prec, by = c("fips", "year", "month"))
 dd_dat <- filter(dd_dat, month >= 3 & month <= 9)
 #dd_dat <- filter(dd_dat, month <= 5 | month >= 9)
 dd_dat$X1 <- NULL
 
 dd_dat <- dd_dat %>%
   group_by(year, fips) %>%
   summarise(dday0C = sum(dday0C),
            dday1C = sum(dday1C),
            dday2C = sum(dday2C),
            dday3C = sum(dday3C),
            dday4C = sum(dday4C),
            dday5C = sum(dday5C),
            dday6C = sum(dday6C),
            dday7C = sum(dday7C),
            dday8C = sum(dday8C),
            dday9C = sum(dday9C),
            dday10C = sum(dday10C),
            dday11C = sum(dday11C),
            dday12C = sum(dday12C),
            dday13C = sum(dday13C),
            dday14C = sum(dday14C),
            dday15C = sum(dday15C),
            dday16C = sum(dday16C),
            dday17C= sum(dday17C),
            dday18C = sum(dday18C),
            dday19C = sum(dday19C),
            dday20C = sum(dday20C),
            dday21C = sum(dday21C),
            dday22C = sum(dday22C),
            dday23C = sum(dday23C),
            dday24C = sum(dday24C),
            dday25C = sum(dday25C),
            dday26C = sum(dday26C),
            dday27C = sum(dday27C),
            dday28C = sum(dday28C),
            dday29C = sum(dday29C),
            dday30C = sum(dday30C),
            dday31C = sum(dday31C),
            dday32C = sum(dday32C),
            dday33C = sum(dday33C),
            dday34C = sum(dday34C),
            dday35C = sum(dday35C),
            ndday0C = sum(ndday0C),
            prec = sum(ppt),
            tavg = mean(tavg),
            tmax = mean(tmax),
            tmin = mean(tmin))



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
 # 


# Merge ag prices, ag crop data, and degree day data ----------------------

fulldat <- left_join(cropdat, crop_prices, by = c("year", "state"))

# Yield
fulldat$corn_yield <- fulldat$corn_grain_p/fulldat$corn_grain_a
fulldat$cotton_yield <- fulldat$cotton_p/fulldat$cotton_a
fulldat$hay_yield <- fulldat$hay_p/fulldat$hay_a
fulldat$wheat_yield <- fulldat$wheat_p/fulldat$wheat_a
fulldat$soybean_yield <- fulldat$soybean_p/fulldat$soybean_a

# # Real revenue per acre
# fulldat$corn_rrev <- (fulldat$corn_grain_p*fulldat$corn_rprice)/fulldat$corn_grain_a
# fulldat$cotton_rrev <- (fulldat$cotton_p*fulldat$cotton_rprice)/fulldat$cotton_a
# fulldat$hay_rrev <- (fulldat$hay_p*fulldat$hay_rprice)/fulldat$hay_a
# fulldat$wheat_rrev <- (fulldat$wheat_p*fulldat$wheat_rprice)/fulldat$wheat_a
# fulldat$soybean_rrev <- (fulldat$soybean_p*fulldat$soybean_rprice)/fulldat$soybean_a

# Real revenue per acre constant price
fulldat$corn_rrev <- (fulldat$corn_grain_p*mean(fulldat$corn_rprice, na.rm = TRUE))/fulldat$corn_grain_a
fulldat$cotton_rrev <- (fulldat$cotton_p*mean(fulldat$cotton_rprice, na.rm = TRUE))/fulldat$cotton_a
fulldat$hay_rrev <- (fulldat$hay_p*mean(fulldat$hay_rprice, na.rm = TRUE))/fulldat$hay_a
fulldat$wheat_rrev <- (fulldat$wheat_p*mean(fulldat$wheat_rprice, na.rm = TRUE))/fulldat$wheat_a
fulldat$soybean_rrev <- (fulldat$soybean_p*mean(fulldat$soybean_rprice, na.rm = TRUE))/fulldat$soybean_a

# Nominal rev
fulldat$corn_nrev <- (fulldat$corn_grain_p*fulldat$corn_nprice)/fulldat$corn_grain_a
fulldat$cotton_nrev <- (fulldat$cotton_p*fulldat$cotton_nprice)/fulldat$cotton_a
fulldat$hay_nrev <- (fulldat$hay_p*fulldat$hay_nprice)/fulldat$hay_a 
fulldat$wheat_nrev <- (fulldat$wheat_p*fulldat$wheat_nprice)/fulldat$wheat_a
fulldat$soybean_nrev <- (fulldat$soybean_p*fulldat$soybean_nprice)/fulldat$soybean_a

# Organize before degree day merge
fulldat <- select(fulldat, year, state, fips, lat, long, gdp_price_def, harvested_cropland_a,
                  corn_grain_a, corn_grain_p, corn_yield, corn_nprice, corn_rprice, corn_rrev, corn_nrev,
                  cotton_a, cotton_p, cotton_yield, cotton_nprice, cotton_rprice, cotton_rrev, cotton_nrev,
                  hay_a, hay_p, hay_yield, hay_nprice, hay_rprice, hay_rrev, hay_nrev,
                  wheat_a, wheat_p, wheat_yield, wheat_nprice, wheat_rprice, wheat_rrev, wheat_nrev,
                  soybean_a, soybean_p, soybean_yield, soybean_nprice, soybean_rprice, soybean_rrev, soybean_nrev)

# Merge degree days
fulldat <- left_join(fulldat, dd_dat, by = c("year", "fips"))

# Soil data
#soildat <- readRDS("data/soilData.rds")

# Merge population and land data
# popland <- readRDS("data/pop_ipc.rds")
# fulldat <- left_join(fulldat, popland, by = c("year", "fips"))

# Convert inf to NA
fulldat <- do.call(data.frame,lapply(fulldat, function(x) replace(x, is.infinite(x),NA)))


# # Label names
# attr(fulldat$year, "label") <- "year"
# attr(fulldat$state, "label") <- "state"
# attr(fulldat$fips, "label") <- "fips code"
# attr(fulldat$lat, "label") <- "latitude"
# attr(fulldat$long, "label") <- "longitude"
# attr(fulldat$gdp_price_def, "label") <- "gdp price def (base = 2010)"
# attr(fulldat$corn_grain_a, "label") <- "corn grain acres"
# attr(fulldat$corn_grain_p, "label") <- "corn grain production"
# attr(fulldat$corn_yield, "label") <- "corn yield (p/a)"
# attr(fulldat$corn_nprice, "label") <- "nominal corn price"
# attr(fulldat$corn_rprice, "label") <- "real corn price"
# attr(fulldat$corn_rrev, "label") <- "real corn revenue per acre"
# attr(fulldat$corn_nrev, "label") <- "nominal corn revenue per acre"
# 
# attr(fulldat$cotton_a, "label") <- "cotton acres"
# attr(fulldat$cotton_p, "label") <- "cotton production"
# attr(fulldat$cotton_yield, "label") <- "cotton yield (p/a)"
# attr(fulldat$cotton_nprice, "label") <- "nominal cotton price"
# attr(fulldat$cotton_rprice, "label") <- "real cotton price"
# attr(fulldat$cotton_rrev, "label") <- "real cotton revenue per acre"
# attr(fulldat$cotton_nrev, "label") <- "nominal cotton revenue per acre"
# 
# attr(fulldat$hay_a, "label") <- "hay acres"
# attr(fulldat$hay_p, "label") <- "hay production"
# attr(fulldat$hay_yield, "label") <- "hay yield (p/a)"
# attr(fulldat$hay_nprice, "label") <- "nominal hay price"
# attr(fulldat$hay_rprice, "label") <- "real hay price"
# attr(fulldat$hay_rrev, "label") <- "real hay revenue per acre"
# attr(fulldat$hay_nrev, "label") <- "nominal hay revenue per acre"
# 
# attr(fulldat$wheat_a, "label") <- "wheat acres"
# attr(fulldat$wheat_p, "label") <- "wheat production"
# attr(fulldat$wheat_yield, "label") <- "wheat yield (p/a)"
# attr(fulldat$wheat_nprice, "label") <- "nominal wheat price"
# attr(fulldat$wheat_rprice, "label") <- "real wheat price"
# attr(fulldat$wheat_rrev, "label") <- "real wheat revenue per acre"
# attr(fulldat$wheat_nrev, "label") <- "nominal wheat revenue per acre"
# 
# attr(fulldat$soybean_a, "label") <- "soybean acres"
# attr(fulldat$soybean_p, "label") <- "soybean production"
# attr(fulldat$soybean_yield, "label") <- "soybean yield (p/a)"
# attr(fulldat$soybean_nprice, "label") <- "nominal soybean price"
# attr(fulldat$soybean_rprice, "label") <- "real soybean price"
# attr(fulldat$soybean_rrev, "label") <- "real soybean revenue per acre"
# attr(fulldat$soybean_nrev, "label") <- "nominal soybean revenue per acre"

# attr(fulldat$dday0C, "label") <- "degree day 0c"
# attr(fulldat$dday1C, "label") <- "degree day 1C"
# attr(fulldat$dday2C, "label") <- "degree day 2C"
# attr(fulldat$dday3C, "label") <- "degree day 3C"
# attr(fulldat$dday4C, "label") <- "degree day 4C"
# attr(fulldat$dday5C, "label") <- "degree day 5C"
# attr(fulldat$dday6C, "label") <- "degree day 6C"
# attr(fulldat$dday7C, "label") <- "degree day 7C"
# attr(fulldat$dday8C, "label") <- "degree day 8C"
# attr(fulldat$dday9C, "label") <- "degree day 9C"
# attr(fulldat$dday10C, "label") <- "degree day 10C"
# attr(fulldat$dday11C, "label") <- "degree day 11C"
# attr(fulldat$dday12C, "label") <- "degree day 12C"
# attr(fulldat$dday13C, "label") <- "degree day 13C"
# attr(fulldat$dday14C, "label") <- "degree day 14C"
# attr(fulldat$dday15C, "label") <- "degree day 15C"
# attr(fulldat$dday16C, "label") <- "degree day 16C"
# attr(fulldat$dday17C, "label") <- "degree day 17C"
# attr(fulldat$dday18C, "label") <- "degree day 18C"
# attr(fulldat$dday19C, "label") <- "degree day 19C"
# attr(fulldat$dday20C, "label") <- "degree day 20C"
# attr(fulldat$dday21C, "label") <- "degree day 21C"
# attr(fulldat$dday22C, "label") <- "degree day 22C"
# attr(fulldat$dday23C, "label") <- "degree day 23C"
# attr(fulldat$dday24C, "label") <- "degree day 24C"
# attr(fulldat$dday25C, "label") <- "degree day 25C"
# attr(fulldat$dday26C, "label") <- "degree day 26C"
# attr(fulldat$dday27C, "label") <- "degree day 27C"
# attr(fulldat$dday28C, "label") <- "degree day 28C"
# attr(fulldat$dday29C, "label") <- "degree day 29C"
# attr(fulldat$dday30C, "label") <- "degree day 30C"
# attr(fulldat$dday31C, "label") <- "degree day 31C"
# attr(fulldat$dday32C, "label") <- "degree day 32C"
# attr(fulldat$dday33C, "label") <- "degree day 33C"
# attr(fulldat$dday34C, "label") <- "degree day 34C"
# attr(fulldat$dday35C, "label") <- "degree day 35C"
# attr(fulldat$ndday0C, "label") <- "degree days < 0C"
# attr(fulldat$prec, "label") <- "precipitation"
# attr(fulldat$tmin, "label") <- "minimum temp"
# attr(fulldat$tmax, "label") <- "maximum temp"
# attr(fulldat$tavg, "label") <- "average temp"
# attr(fulldat$population, "label") <- "population"
# attr(fulldat$pop_dens, "label") <- "population density per sq mi"
# attr(fulldat$land_sqm, "label") <- "county area sq mile"
# attr(fulldat$ipc, "label") <- "income per capita"

#write.csv(fulldat, "data/full_ag_data.csv", row.names = FALSE)

# # Balance panel
# balanced<-function(data, ID, TIME, VARS, required=c("all","shared")) {
#     if(is.character(ID)) {
#         ID <- match(ID, names(data))
#     }
#     if(is.character(TIME)) {
#         TIME <- match(TIME, names(data))
#     }
#     if(missing(VARS)) { 
#         VARS <- setdiff(1:ncol(data), c(ID,TIME))
#     } else if (is.character(VARS)) {
#         VARS <- match(VARS, names(data))
#     }
#     required <- match.arg(required)
#     idf <- do.call(interaction, c(data[, ID, drop=FALSE], drop=TRUE))
#     timef <- do.call(interaction, c(data[, TIME, drop=FALSE], drop=TRUE))
#     complete <- complete.cases(data[, VARS])
#     tbl <- table(idf[complete], timef[complete])
#     if (required=="all") {
#         keep <- which(rowSums(tbl==1)==ncol(tbl))
#         idx <- as.numeric(idf) %in% keep
#     } else if (required=="shared") {
#         keep <- which(colSums(tbl==1)==nrow(tbl))
#         idx <- as.numeric(timef) %in% keep
#     }
#     data[idx, ]
# }
# bd <- select(fulldat, fips, year, corn_grain_a, cotton_a, hay_a, wheat_a, soybean_a)
# #bd <- make.pbalanced(bd, "fill", index = c("fips", "year"))
# test <- bd[complete.cases(bd),]
# View(test)
# head(bd)
# 
# mw <- function(x) min(which(!is.na(x)))
# bdd <- bd %>% 
#   group_by(fips) %>%
#   arrange(year) %>% 
#   summarise(corn = year[mw(corn_grain_a)],
#             cotton = year[mw(cotton_a)],
#             hay = year[mw(hay_a)],
#             wheat = year[mw(wheat_a)],
#             soybean = year[mw(soybean_a)])
#   
# head(bdd)
# table(bdd$cotton)
# test <- filter(bdd, cotton == 1943)
# head(test)

# Filter only counties with acres in at least all five crops



data <- filter(fulldat, state %in% c("mt", "nd", "sd") | abs(long) <= 100)
unique(data$state)
data <- filter(data, year >= 1950 & year <= 2010)
saveRDS(data, "data/full_ag_data.rds")
source("map_of_counties.R")
data <- filter(data, fips %in% fipss)


# dat <- data %>% 
#   group_by(fips) %>% 
#   summarise(corn_a_c = length(which(!is.na(corn_grain_a))),
#             cotton_a_c = length(which(!is.na(cotton_a))),
#             hay_a_c = length(which(!is.na(hay_a))),
#             wheat_a_c = length(which(!is.na(wheat_a))),
#             soybean_a_c = length(which(!is.na(soybean_a))))
# 
# dat <- filter(dat, corn_a_c >= 1 &
#                 cotton_a_c >= 1 &
#                 hay_a_c >= 1 &
#                 wheat_a_c >= 1 &
#                 soybean_a_c >= 1)
# 
# fips.dat <- unique(dat$fips)
# length(fips.dat)
# data <- filter(data, fips %in% fips.dat)
# table(data$year)
# 
# mw <- function(x) min(which(!is.na(x)))
# bdd <- data %>% 
#   group_by(fips) %>%
#   arrange(year) %>% 
#   summarise(corn = year[mw(corn_grain_a)],
#             cotton = year[mw(cotton_a)],
#             hay = year[mw(hay_a)],
#             wheat = year[mw(wheat_a)],
#             soybean = year[mw(soybean_a)])
# 
# head(bdd)


# corn <- filter(fulldat, corn_grain_a >= 0)
# corn.y <- data.frame(table(corn$year))
# 
# cotton <- filter(fulldat, cotton_a >= 0)
# cotton.y <- data.frame(table(cotton$year))
# 
# 
# hay <- filter(fulldat, hay_a >= 0)
# hay.y <- data.frame(table(hay$year))
# 
# wheat <- filter(fulldat, wheat_a >= 0)
# wheat.y <- data.frame(table(wheat$year))
# 
# soybean <- filter(fulldat, soybean_a >= 0)
# soybean.y <- data.frame(table(soybean$year))
# 
# d <- left_join(corn.y, cotton.y, by = "Var1") %>% 
#   left_join(., hay.y, by = "Var1") %>% 
#   left_join(., wheat.y, by = "Var1") %>% 
#   left_join(., soybean.y, by = "Var1")
# 
# names(d) <- c("year", "corn", "cotton", "hay", "wheat", "soybean")
# d$c.diff <- d$corn - d$cotton


saveRDS(data, "data/full_ag_data.rds")
fulldat <- readRDS("data/full_ag_data.rds")



