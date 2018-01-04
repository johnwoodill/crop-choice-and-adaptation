library(rnassqs)
library(tidyverse)

# Get an NASS QuickStats API key: https://quickstats.nass.usda.gov/api

NASSQS_TOKEN = "2BD928ED-10EB-3FB3-9B42-F3F237A067AE"

# State-level Prices ------------------------------------------------------
states <- state.abb
dat <- data.frame()

# Loop through all states, query database, and bind together
for (i in unique(states)){
  cdat <- data.frame()
  ctdat <- data.frame()
  hdat <- data.frame()
  wdat <- data.frame()
  sdat <- data.frame()
  params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="CORN", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      cdat <- data.frame()
      cdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="COTTON", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="COTTON, UPLAND - PRICE RECEIVED, MEASURED IN $ / LB")
    b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      ctdat <- data.frame()
      ctdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="HAY", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="HAY - PRICE RECEIVED, MEASURED IN $ / TON")
    c <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat <- data.frame()
      hdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="WHEAT", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="WHEAT - PRICE RECEIVED, MEASURED IN $ / BU")
    d <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      wdat <- data.frame()
      wdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

      params = list("state_alpha"=i, "agg_level_desc"="STATE","commodity_desc"="SOYBEANS", "source_desc"="SURVEY", "statisticcat_desc" = "PRICE RECEIVED", "short_desc"="SOYBEANS - PRICE RECEIVED, MEASURED IN $ / BU")
    e <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      sdat <- data.frame()
      sdat <- nassqs_parse(req)
      check <- 1
    },error=function(e) e)

    dat <- rbind(dat, cdat, ctdat, hdat, wdat, sdat)
    # dat <- filter(dat, data.reference_period_desc == "MARKETING YEAR")
    print(i)
}

# If only monthly observations exist, then use average
retdat <- dat
dat$data.Value <- as.numeric(dat$data.Value)

newdat <- dat %>%
  group_by(data.year, data.state_alpha, data.short_desc) %>%
  summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                           data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                           mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
  ungroup()

# Tidy up data
newdat <- select(newdat, data.year, data.state_alpha, data.short_desc, data.Value)
names(newdat) <- c("year", "state", "type", "value")
newdat <- spread(newdat, type, value)
names(newdat) <- c("year", "state", "corn_nprice", "cotton_nprice", "hay_nprice", "soybean_nprice", "wheat_nprice")
newdat$year <- as.integer(newdat$year)
newdat$state <- tolower(newdat$state)
newdat <- do.call(data.frame, lapply(newdat, function(x) replace(x, is.na(x), 0)))

# Save to 'data/'
saveRDS(newdat, "/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/data/crop_statelevel_prices.RDS")

# NASS Census Farmland Acres ----------------------------------------------------------

states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  
# Get acres harvested
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="AG LAND", "source_desc"="CENSUS", "short_desc"="AG LAND, CROPLAND - ACRES")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      censusdat = nassqs_parse(req)
      check <- 1
    },error=function(e) e)
    dat <- rbind(dat, censusdat)
    print(i)

  }

census_dat <- dat
census_dat <- filter(census_dat, data.short_desc == "AG LAND, CROPLAND - ACRES")

# Remove comma
census_dat$data.Value <- as.numeric(gsub(",", "", census_dat$data.Value))

#  Convert to state-county fipes
census_dat$fips <- paste(census_dat$data.state_fips_code, census_dat$data.county_code, sep = "")

# If only monthly observations exist, then use average
 census_dat <- census_dat %>%
   group_by(data.year, data.state_alpha, data.short_desc, fips) %>%
   summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                            data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                            mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
   ungroup()



# Tidy up data
census_dat <- select(census_dat, data.year, data.state_alpha, fips, data.short_desc, data.Value)
census_dat<- spread(census_dat, key = data.short_desc, value = data.Value)
names(census_dat) <- c("year", "state", "fips", "cropland_a")

head(census_dat)
min(census_dat$year)
max(census_dat$year)

write_csv(census_dat, "data/census_harv_cropland_a_1997-2012.csv")







# NASS Corn Data ----------------------------------------------------------


states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  
# Get acres harvested
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="CORN", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="CORN, GRAIN - ACRES HARVESTED")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat = nassqs_parse(req)
      check <- 1
    },error=function(e) e)
  
  # Get Production
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="CORN", "source_desc"="SURVEY", "statisticcat_desc" = "PRODUCTION", "short_desc"="CORN, GRAIN - PRODUCTION, MEASURED IN BU")
      b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      pdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
  if(check == 2){
    dat <- rbind(dat, hdat, pdat)
    print(i)
  }
  }

corn_dat <- dat

# Remove comma
corn_dat$data.Value <- as.numeric(gsub(",", "", corn_dat$data.Value))

#  Convert to state-county fipes
corn_dat$fips <- paste(corn_dat$data.state_fips_code, corn_dat$data.county_code, sep = "")

# If only monthly observations exist, then use average
corn_dat <- corn_dat %>%
  group_by(data.year, data.state_alpha, data.short_desc, fips) %>%
  summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                           data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                           mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
  ungroup()

# Tidy up data
corn_dat <- select(corn_dat, data.year, data.state_alpha, fips, data.short_desc, data.Value)
corn_dat<- spread(corn_dat, key = data.short_desc, value = data.Value)
names(corn_dat) <- c("year", "state", "fips", "corn_grain_a", "corn_grain_p")

head(corn_dat)
min(corn_dat$year)
max(corn_dat$year)

write_csv(corn_dat, "data/corn_1910-2016.csv")



# NASS Cotton Data --------------------------------------------------------

states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  
# Get acres harvested
    params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="COTTON", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="COTTON, UPLAND - ACRES HARVESTED")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat = nassqs_parse(req)
      check <- 1
    },error=function(e) e)
  
  # Get Production
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="COTTON", "source_desc"="SURVEY", "statisticcat_desc" = "PRODUCTION", "short_desc"="COTTON, UPLAND - PRODUCTION, MEASURED IN 480 LB BALES")
      b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      pdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
  if(check == 2){
    dat <- rbind(dat, hdat, pdat)
    print(i)
  }
  }

cotton_dat <- dat

# Remove comma
cotton_dat$data.Value <- as.numeric(gsub(",", "", cotton_dat$data.Value))
cotton_dat <- filter(cotton_dat, !is.na(data.Value))

#  Convert to state-county fipes
cotton_dat$fips <- paste(cotton_dat$data.state_fips_code, cotton_dat$data.county_code, sep = "")

# If only monthly observations exist, then use average
cotton_dat <- cotton_dat %>%
  group_by(data.year, data.state_alpha, data.short_desc, fips) %>%
  summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                           data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                           mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
  ungroup()

# Tidy up data
cotton_dat <- select(cotton_dat, data.year, data.state_alpha, fips, data.short_desc, data.Value)
cotton_dat<- spread(cotton_dat, key = data.short_desc, value = data.Value)
names(cotton_dat) <- c("year", "state", "fips", "cotton_a", "cotton_p")

head(cotton_dat)
min(cotton_dat$year)
max(cotton_dat$year)

write_csv(cotton_dat, "data/cotton_1919-2016.csv")



# NASS Hay Data -----------------------------------------------------------

states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  
# Get acres harvested
    params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="HAY", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="HAY - ACRES HARVESTED")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat = nassqs_parse(req)
      check <- 1
    },error=function(e) e)
  
  # Get Production
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="HAY", "source_desc"="SURVEY", "statisticcat_desc" = "PRODUCTION", "short_desc"="HAY - PRODUCTION, MEASURED IN TONS")
      b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      pdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
  if(check == 2){
    dat <- rbind(dat, hdat, pdat)
    print(i)
  }
  }

hay_dat <- dat

# Remove comma
hay_dat$data.Value <- as.numeric(gsub(",", "", hay_dat$data.Value))

#  Convert to state-county fipes
hay_dat$fips <- paste(hay_dat$data.state_fips_code, hay_dat$data.county_code, sep = "")

# If only monthly observations exist, then use average
hay_dat <- hay_dat %>%
  group_by(data.year, data.state_alpha, data.short_desc, fips) %>%
  summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                           data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                           mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
  ungroup()

# Tidy up data
hay_dat <- select(hay_dat, data.year, data.state_alpha, fips, data.short_desc, data.Value)
hay_dat <- spread(hay_dat, key = data.short_desc, value = data.Value)
names(hay_dat) <- c("year", "state", "fips", "hay_a", "hay_p")

head(hay_dat)
min(hay_dat$year)
max(hay_dat$year)

write_csv(hay_dat, "data/hay_1918-2008.csv")



# NASS Wheat Data ---------------------------------------------------------

states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  
# Get acres harvested
    params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="WHEAT", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="WHEAT - ACRES HARVESTED")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat = nassqs_parse(req)
      check <- 1
    },error=function(e) e)
  
  # Get Production
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="WHEAT", "source_desc"="SURVEY", "statisticcat_desc" = "PRODUCTION", "short_desc"="WHEAT - PRODUCTION, MEASURED IN BU")
      b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      pdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
  if(check == 2){
    dat <- rbind(dat, hdat, pdat)
    print(i)
  }
  }

wheat_dat <- dat

# Remove comma
wheat_dat$data.Value <- as.numeric(gsub(",", "", wheat_dat$data.Value))

#  Convert to state-county fipes
wheat_dat$fips <- paste(wheat_dat$data.state_fips_code, wheat_dat$data.county_code, sep = "")

# If only monthly observations exist, then use average
wheat_dat <- wheat_dat %>%
  group_by(data.year, data.state_alpha, data.short_desc, fips) %>%
  summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                           data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                           mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
  ungroup()

# Tidy up data
wheat_dat <- select(wheat_dat, data.year, data.state_alpha, fips, data.short_desc, data.Value)
wheat_dat <- spread(wheat_dat, key = data.short_desc, value = data.Value)
names(wheat_dat) <- c("year", "state", "fips", "wheat_a", "wheat_p")

head(wheat_dat)
min(wheat_dat$year)
max(wheat_dat$year)

write_csv(wheat_dat, "data/wheat_1909-2007.csv")


# NASS Soybean Data ---------------------------------------------------------

states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  
# Get acres harvested
    params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="SOYBEANS", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="SOYBEANS - ACRES HARVESTED")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat = nassqs_parse(req)
      check <- 1
    },error=function(e) e)
  
  # Get Production
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="SOYBEANS", "source_desc"="SURVEY", "statisticcat_desc" = "PRODUCTION", "short_desc"="SOYBEANS - PRODUCTION, MEASURED IN BU")
      b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      pdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
  if(check == 2){
    dat <- rbind(dat, hdat, pdat)
    print(i)
  }
  }

soybean_dat <- dat

# Remove comma
soybean_dat$data.Value <- as.numeric(gsub(",", "", soybean_dat$data.Value))

#  Convert to state-county fipes
soybean_dat$fips <- paste(soybean_dat$data.state_fips_code, soybean_dat$data.county_code, sep = "")

# If only monthly observations exist, then use average
soybean_dat <- soybean_dat %>%
  group_by(data.year, data.state_alpha, data.short_desc, fips) %>%
  summarise(data.Value = ifelse("MARKETING YEAR"%in%data.reference_period_desc, 
                           data.Value[data.reference_period_desc=="MARKETING YEAR"], 
                           mean(data.Value[data.reference_period_desc!="MARKETING YEAR"])), data.reference_period_desc="YEAR") %>% 
  ungroup()

# Tidy up data
soybean_dat <- select(soybean_dat, data.year, data.state_alpha, fips, data.short_desc, data.Value)
soybean_dat <- spread(soybean_dat, key = data.short_desc, value = data.Value)
names(soybean_dat) <- c("year", "state", "fips", "soybean_a", "soybean_p")

head(soybean_dat)
min(soybean_dat$year)
max(soybean_dat$year)

write_csv(soybean_dat, "data/soybean_1927-2016.csv")
