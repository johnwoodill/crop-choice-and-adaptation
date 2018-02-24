library(readr)
library(tidyverse)

cropdat <- readRDS("data/full_ag_data.rds")

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

dat <- read_csv("/home/john/Downloads/7B84917F-B11E-3359-87AB-4B0B7817873D.csv")
dat$fips <- paste(dat$`State ANSI`, dat$`County ANSI`, sep = "")
head(dat)
test <- extract_d_county(dat)
names(test) <- c("fips", "year", "corn", "cotton", "hay", "soybean", "wheat")
head(test)
test$acres <- rowSums(test[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
head(test)
test <- select(test, fips, acres)
names(test) <- c("region", "value")
test$region <- as.numeric(test$region)
head(test)
test <- as.data.frame(test)

#-----------------------------------------------------
# Merge historical Haines data
hdat <- read_dta("data/DustBowl_All_base1910.dta")
hdat <- select(hdat, year, state,  fips, corn_grain_a, corn_grain_y, cotton_a, cotton_y, hay_a, hay_y, wheat_a, wheat_y)

#hains_dat <- select(hains_dat, year, fips, cropland_harvested)
hdat$year <- as.integer(hdat$year)
hdat$fips <- as.integer(hdat$fips)

# expand grid data
fips <- unique(cropdat$fips)
years <- 1910:2012

mdat <- expand.grid(years, fips)
names(mdat) <- c("year", "fips")

# Merge historical with current census data
mdat <- left_join(mdat, hdat, by = c("fips", "year"))
head(mdat)

names(mdat)[3:10] <- c("corn_grain_a", "corn_grain_p", "cotton_a", "cotton_p", "hay_a", "hay_p", "wheat_a", "wheat_p")

mdat <- mdat %>%
  group_by(fips) %>%
  arrange(year) %>%
  mutate(corn_grain_a = na.approx(corn_grain_a, na.rm = FALSE),
         corn_grain_p = na.approx(corn_grain_p, na.rm = FALSE),
         cotton_a = na.approx(cotton_a, na.rm = FALSE),
         cotton_p= na.approx(cotton_p, na.rm = FALSE),
         hay_a = na.approx(hay_a, na.rm = FALSE),
         hay_p = na.approx(hay_p, na.rm = FALSE),
         wheat_a = na.approx(wheat_a, na.rm = FALSE),
         wheat_p = na.approx(wheat_p, na.rm = FALSE)) %>%
   ungroup()
head(mdat)

unique(mdat$year)
mdat50 <- filter(mdat, year == 1950)
mdat50 <- filter(mdat50, !is.na(corn_grain_a) & !is.na(cotton_a) & !is.na(hay_a) & !is.na(wheat_a))
mdat50$acres <- rowSums(mdat50[, c("corn_grain_a", "cotton_a", "hay_a", "wheat_a")], na.rm = TRUE)
mdat50 <- filter(mdat50, !is.na(acres))
head(mdat50)
sdiff <- setdiff(mdat50$fips, test$region)
mdat50 <- filter(mdat50, fips %in% sdiff)
head(sdiff)
length(sdiff)

test2 <- select(mdat50, fips, acres)
names(test2) <- c("region", "value")
test2$region <- as.numeric(test2$region)
head(test2)
test2 <- as.data.frame(test2)

outdat <- as.data.frame(rbind(test, test2))
head(outdat)

cornmap <- county_choropleth(test,
                 title      = NULL)

cornmap <- cornmap + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  # xlab("Change 2000's to 1950's Degree Day 30C Residuals (30-year Rolling Mean) \n County FE & National Quadratic Trend with Lat and Long\n
        # dday30_rm_thirty ~ factor(fips) + dday0_10 + dday10_30 + dday30 + prec + prec_sq + \n
               # trend:(lat + long) + trend_sq:(lat + long)") +
  ylab(NULL)  + xlab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA),
                       plot.margin = unit(c(0, 10, 0, 0), "cm")) 
  
cornmap

cropdat50 <- filter(cropdat, year == 1950)
cropdat50 <- select(cropdat50, fips, acres)
names(cropdat50) <- c("region", "value")
head(cropdat50)

checkmap <- county_choropleth(cropdat50,
                 title      = NULL)

checkmap <- checkmap + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  # xlab("Change 2000's to 1950's Degree Day 30C Residuals (30-year Rolling Mean) \n County FE & National Quadratic Trend with Lat and Long\n
        # dday30_rm_thirty ~ factor(fips) + dday0_10 + dday10_30 + dday30 + prec + prec_sq + \n
               # trend:(lat + long) + trend_sq:(lat + long)") +
  ylab(NULL)  + xlab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA),
                       plot.margin = unit(c(0, 10, 0, 0), "cm")) 
  
cornmap
