library(cowplot)
library(zoo)
library(readr)
library(dplyr)
library(lfe)
library(ggthemes)
library(choroplethr)
library(plm)
library(noncensus)
library(maps)
library(haven)


dd <- read_csv("data/fips_degree_days_1900-2013.csv")
prec <- read_csv("data/fips_precipitation_1900-2013.csv")

cropdat <- readRDS("data/full_ag_data.rds")

dd$year <- as.integer(dd$year)
dd$fips <- as.integer(dd$fips)
dd$X1 <- NULL

dd <- left_join(dd, prec, by = c("fips", "year", "month"))

dd_dat <- filter(dd, month >= 3 & month <= 10)

dd_dat <- dd_dat %>%
    group_by(year, fips) %>%
    summarise(dday0C = sum(dday0C),
             dday10C = sum(dday10C),
             dday30C = sum(dday30C),
             prec = sum(ppt))

dd_dat$dday0_10 <- dd_dat$dday0C - dd_dat$dday10C
dd_dat$dday10_30 <- dd_dat$dday10C - dd_dat$dday30C
dd_dat$dday30 <- dd_dat$dday30C
dd_dat$prec_sq <- dd_dat$prec^2

dd_dat <- select(dd_dat, year, fips, dday0C, dday10C, dday30C, dday0_10, dday10_30, dday30, prec, prec_sq)

data(county.fips)
county.fips$state <- sapply(str_split(county.fips$polyname, ","),'[',1)
county.fips$county <- sapply(str_split(county.fips$polyname, ","),'[',2)
county.fips <- select(county.fips, fips, county, state)
head(county.fips)

states <- data.frame(state = tolower(state.name), stateabb = tolower(state.abb))
states

county.fips <- left_join(county.fips, states, by = "state")
county.fips <- select(county.fips, fips, stateabb)
names(county.fips) <- c("fips", "state")

dd_dat <- left_join(dd_dat, county.fips, by = "fips")

data(zip_codes)
zip_codes <- select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("fips", "lat", "long")
zip_codes <- zip_codes %>% 
  group_by(fips) %>% 
  summarise(lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE))

dd_dat <- left_join(dd_dat, zip_codes, by = "fips")

ers_region <- read_csv("data/ResourceRegionCRDfips.csv")
names(ers_region) <- c("fips", "ers_region", "crd")
dd_dat <- left_join(dd_dat, ers_region, by = "fips")
dd_dat <- filter(dd_dat, abs(long) <= 100)
unique(factor(dd_dat$state))
# dd_dat <- filter(dd_dat, state %in% unique(cropdat$state))
# 
# 
# #--------------------------------------
   # Roll.mean intervals

# 50-year
 dd_dat <- dd_dat %>%
    group_by(fips) %>%
    arrange(year) %>%
    mutate(dday0_10_rm_fifty = lag(rollmean(dday0_10, k = 50, align = "right", fill = "NA")),
           dday10_30_rm_fifty = lag(rollmean(dday10_30, k = 50, align = "right", fill = "NA")),
           dday30_rm_fifty = lag(rollmean(dday30, k = 50, align = "right", fill = "NA")),
           prec_rm_fifty = lag(rollmean(prec, k = 50, align = "right", fill = "NA")),
           prec_sq_rm_fifty = prec_rm_fifty^2)

 # 40-year
 dd_dat <- dd_dat %>%
    group_by(fips) %>%
    arrange(year) %>%
    mutate(dday0_10_rm_fourty = lag(rollmean(dday0_10, k = 40, align = "right", fill = "NA")),
           dday10_30_rm_fourty = lag(rollmean(dday10_30, k = 40, align = "right", fill = "NA")),
           dday30_rm_fourty = lag(rollmean(dday30, k = 40, align = "right", fill = "NA")),
           prec_rm_fourty = lag(rollmean(prec, k = 40, align = "right", fill = "NA")),
           prec_sq_rm_fourty = prec_rm_fourty^2)

 # 30-year
 dd_dat <- dd_dat %>%
    group_by(fips) %>%
    arrange(year) %>%
    mutate(dday0_10_rm_thirty = lag(rollmean(dday0_10, k = 30, align = "right", fill = "NA")),
           dday10_30_rm_thirty = lag(rollmean(dday10_30, k = 30, align = "right", fill = "NA")),
           dday30_rm_thirty = lag(rollmean(dday30, k = 30, align = "right", fill = "NA")),
           prec_rm_thirty = lag(rollmean(prec, k = 30, align = "right", fill = "NA")),
           prec_sq_rm_thirty = prec_rm_thirty^2)

 # 20 year intervals
 dd_dat <- dd_dat %>%
    group_by(fips) %>%
    arrange(year) %>%
    mutate(dday0_10_rm_twenty = lag(rollmean(dday0_10, k = 20, align = "right", fill = "NA")),
           dday10_30_rm_twenty = lag(rollmean(dday10_30, k = 20, align = "right", fill = "NA")),
           dday30_rm_twenty = lag(rollmean(dday30, k = 20, align = "right", fill = "NA")),
           prec_rm_twenty = lag(rollmean(prec, k = 20, align = "right", fill = "NA")),
           prec_sq_rm_twenty = prec_rm_twenty^2)

  # 10-year
 dd_dat <- dd_dat %>%
    group_by(fips) %>%
    arrange(year) %>%
    mutate(dday0_10_rm_ten = lag(rollmean(dday0_10, k = 10, align = "right", fill = "NA")),
           dday10_30_rm_ten = lag(rollmean(dday10_30, k = 10, align = "right", fill = "NA")),
           dday30_rm_ten = lag(rollmean(dday30, k = 10, align = "right", fill = "NA")),
           prec_rm_ten = lag(rollmean(prec, k = 10, align = "right", fill = "NA")),
           prec_sq_rm_ten = prec_rm_ten^2)

 
 dd_dat <- filter(dd_dat, year >= 1950 & year <= 2010)
 dd_dat$trend <- dd_dat$year - 1949
 dd_dat$trend_sq <- dd_dat$trend^2

dd_dat <- dd_dat %>% 
  group_by(fips) %>% 
  distinct(year, .keep_all = TRUE)

dd_dat <- filter(dd_dat, !is.na(state))
ddat <- table(dd_dat$fips)
which(ddat != 61)
saveRDS(dd_dat, "data/full_weather_data.rds")
#-----------------------------------------------


dd_dat <- readRDS("data/full_weather_data.rds")
dd_dat <- filter(dd_dat, !is.na(state))
saveRDS(dd_dat, "/home/john/Dropbox/eastern_weather_data.rds")
dd_dat <- readRDS("/home/john/Dropbox/eastern_weather_data.rds")
cropdat <- readRDS("data/full_ag_data.rds")

head(dd_dat)

# 30 year intervals
dd_dat$thirty <- dd_dat$year - (dd_dat$year %% 30)

dd_dat <- dd_dat %>% 
  group_by(fips, thirty) %>% 
  mutate(dday0_10_thirty = mean(dday0_10, na.rm = TRUE),
         dday10_30_thirty = mean(dday10_30, na.rm = TRUE),
         dday30_thirty = mean(dday30, na.rm = TRUE),
         prec_thirty = mean(prec, na.rm = TRUE),
         prec_sq_thirty = prec_thirty^2)



# nbal <- c(12091, 22099, 37053, 48167, 51001)
# dd_dat <- filter(dd_dat, fips != 12091)
# dd_dat <- filter(dd_dat, fips != 22099)
# dd_dat <- filter(dd_dat, fips != 37053)
# dd_dat <- filter(dd_dat, fips != 48167)
# dd_dat <- filter(dd_dat, fips != 51001)
# ddat <- table(dd_dat$fips)
# which(ddat != 61)

states <- toupper(factor(dd_dat$state))
states <- tolower(unique(state.name[match(states, state.abb)]))
states <- states[!is.na(states)]  

dd_dat$state <- factor(dd_dat$state)
# dd_dat$fips <- factor(dd_dat$fips)
dd_dat$ers_region <- factor(dd_dat$ers_region)

#-----------------------------------
# Maps
# Climate dday30_rm
# Residual checks

# mod1 <- lm(dday30_rm_thirty ~ factor(fips) - 1, data = dd_dat)
# mod11 <- mod1
dd_dat <- filter(dd_dat, !is.na(state))
mod1 <- felm(log(dday30_rm_thirty) ~ I(trend + trend^2):factor(ers_region) + factor(fips) , data = dd_dat)
summary(mod1)
length(mod1$coefficients)
sum(mod1$residuals)

mod1_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)))
mod1_res1$value <- as.numeric(mod1$residuals)
mod1_res1$region <- as.numeric(dd_dat$fips)
mod1_res1 <- as.data.frame(mod1_res1)
head(mod1_res1$value)
mod1_res1 <- mod1_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(value))
head(mod1_res1)
sum(mod1_res1$value)
mod1_res1 <- as.data.frame(mod1_res1)
mod1_map <- county_choropleth(mod1_res1,
                 title      = NULL, state_zoom = states)

mod1_map <- mod1_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year) \n County FE \n dday30_rm_thirty ~ factor(fips)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod1_map




mod2 <- felm(dday30_rm_thirty ~  ers_region:trend | fips, data = dd_dat)
mod2_res1 <- data.frame(value = rep(0, nrow(dd_dat)))
mod2_res1$value <- as.numeric(residuals(mod2))
mod2_res1$region <- dd_dat$fips
mod2_res1 <- as.data.frame(mod2_res1)
mod2_res1 <- mod2_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(value))
head(mod2_res1)
mod2_map <- county_choropleth(mod2_res1,
                 title      = NULL, state_zoom = states)

mod2_map <- mod2_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year) \n County FE & ERS Region Linear Trend \n
       dday30_rm_thirty ~ factor(fips) + ers_region:trend") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod2_map


# mod3 <- lm(dday30_rm_thirty ~ ers_region:trend + ers_region:trend_sq + factor(fips) - 1, data = dd_dat)
# mod33 <- mod3
# sum(mod3$residuals)
#[1] -0.000000000001693925

mod3 <- felm(dday30_rm_thirty ~ ers_region:trend + ers_region:trend_sq | fips, data = dd_dat)
sum(mod3$residuals)

mod3_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)))
mod3_res1$value <- as.numeric(mod3$residuals)
mod3_res1$region <- as.numeric(dd_dat$fips)
mod3_res1 <- as.data.frame(mod3_res1)
mod3_res1 <- mod3_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(value))
head(mod3_res1)
sum(mod3_res1$value)
mod3_res1 <- as.data.frame(mod3_res1)
mod3_map <- county_choropleth(mod3_res1,
                 title      = NULL, state_zoom = states)

mod3_map <- mod3_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year) \n County FE & ERS Region Quad. Trend 
       \n dday30_rm_thirty ~ factor(fips) + ers_region:trend + ers_region:trend_sq") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod3_map



mod4 <- felm(dday30_rm_thirty ~ state:trend | fips, data = dd_dat)
summary(mod4)
sum(mod4$residuals)

mod4_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)))
mod4_res1$value <- as.numeric(mod4$residuals)
mod4_res1$region <- as.numeric(dd_dat$fips)
mod4_res1 <- as.data.frame(mod4_res1)
mod4_res1 <- mod4_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(value))
head(mod4_res1)
sum(mod4_res1$value)
mod4_res1 <- as.data.frame(mod4_res1)
mod4_map <- county_choropleth(mod4_res1,
                 title      = NULL, state_zoom = states)

mod4_map <- mod4_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year) \n County FE & State Linear Trend 
       \n dday30_rm_thirty ~ factor(fips) + state:trend") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod4_map




mod5 <- felm(dday30_rm_thirty ~ state:trend + state:trend_sq | fips, data = dd_dat)
sum(mod5$residuals)

mod5_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)))
mod5_res1$value <- as.numeric(mod5$residuals)
mod5_res1$region <- as.numeric(dd_dat$fips)
mod5_res1 <- as.data.frame(mod5_res1)
mod5_res1 <- mod5_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(value))
head(mod5_res1)
sum(mod5_res1$value)
mod5_res1 <- as.data.frame(mod5_res1)
mod5_map <- county_choropleth(mod5_res1,
                 title      = NULL, state_zoom = states)

mod5_map <- mod5_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year) \n County FE & State Quad. Trend \n
       \n dday30_rm_thirty ~ factor(fips) + state:trend + state:trend_sq") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod5_map

# set.seed(1234)
fdat <- select(cropdat, year, state, fips, acres)
dd_dat <- left_join(dd_dat, fdat, by = c("year", "fips", "state"))
dd_dat <- filter(dd_dat, !is.na(acres))
mod6 <- felm(dday30_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + factor(ers_region):trend + factor(ers_region):trend_sq - 1 | fips, 
             data = dd_dat, weights = dd_dat$acres)
sum(mod6$residuals)
summary(mod6)

mod6_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)))
mod6_res1$value <- as.numeric(mod6$residuals)
mod6_res1$region <- as.numeric(dd_dat$fips)
mod6_res1 <- as.data.frame(mod6_res1)
mod6_res1 <- mod6_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(value))
head(mod6_res1)
sum(mod6_res1$value)
mod6_res1 <- as.data.frame(mod6_res1)
mod6_map <- county_choropleth(mod6_res1,
                 title      = NULL, state_zoom = states)

mod6_map <- mod6_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("2010 Degree Day 30C Residuals (30-year) \n Temperature with County FE & ERS Region Quad Trend \n
        dday30_rm_thirty ~ factor(fips)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod6_map

plot_grid(mod1_map, mod2_map, mod3_map, mod4_map, mod5_map, mod6_map, ncol = 3, 
          labels = c("1", "2", "3", "4", "5", "6"))

head(test)



test <- filter(dd_dat, year == 2010)
test <- dd_dat %>% 
  group_by(fips) %>% 
  summarise(value = mean(dday30_rm_thirty, na.rm = TRUE))
testdat <- data.frame(region = test$fips, value = test$value)
head(testdat)

modmap <- county_choropleth(testdat,
                 title      = NULL, state_zoom = states)

modmap <- modmap + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Average Rolling Degree Day 30C (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
modmap
