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
# dd_dat <- filter(dd_dat, abs(long) <= 100)
# unique(factor(dd_dat$state))
# dd_dat <- filter(dd_dat, state %in% unique(cropdat$state))
# 
# 
# #--------------------------------------
   # Roll.mean intervals

# # 50-year
#  dd_dat <- dd_dat %>%
#     group_by(fips) %>%
#     arrange(year) %>%
#     mutate(dday0_10_rm_fifty = lag(rollmean(dday0_10, k = 50, align = "right", fill = "NA")),
#            dday10_30_rm_fifty = lag(rollmean(dday10_30, k = 50, align = "right", fill = "NA")),
#            dday30_rm_fifty = lag(rollmean(dday30, k = 50, align = "right", fill = "NA")),
#            prec_rm_fifty = lag(rollmean(prec, k = 50, align = "right", fill = "NA")),
#            prec_sq_rm_fifty = prec_rm_fifty^2)
# 
#  # 40-year
#  dd_dat <- dd_dat %>%
#     group_by(fips) %>%
#     arrange(year) %>%
#     mutate(dday0_10_rm_fourty = lag(rollmean(dday0_10, k = 40, align = "right", fill = "NA")),
#            dday10_30_rm_fourty = lag(rollmean(dday10_30, k = 40, align = "right", fill = "NA")),
#            dday30_rm_fourty = lag(rollmean(dday30, k = 40, align = "right", fill = "NA")),
#            prec_rm_fourty = lag(rollmean(prec, k = 40, align = "right", fill = "NA")),
#            prec_sq_rm_fourty = prec_rm_fourty^2)
# 
#  # 30-year
#  dd_dat <- dd_dat %>%
#     group_by(fips) %>%
#     arrange(year) %>%
#     mutate(dday0_10_rm_thirty = lag(rollmean(dday0_10, k = 30, align = "right", fill = "NA")),
#            dday10_30_rm_thirty = lag(rollmean(dday10_30, k = 30, align = "right", fill = "NA")),
#            dday30_rm_thirty = lag(rollmean(dday30, k = 30, align = "right", fill = "NA")),
#            prec_rm_thirty = lag(rollmean(prec, k = 30, align = "right", fill = "NA")),
#            prec_sq_rm_thirty = prec_rm_thirty^2)
# 
#  # 20 year intervals
#  dd_dat <- dd_dat %>%
#     group_by(fips) %>%
#     arrange(year) %>%
#     mutate(dday0_10_rm_twenty = lag(rollmean(dday0_10, k = 20, align = "right", fill = "NA")),
#            dday10_30_rm_twenty = lag(rollmean(dday10_30, k = 20, align = "right", fill = "NA")),
#            dday30_rm_twenty = lag(rollmean(dday30, k = 20, align = "right", fill = "NA")),
#            prec_rm_twenty = lag(rollmean(prec, k = 20, align = "right", fill = "NA")),
#            prec_sq_rm_twenty = prec_rm_twenty^2)
# 
#   # 10-year
#  dd_dat <- dd_dat %>%
#     group_by(fips) %>%
#     arrange(year) %>%
#     mutate(dday0_10_rm_ten = lag(rollmean(dday0_10, k = 10, align = "right", fill = "NA")),
#            dday10_30_rm_ten = lag(rollmean(dday10_30, k = 10, align = "right", fill = "NA")),
#            dday30_rm_ten = lag(rollmean(dday30, k = 10, align = "right", fill = "NA")),
#            prec_rm_ten = lag(rollmean(prec, k = 10, align = "right", fill = "NA")),
#            prec_sq_rm_ten = prec_rm_ten^2)
# 
#  


dd_dat <- dd_dat %>% 
  group_by(fips) %>% 
  distinct(year, .keep_all = TRUE)


saveRDS(dd_dat, "data/full_weather_data.rds")
#-----------------------------------------------


dd_dat <- readRDS("data/full_weather_data.rds")
dd_dat <- filter(dd_dat, !is.na(state))
 dd_dat <- filter(dd_dat, year >= 1950 & year <= 2010)
dd_dat$trend <- dd_dat$year - 1949
dd_dat$trend_sq <- dd_dat$trend^2
dd_dat <- filter(dd_dat, !is.na(state))
ddat <- table(dd_dat$fips)
which(ddat != 61)
# saveRDS(dd_dat, "/home/john/Dropbox/eastern_weather_data.rds")
# dd_dat <- readRDS("/home/john/Dropbox/eastern_weather_data.rds")
# cropdat <- readRDS("data/full_ag_data.rds")

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

dd_dat$region <- 0
dd_dat$region <- ifelse(dd_dat$ers_region == 1, "Heartland", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 2, "Northern Crescent", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 3, "Northern Great Plains", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 4, "Prairie Gateway", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 5, "Eastern Uplands", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 6, "Southern Seaboard", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 7, "Fruitful Rim", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 8, "Basin and Range", dd_dat$region)
dd_dat$region <- ifelse(dd_dat$ers_region == 9, "Mississipi Portal", dd_dat$region)

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
mod1 <- felm(dday30_rm_thirty ~ 0 | fips , data = dd_dat)
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




mod2 <- felm(dday30_rm_thirty ~  trend | fips, data = dd_dat)
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
  xlab("Degree Day 30C Residuals (30-year) \n County FE & National Linear Trend \n
       dday30_rm_thirty ~ factor(fips) + trend") + ylab(NULL) + theme(legend.position = "none",
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

mod3 <- felm(dday30_rm_thirty ~ trend + trend_sq | fips, data = dd_dat)
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
  xlab("Degree Day 30C Residuals (30-year) \n County FE & National Quad. Trend
       \n dday30_rm_thirty ~ factor(fips) + trend + trend_sq") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod3_map



mod4 <- felm(dday30_rm_thirty ~ trend*(lat + long) | fips, data = dd_dat)
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
  xlab("Degree Day 30C Residuals (30-year) \n County FE & Linear Trend interaction with Lat and Long
       \n dday30_rm_thirty ~ factor(fips) + trend*(lat + long)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod4_map




mod5 <- felm(dday30_rm_thirty ~ trend*(lat + long) + trend_sq*(lat + long) | fips, data = dd_dat)
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
  xlab("Degree Day 30C Residuals (30-year) \n County FE & Quad. Trend interaction with Lat and Long \n
       \n dday30_rm_thirty ~ factor(fips) + trend*(lat + long) + trend_sq*(lat + long)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod5_map

# set.seed(1234)
# fdat <- select(cropdat, year, state, fips, acres)
# dd_dat <- left_join(dd_dat, fdat, by = c("year", "fips", "state"))
# dd_dat <- filter(dd_dat, !is.na(acres))
regdat <- filter(dd_dat, ers_region == 7)
mod6 <- felm(dday30_rm_thirty ~ trend + trend_sq|  factor(fips), 
             data = regdat)
sum(mod6$residuals)
summary(mod6)

mod6_res1 <- data.frame(value = rep(0, nrow(regdat)),
                        region = rep(0, nrow(regdat)))
mod6_res1$value <- as.numeric(mod6$residuals)
mod6_res1$region <- as.numeric(regdat$fips)
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
  xlab("Degree Day 30C Residuals (30-year) \n County FE & National Quadratic Trend with Lat and Long\n
        dday30_rm_thirty ~ factor(fips) + trend*(lat + long)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod6_map




#
mod7 <- felm(dday30_rm_thirty ~ 0 |  factor(crd), 
             data = dd_dat)
sum(mod7$residuals)
summary(mod7)

mod7_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)))
mod7_res1$value <- as.numeric(mod7$residuals)
mod7_res1$region <- as.numeric(dd_dat$fips)
mod7_res1 <- as.data.frame(mod7_res1)
mod7_res1 <- mod7_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(value))
head(mod7_res1)
sum(mod7_res1$value)
mod7_res1 <- as.data.frame(mod7_res1)
mod7_map <- county_choropleth(mod7_res1,
                 title      = NULL, state_zoom = states)

mod7_map <- mod7_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year) \n State FE & National Quadratic Trend \n
        dday30_rm_thirty ~ factor(state) + trend + trend_sq") + ylab(NULL) + theme(legend.position = "top",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod7_map






mod4_map



plot_grid(mod1_map, mod2_map, mod3_map, mod4_map, mod5_map, ncol = 3, 
          labels = c("1", "2", "3", "4", "5"))

head(test)



test <- filter(dd_dat, year == 2010)
test <- dd_dat %>% 
  group_by(fips) %>% 
  summarise(value = mean(dday30_rm_thirty, na.rm = TRUE))
testdat <- data.frame(region = test$fips, value = test$value)
head(testdat)
testdat <- select(test, fips)
testdat$value <- rnorm(nrow(testdat))

modmap <- county_choropleth(testdat,
                 title      = NULL, state_zoom = states)

modmap <- modmap + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Average Rolling Degree Day 30C (30-year)") + ylab(NULL) + theme(legend.position = "top",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
modmap

#--------------------------------------
# Change from 1950-1960 to 2000-2010
library(RColorBrewer)
dd56 <- filter(dd_dat, year >= 1950 & year <= 1970) %>% 
  group_by(fips) %>% 
  mutate(dday30_m = dday30_rm_thirty - mean(dday30_rm_thirty))


dd78 <- filter(dd_dat, year >= 1990 & year <= 2010) %>% 
  group_by(fips) %>% 
  mutate(dday30_m = dday30_rm_thirty - mean(dday30_rm_thirty))

# mod56 <- felm(dday30 ~ 0 | fips,
#              data = dd56)
# mod78 <- felm(dday30 ~ 0 | fips, 
#              data = dd78)

testdat <- data.frame(fips = dd56$fips[1:length(unique(dd56$fips))],
                      dd56 = dd56$dday30_m,
                      dd78 = dd78$dday30_m)

testdat$c <- testdat$dd78 - testdat$dd56
testdat$change <- 100*(testdat$dd78 - testdat$dd56)/testdat$dd56
head(testdat)
range(testdat$change)
range(testdat$c)
cols <- colorRampPalette(rev(brewer.pal(7, "RdYlBu")))
testdat$cols <- NA
testdat$cols <- ifelse(testdat$c <= -5.5860, 1, testdat$cols)
testdat$cols <- ifelse(testdat$c >= -5.5859, ifelse(testdat$c <= -3.4247, 2, testdat$col), testdat$cols)
testdat$cols <- ifelse(testdat$c >= -3.4246, ifelse(testdat$c <= -2.0347, 3, testdat$col), testdat$cols)
testdat$cols <- ifelse(testdat$c >= -2.0346, ifelse(testdat$c <= -0.9611, 4, testdat$col), testdat$cols)
testdat$cols <- ifelse(testdat$c >= -0.9610, ifelse(testdat$c <= 0.0292, 5, testdat$col), testdat$cols)
testdat$cols <- ifelse(testdat$c >= 0.0291, ifelse(testdat$c <= 2.6947, 6, testdat$col), testdat$cols)
testdat$cols <- ifelse(testdat$c >= 2.6947, 7, testdat$col)
unique(testdat$cols)


# Histogram
ghist <- ggplot(testdat, aes(c)) + 
  geom_histogram(bins = 300, aes(fill = factor(cols))) + 
  scale_fill_manual(values = cols(7)) + theme_tufte() +
  xlab("Change in Degree Day 30C (30-year Rolling Mean)") + ylab(NULL) +
  theme(legend.position = "none",
                       # axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  

ghist
# Maps

testdat <- select(testdat, fips, c)
names(testdat) <- c("region", "value")
testdat <- testdat %>% 
  distinct(region, .keep_all = TRUE)

modmap <- county_choropleth(testdat,
                 title      = NULL, state_zoom = states)

modmap <- modmap + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Difference in Degree Day 30C 1950's to 2000's \n (30-year rolling average)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
modmap


library(cowplot)
plot_grid(modmap, ghist, ncol = 1, rel_heights = c(1, .35))












# Regional Trends
# ggplot(dd_dat, aes(year, dday30_rm_thirty, color = ers_region, group = fips)) + geom_smooth()

gdat <- dd_dat %>% 
  group_by(fips) %>% 
  mutate(dday30_dm = dday30_rm_thirty - mean(dday30_rm_thirty))


# ggplot(gdat, aes(year, dday30_dm, color = factor(ers_region), group = fips)) + geom_line()

ggdat <- gdat %>% 
  group_by(region,  year) %>% 
  summarise(dday30_dm_m = mean(dday30_dm))

# ggdat$region <- 0
# ggdat$region <- ifelse(ggdat$ers_region == 1, "Heartland", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 2, "Northern Crescent", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 3, "Northern Great Plains", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 4, "Prairie Gateway", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 5, "Eastern Uplands", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 6, "Southern Seaboard", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 7, "Fruitful Rim", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 8, "Basin and Range", ggdat$region)
# ggdat$region <- ifelse(ggdat$ers_region == 9, "Mississipi Portal", ggdat$region)

ggplot(ggdat, aes(year, dday30_dm_m, color = factor(region))) + 
  geom_line() + 
  theme(legend.title = element_blank()) +
  ylab("Demeaned Degree Day 30 \n (30-year Rolling Mean)") +
  xlab(NULL)


mod6 <- felm(dday30_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
               trend:(lat + long) + trend_sq:(lat + long) |  factor(fips), 
             data = dd_dat)
sum(mod6$residuals)
summary(mod6)

mod6_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)),
                        year = rep(0, nrow(dd_dat)))
mod6_res1$value <- as.numeric(mod6$residuals)
mod6_res1$region <- as.numeric(dd_dat$fips)
mod6_res1$year <- dd_dat$year


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
  xlab("Degree Day 30C Residuals (30-year Rolling Mean) \n County FE & National Quadratic Trend with Lat and Long\n
        dday30_rm_thirty ~ factor(fips) + dday0_10 + dday10_30 + dday30 + prec + prec_sq + \n
               trend:(lat + long) + trend_sq:(lat + long)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod6_map


# Difference in residuals 1950 and 2000
mod7 <- felm(dday30_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
               trend:(lat + long) + trend_sq:(lat + long) |  factor(fips), 
             data = dd_dat)
sum(mod7$residuals)
summary(mod7)

mod7_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)),
                        year = rep(0, nrow(dd_dat)))
mod7_res1$value <- as.numeric(mod7$residuals)
mod7_res1$region <- as.numeric(dd_dat$fips)
mod7_res1$year <- dd_dat$year
mod7_res50_00 <- filter(mod7_res1, (year >= 1950 & year <= 1960) | (year >= 2000 & year <= 2010))
mod7_res50_00$decade <- ifelse(mod7_res50_00$year <= 1960, 1, 2)

mod7_res50_00 <- mod7_res50_00 %>% 
  group_by(region, decade) %>% 
  summarise(value = mean(value)) %>% 
  group_by(region) %>% 
  arrange(-decade) %>% 
  mutate(value = first(value) - last(value)) %>% 
  filter(decade == 2)

mod7_res50_00 <- select(mod7_res50_00, region, value)
mod7_map <- county_choropleth(mod7_res50_00,
                 title      = NULL, state_zoom = states)

mod7_map <- mod7_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Change 2000's to 1950's Degree Day 30C Residuals (30-year Rolling Mean) \n County FE & National Quadratic Trend with Lat and Long\n
        dday30_rm_thirty ~ factor(fips) + dday0_10 + dday10_30 + dday30 + prec + prec_sq + \n
               trend:(lat + long) + trend_sq:(lat + long)") + ylab(NULL) + theme(legend.position = "top",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod7_map


cols <- colorRampPalette(rev(brewer.pal(7, "RdYlBu")))
mod7_res50_00_hist <- mod7_res50_00
mod7_res50_00_hist$col <- NA
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value <= -8.972, 1, mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -8.971, ifelse(mod7_res50_00_hist$value <= -4.150, 2, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -4.149, ifelse(mod7_res50_00_hist$value <= -0.943, 3, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -0.942, ifelse(mod7_res50_00_hist$value <= 1.877, 4, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 1.878, ifelse(mod7_res50_00_hist$value <= 4.757, 5, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 4.758, ifelse(mod7_res50_00_hist$value <= 8.915, 6, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 8.916, 7, mod7_res50_00_hist$col)
unique(mod7_res50_00_hist$col)

ghist2 <- ggplot(mod7_res50_00_hist, aes(value)) + 
  geom_histogram(bins = 300, aes(fill = factor(cols))) + 
  scale_fill_manual(values = cols(7)) + theme_tufte() +
  xlab("Change in Degree Day 30C (30-year Rolling Mean)") + ylab(NULL) +
  theme(legend.position = "none",
                       # axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  

ghist2

