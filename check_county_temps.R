library(cowplot)
library(zoo)
library(readr)
library(dplyr)
library(lfe)
library(tidyverse)
library(ggthemes)
library(choroplethr)
library(plm)
library(noncensus)
library(maps)
library(haven)
# # 
# # 
# dd <- read_csv("data/fips_degree_days_1900-2013.csv")
# prec <- read_csv("data/fips_precipitation_1900-2013.csv")
# 
# 
# dd$year <- as.integer(dd$year)
# dd$fips <- as.integer(dd$fips)
# dd$X1 <- NULL
# 
# dd <- left_join(dd, prec, by = c("fips", "year", "month"))
# dd_dat <- filter(dd, month >= 3 & month <= 10)
# 
# dd_dat <- dd_dat %>%
#     group_by(year, fips) %>%
#     summarise(dday0C = sum(dday0C),
#               dday10C = sum(dday10C),
#               dday30C = sum(dday30C),
#               prec = sum(ppt))
# 
# dd_dat$dday0_10 <- dd_dat$dday0C - dd_dat$dday10C
# dd_dat$dday10_30 <- dd_dat$dday10C - dd_dat$dday30C
# dd_dat$dday30 <- dd_dat$dday30C
# dd_dat$prec_sq <- dd_dat$prec^2
# 
# dd_dat <- select(dd_dat, year, fips, dday0C, dday10C, dday30C, dday0_10, dday10_30, dday30, prec, prec_sq)
# 
# data(county.fips)
# county.fips$state <- sapply(str_split(county.fips$polyname, ","),'[',1)
# county.fips$county <- sapply(str_split(county.fips$polyname, ","),'[',2)
# county.fips <- select(county.fips, fips, county, state)
# head(county.fips)
# 
# states <- data.frame(state = tolower(state.name), stateabb = tolower(state.abb))
# states
# 
# county.fips <- left_join(county.fips, states, by = "state")
# county.fips <- select(county.fips, fips, stateabb)
# 
# names(county.fips) <- c("fips", "state")
# dd_dat <- left_join(dd_dat, county.fips, by = "fips")
# 
# data(zip_codes)
# zip_codes <- select(zip_codes, fips, latitude, longitude)
# zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
# names(zip_codes) <- c("fips", "lat", "long")
# zip_codes <- zip_codes %>%
#   group_by(fips) %>%
#   summarise(lat = mean(lat, na.rm = TRUE),
#             long = mean(long, na.rm = TRUE))
# dd_dat <- left_join(dd_dat, zip_codes, by = "fips")
# 
# ers_region <- read_csv("data/ResourceRegionCRDfips.csv")
# names(ers_region) <- c("fips", "ers_region", "crd")
# dd_dat <- left_join(dd_dat, ers_region, by = "fips")
# 
# rm <- readRDS("data/full_rollmean_lag_variables.rds")
# rm <- select(rm, year, fips, dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10,
#              dday0_10_rm20, dday10_30_rm20, dday30_rm20, prec_rm20, prec_sq_rm20,
#              dday0_10_rm30, dday10_30_rm30, dday30_rm30, prec_rm30, prec_sq_rm30)
# dd_dat <- left_join(dd_dat, rm, by = c("year", "fips"))

# dd_dat <- filter(dd_dat, abs(long) <= 100)
# unique(factor(dd_dat$state))
# dd_dat <- filter(dd_dat, state %in% unique(cropdat$state))
#
#
# #--------------------------------------
# # Roll.mean intervals
#
# #50-year
# dd_dat <- dd_dat %>%
#        group_by(fips) %>%
#        arrange(year) %>%
#        mutate(dday0_10_rm_fifty = lag(rollmean(dday0_10, k = 50, align = "right", fill = "NA")),
#               dday10_30_rm_fifty = lag(rollmean(dday10_30, k = 50, align = "right", fill = "NA")),
#               dday30_rm_fifty = lag(rollmean(dday30, k = 50, align = "right", fill = "NA")),
#               prec_rm_fifty = lag(rollmean(prec, k = 50, align = "right", fill = "NA")),
#               prec_sq_rm_fifty = prec_rm_fifty^2)
# 
# 
# # 40-year
#  dd_dat <- dd_dat %>%
#     group_by(fips) %>%
#     arrange(year) %>%
#     mutate(dday0_10_rm_fourty = lag(rollmean(dday0_10, k = 40, align = "right", fill = "NA")),
#            dday10_30_rm_fourty = lag(rollmean(dday10_30, k = 40, align = "right", fill = "NA")),
#            dday30_rm_fourty = lag(rollmean(dday30, k = 40, align = "right", fill = "NA")),
#            prec_rm_fourty = lag(rollmean(prec, k = 40, align = "right", fill = "NA")),
#            prec_sq_rm_fourty = prec_rm_fourty^2)
# 
# # # 30-year
# dd_dat <- dd_dat %>%
#       group_by(fips) %>%
#       arrange(year) %>%
#       mutate(dday0_10_rm_thirty = lag(rollmean(dday0_10, k = 30, align = "right", fill = "NA")),
#              dday10_30_rm_thirty = lag(rollmean(dday10_30, k = 30, align = "right", fill = "NA")),
#              dday30_rm_thirty = lag(rollmean(dday30, k = 30, align = "right", fill = "NA")),
#              prec_rm_thirty = lag(rollmean(prec, k = 30, align = "right", fill = "NA")),
#              prec_sq_rm_thirty = prec_rm_thirty^2)
# 
# # 20 year intervals
# dd_dat <- dd_dat %>%
#       group_by(fips) %>%
#       arrange(year) %>%
#       mutate(dday0_10_rm_twenty = lag(rollmean(dday0_10, k = 20, align = "right", fill = "NA")),
#              dday10_30_rm_twenty = lag(rollmean(dday10_30, k = 20, align = "right", fill = "NA")),
#              dday30_rm_twenty = lag(rollmean(dday30, k = 20, align = "right", fill = "NA")),
#              prec_rm_twenty = lag(rollmean(prec, k = 20, align = "right", fill = "NA")),
#              prec_sq_rm_twenty = prec_rm_twenty^2)
# 
# 
# # 15 year intervals
# dd_dat <- dd_dat %>%
#       group_by(fips) %>%
#       arrange(year) %>%
#       mutate(dday0_10_rm_fifteen = lag(rollmean(dday0_10, k = 15, align = "right", fill = "NA")),
#              dday10_30_rm_fifteen = lag(rollmean(dday10_30, k = 15, align = "right", fill = "NA")),
#              dday30_rm_fifteen = lag(rollmean(dday30, k = 15, align = "right", fill = "NA")),
#              prec_rm_fifteen = lag(rollmean(prec, k = 15, align = "right", fill = "NA")),
#              prec_sq_rm_fifteen = prec_rm_fifteen^2)
# 
# # 10-year
# dd_dat <- dd_dat %>%
#       group_by(fips) %>%
#       arrange(year) %>%
#       mutate(dday0_10_rm_ten = lag(rollmean(dday0_10, k = 10, align = "right", fill = "NA")),
#              dday10_30_rm_ten = lag(rollmean(dday10_30, k = 10, align = "right", fill = "NA")),
#              dday30_rm_ten = lag(rollmean(dday30, k = 10, align = "right", fill = "NA")),
#              prec_rm_ten = lag(rollmean(prec, k = 10, align = "right", fill = "NA")),
#              prec_sq_rm_ten = prec_rm_ten^2)
# # 5-year
# dd_dat <- dd_dat %>%
#       group_by(fips) %>%
#       arrange(year) %>%
#       mutate(dday0_10_rm_five = lag(rollmean(dday0_10, k = 5, align = "right", fill = "NA")),
#              dday10_30_rm_five = lag(rollmean(dday10_30, k = 5, align = "right", fill = "NA")),
#              dday30_rm_five = lag(rollmean(dday30, k = 5, align = "right", fill = "NA")),
#              prec_rm_five = lag(rollmean(prec, k = 5, align = "right", fill = "NA")),
#              prec_sq_rm_five = prec_rm_five^2)
# 
# # 2-year
# dd_dat <- dd_dat %>%
#       group_by(fips) %>%
#       arrange(year) %>%
#       mutate(dday0_10_rm_two = lag(rollmean(dday0_10, k = 2, align = "right", fill = "NA")),
#              dday10_30_rm_two = lag(rollmean(dday10_30, k = 2, align = "right", fill = "NA")),
#              dday30_rm_two = lag(rollmean(dday30, k = 2, align = "right", fill = "NA")),
#              prec_rm_two = lag(rollmean(prec, k = 2, align = "right", fill = "NA")),
#              prec_sq_rm_two = prec_rm_two^2)


# dd_dat <- filter(dd_dat, year >= 1950)
# dd_dat$trend <- dd_dat$year - 1949
# dd_dat$trend_sq <- dd_dat$trend^2
# 
# dd_dat <- dd_dat %>%
#   group_by(fips) %>%
#   distinct(year, .keep_all = TRUE)
# #
# dd_dat <- filter(dd_dat, !is.na(state))
# ddat <- table(dd_dat$fips)
# which(ddat != 61)
# # dd_dat <- filter(dd_dat, !is.na(state))
# 
# # nbal <- c(12091, 22099, 37053, 48167, 51001)
# # dd_dat <- filter(dd_dat, fips != 12091)
# # dd_dat <- filter(dd_dat, fips != 22099)
# # dd_dat <- filter(dd_dat, fips != 37053)
# # dd_dat <- filter(dd_dat, fips != 48167)
# # dd_dat <- filter(dd_dat, fips != 51001)
# # ddat <- table(dd_dat$fips)
#  # which(ddat != 63)
# 
# states <- toupper(factor(dd_dat$state))
# states <- tolower(unique(state.name[match(states, state.abb)]))
# states <- states[!is.na(states)]
# 
# dd_dat$region <- 0
# dd_dat$region <- ifelse(dd_dat$ers_region == 1, "Heartland", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 2, "Northern Crescent", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 3, "Northern Great Plains", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 4, "Prairie Gateway", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 5, "Eastern Uplands", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 6, "Southern Seaboard", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 7, "Fruitful Rim", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 8, "Basin and Range", dd_dat$region)
# dd_dat$region <- ifelse(dd_dat$ers_region == 9, "Mississipi Portal", dd_dat$region)
# #
# dd_dat$state <- factor(dd_dat$state)
# dd_dat$fips <- factor(dd_dat$fips)
# dd_dat$ers_region <- factor(dd_dat$ers_region)
# #
#
#
 # saveRDS(dd_dat, "data/full_weather_data.rds")
 #-----------------------------------------------

cropdat <- readRDS("data/full_ag_data.rds")
dd_dat <- readRDS("data/full_weather_data.rds")
cropdat <- filter(cropdat, long >= -100)
dd_dat <- filter(dd_dat,long >= -100)

states <- toupper(factor(cropdat$state))
states <- toupper(c("al","ar","ct","dc", "de", "fl","ga","il","in","ia","ks","ky","la","me","md","ma","mi","mn","ms","mo",
"ne","nh","nj","ny","nc","nd","oh","ok","pa","ri","sc","sd","tn","tx","vt","va","wv","wi"))
states <- tolower(unique(state.name[match(states, state.abb)]))
states <- states[!is.na(states)]
states
data(zip_codes)
zip_codes <- select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("fips", "lat", "long")
zip_codes <- zip_codes %>%
 group_by(fips) %>%
 summarise(lat = mean(lat, na.rm = TRUE),
           long = mean(long, na.rm = TRUE))

#--------------------------------------
# Change from 1950-1960 to 2000-2010
library(RColorBrewer)
dd50 <- filter(dd_dat, year >= 1950 & year <= 1959) %>% 
  group_by(fips) %>% 
  mutate(dday30_m = dday30_rm30 - mean(dday30_rm30)) %>% 
  ungroup()


dd00 <- filter(dd_dat, year >= 2000 & year <= 2009) %>% 
  group_by(fips) %>% 
  mutate(dday30_m = dday30_rm30 - mean(dday30_rm30)) %>% 
    ungroup()

testdat <- data.frame(fips = as.numeric(as.character(dd50$fips)),
                      dd50 = dd50$dday30_m,
                      dd00 = dd00$dday30_m)

testdat$c <- testdat$dd00 - testdat$dd50
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
  geom_histogram(bins = 100, aes(fill = factor(cols))) + 
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
  mutate(dday30_dm = dday30_rm10 - mean(dday30_rm10))


# ggplot(gdat, aes(year, dday30_dm, color = factor(ers_region), group = fips)) + geom_line()

ggdat <- gdat %>% 
  group_by(region,  year) %>% 
  summarise(dday30_dm_m = mean(dday30_dm))

ggplot(ggdat, aes(year, dday30_dm_m, color = factor(region))) + 
  geom_line() + 
  theme(legend.title = element_blank()) +
  ylab("Demeaned Degree Day 30 \n (10-year Rolling Mean)") +
  xlab(NULL) + geom_hline(yintercept = 0, linetype = "dashed")


# Difference in residuals 1950 and 2000
mod7 <- felm(dday30_rm30 ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
               trend:(lat + long) + trend_sq:(lat + long) |  factor(fips), 
             data = dd_dat)
sum(mod7$residuals)
summary(mod7)

mod7_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)),
                        year = rep(0, nrow(dd_dat)))
mod7_res1$value <- as.numeric(mod7$residuals)
mod7_res1$region <- as.numeric(as.character(dd_dat$fips))
mod7_res1$year <- dd_dat$year

mod7_res50_00 <- filter(mod7_res1, (year >= 1950 & year <= 1970) | (year >= 1990 & year <= 2010))
mod7_res50_00$decade <- ifelse(mod7_res50_00$year <= 1970, 1, 2)

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
  # xlab("Change 2000's to 1950's Degree Day 30C Residuals (30-year Rolling Mean) \n County FE & National Quadratic Trend with Lat and Long\n
        # dday30_rm_thirty ~ factor(fips) + dday0_10 + dday10_30 + dday30 + prec + prec_sq + \n
               # trend:(lat + long) + trend_sq:(lat + long)") +
  ylab(NULL)  + xlab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA),
                       plot.margin = unit(c(0, 10, 2, 0), "cm")) 
  
mod7_map


# Get extreme counties
fipm <- select(cropdat, fips, state, lat, long)
head(fipm)
fipm <- fipm %>%
   distinct(fips, .keep_all = TRUE)
mdat <- as.data.frame(mod7_res50_00)
names(mdat) <- c("fips", "value")
head(mdat)
fdat <- left_join(mdat, fipm, by = c("fips"))
head(fdat)

# Mississippi Delta
ms_fips <- filter(fdat, state %in% c("ms", "la", "ar"))
# ms_fips <- filter(ms_fips, long >= -92.5)
# ms_fips <- filter(ms_fips, long <= -90.5)
# ms_fips <- filter(ms_fips, lat >= 31)
ms_w_fips <- arrange(ms_fips, -value)
ms_w_fips <- ms_w_fips[1:10, c("fips", "value")]

ms_c_fips <- arrange(ms_fips, value)
ms_c_fips <- ms_c_fips[1:10, c("fips", "value")]

# Iowa extreme heat
iowa_fips <- filter(fdat, state == "ia")
iowa_fips <- arrange(iowa_fips, -value)
iowa_fips <- iowa_fips[1:10, ]
# iowa_fips <- filter(iowa_fips, value >= 3.30)
nrow(iowa_fips)

# Kentucky Warming
ky_w_fips <- filter(fdat, state == "ky")
ky_w_fips <- arrange(ky_w_fips, -value)
ky_w_fips <- ky_w_fips[1:10, ]
# ky_w_fips <- filter(ky_w_fips, value >= 3.30)
nrow(ky_w_fips)

# Kentucky Cooling
ky_c_fips <- filter(fdat, state == "ky")
ky_c_fips <- arrange(ky_c_fips, value)
ky_c_fips <- ky_c_fips[1:10, ]
# ky_c_fips <- filter(ky_c_fips, value >= 3.30)
nrow(ky_c_fips)

# South Carolina extreme heat
sc_fips <- filter(fdat, state == "sc")
sc_fips <- arrange(sc_fips, -value)
sc_fips <- sc_fips[1:30, ]
# sc_fips <- filter(sc_fips, value >= 3.30)
nrow(sc_fips)

# Illinois cooling 
il_fips <- filter(fdat, state == "il")
il_fips <- arrange(il_fips, value)
il_fips <- il_fips[1:10, ]
# il_fips <- filter(il_fips, value <= -7.17)
nrow(il_fips)

# Nebraska warming
ne_fips <- filter(fdat, state == "ne")
ne_fips <- arrange(ne_fips, -value)
ne_fips <- ne_fips[1:10, ]
# ne_fips <- filter(ne_fips, value <= -7.17)
nrow(ne_fips)

# Ohio Warming
# oh_fips <- filter(fdat, state == "oh")
# oh_fips <- arrange(oh_fips, -value)
# oh_fips <- oh_fips[1:10, ]
# # oh_fips <- filter(oh_fips, value <= 3.30)
# nrow(oh_fips)

# Indiana cooling
# in_fips <- filter(fdat, state == "in")
# in_fips <- arrange(in_fips, value)
# in_fips <- in_fips[1:10, ]
# # in_fips <- filter(in_fips, value <= -9)
# nrow(in_fips)

# Georgia warming and  cooling
gaw_fips <- filter(fdat, state == "ga")
gaw_fips <- arrange(gaw_fips, -value)
gaw_fips <- gaw_fips[1:10, ]
# in_fips <- filter(in_fips, value <= -9)
nrow(gaw_fips)

gac_fips <- filter(fdat, state == "ga")
gac_fips <- arrange(gac_fips, value)
gac_fips <- gac_fips[1:10, ]
# in_fips <- filter(in_fips, value <= -9)
nrow(gac_fips)

# Oklahoma warming and  cooling
ok_w_fips <- filter(fdat, state == "ok")
ok_w_fips <- arrange(ok_w_fips, -value)
ok_w_fips <- ok_w_fips[1:20, ]
# in_fips <- filter(in_fips, value <= -9)
nrow(ok_w_fips)

ok_c_fips <- filter(fdat, state == "ok")
ok_c_fips <- arrange(ok_c_fips, value)
ok_c_fips <- ok_c_fips[1:20, ]
# in_fips <- filter(in_fips, value <= -9)
nrow(ok_c_fips)


# Oklahoma warming and  cooling
nc_w_fips <- filter(fdat, state == "nc")
nc_w_fips <- arrange(nc_w_fips, -value)
nc_w_fips <- nc_w_fips[1:20, ]
# in_fips <- filter(in_fips, value <= -9)
nrow(nc_w_fips)

nc_c_fips <- filter(fdat, state == "nc")
nc_c_fips <- arrange(nc_c_fips, value)
nc_c_fips <- nc_c_fips[1:20, ]
# in_fips <- filter(in_fips, value <= -9)
nrow(nc_c_fips)

mmap <- select(sc_fips, fips, value)
names(mmap) <- c("region", "value")
outmap <- county_choropleth(mmap,
                 title      = NULL, state_zoom = states)

outmap <- outmap + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
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
                       plot.margin = unit(c(0, 10, 2, 0), "cm")) 
  
outmap



cols <- colorRampPalette(rev(brewer.pal(7, "RdYlBu")))
mod7_res50_00_hist <- mod7_res50_00
mod7_res50_00_hist$col <- NA
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value <= -8.413, 1, mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -8.412, ifelse(mod7_res50_00_hist$value <= -3.932, 2, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -3.931, ifelse(mod7_res50_00_hist$value <= -1.110, 3, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -1.109, ifelse(mod7_res50_00_hist$value <= 0.903, 4, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 0.904, ifelse(mod7_res50_00_hist$value <= 2.949, 5, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 2.950, ifelse(mod7_res50_00_hist$value <= 6.901, 6, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 6.902, 7, mod7_res50_00_hist$col)
unique(mod7_res50_00_hist$col)

gghist <- ggplot(mod7_res50_00_hist, aes(value)) + 
  geom_histogram(bins = 100, aes(fill = factor(col))) + 
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  scale_fill_manual(values = cols(7)) + theme_tufte(base_size = 8) +
  # xlab("Change in Degree Day 30C (30-year Rolling Mean)") + 
  xlab(NULL) + 
  ylab(NULL) +
  theme(legend.position = "none",
                       # axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_blank()) 
  

gghist

library(gridExtra)
# grid.arrange(mod7_map, gghist, heights = c(1, .5), ncol = 1)

modmap_trend <- ggdraw() + draw_plot(mod7_map) + draw_plot(gghist, .00, .00, height = .2, width = .5)
ggdraw() + draw_plot(mod7_map) + draw_plot(gghist, .00, .00, height = .2, width = .5)
ggsave(filename = "figures/residual_change_map.pdf", width = 6, height = 4)

# Region Analysis
#------------------------------------------------------
#----------------------------------
# Mississippi delta north versus south
il <- filter(cropdat, state %in% c("ms", "la", "ar"))
il <- filter(il, fips %in% ms_w_fips$fips)
il$location <- "Mississippi Delta (Warming)"

ia <- filter(cropdat, state %in% c("ms", "la", "ar"))
ia <- filter(ia, fips %in% ms_c_fips$fips)
ia$location <- "Mississippi Delta (Cooling)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D"))
il_p1

il_crops <- filter(cropdat, state %in% c("ms", "la", "ar"))
il_crops <- filter(il_crops, fips %in% ms_w_fips$fips)
ia_crops <- filter(cropdat, state %in% c("ms", "la", "ar"))
ia_crops <- filter(ia_crops, fips %in% ms_c_fips$fips)
il_crops$location <- "Mississippi Delta (Warming)"
ia_crops$location <- "Mississippi Delta (Cooling)"

il_crops <- rbind(il_crops, ia_crops)
il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops$decade <- ifelse(il_crops$year >= 1990, 3, il_crops$decade)
# il_crops <- filter(cropdat, state %in% c("il", "in"))
# il_crops <- filter(il_crops, year <= 1969 | year >= 2000)
# il_crops$location <- ifelse(il_crops$lat <= 40.5, "Southern Illinois/Indiana", "Northen Illinois/Indiana")
# il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops <- il_crops %>% 
  group_by(fips, decade, location) %>% 
  summarise(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            cotton_a = mean(cotton_a,na.rm = TRUE)) %>% 
  group_by(decade, location) %>% 
    summarise(corn_grain_a = sum(corn_grain_a, na.rm = TRUE),
            hay_a = sum(hay_a, na.rm = TRUE),
            soybean_a = sum(soybean_a, na.rm = TRUE),
            wheat_a = sum(wheat_a, na.rm = TRUE),
            cotton_a = sum(cotton_a, na.rm = TRUE)) %>% 
  ungroup()
il_crops
il_crops$acres <- rowSums(il_crops[, c("corn_grain_a", "hay_a", "soybean_a", "wheat_a", "cotton_a")], na.rm = TRUE)

il_crops$corn_p <- 100*il_crops$corn_grain_a/il_crops$acres
il_crops$hay_p <- 100*il_crops$hay_a/il_crops$acres
il_crops$soybean_p <- 100*il_crops$soybean_a/il_crops$acres
il_crops$wheat_p <- 100*il_crops$wheat_a/il_crops$acres
il_crops$cotton_p <- 100*il_crops$cotton_a/il_crops$acres

head(il_crops)  

il_crops <- select(il_crops, decade, location, corn_p, hay_p, soybean_p, wheat_p, cotton_p)
il_crops <- gather(il_crops, key = "crops", value = value, -location, -decade)
head(il_crops)  
il_crops$crops <- paste0(il_crops$crops, il_crops$decade)

il_p2 <- ggplot(il_crops, aes(y=value, x=location, fill = factor(crops), group = factor(crops))) + 
  geom_bar( stat = "identity", position = position_dodge(width = 0.95), width = .95, alpha = 0.75) +
  geom_text(aes(label=paste(round(value, 2), "%")), position=position_dodge(width=.95),   vjust=-0.25, size = 1.5) +
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", "Corn", "Corn", "Hay", "Hay", "Hay", "Hay","Hay", "Hay",
                                                  "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean","Wheat", "Wheat", "Wheat", "Wheat","Wheat", "Wheat",
                         "Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("1960", "1960", "1980", "1980", "2000", "2000", "1960", "1960", "1980", "1980", "2000", "2000",
                                                "1960", "1960", "1980", "1980", "2000", "2000", "1960", "1960", "1980", "1980", "2000", "2000",
                         "1960", "1960", "1980", "1980", "2000", "2000")), position=position_dodge(width=.95),   vjust=2.60, size = 1.5) +

  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none") +
  ylim(-2, max(il_crops$value)) +
  geom_vline(xintercept = 10)
  # legend.justification = c("left", "top"),
  # legend.box.background = element_rect(colour = "grey"),
  # legend.title = element_blank(), legend.key = element_blank()) +
  # scale_fill_manual("legend", values=c("#8dd3c7", "#8dd3c7", "#ffffb3", "#ffffb3", "#bebada", "#bebada", "#fb8072", "#fb8072"))
il_p2
# ggsave("figures/ms_delta.pdf", width = 6, height = 4)



ggdraw() + draw_plot(modmap_trend, width = .85) + 
  draw_plot(il_p1, .46, .5, height = .5, width = .55) +
  draw_plot(il_p2, .46, .02, height = .5, width = .55)
ggsave("figures/sc_ga.pdf", width = 10, height = 4)



#----------------------------------
# South Carolina (warming) Georgia (cooling)
il <- filter(cropdat, state %in% c("ga"))
il <- filter(il, fips %in% gac_fips$fips)
il$location <- "Georgia (Cooling)"

ia <- filter(cropdat, state %in% c("sc"))
ia <- filter(ia, fips %in% sc_fips$fips)
ia$location <- "South Carolina (Warming)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE),
            acres_m = mean(acres, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  ylim(0, max(il$ln_rev_m)) +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D"))
il_p1

il_p3 <- ggplot(il, aes(year, log(1 + acres_m), color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # ylim(0, max(il$ln_rev_m)) +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D"))
il_p3

il_crops <- filter(cropdat, state %in% c("ga"))
il_crops <- filter(il_crops, fips %in% gac_fips$fips)
ia_crops <- filter(cropdat, state %in% c("sc"))
ia_crops <- filter(ia_crops, fips %in% sc_fips$fips)
il_crops$location <- "Georgia (Cooling)"
ia_crops$location <- "South Carolina (Warming)"

il_crops <- rbind(il_crops, ia_crops)
il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops$decade <- ifelse(il_crops$year >= 1990, 3, il_crops$decade)
# il_crops <- filter(cropdat, state %in% c("il", "in"))
# il_crops <- filter(il_crops, year <= 1969 | year >= 2000)
# il_crops$location <- ifelse(il_crops$lat <= 40.5, "Southern Illinois/Indiana", "Northen Illinois/Indiana")
# il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops <- il_crops %>% 
  group_by(fips, decade, location) %>% 
  summarise(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            cotton_a = mean(cotton_a,na.rm = TRUE)) %>% 
  group_by(decade, location) %>% 
    summarise(corn_grain_a = sum(corn_grain_a, na.rm = TRUE),
            hay_a = sum(hay_a, na.rm = TRUE),
            soybean_a = sum(soybean_a, na.rm = TRUE),
            wheat_a = sum(wheat_a, na.rm = TRUE),
            cotton_a = sum(cotton_a, na.rm = TRUE)) %>% 
  ungroup()
il_crops
il_crops$acres <- rowSums(il_crops[, c("corn_grain_a", "hay_a", "soybean_a", "wheat_a", "cotton_a")], na.rm = TRUE)

il_crops$corn_p <- 100*il_crops$corn_grain_a/il_crops$acres
il_crops$hay_p <- 100*il_crops$hay_a/il_crops$acres
il_crops$soybean_p <- 100*il_crops$soybean_a/il_crops$acres
il_crops$wheat_p <- 100*il_crops$wheat_a/il_crops$acres
il_crops$cotton_p <- 100*il_crops$cotton_a/il_crops$acres

head(il_crops)  

il_crops <- select(il_crops, decade, location, corn_p, hay_p, soybean_p, wheat_p, cotton_p)
il_crops <- gather(il_crops, key = "crops", value = value, -location, -decade)
head(il_crops)  
il_crops$crops <- paste0(il_crops$crops, il_crops$decade)

il_p2 <- ggplot(il_crops, aes(y=value, x=location, fill = factor(crops), group = factor(crops))) + 
  geom_bar( stat = "identity", position = position_dodge(width = 0.95), width = .95, alpha = 0.75) +
  geom_text(aes(label=paste0(round(value, 2), "%")), position=position_dodge(width=.95),   vjust=-0.25, size = 1.5) +
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", "Corn", "Corn", "Hay", "Hay", "Hay", "Hay","Hay", "Hay",
                                                  "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean","Wheat", "Wheat", "Wheat", "Wheat","Wheat", "Wheat",
                         "Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("1960", "1960", "1980", "1980", "2000", "2000", "1960", "1960", "1980", "1980", "2000", "2000",
                                                "1960", "1960", "1980", "1980", "2000", "2000", "1960", "1960", "1980", "1980", "2000", "2000",
                         "1960", "1960", "1980", "1980", "2000", "2000")), position=position_dodge(width=.95),   vjust=2.60, size = 1.5) +

  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none") +
  ylim(-2, max(il_crops$value)) +
  geom_vline(xintercept = 10) +
  # legend.justification = c("left", "top"),
  # legend.box.background = element_rect(colour = "grey"),
  # legend.title = element_blank(), legend.key = element_blank()) +
  scale_fill_manual("legend", values=c("#8dd3c7", "#8dd3c7", "#8dd3c7", "#ffffb3", "#ffffb3", "#ffffb3", "#bebada", "#bebada", "#bebada", "#fb8072", "#fb8072", "#fb8072", "#00BA38", "#00BA38", "#00BA38"))
il_p2
# ggsave("figures/ms_delta.pdf", width = 6, height = 4)



ggdraw() + draw_plot(modmap_trend, width = .85) + 
  draw_plot(il_p1, .46, .5, height = .5, width = .55) +
  draw_plot(il_p2, .46, .02, height = .5, width = .55)
ggsave("figures/sc_ga.pdf", width = 10, height = 4)


#----------------------------------------------------------
# Kentucky (warming and Cooling)
il <- filter(cropdat, state %in% c("ky"))
il <- filter(il, fips %in% ky_w_fips$fips)
il$location <- "Kentucky (Warming)"

ia <- filter(cropdat, state %in% c("ky"))
ia <- filter(ia, fips %in% ky_c_fips$fips)
ia$location <- "Kentucky (Cooling)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D"))
il_p1

il_crops <- filter(cropdat, state %in% c("ky"))
il_crops <- filter(il_crops, fips %in% ky_w_fips$fips)
ia_crops <- filter(cropdat, state %in% c("ky"))
ia_crops <- filter(ia_crops, fips %in% ky_c_fips$fips)
il_crops$location <- "Kentucky (Warming)"
ia_crops$location <- "Kentucky (Cooling)"

il_crops <- rbind(il_crops, ia_crops)
il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops$decade <- ifelse(il_crops$year >= 1990, 3, il_crops$decade)
# il_crops <- filter(cropdat, state %in% c("il", "in"))
# il_crops <- filter(il_crops, year <= 1969 | year >= 2000)
# il_crops$location <- ifelse(il_crops$lat <= 40.5, "Southern Illinois/Indiana", "Northen Illinois/Indiana")
# il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops <- il_crops %>% 
  group_by(fips, decade, location) %>% 
  summarise(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            cotton_a = mean(cotton_a,na.rm = TRUE)) %>% 
  group_by(decade, location) %>% 
    summarise(corn_grain_a = sum(corn_grain_a, na.rm = TRUE),
            hay_a = sum(hay_a, na.rm = TRUE),
            soybean_a = sum(soybean_a, na.rm = TRUE),
            wheat_a = sum(wheat_a, na.rm = TRUE),
            cotton_a = sum(cotton_a, na.rm = TRUE)) %>% 
  ungroup()
il_crops
il_crops$acres <- rowSums(il_crops[, c("corn_grain_a", "hay_a", "soybean_a", "wheat_a", "cotton_a")], na.rm = TRUE)

il_crops$corn_p <- 100*il_crops$corn_grain_a/il_crops$acres
il_crops$hay_p <- 100*il_crops$hay_a/il_crops$acres
il_crops$soybean_p <- 100*il_crops$soybean_a/il_crops$acres
il_crops$wheat_p <- 100*il_crops$wheat_a/il_crops$acres
il_crops$cotton_p <- 100*il_crops$cotton_a/il_crops$acres

head(il_crops)  

il_crops <- select(il_crops, decade, location, corn_p, hay_p, soybean_p, wheat_p, cotton_p)
il_crops <- gather(il_crops, key = "crops", value = value, -location, -decade)
head(il_crops)  
il_crops$crops <- paste0(il_crops$crops, il_crops$decade)

il_p2 <- ggplot(il_crops, aes(y=value, x=location, fill = factor(crops), group = factor(crops))) + 
  geom_bar( stat = "identity", position = position_dodge(width = 0.95), width = .95, alpha = 0.75) +
  geom_text(aes(label=paste(round(value, 2), "%")), position=position_dodge(width=.95),   vjust=-0.25, size = 1.5) +
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", "Corn", "Corn", "Hay", "Hay", "Hay", "Hay","Hay", "Hay",
                                                  "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean","Wheat", "Wheat", "Wheat", "Wheat","Wheat", "Wheat",
                         "Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("1960", "1960", "1980", "1980", "2000", "2000", "1960", "1960", "1980", "1980", "2000", "2000",
                                                "1960", "1960", "1980", "1980", "2000", "2000", "1960", "1960", "1980", "1980", "2000", "2000",
                         "1960", "1960", "1980", "1980", "2000", "2000")), position=position_dodge(width=.95),   vjust=2.60, size = 1.5) +

  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none") +
  ylim(-2, max(il_crops$value)) +
  geom_vline(xintercept = 10)
  # legend.justification = c("left", "top"),
  # legend.box.background = element_rect(colour = "grey"),
  # legend.title = element_blank(), legend.key = element_blank()) +
  # scale_fill_manual("legend", values=c("#8dd3c7", "#8dd3c7", "#ffffb3", "#ffffb3", "#bebada", "#bebada", "#fb8072", "#fb8072"))
il_p2
# ggsave("figures/ms_delta.pdf", width = 6, height = 4)



ggdraw() + draw_plot(modmap_trend, width = .85) + 
  draw_plot(il_p1, .46, .5, height = .5, width = .55) +
  draw_plot(il_p2, .46, .02, height = .5, width = .55)
ggsave("figures/ky.pdf", width = 10, height = 4)


#----------------------------------------------------------
# Oklahom (warming and cooling)
il <- filter(cropdat, state %in% c("ok"))
il <- filter(il, fips %in% ok_w_fips$fips)
ia <- filter(cropdat, state %in% c("ok"))
ia <- filter(ia, fips %in% ok_c_fips$fips)
il$location <- "Oklahoma (Warming)"
ia$location <- "Oklahoma (Cooling)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  ylim(0, max(il$ln_rev_m)) +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D"))
il_p1

il_crops <- filter(cropdat, state %in% c("ne"))
il_crops <- filter(il_crops, fips %in% ne_fips$fips)
ia_crops <- filter(cropdat, state %in% c("il"))
ia_crops <- filter(ia_crops, fips %in% il_fips$fips)
il_crops$location <- "Nebraska (Warming)"
ia_crops$location <- "Illinois (Cooling)"
il_crops <- rbind(il_crops, ia_crops)
il_crops$decade <- ifelse(il_crops$year <= 1980, 1, 2)

# il_crops <- filter(cropdat, state %in% c("il", "in"))
# il_crops <- filter(il_crops, year <= 1969 | year >= 2000)
# il_crops$location <- ifelse(il_crops$lat <= 40.5, "Southern Illinois/Indiana", "Northen Illinois/Indiana")
# il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops <- il_crops %>% 
  group_by(fips, decade, location) %>% 
  summarise(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            cotton_a = mean(cotton_a,na.rm = TRUE)) %>% 
  group_by(decade, location) %>% 
    summarise(corn_grain_a = sum(corn_grain_a, na.rm = TRUE),
            hay_a = sum(hay_a, na.rm = TRUE),
            soybean_a = sum(soybean_a, na.rm = TRUE),
            wheat_a = sum(wheat_a, na.rm = TRUE),
            cotton_a = sum(cotton_a, na.rm = TRUE)) %>% 
  ungroup()
il_crops
il_crops$acres <- rowSums(il_crops[, c("corn_grain_a", "hay_a", "soybean_a", "wheat_a", "cotton_a")], na.rm = TRUE)

il_crops$corn_p <- 100*il_crops$corn_grain_a/il_crops$acres
il_crops$hay_p <- 100*il_crops$hay_a/il_crops$acres
il_crops$soybean_p <- 100*il_crops$soybean_a/il_crops$acres
il_crops$wheat_p <- 100*il_crops$wheat_a/il_crops$acres
il_crops$cotton_p <- 100*il_crops$cotton_a/il_crops$acres

head(il_crops)  

il_crops <- select(il_crops, decade, location, corn_p, hay_p, soybean_p, wheat_p, cotton_p)
il_crops <- gather(il_crops, key = "crops", value = value, -location, -decade)
head(il_crops)  
il_crops$crops <- paste0(il_crops$crops, il_crops$decade)

il_p2 <- ggplot(il_crops, aes(y=value, x=location, fill = factor(crops), group = factor(crops))) + 
  geom_bar( stat = "identity", position = position_dodge(width = 0.95), width = .95, alpha = 0.75) +
  geom_text(aes(label=paste(round(value, 2), "%")), position=position_dodge(width=.95),   vjust=-0.25, size = 1.5) +
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", 
                        "Hay", "Hay", "Hay", "Hay",
                        "Soybean", "Soybean", "Soybean", "Soybean",
                        "Wheat", "Wheat", "Wheat", "Wheat",
                        "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("pre", "pre", "post", "post", "pre", "pre", "post", "post",
                                                 "pre", "pre", "post", "post", "pre", "pre", "post", "post",
                        "pre", "pre", "post", "post")), position=position_dodge(width=.95),   vjust=2.60, size = 1.5) +

  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none") +
  ylim(-2, max(il_crops$value)) +
  geom_vline(xintercept = 10)
  # legend.justification = c("left", "top"),
  # legend.box.background = element_rect(colour = "grey"),
  # legend.title = element_blank(), legend.key = element_blank()) +
  # scale_fill_manual("legend", values=c("#8dd3c7", "#8dd3c7", "#ffffb3", "#ffffb3", "#bebada", "#bebada", "#fb8072", "#fb8072"))
il_p2
# ggsave("figures/ms_delta.pdf", width = 6, height = 4)



ggdraw() + draw_plot(modmap_trend, width = .85) + 
  draw_plot(il_p1, .46, .5, height = .5, width = .55) +
  draw_plot(il_p2, .46, .02, height = .5, width = .55)
ggsave("figures/ky.pdf", width = 10, height = 4)


#----------------------------------------------------------
# Sourthern Georgia versus Northern Georgia
il <- filter(cropdat, state == "ga")
il <- filter(il, fips %in% gaw_fips$fips)
# il <- filter(il, lat <= 40.5)
ia <- filter(cropdat, state %in% c("ga"))
ia <- filter(ia, fips %in% gac_fips$fips)
# ia <- filter(ia, long >= -93)
# il$location <- ifelse(il$lat <= 40.5, "Southern Illinois/Indiana", "Northen Illinois/Indiana")
il$location <- "Sourthern Georgia"
ia$location <- "Northern Georgia"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D"))
il_p1

il_crops <- filter(cropdat, state == "ga")
il_crops <- filter(il_crops, fips %in% gaw_fips$fips)
# il_crops <- filter(il_crops, lat <= 40.5)
ia_crops <- filter(cropdat, state %in% c("ga"))
ia_crops <- filter(ia_crops, fips %in% gac_fips$fips)
# ia <- filter(ia_crops, long >= -93)
# _cropsil$location <- ifelse(il$lat <= 40.5, "Southern Illinois/Indiana", "Northen Illinois/Indiana")
il_crops$location <- "Sourthern Georgia"
ia_crops$location <- "Northern Georgia"
il_crops <- rbind(il_crops, ia_crops)
# il_crops <- filter(il_crops, year <= 1969 | year >= 2000)
il_crops$decade <- ifelse(il_crops$year <= 1980, 1, 2)

# il_crops <- filter(cropdat, state %in% c("il", "in"))
# il_crops <- filter(il_crops, year <= 1969 | year >= 2000)
# il_crops$location <- ifelse(il_crops$lat <= 40.5, "Southern Illinois/Indiana", "Northen Illinois/Indiana")
# il_crops$decade <- ifelse(il_crops$year <= 1969, 1, 2)
il_crops <- il_crops %>% 
  group_by(fips, decade, location) %>% 
  summarise(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            hay_a = mean(hay_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE),
            cotton_a = mean(cotton_a,na.rm = TRUE)) %>% 
  group_by(decade, location) %>% 
    summarise(corn_grain_a = sum(corn_grain_a, na.rm = TRUE),
            hay_a = sum(hay_a, na.rm = TRUE),
            soybean_a = sum(soybean_a, na.rm = TRUE),
            wheat_a = sum(wheat_a, na.rm = TRUE),
            cotton_a = sum(cotton_a,na.rm = TRUE)) %>% 
  ungroup()
il_crops
il_crops$acres <- rowSums(il_crops[, c("corn_grain_a", "hay_a", "soybean_a", "wheat_a", "cotton_a")], na.rm = TRUE)

il_crops$corn_p <- 100*il_crops$corn_grain_a/il_crops$acres
il_crops$hay_p <- 100*il_crops$hay_a/il_crops$acres
il_crops$soybean_p <- 100*il_crops$soybean_a/il_crops$acres
il_crops$wheat_p <- 100*il_crops$wheat_a/il_crops$acres
il_crops$cotton_p <- 100*il_crops$cotton_a/il_crops$acres

head(il_crops)  

# 
# group_by(location) %>% 
#   arrange(-decade) %>%
#   mutate(corn_c = 100*(first(corn_grain_a) - last(corn_grain_a))/last(corn_grain_a),
#          cotton_c = 100*(first(hay_a) - last(hay_a))/last(hay_a),
#          soybean_c = 100*(first(soybean_a) - last(soybean_a))/last(soybean_a),
#          wheat_c = 100*(first(wheat_a) - last(wheat_a))/last(wheat_a)) %>% 
#   filter(decade == 2)

il_crops <- select(il_crops, decade, location, corn_p,soybean_p, wheat_p, cotton_p)
il_crops <- gather(il_crops, key = "crops", value = value, -location, -decade)
head(il_crops)  
il_crops$crops <- paste0(il_crops$crops, il_crops$decade)

il_p2 <- ggplot(il_crops, aes(y=value, x=location, fill = crops)) + 
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  geom_text(aes(label=paste(round(value, 2), "%")), position=position_dodge(width=1),   vjust=-0.25, size = 2) +
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn",  
                                                 "Soybean", "Soybean", "Soybean", "Soybean","Wheat", "Wheat", "Wheat", "Wheat", 
                        "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=1), vjust=1.50, size = 2) +
  geom_text(aes(label=c("1960", "1960", "2000", "2000", 
                                                 "1960", "1960", "2000", "2000", "1960", "1960", "2000", "2000", "1960", "1960", "2000", "2000")), position=position_dodge(width=1),   vjust=2.60, size = 2) +
  
  
  # geom_text(colour="darkgray", aes(y=-3, label=c("Corn", "Corn", "Corn", "Corn", "Cotton", "Cotton", "Cotton", "Cotton", 
                                                 # "Soybean", "Soybean", "Soybean", "Soybean","Wheat", "Wheat", "Wheat", "Wheat")),  position=position_dodge(width=1), col=gray) +
  # geom_text(colour="darkgray", aes(y=-4, label=c("1960", "1960", "2000", "2000", "1960", "1960", "2000", "2000", 
                                                 # "1960", "1960", "2000", "2000", "1960", "1960", "2000", "2000")),  position=position_dodge(width=1), col=gray ) +
  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none") 
  # ylim(-5, max(il_crops$value)) +
  # #   # legend.justification = c("left", "top"),
  # #   legend.box.background = element_rect(colour = "grey"),
  # #   legend.title = element_blank(), legend.key = element_blank()) +
  # scale_fill_manual("legend", values=c("#8dd3c7", "#8dd3c7", "#ffffb3", "#ffffb3", "#bebada", "#bebada", "#fb8072", "#fb8072"))
il_p2
# ggsave("figures/ms_delta.pdf", width = 6, height = 4)



ggdraw() + draw_plot(, width = .85) + 
  draw_plot(il_p1, .46, .5, height = .5, width = .55) +
  draw_plot(il_p2, .46, .02, height = .5, width = .55)
ggsave("figures/georgia.pdf", width = 10, height = 4)


