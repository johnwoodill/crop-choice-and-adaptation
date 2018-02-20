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
# # 
# # 
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
 # unique(factor(dd_dat$state))
 # dd_dat <- filter(dd_dat, state %in% unique(cropdat$state))


#--------------------------------------
# Roll.mean intervals

#50-year
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


# 15 year intervals
dd_dat <- dd_dat %>%
     group_by(fips) %>%
     arrange(year) %>%
     mutate(dday0_10_rm_fifteen = lag(rollmean(dday0_10, k = 15, align = "right", fill = "NA")),
            dday10_30_rm_fifteen = lag(rollmean(dday10_30, k = 15, align = "right", fill = "NA")),
            dday30_rm_fifteen = lag(rollmean(dday30, k = 15, align = "right", fill = "NA")),
            prec_rm_fifteen = lag(rollmean(prec, k = 15, align = "right", fill = "NA")),
            prec_sq_rm_fifteen = prec_rm_fifteen^2)

# 10-year
dd_dat <- dd_dat %>%
     group_by(fips) %>%
     arrange(year) %>%
     mutate(dday0_10_rm_ten = lag(rollmean(dday0_10, k = 10, align = "right", fill = "NA")),
            dday10_30_rm_ten = lag(rollmean(dday10_30, k = 10, align = "right", fill = "NA")),
            dday30_rm_ten = lag(rollmean(dday30, k = 10, align = "right", fill = "NA")),
            prec_rm_ten = lag(rollmean(prec, k = 10, align = "right", fill = "NA")),
            prec_sq_rm_ten = prec_rm_ten^2)
# 5-year
dd_dat <- dd_dat %>%
     group_by(fips) %>%
     arrange(year) %>%
     mutate(dday0_10_rm_five = lag(rollmean(dday0_10, k = 5, align = "right", fill = "NA")),
            dday10_30_rm_five = lag(rollmean(dday10_30, k = 5, align = "right", fill = "NA")),
            dday30_rm_five = lag(rollmean(dday30, k = 5, align = "right", fill = "NA")),
            prec_rm_five = lag(rollmean(prec, k = 5, align = "right", fill = "NA")),
            prec_sq_rm_five = prec_rm_five^2)

# 2-year
dd_dat <- dd_dat %>%
     group_by(fips) %>%
     arrange(year) %>%
     mutate(dday0_10_rm_two = lag(rollmean(dday0_10, k = 2, align = "right", fill = "NA")),
            dday10_30_rm_two = lag(rollmean(dday10_30, k = 2, align = "right", fill = "NA")),
            dday30_rm_two = lag(rollmean(dday30, k = 2, align = "right", fill = "NA")),
            prec_rm_two = lag(rollmean(prec, k = 2, align = "right", fill = "NA")),
            prec_sq_rm_two = prec_rm_two^2)


dd_dat <- filter(dd_dat, year >= 1950)
dd_dat$trend <- dd_dat$year - 1949
dd_dat$trend_sq <- dd_dat$trend^2
#
dd_dat <- dd_dat %>%
   group_by(fips) %>%
   distinct(year, .keep_all = TRUE)
#
dd_dat <- filter(dd_dat, !is.na(state))
# # ddat <- table(dd_dat$fips)
# which(ddat != 61)
dd_dat <- filter(dd_dat, !is.na(state))

# nbal <- c(12091, 22099, 37053, 48167, 51001)
dd_dat <- filter(dd_dat, fips != 12091)
dd_dat <- filter(dd_dat, fips != 22099)
dd_dat <- filter(dd_dat, fips != 37053)
dd_dat <- filter(dd_dat, fips != 48167)
dd_dat <- filter(dd_dat, fips != 51001)
ddat <- table(dd_dat$fips)
# which(ddat != 63)

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


 
saveRDS(dd_dat, "data/full_weather_data.rds")
#-----------------------------------------------


dd_dat <- readRDS("data/full_weather_data.rds")


#--------------------------------------
# Change from 1950-1960 to 2000-2010
library(RColorBrewer)
dd50 <- filter(dd_dat, year >= 1960 & year <= 1969) %>% 
  group_by(fips) %>% 
  mutate(dday30_m = dday30_rm_thirty - mean(dday30_rm_thirty))


dd00 <- filter(dd_dat, year >= 2000 & year <= 2009) %>% 
  group_by(fips) %>% 
  mutate(dday30_m = dday30_rm_thirty - mean(dday30_rm_thirty))

testdat <- data.frame(fips = dd50$fips,
                      dd50 = dd50$dday30_m,
                      dd00 = dd00$dday30_m)

testdat$c <- testdat$dd00 - testdat$dd50
testdat$change <- 100*(testdat$dd00 - testdat$dd50)/testdat$dd50
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
  mutate(dday30_dm = dday30_rm_thirty - mean(dday30_rm_thirty))


# ggplot(gdat, aes(year, dday30_dm, color = factor(ers_region), group = fips)) + geom_line()

ggdat <- gdat %>% 
  group_by(region,  year) %>% 
  summarise(dday30_dm_m = mean(dday30_dm))

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
mod7_res50_00 <- filter(mod7_res1, (year >= 1960 & year <= 1959) | (year >= 2000 & year <= 2010))
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
  # xlab("Change 2000's to 1950's Degree Day 30C Residuals (30-year Rolling Mean) \n County FE & National Quadratic Trend with Lat and Long\n
        # dday30_rm_thirty ~ factor(fips) + dday0_10 + dday10_30 + dday30 + prec + prec_sq + \n
               # trend:(lat + long) + trend_sq:(lat + long)") +
  ylab(NULL)  + xlab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
mod7_map





cols <- colorRampPalette(rev(brewer.pal(7, "RdYlBu")))
mod7_res50_00_hist <- mod7_res50_00
mod7_res50_00_hist$col <- NA
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value <= -9.154, 1, mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -9.153, ifelse(mod7_res50_00_hist$value <= -4.124, 2, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -4.123, ifelse(mod7_res50_00_hist$value <= -0.929, 3, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -0.928, ifelse(mod7_res50_00_hist$value <= 1.833, 4, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 1.834, ifelse(mod7_res50_00_hist$value <= 4.749, 5, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 4.750, ifelse(mod7_res50_00_hist$value <= 8.869, 6, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 8.869, 7, mod7_res50_00_hist$col)
unique(mod7_res50_00_hist$col)

gghist <- ggplot(mod7_res50_00_hist, aes(value)) + 
  geom_histogram(bins = 100, aes(fill = factor(col))) + 
  scale_fill_manual(values = cols(7)) + theme_tufte(base_size = 8) +
  # xlab("Change in Degree Day 30C (30-year Rolling Mean)") + 
  xlab(NULL) + 
  ylab(NULL) +
  theme(legend.position = "none",
                       # axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  

gghist

library(gridExtra)
# grid.arrange(mod7_map, gghist, heights = c(1, .5), ncol = 1)

ggdraw() + draw_plot(mod7_map) + draw_plot(gghist, .7, .025, height = .2, width = .2)
ggsave(filename = "figures/residual_change_map.pdf", width = 6, height = 4)



# Mississippi delta
msd <- filter(cropdat, state %in% c("la", "ar", "ms"))
msd$location <- ifelse(msd$lat <= 32, "Southern Mississippi Delta", "Northen Mississippi Delta")

msd <- msd %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE))

ggplot(msd, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 10) +
  ylab("log(Revenue per Acre)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 


msd_crops <- filter(cropdat, state %in% c("la", "ar", "ms"))
msd_crops <- filter(msd_crops, year <= 1969 | year >= 2000)
msd_crops$location <- ifelse(msd_crops$lat <= 32, "Southern Mississippi Delta", "Northen Mississippi Delta")
msd_crops$decade <- ifelse(msd_crops$year <= 1969, 1, 2)
msd_crops <- msd_crops %>% 
  group_by(fips, decade, location) %>% 
  summarise(corn_grain_a = mean(corn_grain_a, na.rm = TRUE),
            cotton_a = mean(cotton_a, na.rm = TRUE),
            soybean_a = mean(soybean_a, na.rm = TRUE),
            wheat_a = mean(wheat_a, na.rm = TRUE)) %>% 
  group_by(decade, location) %>% 
    summarise(corn_grain_a = sum(corn_grain_a, na.rm = TRUE),
            cotton_a = sum(cotton_a, na.rm = TRUE),
            soybean_a = sum(soybean_a, na.rm = TRUE),
            wheat_a = sum(wheat_a, na.rm = TRUE)) %>% 
  ungroup()
msd_crops$acres <- msd_crops$corn_grain_a + msd_crops$cotton_a + msd_crops$soybean_a + msd_crops$wheat_a

msd_crops$corn_p <- 100*msd_crops$corn_grain_a/msd_crops$acres
msd_crops$cotton_p <- 100*msd_crops$cotton_a/msd_crops$acres
msd_crops$soybean_p <- 100*msd_crops$soybean_a/msd_crops$acres
msd_crops$wheat_p <- 100*msd_crops$wheat_a/msd_crops$acres

head(msd_crops)  

# 
# group_by(location) %>% 
#   arrange(-decade) %>%
#   mutate(corn_c = 100*(first(corn_grain_a) - last(corn_grain_a))/last(corn_grain_a),
#          cotton_c = 100*(first(cotton_a) - last(cotton_a))/last(cotton_a),
#          soybean_c = 100*(first(soybean_a) - last(soybean_a))/last(soybean_a),
#          wheat_c = 100*(first(wheat_a) - last(wheat_a))/last(wheat_a)) %>% 
#   filter(decade == 2)

msd_crops <- select(msd_crops, decade, location, corn_p, cotton_p, soybean_p, wheat_p)
msd_crops <- gather(msd_crops, key = "crops", value = value, -location, -decade)
head(msd_crops)  
msd_crops$crops <- paste0(msd_crops$crops, msd_crops$decade)

ggplot(msd_crops, aes(y=value, x=location, fill = crops)) + 
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  geom_text(aes(label=paste(round(value, 2), "%")), position=position_dodge(width=0.9),   vjust=-0.25) +
  geom_text(colour="darkgray", aes(y=-3, label=c("Corn", "Corn", "Corn", "Corn", "Cotton", "Cotton", "Cotton", "Cotton", 
                                                 "Soybean", "Soybean", "Soybean", "Soybean","Wheat", "Wheat", "Wheat", "Wheat")),  position=position_dodge(width=1), col=gray) +
  geom_text(colour="darkgray", aes(y=-4, label=c("1960", "1960", "2000", "2000", "1960", "1960", "2000", "2000", 
                                                 "1960", "1960", "2000", "2000", "1960", "1960", "2000", "2000")),  position=position_dodge(width=1), col=gray ) +
  theme_tufte(base_size = 10) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # theme(legend.position = "none",
  #   # legend.justification = c("left", "top"),
  #   legend.box.background = element_rect(colour = "grey"),
  #   legend.title = element_blank(), legend.key = element_blank()) +
  scale_fill_manual("legend", values=c("yellow", "yellow", "grey", "grey", "orange", "orange", "brown", "brown"))



group_by(fips) %>% 
  arrange(-decade) %>%
  mutate(corn_c = (first(corn_grain_a) - last(corn_grain_a)),
         cotton_c = (first(cotton_a) - last(cotton_a)),
         hay_c = (first(hay_a) - last(hay_a)),
         soybean_c = (first(soybean_a) - last(soybean_a)),
         wheat_c = (first(wheat_a) - last(wheat_a)))
                     
sum(msd_crops$corn_c)
sum(msd_crops$cotton_c)
sum(msd_crops$hay_c)                   
sum(msd_crops$soybean_c)                   
sum(msd_crops$wheat_c)                   
  
  
  mutate(corn_c = (first(corn_grain_a) - last(corn_grain_a))/last(corn_grain_a),
         cotton_c = (first(cotton_a) - last(cotton_a))/last(cotton_a),
         hay_c = (first(hay_a) - last(hay_a))/last(hay_a),
         soybean_c = (first(soybean_a) - last(soybean_a))/last(soybean_a),
         wheat_c = (first(wheat_a) - last(wheat_a))/last(wheat_a),)

sum(msd_crops$corn_c)
