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


 # dd <- read_csv("data/fips_degree_days_1900-2013.csv")
 # prec <- read_csv("data/fips_precipitation_1900-2013.csv")
 # 
 # cropdat <- readRDS("data/full_ag_data.rds")
 # 
 # dd$year <- as.integer(dd$year)
 # dd$fips <- as.integer(dd$fips)
 # dd$X1 <- NULL
 # 
 # dd <- left_join(dd, prec, by = c("fips", "year", "month"))
 # 
 # dd_dat <- filter(dd, month >= 3 & month <= 10)
 # 
 # dd_dat <- dd_dat %>%
 #     group_by(year, fips) %>%
 #     summarise(dday0C = sum(dday0C),
 #              dday10C = sum(dday10C),
 #              dday30C = sum(dday30C),
 #              prec = sum(ppt))
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
 # names(county.fips) <- c("fips", "state")
 # 
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
 # 
 # dd_dat <- left_join(dd_dat, zip_codes, by = "fips")
 # 
 # ers_region <- read_csv("data/ResourceRegionCRDfips.csv")
 # names(ers_region) <- c("fips", "ers_region", "crd")
 # dd_dat <- left_join(dd_dat, ers_region, by = "fips")
 # dd_dat <- filter(dd_dat, abs(long) <= 100)
 # unique(factor(dd_dat$state))
 # # dd_dat <- filter(dd_dat, state %in% unique(cropdat$state))
 # 

#--------------------------------------
    #Roll.mean intervals

# 
#  dd_dat <- dd_dat %>%
#    group_by(fips) %>%
#    distinct(year, .keep_all = TRUE)
# 
# dd_dat <- dd_dat %>% 
#   group_by(fips) %>% 
#   mutate(dday30_lag1 = lag(dday30)) %>% 
#   arrange(year) %>% 
#   mutate(dday30_rm10 = roll_mean(dday30_lag1, 10, align = "right", fill = "NA"))
# 
#  saveRDS(dd_dat, "data/full_weather_data.rds")
#-----------------------------------------------


# dd_dat <- readRDS("data/full_weather_data.rds")
cropdat <- readRDS("data/full_ag_data.rds")
dd_dat <- cropdat
dd_dat <- filter(dd_dat, !is.na(state))
dd_dat <- filter(dd_dat, year >= 1960 & year <= 2010)
dd_dat$trend <- dd_dat$year - 1959
dd_dat$trend_sq <- dd_dat$trend^2
ddat <- table(dd_dat$fips)
which(ddat != 51)

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

#--------------------
# Change from 1950-1960 to 2000-2010
library(RColorBrewer)
# Regional Trends
# ggplot(dd_dat, aes(year, dday30_rm10, color = ers_region, group = fips)) + geom_smooth()

gdat <- dd_dat %>% 
  group_by(fips) %>% 
  mutate(dday30_dm = dday30_rm10 - mean(dday30_rm10))


ggdat <- gdat %>% 
  group_by(region,  year) %>% 
  summarise(dday30_dm_m = mean(dday30_dm))

ggplot(ggdat, aes(year, dday30_dm_m, color = factor(region))) + geom_line() +
    theme_tufte(base_size = 8) +
  ylab("Demeaned Degre Day 30C /n (10-year rolling mean)") +
  xlab(NULL) +
  ylim(min(ggdat$dday30_dm_m), max(ggdat$dday30_dm_m)) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none") +
  geom_vline(xintercept = 1980, linetype = "dashed", color = "grey") +
    theme(legend.position = "top",
    # legend.justification = c("right", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank())

# revenue by ers region
gdat <- dd_dat %>% 
  group_by(fips) %>% 
  mutate(ln_rev_dm = ln_rev - mean(ln_rev))


ggdat <- gdat %>% 
  group_by(region,  year) %>% 
  summarise(ln_rev_m = mean(ln_rev_dm))

ggplot(ggdat, aes(year, ln_rev_m, color = factor(region))) + geom_line() 

region_dat <- dd_dat %>% 
  group_by(year, region) %>% 
  summarise(ln_rev = mean(ln_rev, na.rm = TRUE),
            dday30_rm10 = mean(dday30_rm10, na.rm = TRUE))
head(region_dat)

ggplot(region_dat, aes(year, (ln_rev), color = region)) + geom_line() +
  geom_line(aes(year, (dday30_rm10/10), color = region), linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Degree Day 30C  (10-year Rolling Mean) \n (dashed)")) +
  expand_limits(y = 0) +
  facet_wrap(~region, scales = "free") +
  theme_tufte(base_size = 8) +
  ylab("Log(Revenue per acre) \n (solid)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none")

range(region_dat$ln_rev)

# Difference in residuals 1950 and 2000
mod7 <- felm(dday30_rm10 ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
               trend:(lat + long) + trend_sq:(lat + long) | fips, 
             data = dd_dat)

sum(mod7$residuals)
summary(mod7)

mod7_res1 <- data.frame(value = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)),
                        year = rep(0, nrow(dd_dat)))
mod7_res1$value <- as.numeric(mod7$residuals)
mod7_res1$region <- as.numeric(as.character(dd_dat$fips))
mod7_res1$year <- dd_dat$year

mod7_res50_00 <- filter(mod7_res1, (year >= 1960 & year <= 1979) | (year >= 1980 & year <= 2010))
mod7_res50_00$decade <- ifelse(mod7_res50_00$year <= 1979, 1, 2)

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


check_map <- function(x){
  mapdat <- select(x, fips, value)
  names(mapdat) <- c("region", "value")
  mod_map <- county_choropleth(mapdat,
                   title      = NULL, state_zoom = states)
  
  mod_map <- mod_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
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
    
  mod_map
}

# Summary Statistics
sumstat <- dd_dat
sumstat <- filter(sumstat, (year >= 1960 & year <= 1979) | (year >= 1980 & year <= 2010))
sumstat$decade <- ifelse(sumstat$year <= 1979, 1, 2)

sumstat <- sumstat %>% 
  select(fips, decade, ln_rev) %>% 
  group_by(fips, decade) %>% 
  summarise(ln_rev = mean(ln_rev, na.rm = TRUE)) %>% 
  arrange(-decade) %>% 
  mutate(value = first(ln_rev) - last(ln_rev)) %>% 
  filter(decade == 2) %>% 
  ungroup() %>% 
  select(fips, value)

check_map(sumstat)

# Get extreme counties
fipm <- select(dd_dat, fips, ers_region, state, lat, long)
head(fipm)
fipm <- fipm %>%
   distinct(fips, .keep_all = TRUE)
mdat <- as.data.frame(mod7_res50_00)
names(mdat) <- c("fips", "value")
head(mdat)
fdat <- left_join(mdat, fipm, by = c("fips"))
head(fdat)

# Mississippi Delta

ms_fips <- filter(fdat, ers_region == 9)
# ms_fips <- filter(ms_fips, long >= -92.5)
# ms_fips <- filter(ms_fips, long <= -90.5)
# ms_fips <- filter(ms_fips, lat <= 32)

ms_fips <- filter(ms_fips, state == "la")
ms_w_fips <- filter(ms_fips, lat <= 32)
ms_w_fips <- filter(ms_w_fips, long <= -91.5)
ms_w_fips <- arrange(ms_w_fips, -value)
ms_w_fips <- ms_w_fips[1:10, c("fips", "value")]

ms_c_fips <- filter(ms_fips, lat <= 31)
ms_c_fips <- filter(ms_c_fips, long >= -91.5)
ms_c_fips <- arrange(ms_fips, value)
ms_c_fips <- ms_c_fips[1:10, c("fips", "value")]

check_map(ms_w_fips)
check_map(ms_c_fips)

# KS NE Prairie
ks_fips <- filter(fdat, state == c("ks"))
# ks_w_fips <- filter(ks_fips, lat <= 32)
# ks_w_fips <- filter(ks_w_fips, long <= -91.5)
ks_w_fips <- arrange(ks_fips, -value)
ks_w_fips <- ks_w_fips[1:20, c("fips", "value")]
check_map(ks_w_fips)

ks_c_fips <- filter(fdat, state == "ne")
ks_c_fips <- filter(ks_c_fips, lat <= 41)
ks_c_fips <- arrange(ks_c_fips, value)
ks_c_fips <- ks_c_fips[1:20, c("fips", "value")]
check_map(ks_c_fips)


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




cols <- colorRampPalette(rev(brewer.pal(7, "RdYlBu")))
mod7_res50_00_hist <- mod7_res50_00
mod7_res50_00_hist$col <- NA
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value <= -1.142, 1, mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -1.141, ifelse(mod7_res50_00_hist$value <= 0.299, 2, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 0.2991, ifelse(mod7_res50_00_hist$value <= 1.604, 3, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 1.605, ifelse(mod7_res50_00_hist$value <= 3.319, 4, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 3.320, ifelse(mod7_res50_00_hist$value <= 5.225, 5, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 5.226, ifelse(mod7_res50_00_hist$value <= 8.491, 6, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 8.492, 7, mod7_res50_00_hist$col)
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
# il <- filter(cropdat, state %in% c("ms", "la", "ar"))
il <- filter(dd_dat, fips %in% ms_w_fips$fips)
il$location <- "Mississippi Delta (Warming)"

# ia <- filter(cropdat, state %in% c("ms", "la", "ar"))
ia <- filter(dd_dat, fips %in% ms_c_fips$fips)
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
  ylim(0, max(il$ln_rev_m)) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D"))
il_p1

il_crops <- filter(dd_dat, fips %in% ms_w_fips$fips)
ia_crops <- filter(dd_dat, fips %in% ms_c_fips$fips)
il_crops$location <- "Mississippi Delta (Warming)"
ia_crops$location <- "Mississippi Delta (Cooling)"

il_crops <- rbind(il_crops, ia_crops)
il_crops$decade <- ifelse(il_crops$year <= 1970, 1, 2)
il_crops$decade <- ifelse(il_crops$year >= 1985, 3, il_crops$decade)
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
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", "Corn", "Corn", 
                        "Hay", "Hay", "Hay", "Hay","Hay", "Hay", 
                        "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean",
                        "Wheat", "Wheat", "Wheat", "Wheat","Wheat", "Wheat", 
                        "Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
                        "1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
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
ggsave("figures/ms_delta.pdf", width = 10, height = 4)



#----------------------------------
# Southern Seaboard

al_fips <- filter(fdat, state == "al")
# al_fips <- filter(al_fips, long >= -92.5)
# al_fips <- filter(al_fips, long <= -90.5)
# al_fips <- filter(al_fips, lat <= 32)

al_w_fips <- filter(al_fips, lat <= 34)
al_w_fips <- arrange(al_w_fips, -value)
al_w_fips <- al_w_fips[1:30, c("fips", "value")]

al_c_fips <- arrange(al_fips, value)
al_c_fips <- al_c_fips[1:30, c("fips", "value")]

check_map(al_w_fips)
check_map(al_c_fips)

# South Carolina (warming) Georgia (cooling)
il <- filter(dd_dat, fips %in% al_w_fips$fips)
il$location <- "Alabama (Warming)"

ia <- filter(dd_dat, fips %in% al_c_fips$fips)
ia$location <- "Alabama (Cooling)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE),
            dday30_m = mean(dday30_rm10, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre) \n (solid)") +
  xlab(NULL) +
  # ylim(0, max(il$ln_rev_m)) +
  geom_line(aes(year, dday30_m/20, color = location), linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D")) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Degree Day 30C \n (10-year Rolling Mean) \n (dashed)")) 
il_p1

il_crops <- filter(dd_dat, fips %in% ap_w_fips$fips)
ia_crops <- filter(dd_dat, fips %in% ap_c_fips$fips)
il_crops$location <- "Alabama (Warming)"
ia_crops$location <- "Alabama (Cooling)"

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
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", "Corn", "Corn", 
                        "Hay", "Hay", "Hay", "Hay","Hay", "Hay",
                        "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean",
                        "Wheat", "Wheat", "Wheat", "Wheat","Wheat", "Wheat",
                         "Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
                        "1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
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
  draw_plot(il_p1, .4, .5, height = .5, width = .59) +
  draw_plot(il_p2, .4, .02, height = .5, width = .59)
ggsave("figures/southern_seaboard.pdf", width = 10, height = 4)


#----------------------------------------------------------
# Praire
# Oklahoma warming and  cooling
il <- filter(dd_dat, fips %in% ks_w_fips$fips)
il$location <- "Prairie (Warming)"

ia <- filter(dd_dat, fips %in% ks_c_fips$fips)
ia$location <- "Prairie (Cooling)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE),
            dday30_m = mean(dday30_rm10, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre) \n (solid)") +
  xlab(NULL) +
  # ylim(0, max(il$ln_rev_m)) +
  geom_line(aes(year, dday30_m/20, color = location), linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D")) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Degree Day 30C \n (10-year Rolling Mean) \n (dashed)")) 
il_p1

il_crops <- filter(dd_dat, fips %in% ks_w_fips$fips)
ia_crops <- filter(dd_dat, fips %in% ks_c_fips$fips)
il_crops$location <- "Oklahoma (Warming)"
ia_crops$location <- "Oklahoma (Cooling)"

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
ggsave("figures/prairie.pdf", width = 10, height = 4)

#----------------------------------------------------------
# Heartland
# Illinois warming and  cooling
il_w_fips <- filter(fdat, state == "il")
# il_w_fips <- filter(il_w_fips, long <= -98)
il_w_fips <- arrange(il_w_fips, -value)
il_w_fips <- il_w_fips[1:10, ]
# in_fips <- filter(in_fips, value <= -9)
nrow(il_w_fips)
check_map(il_w_fips)

il_c_fips <- filter(fdat, state == "il")
# il_c_fips <- filter(il_c_fips, lat <= 40)
il_c_fips <- arrange(il_c_fips, value)
il_c_fips <- il_c_fips[1:10, ]
check_map(il_c_fips)

il <- filter(dd_dat, fips %in% il_w_fips$fips)
il$location <- "Heartland (Warming)"

ia <- filter(dd_dat, fips %in% il_c_fips$fips)
ia$location <- "Heartland (Cooling)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE),
            dday30_m = mean(dday30_rm10, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre) \n (solid)") +
  xlab(NULL) +
  # ylim(0, max(il$ln_rev_m)) +
  geom_line(aes(year, dday30_m/10, color = location), linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#619CFF", "#F8766D")) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Degree Day 30C \n (10-year Rolling Mean) \n (dashed)")) 
il_p1

il_crops <- filter(dd_dat, fips %in% il_w_fips$fips)
ia_crops <- filter(dd_dat, fips %in% il_c_fips$fips)
il_crops$location <- "Heartland (Warming)"
ia_crops$location <- "Heartland (Cooling)"

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
il_crops$location <- factor(il_crops$location, levels = c("Heartland (Cooling)", "Heartland (Warming)"))

il_p2 <- ggplot(il_crops, aes(y=value, x=factor(location), fill = factor(crops), group = factor(crops))) + 
  geom_bar( stat = "identity", position = position_dodge(width = 0.95), width = .95, alpha = 0.75) +
  geom_text(aes(label=paste(round(value, 2), "%")), position=position_dodge(width=.95),   vjust=-0.25, size = 1.5) +
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", "Corn", "Corn", 
                        "Hay", "Hay", "Hay", "Hay","Hay", "Hay",
                        "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean",
                        "Wheat", "Wheat", "Wheat", "Wheat","Wheat", "Wheat",
                         "Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
                        "1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
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
  draw_plot(il_p1, .4, .5, height = .5, width = .59) +
  draw_plot(il_p2, .4, .02, height = .5, width = .59)
ggsave("figures/heartland.pdf", width = 10, height = 4)


#----------------------------------------- Ohio and WV
# Easter Uplands warming and  cooling
ap_w_fips <- filter(fdat, state %in% c("ky"))
ap_w_fips <- filter(ap_w_fips, long >= -84)
ap_w_fips <- arrange(ap_w_fips, -value)
ap_w_fips <- ap_w_fips[1:15, ]
# in_fips <- filter(in_fips, value <= -9)
check_map(ap_w_fips)

ap_c_fips <- filter(fdat, ers_region == 5)
ap_c_fips <- filter(ap_c_fips, state == "oh")

ap_c_fips <- arrange(ap_c_fips, value)
ap_c_fips <- ap_c_fips[1:15, ]
# in_fips <- filter(in_fips, value <= -9)
check_map(ap_c_fips)


il <- filter(dd_dat, fips %in% ap_w_fips$fips)
il$location <- "Kentucky (Warming)"

ia <- filter(dd_dat, fips %in% ap_c_fips$fips)
ia$location <- "Ohio (Cooling)"
il <- rbind(il, ia)

# Log revenue
il <- il %>%
  group_by(year, location) %>%
  summarise(ln_rev_m = mean(ln_rev, na.rm = TRUE),
            dday30_m = mean(dday30_rm10, na.rm = TRUE))

il_p1 <- ggplot(il, aes(year, ln_rev_m, color = factor(location))) + geom_line() +
  theme_tufte(base_size = 8) +
  ylab("log(Revenue per Acre) \n (solid)") +
  xlab(NULL) +
  # ylim(0, max(il$ln_rev_m)) +
  geom_line(aes(year, dday30_m/10, color = location), linetype = "dashed") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    # legend.justification = c("left", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  scale_color_manual(values=c("#F8766D", "#619CFF")) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Degree Day 30C \n (10-year Rolling Mean) \n (dashed)")) 
il_p1

il_crops <- filter(dd_dat, fips %in% ap_w_fips$fips)
ia_crops <- filter(dd_dat, fips %in% ap_c_fips$fips)
il_crops$location <- "Kentucky (Warming)"
ia_crops$location <- "Ohio (Cooling)"

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
il_crops$location <- factor(il_crops$location, c("Ohio (Cooling)", "Kentucky (Warming)"))

il_p2 <- ggplot(il_crops, aes(y=value, x=location, fill = factor(crops), group = factor(crops))) + 
  geom_bar( stat = "identity", position = position_dodge(width = 0.95), width = .95, alpha = 0.75) +
  geom_text(aes(label=paste(round(value, 2), "%")), position=position_dodge(width=.95),   vjust=-0.25, size = 1.5) +
  geom_text(aes(label=c("Corn", "Corn", "Corn", "Corn", "Corn", "Corn", 
                        "Hay", "Hay", "Hay", "Hay","Hay", "Hay",
                        "Soybean", "Soybean", "Soybean", "Soybean", "Soybean", "Soybean",
                        "Wheat", "Wheat", "Wheat", "Wheat","Wheat", "Wheat",
                         "Cotton", "Cotton", "Cotton", "Cotton", "Cotton", "Cotton")), position=position_dodge(width=.95), vjust=1.50, size = 1.5) +
  geom_text(aes(label=c("1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
                        "1960", "1960", "1980", "1980", "2000", "2000", 
                        "1960", "1960", "1980", "1980", "2000", "2000",
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
  draw_plot(il_p1, .4, .5, height = .5, width = .59) +
  draw_plot(il_p2, .4, .02, height = .5, width = .59)
ggsave("figures/app.pdf", width = 10, height = 4)
