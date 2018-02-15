library(cowplot)
library(zoo)
library(readr)
library(dplyr)
library(lfe)
library(ggthemes)
library(choroplethr)

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
# dd_dat <- select(dd_dat, year, fips, dday0_10, dday10_30, dday30, prec, prec_sq)
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
# dd_dat <- filter(dd_dat, state %in% unique(cropdat$state))

ers_region <- read_csv("data/ResourceRegionCRDfips.csv")
names(ers_region) <- c("fips", "ers_region", "crd")
dd_dat <- left_join(dd_dat, ers_region, by = "fips")

# 
# #--------------------------------------
# #  Roll.mean intervals
# 
# # 40-year
# dd_dat <- dd_dat %>%
#    group_by(fips) %>%
#    arrange(year) %>%
#    mutate(dday0_10_rm_fifty = lag(rollmean(dday0_10, k = 50, align = "right", fill = "NA")),
#           dday10_30_rm_fifty = lag(rollmean(dday10_30, k = 50, align = "right", fill = "NA")),
#           dday30_rm_fifty = lag(rollmean(dday30, k = 50, align = "right", fill = "NA")),
#           prec_rm_fifty = lag(rollmean(prec, k = 50, align = "right", fill = "NA")),
#           prec_sq_rm_fifty = prec_rm_fifty^2)
# 
# #40-year
# dd_dat <- dd_dat %>%
#    group_by(fips) %>%
#    arrange(year) %>%
#    mutate(dday0_10_rm_fourty = lag(rollmean(dday0_10, k = 40, align = "right", fill = "NA")),
#           dday10_30_rm_fourty = lag(rollmean(dday10_30, k = 40, align = "right", fill = "NA")),
#           dday30_rm_fourty = lag(rollmean(dday30, k = 40, align = "right", fill = "NA")),
#           prec_rm_fourty = lag(rollmean(prec, k = 40, align = "right", fill = "NA")),
#           prec_sq_rm_fourty = prec_rm_fourty^2)
# 
# #30-year
# dd_dat <- dd_dat %>%
#    group_by(fips) %>%
#    arrange(year) %>%
#    mutate(dday0_10_rm_thirty = lag(rollmean(dday0_10, k = 30, align = "right", fill = "NA")),
#           dday10_30_rm_thirty = lag(rollmean(dday10_30, k = 30, align = "right", fill = "NA")),
#           dday30_rm_thirty = lag(rollmean(dday30, k = 30, align = "right", fill = "NA")),
#           prec_rm_thirty = lag(rollmean(prec, k = 30, align = "right", fill = "NA")),
#           prec_sq_rm_thirty = prec_rm_thirty^2)
# 
# #20 year intervals
# dd_dat <- dd_dat %>%
#    group_by(fips) %>%
#    arrange(year) %>%
#    mutate(dday0_10_rm_twenty = lag(rollmean(dday0_10, k = 20, align = "right", fill = "NA")),
#           dday10_30_rm_twenty = lag(rollmean(dday10_30, k = 20, align = "right", fill = "NA")),
#           dday30_rm_twenty = lag(rollmean(dday30, k = 20, align = "right", fill = "NA")),
#           prec_rm_twenty = lag(rollmean(prec, k = 20, align = "right", fill = "NA")),
#           prec_sq_rm_twenty = prec_rm_twenty^2)
# 
# # 10-year
# dd_dat <- dd_dat %>%
#    group_by(fips) %>%
#    arrange(year) %>%
#    mutate(dday0_10_rm_ten = lag(rollmean(dday0_10, k = 10, align = "right", fill = "NA")),
#           dday10_30_rm_ten = lag(rollmean(dday10_30, k = 10, align = "right", fill = "NA")),
#           dday30_rm_ten = lag(rollmean(dday30, k = 10, align = "right", fill = "NA")),
#           prec_rm_ten = lag(rollmean(prec, k = 10, align = "right", fill = "NA")),
#           prec_sq_rm_ten = prec_rm_ten^2)
# 
# dd_dat <- filter(dd_dat, year >= 1950 & year <= 2010)
# dd_dat$trend <- dd_dat$year - 1949
# dd_dat$trend_sq <- dd_dat$trend^2
# 
# saveRDS(dd_dat, "data/full_weather_data.rds")
#-----------------------------------------------

dd_dat <- readRDS("data/full_weather_data.rds")
cropdat <- readRDS("data/full_ag_data.rds")

states <- toupper(factor(dd_dat$state))
states <- tolower(unique(state.name[match(states, state.abb)]))
states <- states[!is.na(states)]  

dd_dat$state <- factor(dd_dat$state)
dd_dat$ers_region <- factor(dd_dat$ers_region)

#-----------------------------------
# Maps
# Climate dday0_10_rm
# Residual checks
mod11_ten <- felm(dday0_10_rm_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod11_ten)

mod_ten_res11 <- data.frame(value = residuals(mod11_ten))
mod_ten_res11$region <- dd_dat$fips

mod_ten_res11 <- mod_ten_res11 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_rm_ten))

map11_ten <- county_choropleth(mod_ten_res11,
                 title      = NULL, state_zoom = states)
     
map11_ten <- map11_ten + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (10-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map11_ten


mod11_twenty <- felm(dday0_10_rm_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod11_twenty)

mod_twenty_res11 <- data.frame(value = residuals(mod11_twenty))
mod_twenty_res11$region <- dd_dat$fips

mod_twenty_res11 <- mod_twenty_res11 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_rm_twenty))

map11_twenty <- county_choropleth(mod_twenty_res11,
                 title      = NULL, state_zoom = states)
     
map11_twenty <- map11_twenty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map11_twenty



mod11_thirty <- felm(dday0_10_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod11_thirty)

mod_thirty_res11 <- data.frame(value = residuals(mod11_thirty))
mod_thirty_res11$region <- dd_dat$fips

mod_thirty_res11 <- mod_thirty_res11 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_rm_thirty))

map11_thirty <- county_choropleth(mod_thirty_res11,
                 title      = NULL, state_zoom = states)
     
map11_thirty <- map11_thirty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map11_thirty



# dday10_30_rm
mod22_ten <- felm(dday10_30_rm_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod22_ten)

mod_ten_res22 <- data.frame(value = residuals(mod22_ten))
mod_ten_res22$region <- dd_dat$fips

mod_ten_res22 <- mod_ten_res22 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_rm_ten))

map22_ten <- county_choropleth(mod_ten_res22,
                 title      = NULL, state_zoom = states)
     
map22_ten <- map22_ten + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 10-30C Residuals (10-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map22_ten


mod22_twenty <- felm(dday10_30_rm_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod22_twenty)

mod_twenty_res22 <- data.frame(value = residuals(mod22_twenty))
mod_twenty_res22$region <- dd_dat$fips

mod_twenty_res22 <- mod_twenty_res22 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_rm_twenty))

map22_twenty <- county_choropleth(mod_twenty_res22,
                 title      = NULL, state_zoom = states)
     
map22_twenty <- map22_twenty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 10-30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map22_twenty



mod22_thirty <- felm(dday10_30_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod22_thirty)

mod_thirty_res22 <- data.frame(value = residuals(mod22_thirty))
mod_thirty_res22$region <- dd_dat$fips

mod_thirty_res22 <- mod_thirty_res22 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_rm_thirty))

map22_thirty <- county_choropleth(mod_thirty_res22,
                 title      = NULL, state_zoom = states)
     
map22_thirty <- map22_thirty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 10-30C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map22_thirty


plot_grid(map1_ten, map1_twenty, map1_thirty, ncol = 3)



# dday30_rm
mod33_ten <- felm(dday30_rm_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod33_ten)

mod_ten_res33 <- data.frame(value = residuals(mod33_ten))
mod_ten_res33$region <- dd_dat$fips

mod_ten_res33 <- mod_ten_res33 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_rm_ten))

map33_ten <- county_choropleth(mod_ten_res33,
                 title      = NULL, state_zoom = states)
     
map33_ten <- map33_ten + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (10-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map33_ten


mod33_twenty <- felm(dday30_rm_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod33_twenty)

mod_twenty_res33 <- data.frame(value = residuals(mod33_twenty))
mod_twenty_res33$region <- dd_dat$fips

mod_twenty_res33 <- mod_twenty_res33 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_rm_twenty))

map33_twenty <- county_choropleth(mod_twenty_res33,
                 title      = NULL, state_zoom = states)
     
map33_twenty <- map33_twenty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map33_twenty



mod33_thirty <- felm(dday30_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + state:trend + state:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod33_thirty)

mod_thirty_res33 <- data.frame(value = residuals(mod33_thirty))
mod_thirty_res33$region <- dd_dat$fips

mod_thirty_res33 <- mod_thirty_res33 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_rm_thirty))

map33_thirty <- county_choropleth(mod_thirty_res33,
                 title      = NULL, state_zoom = states)
     
map33_thirty <- map33_thirty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map33_thirty

plot_grid(map11_ten, map11_twenty, map11_thirty,
          map22_ten, map22_twenty, map22_thirty,
          map33_ten, map33_twenty, map33_thirty, ncol = 3)



# Ers-region trends


#-----------------------------------
# Maps
# Climate dday0_10_rm
# Residual checks
mod111_ten <- felm(dday0_10_rm_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod111_ten)

mod_ten_res111 <- data.frame(value = residuals(mod111_ten))
mod_ten_res111$region <- dd_dat$fips

mod_ten_res111 <- mod_ten_res111 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_rm_ten))

map111_ten <- county_choropleth(mod_ten_res111,
                 title      = NULL, state_zoom = states)
     
map111_ten <- map111_ten + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (10-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map111_ten


mod111_twenty <- felm(dday0_10_rm_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod111_twenty)

mod_twenty_res111 <- data.frame(value = residuals(mod111_twenty))
mod_twenty_res111$region <- dd_dat$fips

mod_twenty_res111 <- mod_twenty_res111 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_rm_twenty))

map111_twenty <- county_choropleth(mod_twenty_res111,
                 title      = NULL, state_zoom = states)
     
map111_twenty <- map111_twenty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map111_twenty



mod111_thirty <- felm(dday0_10_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod111_thirty)

mod_thirty_res111 <- data.frame(value = residuals(mod111_thirty))
mod_thirty_res111$region <- dd_dat$fips

mod_thirty_res111 <- mod_thirty_res111 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_rm_thirty))

map111_thirty <- county_choropleth(mod_thirty_res111,
                 title      = NULL, state_zoom = states)
     
map111_thirty <- map111_thirty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map111_thirty



# dday10_30_rm
mod222_ten <- felm(dday10_30_rm_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod222_ten)

mod_ten_res222 <- data.frame(value = residuals(mod222_ten))
mod_ten_res222$region <- dd_dat$fips

mod_ten_res222 <- mod_ten_res222 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_rm_ten))

map222_ten <- county_choropleth(mod_ten_res222,
                 title      = NULL, state_zoom = states)
     
map222_ten <- map222_ten + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 10-30C Residuals (10-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map222_ten


mod222_twenty <- felm(dday10_30_rm_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod222_twenty)

mod_twenty_res222 <- data.frame(value = residuals(mod222_twenty))
mod_twenty_res222$region <- dd_dat$fips

mod_twenty_res222 <- mod_twenty_res222 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_rm_twenty))

map222_twenty <- county_choropleth(mod_twenty_res222,
                 title      = NULL, state_zoom = states)
     
map222_twenty <- map222_twenty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 10-30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map222_twenty



mod222_thirty <- felm(dday10_30_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod222_thirty)

mod_thirty_res222 <- data.frame(value = residuals(mod222_thirty))
mod_thirty_res222$region <- dd_dat$fips

mod_thirty_res222 <- mod_thirty_res222 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_rm_thirty))

map222_thirty <- county_choropleth(mod_thirty_res222,
                 title      = NULL, state_zoom = states)
     
map222_thirty <- map222_thirty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 10-30C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map222_thirty


plot_grid(map1_ten, map1_twenty, map1_thirty, ncol = 3)



# dday30_rm
mod333_ten <- felm(dday30_rm_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod333_ten)

mod_ten_res333 <- data.frame(value = residuals(mod333_ten))
mod_ten_res333$region <- dd_dat$fips

mod_ten_res333 <- mod_ten_res333 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_rm_ten))

map333_ten <- county_choropleth(mod_ten_res333,
                 title      = NULL, state_zoom = states)
     
map333_ten <- map333_ten + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (10-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map333_ten


mod333_twenty <- felm(dday30_rm_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod333_twenty)

mod_twenty_res333 <- data.frame(value = residuals(mod333_twenty))
mod_twenty_res333$region <- dd_dat$fips

mod_twenty_res333 <- mod_twenty_res333 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_rm_twenty))

map333_twenty <- county_choropleth(mod_twenty_res333,
                 title      = NULL, state_zoom = states)
     
map333_twenty <- map333_twenty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map333_twenty



mod333_thirty <- felm(dday30_rm_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq
                 + ers_region:trend + ers_region:trend_sq
             | fips  | 0 | state, 
            data = dd_dat)
summary(mod333_thirty)

mod_thirty_res333 <- data.frame(value = residuals(mod333_thirty))
mod_thirty_res333$region <- dd_dat$fips

mod_thirty_res333 <- mod_thirty_res333 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_rm_thirty))

map333_thirty <- county_choropleth(mod_thirty_res333,
                 title      = NULL, state_zoom = states)
     
map333_thirty <- map333_thirty + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 30C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
map333_thirty

plot_grid(map111_ten, map111_twenty, map111_thirty,
          map222_ten, map222_twenty, map222_thirty,
          map333_ten, map333_twenty, map333_thirty, ncol = 3)







