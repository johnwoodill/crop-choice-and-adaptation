library(tidyverse)
library(ggthemes)
library(boot)
library(dplyr)
library(cowplot)
library(choroplethr)

rev_crop_pred <- readRDS("data/rev_crop_pred.rds")

cropdat <- readRDS("data/full_ag_data.rds")

cropdat$state <- toupper(factor(cropdat$state))
states <- tolower(unique(state.name[match(cropdat$state, state.abb)]))
states <- states[!is.na(states)]   
states

# Load Climate Sur Models
cten <- readRDS("data/cten.rds")
ctwenty <- readRDS("data/ctwenty.rds")
cthirty <- readRDS("data/cthirty.rds")

# Assign intervals
cten$predictions$type <- "10-year"
ctwenty$predictions$type <- "11-year"
cthirty$predictions$type <- "12-year"


# Bind SUR climate data
cdat <- bind_rows(cten$predictions, ctwenty$predictions, cthirty$predictions)
cdat$effect <- "Climate-effect"

# Get crop acres by county
cdat$corn.pred <- cdat$corn.pred*cropdat$acres
cdat$cotton.pred <- cdat$cotton.pred*cropdat$acres
cdat$hay.pred <- cdat$hay.pred*cropdat$acres
cdat$soybean.pred <- cdat$soybean.pred*cropdat$acres
cdat$wheat.pred <- cdat$wheat.pred*cropdat$acres

cdat$region <- rep(cropdat$fips, 18)

cdat <- cdat %>%
  group_by(region, temp, type, effect) %>%
  summarise_all(mean) %>%
  ungroup()
# 
head(cdat)
# nrow(cdat)

cdat <- filter(cdat, temp == 0 | temp == 2)

# cdat <- cdat %>%
#   group_by(region, type, effect) %>%
#   mutate(corn.pred = (corn.pred - first(corn.pred)),
#          cotton.pred = (cotton.pred - first(cotton.pred)),
#          hay.pred = (hay.pred - first(hay.pred)),
#          soybean.pred = (soybean.pred - first(soybean.pred)),
#          wheat.pred = (wheat.pred - first(wheat.pred)))

head(cdat)

cdat <- cdat %>%
  group_by(region, type, effect) %>%
  mutate(corn.pred = 100*(corn.pred - first(corn.pred))/first(corn.pred),
         cotton.pred = 100*(cotton.pred - first(cotton.pred))/first(cotton.pred),
         hay.pred = 100*(hay.pred - first(hay.pred))/first(hay.pred),
         soybean.pred = 100*(soybean.pred - first(soybean.pred))/first(soybean.pred),
         wheat.pred = 100*(wheat.pred - first(wheat.pred))/first(wheat.pred)) %>% 
  ungroup()
head(cdat)

# Corn
#----------------------------------------------------------------------------------
# Corn Temp 
corndat2 <- filter(cdat, temp == 2 & type == "10-year")
corndat2 <- select(corndat2 , region, corn.pred) %>%  rename(region = region, value = corn.pred)
corn_map2 <- county_choropleth(corndat2 ,
                 title      = NULL, state_zoom = states)

                
corn_map2 <- corn_map2 + scale_fill_brewer(palette = "RdYlBu", direction = 1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Change in Corn Acres with +2C") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
  
corn_map2 


# cotton
#----------------------------------------------------------------------------------
# cotton 
cottondat2 <- filter(cdat, temp == 5 & type == "10-year")
cottondat2 <- select(cottondat2 , region, cotton.pred) %>%  rename(region = region, value = cotton.pred)
cotton_map2 <- county_choropleth(cottondat2 ,
                 title      = NULL, state_zoom = states)

                
cotton_map2 <- cotton_map2 + scale_fill_brewer(palette = "RdYlBu", direction = 1) + 
  theme_tufte(base_size = 10)+ 
  xlab("cotton (+5C) 10-year") + ylab(NULL) + theme(legend.position = "top",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
  
cotton_map2 


# hay
#----------------------------------------------------------------------------------
# hay Temp 0
haydat0 <- filter(cdat, temp == 0 & type == "10-year")
haydat0 <- select(haydat0, region, hay.pred) %>%  rename(region = region, value = hay.pred)
head(haydat0)
hay_map0 <- county_choropleth(haydat0,
                 title      = NULL, state_zoom = states)

                
hay_map0 <- hay_map0 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 10)+ 
  xlab("hay (+0C) - 10-year") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA),
                       panel.grid = element_blank()) 
  #(top, right )
hay_map0

# hay Temp 2
haydat2 <- filter(cdat, temp == 5 & type == "10-year")
haydat2 <- select(haydat2 , region, hay.pred) %>%  rename(region = region, value = hay.pred)
hay_map2 <- county_choropleth(haydat2 ,
                 title      = NULL, state_zoom = states)

                
hay_map2 <- hay_map2 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 10)+ 
  xlab("hay (+5C) 10-year") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
  
hay_map2 


plot_grid(hay_map0, hay_map2)


# soybean
#----------------------------------------------------------------------------------
# soybean Temp 0
soybeandat0 <- filter(cdat, temp == 0 & type == "10-year")
soybeandat0 <- select(soybeandat0, region, soybean.pred) %>%  rename(region = region, value = soybean.pred)
head(soybeandat0)
soybean_map0 <- county_choropleth(soybeandat0,
                 title      = NULL, state_zoom = states)

                
soybean_map0 <- soybean_map0 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 10)+ 
  xlab("soybean (+0C) - 10-year") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA),
                       panel.grid = element_blank()) 
  #(top, right )
soybean_map0

# soybean Temp 2
soybeandat2 <- filter(cdat, temp == 5 & type == "10-year")
soybeandat2 <- select(soybeandat2 , region, soybean.pred) %>%  rename(region = region, value = soybean.pred)
soybean_map2 <- county_choropleth(soybeandat2 ,
                 title      = NULL, state_zoom = states)

                
soybean_map2 <- soybean_map2 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 10)+ 
  xlab("soybean (+5C) 10-year") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
  
soybean_map2 


plot_grid(soybean_map0, soybean_map2)

# wheat
#----------------------------------------------------------------------------------
# wheat Temp 0
wheatdat0 <- filter(cdat, temp == 0 & type == "10-year")
wheatdat0 <- select(wheatdat0, region, wheat.pred) %>%  rename(region = region, value = wheat.pred)
head(wheatdat0)
wheat_map0 <- county_choropleth(wheatdat0,
                 title      = NULL, state_zoom = states)

                
wheat_map0 <- wheat_map0 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 10)+ 
  xlab("wheat (+0C) - 10-year") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA),
                       panel.grid = element_blank()) 
  #(top, right )
wheat_map0

# wheat Temp 2
wheatdat2 <- filter(cdat, temp == 5 & type == "10-year")
wheatdat2 <- select(wheatdat2 , region, wheat.pred) %>%  rename(region = region, value = wheat.pred)
wheat_map2 <- county_choropleth(wheatdat2 ,
                 title      = NULL, state_zoom = states)

                
wheat_map2 <- wheat_map2 + scale_fill_brewer(palette = "YlGnBu") + 
  theme_tufte(base_size = 10)+ 
  xlab("wheat (+5C) 10-year") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
  
wheat_map2 


plot_grid(wheat_map0, wheat_map2)



cornmap_plot <- plot_grid(corn_map0, corn_map1, corn_map2)
cottonmap_plot <- plot_grid(cotton_map0, cotton_map1, cotton_map2)
haymap_plot <- plot_grid(hay_map0, hay_map1, hay_map2)
soybeanmap_plot <- plot_grid(soybean_map0, soybean_map1, soybean_map2)
wheatmap_plot <- plot_grid(wheat_map0, wheat_map1, wheat_map2)


ggsave("figures/corn_map.jpg", plot = cornmap_plot, width = 6, height = 5)
ggsave("figures/cotton_map.jpg", plot = cottonmap_plot, width = 6, height = 5)
ggsave("figures/hay_map.jpg", plot = haymap_plot, width = 6, height = 5)
ggsave("figures/soybean_map.jpg", plot = soybeanmap_plot, width = 6, height = 5)
ggsave("figures/wheat_map.jpg", plot = wheatmap_plot, width = 6, height = 5)

