library(tidyverse)
library(ggthemes)
library(choroplethr)
library(RColorBrewer)

cropdat <- readRDS("data/full_ag_data.rds")

states <- toupper(factor(cropdat$state))
states <- tolower(unique(state.name[match(states, state.abb)]))
states <- states[!is.na(states)]  

cropdat$region <- 0
cropdat$region <- ifelse(cropdat$ers_region == 1, "Heartland", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 2, "Northern Crescent", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 3, "Northern Great Plains", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 4, "Prairie Gateway", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 5, "Eastern Uplands", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 6, "Southern Seaboard", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 7, "Fruitful Rim", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 8, "Basin and Range", cropdat$region)
cropdat$region <- ifelse(cropdat$ers_region == 9, "Mississipi Portal", cropdat$region)

gdat <- cropdat %>% 
  group_by(fips) %>% 
  mutate(dday30_dm = dday30_rm10 - mean(dday30_rm10),
         dday10_30_dm = dday10_30_rm10 - mean(dday10_30_rm10, na.rm=TRUE))


ggdat <- gdat %>% 
  group_by(region,  year) %>% 
  summarise(dday30_dm_m = mean(dday30_dm),
            dday10_30_dm_m = mean(dday10_30_dm))

p1 <- ggplot(ggdat, aes(year, dday30_dm_m, color = factor(region))) + geom_line() +
    theme_tufte(base_size = 10) +
  geom_hline(yintercept = 0, size = .5) +
  ylab("Demeaned Degree Day 30C \n (Rolling Mean)") +
  xlab(NULL) +
  ylim(min(ggdat$dday30_dm_m), max(ggdat$dday30_dm_m)) +
  # ylim(-60, 60) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  # theme(legend.position = "none") +
  # geom_vline(xintercept = 1970, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 1980, linetype = "dashed", color = "grey") +
    theme(legend.position = "none",
    # legend.justification = c("right", "top"),
    # axis.ticks.x=element_blank(),
    # axis.text.x=element_blank(),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank())
p1
p2 <- ggplot(ggdat, aes(year, dday10_30_dm_m, color = factor(region))) + geom_line() +
    theme_tufte(base_size = 10) +
  geom_hline(yintercept = 0, size = .5) +
  ylab("Demeaned Degree Day 10-30C \n (Rolling Mean)") +
  xlab(NULL) +
  ylim(min(ggdat$dday10_30_dm_m), max(ggdat$dday10_30_dm_m)) +
  # ylim(-70, 70) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  # theme(legend.position = "none") +
  # geom_vline(xintercept = 1970, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 1980, linetype = "dashed", color = "grey") +
    theme(legend.position = "top",
          # legend.text = element_text(size = 8),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    # legend.justification = c("right", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank())
p2
plot_grid(p2, p1, ncol= 1)
ggsave("figures/id_region.pdf", width = 6, height = 5)

# Map changes from 1950-2010
cropdat$pre <- ifelse(cropdat$year <= 1960, 0, 1)
cropdat <- filter(cropdat, year <= 1960 | year >= 2000)

mod7 <- felm(dday30_rm10 ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
               trend:(lat + long) + trend_sq:(lat + long) + pre | fips, 
             data = cropdat)

sum(mod7$residuals)
summary(mod7)

mod7_res1 <- data.frame(value = rep(0, nrow(cropdat)),
                        region = rep(0, nrow(cropdat)),
                        year = rep(0, nrow(cropdat)))
mod7_res1$value <- as.numeric(mod7$residuals)
mod7_res1$region <- as.numeric(as.character(cropdat$fips))
mod7_res1$year <- cropdat$year

# mod7_res50_00 <- filter(mod7_res1, (year >= 1960 & year <= 1979) | (year >= 1980 & year <= 2010))
mod7_res50_00 <- mod7_res1
mod7_res50_00$decade <- ifelse(mod7_res1$year <= 1960, 1, 2)

mod7_res50_00 <- mod7_res50_00 %>% 
  group_by(region, decade) %>% 
  summarise(value = mean(value)) %>% 
  group_by(region) %>% 
  arrange(-decade) %>% 
  mutate(value = first(value) - last(value)) %>% 
  filter(decade == 2) %>% 
  ungroup()

mod7_res50_00 <- select(mod7_res50_00, region, value)
mod7_map <- county_choropleth(mod7_res50_00,
                 title      = NULL, state_zoom = states)

mod7_map <- mod7_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 12)+ 
  # xlab("Change 2000's to 1950's Degree Day 30C Residuals (30-year Rolling Mean) \n County FE & National Quadratic Trend with Lat and Long\n
        # dday30_rm_thirty ~ factor(fips) + dday0_10 + dday10_30 + dday30 + prec + prec_sq + \n
               # trend:(lat + long) + trend_sq:(lat + long)") +
  ylab(NULL)  + xlab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA),
                       plot.margin = unit(c(0, 2, 2, 0), "cm")) 
  
mod7_map



cols <- colorRampPalette(rev(brewer.pal(7, "RdYlBu")))
mod7_res50_00_hist <- mod7_res50_00
mod7_res50_00_hist$col <- NA
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value <= -7.645, 1, mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -7.644, ifelse(mod7_res50_00_hist$value <= -3.507, 2, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -3.506, ifelse(mod7_res50_00_hist$value <= -1.004, 3, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= -1.003, ifelse(mod7_res50_00_hist$value <= 0.877, 4, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 0.878, ifelse(mod7_res50_00_hist$value <= 2.928, 5, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 2.929, ifelse(mod7_res50_00_hist$value <= 7.222, 6, mod7_res50_00_hist$col), mod7_res50_00_hist$col)
mod7_res50_00_hist$col <- ifelse(mod7_res50_00_hist$value >= 7.223, 7, mod7_res50_00_hist$col)
unique(mod7_res50_00_hist$col)

mod7_res50_00_hist <- filter(mod7_res50_00_hist, value >= -30 & value <= 30)

gghist <- ggplot(mod7_res50_00_hist, aes(value)) + 
  geom_histogram(bins = 90, aes(fill = factor(col))) + 
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  scale_fill_manual(values = cols(7)) + 
  theme_tufte(base_size = 12) +
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

# modmap_trend <- ggdraw() + draw_plot(map) + draw_plot(gghist, .00, .00, height = .2, width = .5)
ggdraw() + draw_plot(mod7_map) + draw_plot(gghist, .14, .00, height = .2, width = .6)
ggsave("figures/id_map_dday30.jpg", width = 6, height = 4)


# IV Idenfication

gdat <- cropdat %>% 
  group_by(fips) %>% 
  mutate(dday30_dm = dday30_iv10 - mean(dday30_iv10),
         dday10_30_dm = dday10_30_iv10 - mean(dday10_30_iv10, na.rm=TRUE))


ggdat <- gdat %>% 
  group_by(region,  year) %>% 
  summarise(dday30_dm_m = mean(dday30_dm),
            dday10_30_dm_m = mean(dday10_30_dm))

p1 <- ggplot(ggdat, aes(year, dday30_dm_m, color = factor(region))) + geom_line() +
    theme_tufte(base_size = 10) +
  geom_hline(yintercept = 0, size = .5) +
  ylab("Demeaned Degree Day 30C \n (10-year lags)") +
  xlab(NULL) +
  ylim(min(ggdat$dday30_dm_m), max(ggdat$dday30_dm_m)) +
  # ylim(-60, 60) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  # theme(legend.position = "none") +
  # geom_vline(xintercept = 1970, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 1980, linetype = "dashed", color = "grey") +
    theme(legend.position = "none",
    # legend.justification = c("right", "top"),
    # axis.ticks.x=element_blank(),
    # axis.text.x=element_blank(),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank())
p1

p2 <- ggplot(ggdat, aes(year, dday10_30_dm_m, color = factor(region))) + geom_line() +
    theme_tufte(base_size = 10) +
  geom_hline(yintercept = 0, size = .5) +
  ylab("Demeaned Degree Day 10-30C \n (10-year lags)") +
  xlab(NULL) +
  ylim(min(ggdat$dday10_30_dm_m), max(ggdat$dday10_30_dm_m)) +
  # ylim(-70, 70) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  # theme(legend.position = "none") +
  # geom_vline(xintercept = 1970, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 1980, linetype = "dashed", color = "grey") +
    theme(legend.position = "top",
          # legend.text = element_text(size = 8),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank(),
    # legend.justification = c("right", "top"),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank())
p2
plot_grid(p2, p1, ncol= 1)
ggsave("figures/id_iv_region.pdf", width = 6, height = 5)

p1 <- ggplot(gdat, aes(year, dday30_dm, color = factor(region), group = fips)) + geom_line() +
    theme_tufte(base_size = 10) +
  geom_hline(yintercept = 0, size = .5) +
  ylab("Demeaned Degree Day 30C \n (10-year lags)") +
  xlab(NULL) +
  # ylim(min(ggdat$dday30_dm_m), max(ggdat$dday30_dm_m)) +
  # ylim(-60, 60) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  # theme(legend.position = "none") +
  # geom_vline(xintercept = 1970, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 1980, linetype = "dashed", color = "grey") +
    theme(legend.position = "none",
    # legend.justification = c("right", "top"),
    # axis.ticks.x=element_blank(),
    # axis.text.x=element_blank(),
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) + 
  facet_wrap(~region, scales = 'free')
p1
ggsave("figures/id_iv_region_county.pdf", width = 6, height = 5)
