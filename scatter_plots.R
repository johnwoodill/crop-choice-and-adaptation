library(tidyverse)

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

# Difference in residuals 1960 and 1970's
dd_dat$pre <- ifelse(dd_dat$year <= 1970, 0, 1)
dd_dat <- filter(dd_dat, year <= 1980)

mod7 <- felm(dday30_rm10 ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
               trend:(lat + long) + trend_sq:(lat + long) + pre | fips, 
             data = dd_dat)

sum(mod7$residuals)
summary(mod7)


mod7_res1 <- data.frame(res = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)),
                        year = rep(0, nrow(dd_dat)))

mod7_res1$ln_rev <- dd_dat$ln_rev

mod7_res1$res <- as.numeric(mod7$residuals)
mod7_res1$region <- as.numeric(as.character(dd_dat$fips))
mod7_res1$year <- dd_dat$year

mod7_res50_00 <- mod7_res1
mod7_res50_00$decade <- ifelse(mod7_res1$year <= 1970, 1, 2)

# mod7_res1$corn_grain_a <- dd_dat$corn_grain_a
# mod7_res1$cotton_a <- dd_dat$cotton_a
# mod7_res1$hay_a <- dd_dat$hay_a
# mod7_res1$soybean_a <- dd_dat$soybean_a
# mod7_res1$wheat_a <- dd_dat$wheat_a
mod7_res1$p_corn_a <- dd_dat$p_corn_a
mod7_res1$p_cotton_a <- dd_dat$p_cotton_a
mod7_res1$p_hay_a <- dd_dat$p_hay_a
mod7_res1$p_soybean_a <- dd_dat$p_soybean_a
mod7_res1$p_wheat_a <- dd_dat$p_wheat_a

head(mod7_res1)

mod7_res1$ers <- dd_dat$region

test <- mod7_res1
test$group <- as.numeric(cut_number(test$res, 3))
test$group <- factor(test$group, labels = c("Coolest", "Neutral", "Warmest"))

test <- filter(test, ers %in% c("Heartland", "Southern Seaboard", "Northern Great Plains", "Mississipi Portal"))

test <- select(test, group, ers, res, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
test <- gather(test, key = crop, value = value, -group, -ers, -res)
test$crop <- factor(test$crop, labels = c("Corn", "Cotton", "Hay", "Soybean", "Wheat"))
head(test)
# test <- test %>%
#   group_by(region, ers, group) %>%
#   summarise(ln_rev = mean(ln_rev),
#             value = mean(value),
#             corn_grain_a = mean(corn_grain_a),
#             cotton_a = mean(cotton_a),
#             hay_a = mean(hay_a),
#             soybean_a = mean(soybean_a),
#             wheat_a = mean(wheat_a),
#             p_corn_a = mean(p_corn_a),
#             p_)
# head(test)
# ggplot(test, aes(value, p_corn_a, color = factor(group))) + 
#   geom_point(alpha = 0.3) + 
#   geom_smooth(method = "lm") +
#   scale_colour_manual(values = c("blue", "orange", "red")) + 
#   theme_tufte(base_size = 8) +
#   ylab("Crop Share of Total Acres (%)") +
#   xlab(NULL) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   theme(legend.position = "top",
#         legend.title = element_blank()) + 
#   facet_wrap(~ers, scales = "free")

a1 <- filter(test, ers == "Heartland" & crop == "Corn" )
a2 <- filter(test, ers == "Southern Seaboard" & crop == "Cotton" )
a3 <- filter(test, ers == "Northern Great Plains" & crop == "Hay" )
a4 <- filter(test, ers == "Heartland" & crop == "Soybean" )
a5 <- filter(test, ers == "Northern Great Plains" & crop == "Wheat" )
a <- rbind(a1, a2, a3, a4, a5)
a <- filter(a, group %in% c("Coolest", "Warmest"))

ggplot(a, aes(res, value, color = factor(group), group = group)) + 
  theme_tufte(base_size = 8) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "black") +
  scale_colour_manual(values = c("blue", "red")) + 
  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  ylim(0, 1) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  facet_wrap(ers~crop, scales = "free", ncol = 2)

ggplot(filter(test, group %in% c("Coolest", "Warmest")), aes(res, value, color = factor(group), group = group)) + 
  geom_point(alpha = 0.3) +
  theme_tufte(base_size = 8) +
  geom_smooth(method = "lm", color = "black") +
  scale_colour_manual(values = c("blue", "red")) + 
  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  ylim(0, 1) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  facet_wrap(ers~crop, scales = "free", ncol = 5)


# ggplot(test, aes(res, ln_rev, color = factor(group))) + 
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm", se = FALSE) +
#   scale_colour_manual(values = c("blue", "orange", "red")) + 
#   theme_tufte(base_size = 8) +
#   ylab("Crop Share of Total Acres (%)") +
#   xlab(NULL) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   theme(legend.position = "top",
#         legend.title = element_blank()) + 
#   facet_wrap(ers~crop, scales = "free", ncol = 5)
