library(tidyverse)
library(lfe)
library(ggthemes)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

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
dd_dat$ers_region <- factor(dd_dat$ers_region)

# Difference in residuals 1960 and 1970's
regdat1 <- dd_dat
regdat1$pre <- ifelse(regdat1$year <= 1970, 0, 1)
regdat1 <- filter(regdat1, year <= 1980)

mod1 <- felm(dday30_rm10 ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
               trend:(lat + long) + trend_sq:(lat + long) + pre | fips, 
             data = regdat1)

sum(mod1$residuals)
summary(mod1)


mod1_res1 <- data.frame(res = rep(0, nrow(regdat1)),
                        region = rep(0, nrow(regdat1)),
                        year = rep(0, nrow(regdat1)))

mod1_res1$ln_rev <- regdat1$ln_rev

mod1_res1$res <- as.numeric(mod1$residuals)
mod1_res1$region <- as.numeric(as.character(regdat1$fips))
mod1_res1$year <- regdat1$year

mod1_res50_00 <- mod1_res1
mod1_res50_00$decade <- ifelse(mod1_res1$year <= 1970, 1, 2)

mod1_res1$p_corn_a <- regdat1$p_corn_a
mod1_res1$p_cotton_a <- regdat1$p_cotton_a
mod1_res1$p_hay_a <- regdat1$p_hay_a
mod1_res1$p_soybean_a <- regdat1$p_soybean_a
mod1_res1$p_wheat_a <- regdat1$p_wheat_a

mod1_res1$ers <- regdat1$region

test <- mod1_res1
test$group <- as.numeric(cut_number(test$res, 3))
test$group <- factor(test$group, labels = c("Coolest", "Neutral", "Warmest"))

test <- filter(test, ers %in% c("Heartland", "Southern Seaboard", "Northern Great Plains", "Mississipi Portal"))

test <- select(test, group, ers, res, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
test <- gather(test, key = crop, value = value, -group, -ers, -res)
test$crop <- factor(test$crop, labels = c("Corn", "Cotton", "Hay", "Soybean", "Wheat"))
head(test)

a1 <- filter(test, ers == "Heartland" & crop == "Corn" )
a2 <- filter(test, ers == "Southern Seaboard" & crop == "Cotton" )
a3 <- filter(test, ers == "Northern Great Plains" & crop == "Hay" )
a4 <- filter(test, ers == "Heartland" & crop == "Soybean" )
a5 <- filter(test, ers == "Northern Great Plains" & crop == "Wheat" )
a <- rbind(a1, a2, a3, a4, a5)
a <- filter(a, group %in% c("Coolest", "Warmest"))

a <- a %>% 
  group_by(group, ers, crop) %>% 
  mutate(value_ol = remove_outliers(value, na.rm = FALSE))
which(is.na(a$value_ol))

ggplot(a, aes(res, value_ol, color = factor(group), group = group)) + 
  theme_tufte(base_size = 8) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "black") +
  geom_smooth(aes(res, value_ol), method = "lm", color = "black", linetype = "dashed") +
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

# ggplot(filter(test, group %in% c("Coolest", "Warmest")), aes(res, value, color = factor(group), group = group)) + 
#   geom_point(alpha = 0.3) +
#   theme_tufte(base_size = 8) +
#   geom_smooth(method = "lm", color = "black") +
#   scale_colour_manual(values = c("blue", "red")) + 
#   theme_tufte(base_size = 8) +
#   ylab("Crop Share of Total Acres (%)") +
#   xlab(NULL) +
#   ylim(0, 1) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   theme(legend.position = "top",
#         legend.title = element_blank()) + 
#   facet_wrap(ers~crop, scales = "free", ncol = 5)


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


# Difference in residuals 1980-1995 and 1995-2010
regdat2 <- dd_dat
regdat2$pre <- ifelse(regdat2$year <= 1995, 0, 1)
regdat2 <- filter(regdat2, year >= 1980)

mod2 <- felm(dday30_rm10 ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
               trend:(lat + long) + trend_sq:(lat + long) | fips, 
             data = regdat2)

sum(mod2$residuals)
summary(mod2)


mod2_res1 <- data.frame(res = rep(0, nrow(regdat2)),
                        region = rep(0, nrow(regdat2)),
                        year = rep(0, nrow(regdat2)))

mod2_res1$ln_rev <- regdat2$ln_rev

mod2_res1$res <- as.numeric(mod2$residuals)
mod2_res1$region <- as.numeric(as.character(regdat2$fips))
mod2_res1$year <- regdat2$year

mod2_res50_00 <- mod2_res1
mod2_res50_00$decade <- ifelse(mod2_res1$year <= 1995, 1, 2)

mod2_res1$p_corn_a <- regdat2$p_corn_a
mod2_res1$p_cotton_a <- regdat2$p_cotton_a
mod2_res1$p_hay_a <- regdat2$p_hay_a
mod2_res1$p_soybean_a <- regdat2$p_soybean_a
mod2_res1$p_wheat_a <- regdat2$p_wheat_a

mod2_res1$ers <- regdat2$region

test <- mod2_res1
test$group <- as.numeric(cut_number(test$res, 3))
test$group <- factor(test$group, labels = c("Coolest", "Neutral", "Warmest"))

test <- filter(test, ers %in% c("Heartland", "Southern Seaboard", "Northern Great Plains", "Mississipi Portal"))

test <- select(test, group, ers, res, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
test <- gather(test, key = crop, value = value, -group, -ers, -res)
test$crop <- factor(test$crop, labels = c("Corn", "Cotton", "Hay", "Soybean", "Wheat"))
head(test)

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
