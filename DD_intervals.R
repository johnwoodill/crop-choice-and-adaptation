library(lfe)
library(tidyverse)
library(ggthemes)
library(systemfit)


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year < 2010)
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C
cropdat$state <- factor(cropdat$state)

dummyCreator <- function(invec, prefix = NULL) {
     L <- length(invec)
     ColNames <- sort(unique(invec))
     M <- matrix(0L, ncol = length(ColNames), nrow = L,
                 dimnames = list(NULL, ColNames))
     M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
     if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
     M
} 

# Exposure weighted values equal zero
# cropdat$tavg <- cropdat$tavg - mean(cropdat$tavg, na.rm = TRUE)
# cropdat$dday0_10 <- cropdat$dday0_10 - mean(cropdat$dday0_10, na.rm = TRUE)
# cropdat$dday10_30 <- cropdat$dday10_30 - mean(cropdat$dday10_30, na.rm = TRUE)
# cropdat$dday30C <- cropdat$dday30C - mean(cropdat$dday30C, na.rm = TRUE)
# cropdat$prec <- cropdat$prec - mean(cropdat$prec, na.rm = TRUE)
cropdat$prec_sq <- cropdat$prec^2

#cropdat <- filter(cropdat, state == "wi")
#cropdat

# # Constant prices
cropdat$corn_rprice <- mean(cropdat$corn_rprice, na.rm = TRUE)
cropdat$cotton_rprice <- mean(cropdat$cotton_rprice, na.rm = TRUE)
cropdat$hay_rprice <- mean(cropdat$hay_rprice, na.rm = TRUE)
cropdat$wheat_rprice <- mean(cropdat$wheat_rprice, na.rm = TRUE)
cropdat$soybean_rprice <- mean(cropdat$soybean_rprice, na.rm = TRUE)

cropdat <- cropdat %>% 
   group_by(fips) %>% 
   mutate(avg_corn_a = mean(corn_grain_a, na.rm = TRUE),
          avg_cotton_a = mean(cotton_a, na.rm = TRUE),
          avg_hay_a = mean(hay_a, na.rm = TRUE),
          avg_soybean_a = mean(soybean_a, na.rm = TRUE),
          avg_wheat_a = mean(wheat_a, na.rm = TRUE))
 
# Total Activity
cropdat$corn <- cropdat$corn_yield*cropdat$corn_rprice
cropdat$cotton <- cropdat$cotton_yield*cropdat$cotton_rprice
cropdat$hay <- cropdat$hay_yield*cropdat$hay_rprice
cropdat$wheat <- cropdat$wheat_yield*cropdat$wheat_rprice
cropdat$soybean <- cropdat$soybean_yield*cropdat$soybean_rprice

# Constant acres in revenue per acre
cropdat$c_corn <- (cropdat$corn_grain_p/cropdat$avg_corn_a)*cropdat$corn_rprice
cropdat$c_cotton <- (cropdat$cotton_p/cropdat$avg_cotton_a)*cropdat$cotton_rprice
cropdat$c_hay <- (cropdat$hay_p/cropdat$avg_hay_a)*cropdat$hay_rprice
cropdat$c_soybean <- (cropdat$soybean_p/cropdat$avg_soybean_a)*cropdat$soybean_rprice
cropdat$c_wheat <- (cropdat$wheat_p/cropdat$avg_wheat_a)*cropdat$wheat_rprice


cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
cropdat$c_rev <- rowSums(cropdat[, c("c_corn", "c_cotton", "c_hay", "c_soybean", "c_wheat")], na.rm = TRUE)

cropdat$acres <- rowSums(cropdat[, c("corn_grain_a", "cotton_a", "hay_a", "soybean_a", "wheat_a")], na.rm = TRUE)
cropdat$c_acres <- rowSums(cropdat[, c("avg_corn_a", "avg_cotton_a", "avg_hay_a", "avg_soybean_a", "avg_wheat_a")], na.rm = TRUE)

cropdat$ln_rev <- log(1 + cropdat$rev)
cropdat$c_ln_rev <- log(1 + cropdat$c_rev)

cropdat$ln_acres <- log(1 + cropdat$acres)
cropdat$c_ln_acres <- log(1 + cropdat$c_acres)

# Set weights 1950-1980 Average Acres
wdat <- cropdat %>% 
  filter(year <= 1979) %>% 
  group_by(fips) %>% 
  summarise(w_acres = mean(acres, na.rm = TRUE),
            w_corn_a = mean(corn_grain_a, na.rm = TRUE))

cropdat <- left_join(cropdat, wdat, by = "fips")

# Proportion of crop acres as total of harvested_farmland_a
cropdat$corn_grain_a <- ifelse(is.na(cropdat$corn_grain_a), 0, cropdat$corn_grain_a)
cropdat$cotton_a <- ifelse(is.na(cropdat$cotton_a), 0, cropdat$cotton_a)
cropdat$hay_a <- ifelse(is.na(cropdat$hay_a), 0, cropdat$hay_a)
cropdat$wheat_a <- ifelse(is.na(cropdat$wheat_a), 0, cropdat$wheat_a)
cropdat$soybean_a <- ifelse(is.na(cropdat$soybean_a), 0, cropdat$soybean_a)


cropdat$p_corn_a <- cropdat$corn_grain_a/cropdat$acres
cropdat$p_cotton_a <- cropdat$cotton_a/cropdat$acres
cropdat$p_hay_a <- cropdat$hay_a/cropdat$acres
cropdat$p_soybean_a <- cropdat$soybean_a/cropdat$acres
cropdat$p_wheat_a <- cropdat$wheat_a/cropdat$acres

# cropdat$p_corn_a <- cropdat$corn_grain_a/cropdat$cropland_a
# cropdat$p_cotton_a <- cropdat$cotton_a/cropdat$cropland_a
# cropdat$p_hay_a <- cropdat$hay_a/cropdat$cropland_a
# cropdat$p_soybean_a <- cropdat$soybean_a/cropdat$cropland_a
# cropdat$p_wheat_a <- cropdat$wheat_a/cropdat$cropland_a

cropdat$p_corn_a <- ifelse(is.infinite(cropdat$p_corn_a), NA, cropdat$p_corn_a)
cropdat$p_cotton_a <- ifelse(is.infinite(cropdat$p_cotton_a), NA, cropdat$p_cotton_a)
cropdat$p_hay_a <- ifelse(is.infinite(cropdat$p_hay_a), NA, cropdat$p_hay_a)
cropdat$p_soybean_a <- ifelse(is.infinite(cropdat$p_soybean_a), NA, cropdat$p_soybean_a)
cropdat$p_wheat_a <- ifelse(is.infinite(cropdat$p_wheat_a), NA, cropdat$p_wheat_a)



# Find warmest counties
# Find warmest counties
dat1950 <- filter(cropdat, year >= 1950 & year <= 1979)
dat1950 <- dat1950 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C_1950 = mean(dday30C, na.rm = TRUE))

dat2000 <- filter(cropdat, year >= 1980 & year <= 2009)
dat2000 <- dat2000 %>% 
  group_by(state, fips) %>% 
  summarise(dday30C_2000 = mean(dday30C, na.rm = TRUE))

dat <- left_join(dat1950, dat2000, by = c("state", "fips"))
dat$tdiff <- dat$dday30C_2000 - dat$dday30C_1950

diff <- arrange(dat, -tdiff)
head(diff)

# Split into thirds by state
diff <- diff %>% 
   group_by(state) %>% 
   mutate(thirds = dplyr::ntile(tdiff, 3))

spdiff <- filter(diff, thirds == 3) # Warmest
wfips <- spdiff$fips

tpdiff <- filter(diff, thirds == 1) # Coolest
cfips <- tpdiff$fips

moddat1 <- filter(cropdat, fips %in% wfips)
moddat1$type <- "Counties that warmed the most"
moddat2 <- filter(cropdat, fips %in% cfips)
moddat2$type <- "Counties that cooled the most"

moddat1$omega <- 1
moddat2$omega <- 0

moddat <- rbind(moddat1, moddat2)
head(moddat)

moddat$tau <- ifelse(moddat$year >= 1980, 1, 0)

moddat$did <- moddat$tau*moddat$omega
moddat$trend <- moddat$year - 1949


# Use average acres in warmest counties
# moddat$corn <- ifelse(moddat$omega == 1, (moddat$corn_grain_p/moddat$avg_corn_a)*moddat$corn_rprice, moddat$corn)
# moddat$cotton <- ifelse(moddat$omega == 1, (moddat$cotton_p/moddat$avg_cotton_a)*moddat$cotton_rprice, moddat$cotton)
# moddat$hay <- ifelse(moddat$omega == 1, (moddat$hay_p/moddat$avg_hay_a)*moddat$hay_rprice, moddat$hay)
# moddat$soybean <- ifelse(moddat$omega == 1, (moddat$soybean_p/moddat$avg_soybean_a)*moddat$soybean_rprice, moddat$soybean)
# moddat$wheat <- ifelse(moddat$omega == 1, (moddat$wheat_p/moddat$avg_wheat_a)*moddat$wheat_rprice, moddat$wheat)


state_trend <- dummyCreator(moddat$state, prefix = "state")
state_trend <- state_trend*moddat$trend

# Hand computer data
a = sapply(subset(moddat, tau == 0 & omega == 0, select = rev), mean)
b = sapply(subset(moddat, tau == 0 & omega == 1, select = rev), mean)
c = sapply(subset(moddat, tau == 1 & omega == 0, select = rev), mean)
d = sapply(subset(moddat, tau == 1 & omega == 1, select = rev), mean)

# average difference
(d - c) - (b - a)
# -10.88443

# percentage difference
((d - c) / c)* 100 - ((b - a) / a)*100
#-3.037302

moddat$type <- factor(moddat$type, levels = c("Counties that warmed the most", "Counties that cooled the most"),
                      labels = c("Counties that warmed the most", "Counties that cooled the most"))

moddat$pre <- ifelse(moddat$year >= 1980, moddat$rev, NA)

# Cluster years by 5
moddat$clyear <- floor((moddat$year - 1945)/5)


ggplot(moddat, aes(year, rev, color = factor(type))) + 
  geom_smooth(method='lm',formula=y~x) + 
  theme_tufte(base_size = 12) +
  geom_hline(yintercept = 0, color = "grey") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab(NULL) + ylab("Crop Revenue per Acre") +
  theme(legend.position = c(0,1), 
        legend.justification = c("left", "top"), 
        legend.box.background = element_rect(colour = "grey"), 
        legend.key = element_blank(),
        legend.title = element_blank()) 
  
# 5-year interval
moddat1 <- filter(moddat, year >= 1975 & year <= 1984)
state_trend1 <- dummyCreator(moddat1$state, prefix = "state")
state_trend1 <- state_trend1*(1:10)

mod1 <- felm(ln_rev ~ state_trend1 + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | state, data = moddat1, weights = moddat1$w_acres)
summary(mod1)

# 10-year interval
moddat2 <- filter(moddat, year >= 1970 & year <= 1989)
state_trend2 <- dummyCreator(moddat2$state, prefix = "state")
state_trend2 <- state_trend2*(1:20)

mod2 <- felm(ln_rev ~ state_trend2 + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | state, data = moddat2, weights = moddat2$w_acres)
summary(mod2)

# 20-year interval
moddat3 <- filter(moddat, year >= 1960 & year <= 1999)
state_trend3 <- dummyCreator(moddat3$state, prefix = "state")
state_trend3 <- state_trend3*(1:40)

mod3 <- felm(ln_rev ~ state_trend3 + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | state, data = moddat3, weights = moddat3$w_acres)
summary(mod3)

# 30-year interval
mod4 <- felm(ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | state, data = moddat, weights = moddat$w_acres)
summary(mod4)

# Regression Plot
coef <- c(mod1$coefficients[33], mod2$coefficients[33], mod3$coefficients[33], mod4$coefficients[33])
se <- c(mod1$cse[33], mod2$cse[33], mod3$cse[33], mod4$cse[33])

pdat <- data.frame(time = c("1975-1985", "1970-1990", "1960-2000", "1950-2010"),
                   coef = coef*100,
                   se = se*100)
pdat$time <- factor(pdat$time, levels = c("1975-1985", "1970-1990", "1960-2000", "1950-2010"))
ggplot(pdat, aes(time, coef)) + 
  theme_tufte() +
  geom_errorbar(data = pdat, aes(ymin = coef - (se*1.96), ymax = coef + (se*1.96)), width=.05, color = "grey") +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  #ylim(-0.06, 0.05) +
  ylim(-12, 12) +
  xlab(NULL) + 
  scale_x_discrete(labels = c("5-year difference \n 1975-1985", 
                             "10-year difference \n 1970-1990", 
                             "20-year difference \n 1960-2000", 
                             "30-year difference \n 1950-2010")) +
 annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
 annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  ylab("Treatment-effect \n (% change in rev/acre)")
