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
cropdat$fips <- factor(cropdat$fips)

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

# State-trend dummy variables
state_trend <- dummyCreator(moddat$state, prefix = "state")
state_trend <- state_trend*moddat$trend

# County-fe dummy variables
fips_dummy <- dummyCreator(moddat$fips, prefix = "fips")

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
  

# Log revenue per acre regression

mod0 <- felm(ln_rev ~ tau + omega + tau + did, data = moddat)
summary(mod0)

mod1 <- felm(ln_rev ~ state_trend + tau + tau + did | fips | 0 | 0, data = moddat, weights = moddat$w_acres)
summary(mod1)

mod2 <- felm(ln_rev ~ state_trend + tau + did  | fips | 0 | state, data = moddat, weights = moddat$w_acres)
summary(mod2)

mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | 0 | 0 | 0, data = moddat)
summary(mod3)

mod4 <- felm(ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | 0, data = moddat, weights = moddat$w_acres)
summary(mod4)

mod5 <- felm(ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | state, data = moddat, weights = moddat$w_acres)

summary(mod5)

saveRDS(mod0, "models/dd_mod0.rds")
saveRDS(mod1, "models/dd_mod1.rds")
saveRDS(mod2, "models/dd_mod2.rds")
saveRDS(mod3, "models/dd_mod3.rds")
saveRDS(mod4, "models/dd_mod4.rds")
saveRDS(mod5, "models/dd_mod5.rds")
 

##################

# Constant log revenue per acre regression

cmod0 <- felm(c_ln_rev ~ tau + omega + tau + did, data = moddat)
summary(cmod0)

cmod1 <- felm(c_ln_rev ~ state_trend + tau + tau + did | fips | 0 | 0, data = moddat, weights = moddat$w_acres)
summary(cmod1)

cmod2 <- felm(c_ln_rev ~ state_trend + tau + did  | fips | 0 | state, data = moddat, weights = moddat$w_acres)
summary(cmod2)

cmod3 <- felm(c_ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | 0 | 0 | 0, data = moddat)
summary(cmod3)

cmod4 <- felm(c_ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | 0, data = moddat, weights = moddat$w_acres)
summary(cmod4)

cmod5 <- felm(c_ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | state, data = moddat, weights = moddat$w_acres)

summary(cmod5)

saveRDS(cmod0, "models/dd_cmod0.rds")
saveRDS(cmod1, "models/dd_cmod1.rds")
saveRDS(cmod2, "models/dd_cmod2.rds")
saveRDS(cmod3, "models/dd_cmod3.rds")
saveRDS(cmod4, "models/dd_cmod4.rds")
saveRDS(cmod5, "models/dd_cmod5.rds")
 

##################

# Acres

moda <- felm(ln_acres ~ tau + omega + tau + did, data = moddat)
summary(mod0)

modb <- felm(ln_acres ~ state_trend + tau + omega + tau + did | fips | 0 | 0, data = moddat)
summary(mod1)

modc <- felm(ln_acres ~ state_trend + omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod2)

modd <- felm(ln_acres ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | 0 | 0 | 0, data = moddat)
summary(mod3)

mode <- felm(ln_acres ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | 0, data = moddat)
summary(mode)

modf <- felm(ln_acres ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + did  | fips | 0 | state, data = moddat)
summary(modf) 

saveRDS(moda, "models/dd_moda.rds")
saveRDS(modb, "models/dd_modb.rds")
saveRDS(modc, "models/dd_modc.rds")
saveRDS(modd, "models/dd_modd.rds")
saveRDS(mode, "models/dd_mode.rds")
saveRDS(modf, "models/dd_modf.rds")

# Individual crop changes

moddat$w_corn_a <- ifelse(is.na(moddat$w_corn_a), 0, moddat$w_corn_a)

mod1a <- felm(log(corn) ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat, weights = moddat$w_acres)
summary(mod1a) 

mod2a <- felm(log(cotton) ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod2a) 

mod3a <- felm(log(hay) ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod3a) 


mod4a <- felm(log(soybean) ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod4a) 

mod5a <- felm(log(wheat) ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod5a) 

saveRDS(mod1a, "models/dd_mod1a.rds")
saveRDS(mod2a, "models/dd_mod2a.rds")
saveRDS(mod3a, "models/dd_mod3a.rds")
saveRDS(mod4a, "models/dd_mod4a.rds")
saveRDS(mod5a, "models/dd_mod5a.rds")

# Crop Shares
mod1b <- felm(p_corn_a ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod1b) 

mod2b <- felm(p_cotton_a ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod2b) 

mod3b <- felm(p_hay_a ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod3b) 


mod4b <- felm(p_soybean_a ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod4b) 

mod5b <- felm(p_wheat_a ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did  | fips | 0 | state, data = moddat)
summary(mod5b) 

saveRDS(mod1b, "models/dd_mod1b.rds")
saveRDS(mod2b, "models/dd_mod2b.rds")
saveRDS(mod3b, "models/dd_mod3b.rds")
saveRDS(mod4b, "models/dd_mod4b.rds")
saveRDS(mod5b, "models/dd_mod5b.rds")

# Crop shares convert to z-scores
# zcorn <- scale(moddat$p_corn_a, center = TRUE, scale = TRUE) 
# zcotton <- scale(moddat$p_cotton_a, center = TRUE, scale = TRUE)
# zhay <- scale(moddat$p_hay_a, center = TRUE, scale = TRUE)
# zsoybean <- scale(moddat$p_soybean_a, center = TRUE, scale = TRUE)
# zwheat <- scale(moddat$p_wheat_a, center = TRUE, scale = TRUE)


mod1b <- felm(zcorn ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips | 0 | state, data = moddat)
summary(mod1b) 

mod2b <- felm(zcotton ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips | 0 | state, data = moddat)
summary(mod2b) 

mod3b <- felm(zhay ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips | 0 | state, data = moddat)
summary(mod3b) 


mod4b <- felm(zsoybean ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips | 0 | state, data = moddat)
summary(mod4b) 

mod5b <- felm(zwheat ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq | fips | 0 | state, data = moddat)
summary(mod5b) 

tmod1 <- lm(ln_rev ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq, data = moddat )
tmod2 <- lm(tavg ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq, data = moddat )
tmod2 <- lm(p_cotton_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq, data = moddat )
tmod3 <- lm(p_hay_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq, data = moddat )
tmod4 <- lm(p_soybean_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq, data = moddat )
cor(resid(tmod1), resid(tmod2))


mod1b <- zcorn ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1

mod2b <- zcotton ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1

mod3b <- zhay ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1

mod4b <- zsoybean ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1 

mod5b <- zwheat ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1



mod1b <- p_corn_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1

mod2b <- p_cotton_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq -1

mod3b <- p_hay_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq -1

mod4b <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq -1

mod5b <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq -1

restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30C + corn_prec + corn_prec_sq  +
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30C + cotton_prec + cotton_prec_sq  +
              hay_dday0_10 + hay_dday10_30 + hay_dday30C + hay_prec + hay_prec_sq  +
              soybean_dday0_10 + soybean_dday10_30 + soybean_dday30C + soybean_prec + soybean_prec_sq +
              wheat_dday0_10 + wheat_dday10_30 + wheat_dday30C + wheat_prec + wheat_prec_sq = 0")

mod <- systemfit(list(corn = mod1b, 
                      cotton = mod2b, 
                      hay = mod3b, 
                      soybean = mod4b,
                      wheat = mod5b), data = moddat, method = "SUR",
                 restrict.matrix = restrict)

#, solvetol = 1e-40)

sur_corn <- lm(p_corn_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1, data = moddat)
sur_cotton <- lm(p_cotton_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1, data = moddat)
sur_hay <- lm(p_hay_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1, data = moddat)
sur_soybean <- lm(p_soybean_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1, data = moddat)
sur_wheat <- lm(p_wheat_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq - 1, data = moddat)

sur_corn$coefficients <- mod$coefficients[1:5]
sur_corn$se <- mod$se[1:5]
names(sur_corn$coefficients) <- c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")

sur_cotton$coefficients <- mod$coefficients[6:10]
sur_cotton$se <- mod$se[6:10]
names(sur_cotton$coefficients) <- c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")

sur_hay$coefficients <- mod$coefficients[11:15]
sur_hay$se <- mod$se[11:15]
names(sur_hay$coefficients) <- c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")

sur_soybean$coefficients <- mod$coefficients[16:20]
sur_soybean$se <- mod$se[16:20]
names(sur_soybean$coefficients) <- c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")

sur_wheat$coefficients <- mod$coefficients[21:25]
sur_wheat$se <- mod$se[21:25]
names(sur_wheat$coefficients) <- c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")
summary(sur_wheat)

saveRDS(sur_corn, "models/sur_corn.rds")
saveRDS(sur_cotton, "models/sur_cotton.rds")
saveRDS(sur_hay, "models/sur_hay.rds")
saveRDS(sur_soybean, "models/sur_soybean.rds")
saveRDS(sur_wheat, "models/sur_wheat.rds")


summary(mod)
sum(mod$coefficients)
warnings()
pmod <- predict(mod)
pmod[, 5] <- 1 - rowSums(pmod)
rowSums(pmod)
which(rowSums(pmod) != 1)
which(rowSums(pmod) < 0)

mod$eq



# Trend

moddat$tau <- moddat$trend
moddat$did <- moddat$trend*moddat$omega

moda <- felm(ln_acres ~ tau + omega + did + omega:trend, data = moddat)
summary(mod0)

modb <- felm(ln_acres ~ state_trend + tau + omega + did | fips | 0 | 0, data = moddat)
summary(mod1)

modc <- felm(ln_acres ~ state_trend + tau + omega + did  | fips | 0 | state, data = moddat)
summary(mod2)

modd <- felm(ln_acres ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + did  | 0 | 0 | 0, data = moddat)
summary(mod3)

mode <- felm(ln_acres ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + did   | fips | 0 | 0, data = moddat)
summary(mod4)

modf <- felm(ln_acres ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + did   | fips | 0 | state, data = moddat)
summary(mod5) 

saveRDS(moda, "models/dd_moda.rds")
saveRDS(modb, "models/dd_modb.rds")
saveRDS(modc, "models/dd_modc.rds")
saveRDS(modd, "models/dd_modd.rds")
saveRDS(mode, "models/dd_mode.rds")
saveRDS(modf, "models/dd_modf.rds")

mod0 <- felm(ln_acres ~ tau + omega + tau + did, data = moddat)
summary(mod0)

mod1 <- felm(ln_rev ~ tau + omega + tau + did + state_trend | fips, data = moddat)

#mod2 <- felm(ln_rev ~ tau + omega + tau + did | fips + year, data = moddat)

mod2 <- felm(ln_acres ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did | 0 | 0 | state, data = moddat)
summary(mod2)

mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did + state_trend | fips, data = moddat)

mod4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
              tau + omega + tau + did | fips + year, data = moddat)

# summary(mod1)
# summary(mod2)
# summary(mod3)
# summary(mod4)


 saveRDS(mod0, "models/dd_mod0.rds")
 saveRDS(mod1, "models/dd_mod1.rds")
 saveRDS(mod2, "models/dd_mod2.rds")
 saveRDS(mod3, "models/dd_mod3.rds")
 saveRDS(mod4, "models/dd_mod4.rds")
 saveRDS(mod5, "models/dd_mod5.rds")
 

#---------------------------
# Bootstrapping Regression

bs.dd_reg <- function (dat, model, state_trend) {
   cropdat <- dat
   cropdat$rand <- 1
   
   rfips <- sample(unique(cropdat$fips), size = length(unique(cropdat$fips))/2)
   
   cropdat$rand <- ifelse(cropdat$fips %in% rfips, 0, 1)
   
  cropdat$corn <- ifelse(cropdat$rand == 0, (cropdat$corn_grain_p/cropdat$avg_corn_a)*cropdat$corn_rprice, cropdat$corn)
  cropdat$cotton <- ifelse(cropdat$rand == 0, (cropdat$cotton_p/cropdat$avg_cotton_a)*cropdat$cotton_rprice, cropdat$cotton)
  cropdat$hay <- ifelse(cropdat$rand == 0, (cropdat$hay_p/cropdat$avg_hay_a)*cropdat$hay_rprice, cropdat$hay)
  cropdat$soybean <- ifelse(cropdat$rand == 0, (cropdat$soybean_p/cropdat$avg_soybean_a)*cropdat$soybean_rprice, cropdat$soybean)
  cropdat$wheat <- ifelse(cropdat$rand == 0, (cropdat$wheat_p/cropdat$avg_wheat_a)*cropdat$wheat_rprice, cropdat$wheat)

  cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
  moddat <- cropdat
  
  moddat$tau <- ifelse(moddat$year >= 1980, 1, 0)
  moddat$omega <- moddat$rand
  moddat$did <- moddat$tau*moddat$omega
  #moddat$trend <- moddat$year - 1949
  #moddat$state_trend <- as.numeric(factor(moddat$state, levels = unique(moddat$state)))*moddat$trend
  # fit <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
  #              omega + tau + did | state | 0 | state, data = moddat, subset = sample(nrow(moddat), 
  #                                                                                    nrow(moddat)/2, 
  #                                                                                    replace = TRUE))
  if(model == 1){
  fit <- felm(ln_rev ~ omega + tau + did, data = moddat, weights = moddat$w_acres)
  return(coef(fit))
  }
  
  if(model == 2){
  fit <- felm(ln_rev ~ omega + tau + did | state | 0 | state, data = moddat, weights = moddat$w_acres)
  return(coef(fit))
  }
  
  if(model == 3){
  fit <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did, data = moddat, weights = moddat$w_acres)
  return(coef(fit))
  }
  
  if(model == 4){
  fit <- felm(ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did | fips | 0 | state, data = moddat, weights = moddat$w_acres)
  return(coef(fit))
  }
  
}
 
bs_mod1 <- lm(ln_rev ~ state_trend + dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               omega + tau + did - 1, data = moddat)
z1 <- t(replicate(1000, bs.dd_reg(moddat, 4, state_trend)))

plot(density(z1[, 34]))
mean(z1[, 34])

for (i in 1:length(bs_mod1$coefficients)){
  bs_mod1$coefficients[i] <- mean(z1[, i], na.rm = TRUE)
  bs_mod1$se[i] <- sd(z1[, i], na.rm = TRUE)
}

bs_mod1


summary(bs_mod1)
 
 
 
#------------------------------------
# Without adaptation for warmest counties
setwd("/run/media/john/1TB/SpiderOak/Projects/adaptation-along-the-envelope/")
cropdat <- readRDS("data/full_ag_data.rds")
cropdat <- filter(cropdat, year < 2010)
cropdat$dday0_10 <- cropdat$dday0C - cropdat$dday10C
cropdat$dday10_30 <- cropdat$dday10C - cropdat$dday30C

cropdat <- filter(cropdat, fips %in% wfips)

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

set.seed(123)
cropdat$rand <- 1

rfips <- sample(unique(cropdat$fips), size = length(unique(cropdat$fips))/2)

cropdat$rand <- ifelse(cropdat$fips %in% rfips, 0, 1)

cropdat$corn <- ifelse(cropdat$rand == 0, (cropdat$corn_grain_p/cropdat$avg_corn_a)*cropdat$corn_rprice, cropdat$corn)
cropdat$cotton <- ifelse(cropdat$rand == 0, (cropdat$cotton_p/cropdat$avg_cotton_a)*cropdat$cotton_rprice, cropdat$cotton)
cropdat$hay <- ifelse(cropdat$rand == 0, (cropdat$hay_p/cropdat$avg_hay_a)*cropdat$hay_rprice, cropdat$hay)
cropdat$soybean <- ifelse(cropdat$rand == 0, (cropdat$soybean_p/cropdat$avg_soybean_a)*cropdat$soybean_rprice, cropdat$soybean)
cropdat$wheat <- ifelse(cropdat$rand == 0, (cropdat$wheat_p/cropdat$avg_wheat_a)*cropdat$wheat_rprice, cropdat$wheat)

cropdat$rev <- rowSums(cropdat[, c("corn", "cotton", "hay", "soybean", "wheat")], na.rm = TRUE)
cropdat$ln_rev <- log(1 + cropdat$rev)
cropdat$prec_sq <- cropdat$prec^2

moddat <- cropdat

moddat$tau <- ifelse(moddat$year >= 1980, 1, 0)
moddat$omega <- moddat$rand

moddat$did <- moddat$tau*moddat$omega
moddat$trend <- moddat$year - 1949
moddat$state_trend <- as.numeric(factor(moddat$state, levels = unique(moddat$state)))*moddat$trend

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

moddat$type <- ifelse(moddat$rand == 0, "No Adaptation", "Adaptation")
moddat$type <- factor(moddat$type, levels = c("No Adaptation", "Adaptation"),
                      labels = c("No Adaptation", "Adaptation"))

moddat$pre <- ifelse(moddat$year >= 1980, moddat$rev, NA)

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
  


mod0 <- felm(ln_rev ~ tau + omega + tau + did, data = moddat)

mod1 <- felm(ln_rev ~ tau + omega + tau + did + state_trend | fips, data = moddat)

#mod2 <- felm(ln_rev ~ tau + omega + tau + did | fips + year, data = moddat)

mod2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did, data = moddat)

mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
               tau + omega + tau + did + state_trend | fips, data = moddat)

mod4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + 
              tau + omega + tau + did | fips + year, data = moddat)

summary(mod0)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)


saveRDS(mod0, "models/dd_mod0.rds")
saveRDS(mod1, "models/dd_mod1.rds")
saveRDS(mod2, "models/dd_mod2.rds")
saveRDS(mod3, "models/dd_mod3.rds")
saveRDS(mod4, "models/dd_mod4.rds")
 
 