library(tidyverse)
library(lfe)
library(systemfit)
library(doParallel)

source("R/predictSUR.R")
source("R/predictFelm.R")

# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/data/full_ag_data.rds",
#                destfile = "data/full_ag_data.rds", method = "auto")

# Crop data
regdat <- readRDS("data/full_ag_data.rds")


`%!in%` = Negate(`%in%`)

cl <- makeCluster(1)
registerDoParallel(cl)

#---------------------------------------------------------------------------------------------------
# Aggregate rev/acre
mse <- data.frame(rep = 1:1000,
                  agg_base = 0,
                  agg_t_fe = 0,
                  agg_weather_mse = 0,
                  agg_climate_mse = 0,                 
                  agg_weather_climate_mse = 0)

# Weather
for(i in 1:1000){
  test_years <- sample(1950:2010, 5)
  test <- filter(regdat, year %in% test_years)
  dep_var <- test$ln_rev
  train <- filter(regdat, year %!in% test_years)
  
  # Baseline
  mod1 <- felm(ln_rev ~ 1 | 0 | 0 | 0, 
            data = train, weights = train$w)
    
  # Trends/Fixed-effects
  mod2 <- felm(ln_rev ~ trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | 0, 
            data = train, weights = train$w)
  
  # Weather
  mod3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | 0, 
            data = train, weights = train$w)
    
  # Climate
  mod4 <- felm(ln_rev ~   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | 0, 
            data = train, weights = train$w)
    
  # Weather-climate
  mod5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
                trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | 0, 
            data = train, weights = train$w)
  
  # Mod1 RMS
  mse$agg_base[i] <- sqrt(mean(mod1$residuals^2))
  
  # Mod2 RMS
  # Get fixed effects
  fe <- predictFelm(mod2, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test2 <- as.matrix(select(test, rownames(mod2$coefficients)))
  coef <- as.matrix(mod2$coefficients)
  fit <- test2 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(regdat, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- dep_var - pdat$pred
  mse$agg_t_fe[i] <- sqrt(mean(residuals^2))
  
  # Mod3 RMS
  # Get fixed effects
  fe <- predictFelm(mod3, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test3 <- as.matrix(select(test, rownames(mod3$coefficients)))
  coef <- as.matrix(mod3$coefficients)
  fit <- test3 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(regdat, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- dep_var - pdat$pred
  mse$agg_weather_mse[i] <- sqrt(mean(residuals^2))
  
  # Mod4 RMS
  # Get fixed effects
  fe <- predictFelm(mod4, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test4 <- as.matrix(select(test, rownames(mod4$coefficients)))
  coef <- as.matrix(mod4$coefficients)
  fit <- test4 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(regdat, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- dep_var - pdat$pred
  mse$agg_climate_mse[i] <- sqrt(mean(residuals^2))
  
  
  # Mod5 RMS
  # Get fixed effects
  fe <- predictFelm(mod5, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test5 <- as.matrix(select(test, rownames(mod5$coefficients)))
  coef <- as.matrix(mod5$coefficients)
  fit <- test5 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(regdat, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- dep_var - pdat$pred
  mse$agg_weather_climate_mse[i] <- sqrt(mean(residuals^2))
  

  print(i)
}  


saveRDS(mse, "data/rep_cv_model.rds")
mse <- readRDS("data/rep_cv_model.rds")
mse_func <- apply(mse, 2, mean)
mse_func

#---------------------------------------------------------------------------------------------------
# Disaggregate Revenue/Acre

dmse <- data.frame(rep = 1:1000,
                  disagg_base = 0,
                  disagg_t_fe = 0,
                  disagg_weather_mse = 0,
                  disagg_climate_mse = 0,                 
                  disagg_weather_climate_mse = 0)

# for (i in 1:1){  

d <- foreach(i = 1:2, .combine = rbind, .packages = c("dplyr", "systemfit", "lfe")) %dopar% {
  test_years <- sample(1950:2010, 5)
  test <- filter(regdat, year %in% test_years)
  dep_var <- rowSums(test[, c("ln_rev_corn", "ln_rev_cotton", "ln_rev_hay", "ln_rev_soybean", "ln_rev_wheat")])
  # dep_var <- test$ln_rev
  train <- filter(regdat, year %!in% test_years)
  
  train_dm <- demeanlist(train, fl = list(fips = factor(train$fips)))
  test_dm <- demeanlist(test, fl = list(fips = factor(test$fips)))
  
  train_means <- demeanlist(train, fl = list(fips = factor(train$fips)), means = TRUE)
  test_means <- demeanlist(test, fl = list(fips = factor(test$fips)), means = TRUE)

  
  

  # Baseline
  
  mod1 <- ln_rev_corn ~  1
  
  
  mod2 <- ln_rev_cotton ~  1
   
  
  mod3 <- ln_rev_hay ~ 1
  
  
  mod4 <- ln_rev_soybean ~  1
  
  
  mod5 <- ln_rev_wheat ~ 1
  
  baseline <- systemfit(list(corn = mod1, 
                         cotton = mod2, 
                         hay = mod3, 
                         soybean = mod4,
                         wheat = mod5), data = train, method = "SUR")
  
  
  # Trend and Fixed-effect
  
  mod1 <- ln_rev_corn ~
    trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod2 <- ln_rev_cotton ~
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
   
  
  mod3 <- ln_rev_hay ~
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod4 <- ln_rev_soybean ~
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod5 <- ln_rev_wheat ~
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
    trend_fe <- systemfit(list(corn = mod1, 
                         cotton = mod2, 
                         hay = mod3, 
                         soybean = mod4,
                         wheat = mod5), data = train_dm, method = "SUR")
  
  # Weather
  
  mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
    trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod2 <- ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
   
  
  mod3 <- ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
    weather <- systemfit(list(corn = mod1, 
                         cotton = mod2, 
                         hay = mod3, 
                         soybean = mod4,
                         wheat = mod5), data = train_dm, method = "SUR")
  
  # Climate
  
  mod1 <- ln_rev_corn ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
    trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod2 <- ln_rev_cotton ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
   
  
  mod3 <- ln_rev_hay ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod4 <- ln_rev_soybean ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod5 <- ln_rev_wheat ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
    climate <- systemfit(list(corn = mod1, 
                         cotton = mod2, 
                         hay = mod3, 
                         soybean = mod4,
                         wheat = mod5), data = train_dm, method = "SUR")
  
  # Weather and Climate
  
  mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
    trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod2 <- ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
   
  
  mod3 <- ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
  
  mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
  
    weather_climate <- systemfit(list(corn = mod1, 
                                      cotton = mod2, 
                                      hay = mod3, 
                                      soybean = mod4,
                                      wheat = mod5), data = train_dm, method = "SUR")

  # Baseline RMS
  a <- predict(baseline, test)
  a <- rowSums(a) - dep_var
  b <- dep_var - a
  head(b)
  dmse$disagg_base[i] <- sqrt(mean(b^2))
  
  
  # Trends and Fixed-effect RMS
  fit <- predict(trend_fe, test_dm)
  fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
  fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
  fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
  fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
  fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat

  fit <- rowSums(fit[1:5])
  residuals <- dep_var - fit
  dmse$disagg_t_fe[i] <- sqrt(mean(residuals^2))
  
  # Weather RMS
  fit <- predict(weather, test_dm)
  fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
  fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
  fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
  fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
  fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat

  fit <- rowSums(fit[1:5])
  residuals <- dep_var - fit
  dmse$disagg_weather_mse[i] <- sqrt(mean(residuals^2))
  
  # Climate RMS
  fit <- predict(climate, test_dm)
  fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
  fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
  fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
  fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
  fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat

  fit <- rowSums(fit[1:5])
  residuals <- dep_var - fit
  dmse$disagg_climate_mse[i] <- sqrt(mean(residuals^2))
  
  # Weather and Climate RMS
  fit <- predict(weather_climate, test_dm)
  fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
  fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
  fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
  fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
  fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat

  fit <- rowSums(fit[1:5])
  residuals <- dep_var - fit
  dmse$disagg_weather_climate_mse[i] <- sqrt(mean(residuals^2))

  dmse[i, ]  
}


stopCluster(cl)


# Results
 rep                disagg_base 
                500.500000                  17.160132 
               disagg_t_fe         disagg_weather_mse 
                  2.625683                   2.621718 
        disagg_climate_mse disagg_weather_climate_mse 
                  2.622624                   2.619858
  