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
regdat$ln_corn_mrev <- log(1 + regdat$corn_mrev)
regdat$ln_cotton_mrev <- log(1 + regdat$cotton_mrev)
regdat$ln_hay_mrev <- log(1 + regdat$hay_mrev)
regdat$ln_wheat_mrev <- log(1 + regdat$wheat_mrev)
regdat$ln_soybean_mrev <- log(1 + regdat$soybean_mrev)

`%!in%` = Negate(`%in%`)


# See cv_model_plot.R for results and plot

#---------------------------------------------------------------------------------------------------
# Aggregate rev/acre (rolling mean climate)

cl <- makeCluster(1)
registerDoParallel(cl)

outdat <- data.frame()

for (j in 1:6){
  if (j == 1){
    dep_var = "ln_rev"
    data <- filter(regdat, ln_rev > 0)
  }
  
  if (j == 2){
    dep_var = "ln_corn_mrev"
    data <- filter(regdat, ln_corn_mrev > 0)
  }
  
  if (j == 3){
    dep_var = "ln_cotton_mrev"
    data <- filter(regdat, ln_cotton_mrev > 0)
  }
  
  if (j == 4){
    dep_var = "ln_hay_mrev"
    data <- filter(regdat, ln_hay_mrev > 0)
  }
  
  if (j == 5){
    dep_var = "ln_soybean_mrev"
    data <- filter(regdat, ln_soybean_mrev > 0)
  }
  
  if (j == 6){
    dep_var = "ln_wheat_mrev"
    data <- filter(regdat, ln_wheat_mrev > 0)
  }
  
  mse <- data.frame(rep = 1:1,
                  agg_t_fe = 0,
                  agg_weather_mse = 0,
                  agg_climate_mse = 0,                 
                  agg_weather_climate_mse = 0,
                  dep_var = dep_var,
                  climate_var = 0)
  
  d <- foreach(i = 1:1, .combine = rbind, .packages = c("lfe", "dplyr")) %dopar% {
  
  test_years <- sample(1950:2010, 5)
  test <- filter(data, year %in% test_years)
  train <- filter(data, year %!in% test_years)
  
  form1 <- as.formula(paste0(dep_var, "~ trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form2 <- as.formula(paste0(dep_var, "~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form3 <- as.formula(paste0(dep_var, "~   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form4 <- as.formula(paste0(dep_var, "~  dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                    dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  
  # Baseline
  mod1 <- felm(form1, data = train)
  
  # Weather
  mod2 <- felm(form2, data = train)
    
  # Climate
  mod3 <- felm(form3, data = train)
    
  # Weather-climate
  mod4 <- felm(form4, data = train)
  
  mod1$call[[2]] <- form1
  mod2$call[[2]] <- form2
  mod3$call[[2]] <- form3
  mod4$call[[2]] <- form4
  
  # Mod1 RMS
  # Get fixed effects
  fe <- predictFelm(mod1, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test2 <- as.matrix(select(test, rownames(mod1$coefficients)))
  coef <- as.matrix(mod1$coefficients)
  fit <- test2 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_t_fe[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # Mod2 RMS
  # Get fixed effects
  fe <- predictFelm(mod2, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test3 <- as.matrix(select(test, rownames(mod2$coefficients)))
  coef <- as.matrix(mod2$coefficients)
  fit <- test3 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_weather_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # Mod3 RMS
  # Get fixed effects
  fe <- predictFelm(mod3, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test4 <- as.matrix(select(test, rownames(mod3$coefficients)))
  coef <- as.matrix(mod3$coefficients)
  fit <- test4 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_climate_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  
  # Mod4 RMS
  # Get fixed effects
  fe <- predictFelm(mod4, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>%
    group_by(fips) %>%
    summarise_all(mean)

  # Use test data
  test5 <- as.matrix(select(test, rownames(mod4$coefficients)))
  coef <- as.matrix(mod4$coefficients)
  fit <- test5 %*% coef

  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")

  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_weather_climate_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  mse$climate_var[1] <- "rm"
  # print(i)
  mse[1, ]
  
  }  

  dd <- foreach(i = 1:1, .combine = rbind, .packages = c("lfe", "dplyr")) %dopar% {
  
  test_years <- sample(1950:2010, 5)
  test <- filter(data, year %in% test_years)
  # dep_var <- test$ln_corn_mrev
  train <- filter(data, year %!in% test_years)
  
  form1 <- as.formula(paste0(dep_var, "~ trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form2 <- as.formula(paste0(dep_var, "~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form3 <- as.formula(paste0(dep_var, "~   dday0_10_iv + dday10_30_iv + dday30_iv + prec_iv + prec_sq_iv + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form4 <- as.formula(paste0(dep_var, "~  dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                    dday0_10_iv + dday10_30_iv + dday30_iv + prec_iv + prec_sq_iv +
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  
  # Baseline
  mod1 <- felm(form1, data = train)
  
  # Weather
  mod2 <- felm(form2, data = train)
    
  # Climate
  mod3 <- felm(form3, data = train)
    
  # Weather-climate
  mod4 <- felm(form4, data = train)
  
  mod1$call[[2]] <- form1
  mod2$call[[2]] <- form2
  mod3$call[[2]] <- form3
  mod4$call[[2]] <- form4
  
  # Mod1 RMS
  # Get fixed effects
  fe <- predictFelm(mod1, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test2 <- as.matrix(select(test, rownames(mod1$coefficients)))
  coef <- as.matrix(mod1$coefficients)
  fit <- test2 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_t_fe[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # Mod2 RMS
  # Get fixed effects
  fe <- predictFelm(mod2, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test3 <- as.matrix(select(test, rownames(mod2$coefficients)))
  coef <- as.matrix(mod2$coefficients)
  fit <- test3 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_weather_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # Mod3 RMS
  # Get fixed effects
  fe <- predictFelm(mod3, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test4 <- as.matrix(select(test, rownames(mod3$coefficients)))
  coef <- as.matrix(mod3$coefficients)
  fit <- test4 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_climate_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  
  # Mod4 RMS
  # Get fixed effects
  fe <- predictFelm(mod4, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>%
    group_by(fips) %>%
    summarise_all(mean)

  # Use test data
  test5 <- as.matrix(select(test, rownames(mod4$coefficients)))
  coef <- as.matrix(mod4$coefficients)
  fit <- test5 %*% coef

  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")

  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_weather_climate_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  mse$climate_var[1] <- "iv"

  # print(i)
  mse[1, ]
  
  }  


  ddd <- foreach(i = 1:1, .combine = rbind, .packages = c("lfe", "dplyr")) %dopar% {
  
  test_years <- sample(1950:2010, 5)
  test <- filter(data, year %in% test_years)
  train <- filter(data, year %!in% test_years)
  
  form1 <- as.formula(paste0(dep_var, "~ trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form2 <- as.formula(paste0(dep_var, "~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form3 <- as.formula(paste0(dep_var, "~ dday0_10_davg + dday10_30_davg + dday30_davg + prec_davg + prec_sq_davg + 
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  form4 <- as.formula(paste0(dep_var, "~  dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
                    dday0_10_davg + dday10_30_davg + dday30_davg + prec_davg + prec_sq_davg +
                trend_lat + trend_long + trend_sq_lat + trend_sq_long  | fips | 0 | 0"))
  
  # Baseline
  mod1 <- felm(form1, data = train)
  
  # Weather
  mod2 <- felm(form2, data = train)
    
  # Climate
  mod3 <- felm(form3, data = train)
    
  # Weather-climate
  mod4 <- felm(form4, data = train)
  
  mod1$call[[2]] <- form1
  mod2$call[[2]] <- form2
  mod3$call[[2]] <- form3
  mod4$call[[2]] <- form4
  
  # Mod1 RMS
  # Get fixed effects
  fe <- predictFelm(mod1, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test2 <- as.matrix(select(test, rownames(mod1$coefficients)))
  coef <- as.matrix(mod1$coefficients)
  fit <- test2 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_t_fe[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # Mod2 RMS
  # Get fixed effects
  fe <- predictFelm(mod2, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test3 <- as.matrix(select(test, rownames(mod2$coefficients)))
  coef <- as.matrix(mod2$coefficients)
  fit <- test3 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_weather_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  # Mod3 RMS
  # Get fixed effects
  fe <- predictFelm(mod3, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test4 <- as.matrix(select(test, rownames(mod3$coefficients)))
  coef <- as.matrix(mod3$coefficients)
  fit <- test4 %*% coef
  
  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_climate_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  
  # Mod4 RMS
  # Get fixed effects
  fe <- predictFelm(mod4, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>%
    group_by(fips) %>%
    summarise_all(mean)

  # Use test data
  test5 <- as.matrix(select(test, rownames(mod4$coefficients)))
  coef <- as.matrix(mod4$coefficients)
  fit <- test5 %*% coef

  # test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(data, year %in% test_years)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")

  pdat$pred <- pdat$fit + pdat$effect
  residuals <- test[`dep_var`] - pdat$pred
  mse$agg_weather_climate_mse[1] <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  mse$climate_var[1] <- "davg"
  # print(i)
  mse[1, ]
  
  }  
  
  outdat <- rbind(outdat, d, dd, ddd)
}

outdat <- readRDS("/home/john/Dropbox/Apps/Remote Link/cv_model_outdat.rds")

outdat$n <- NULL
outdat$rep <- NULL
test <- gather(outdat, key = measure, value = value, -dep_var, -climate_var)
test <- filter(test, climate_var != 'n')

test2 <- test %>% 
  group_by(dep_var, climate_var, measure) %>% 
  summarise(value = mean(value)) %>% 
  group_by(dep_var, climate_var) %>% 
  mutate(change = 100*(value - first(value))/first(value)) %>% 
  ungroup()

dput(test2)

ggplot(test2, aes(dep_var, abs(change), fill = climate_var)) + 
  geom_bar(stat = "identity",position="dodge")


# 
# 
# 
# saveRDS(mse, "data/rep_cv_model.rds")
# mse <- readRDS("data/rep_cv_model.rds")
# mse_func <- apply(mse, 2, mean)
# mse_func
# 
# #---------------------------------------------------------------------------------------------------
# # Disaggregate Revenue/Acre
# 
# dmse <- data.frame(rep = 1:1000,
#                   disagg_base = 0,
#                   disagg_t_fe = 0,
#                   disagg_weather_mse = 0,
#                   disagg_climate_mse = 0,                 
#                   disagg_weather_climate_mse = 0)
# 
# # for (i in 1:1){  
# 
# d <- foreach(i = 1:2, .combine = rbind, .packages = c("dplyr", "systemfit", "lfe")) %dopar% {
#   test_years <- sample(1950:2010, 5)
#   test <- filter(regdat, year %in% test_years)
#   dep_var <- rowSums(test[, c("ln_rev_corn", "ln_rev_cotton", "ln_rev_hay", "ln_rev_soybean", "ln_rev_wheat")])
#   # dep_var <- test$ln_rev
#   train <- filter(regdat, year %!in% test_years)
#   
#   train_dm <- demeanlist(train, fl = list(fips = factor(train$fips)))
#   test_dm <- demeanlist(test, fl = list(fips = factor(test$fips)))
#   
#   train_means <- demeanlist(train, fl = list(fips = factor(train$fips)), means = TRUE)
#   test_means <- demeanlist(test, fl = list(fips = factor(test$fips)), means = TRUE)
# 
#   
#   
# 
#   # Baseline
#   
#   mod1 <- ln_rev_corn ~  1
#   
#   
#   mod2 <- ln_rev_cotton ~  1
#    
#   
#   mod3 <- ln_rev_hay ~ 1
#   
#   
#   mod4 <- ln_rev_soybean ~  1
#   
#   
#   mod5 <- ln_rev_wheat ~ 1
#   
#   baseline <- systemfit(list(corn = mod1, 
#                          cotton = mod2, 
#                          hay = mod3, 
#                          soybean = mod4,
#                          wheat = mod5), data = train, method = "SUR")
#   
#   
#   # Trend and Fixed-effect
#   
#   mod1 <- ln_rev_corn ~
#     trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod2 <- ln_rev_cotton ~
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#    
#   
#   mod3 <- ln_rev_hay ~
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod4 <- ln_rev_soybean ~
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod5 <- ln_rev_wheat ~
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#     trend_fe <- systemfit(list(corn = mod1, 
#                          cotton = mod2, 
#                          hay = mod3, 
#                          soybean = mod4,
#                          wheat = mod5), data = train_dm, method = "SUR")
#   
#   # Weather
#   
#   mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#     trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod2 <- ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#    
#   
#   mod3 <- ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#     weather <- systemfit(list(corn = mod1, 
#                          cotton = mod2, 
#                          hay = mod3, 
#                          soybean = mod4,
#                          wheat = mod5), data = train_dm, method = "SUR")
#   
#   # Climate
#   
#   mod1 <- ln_rev_corn ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#     trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod2 <- ln_rev_cotton ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#    
#   
#   mod3 <- ln_rev_hay ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod4 <- ln_rev_soybean ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod5 <- ln_rev_wheat ~ dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#     climate <- systemfit(list(corn = mod1, 
#                          cotton = mod2, 
#                          hay = mod3, 
#                          soybean = mod4,
#                          wheat = mod5), data = train_dm, method = "SUR")
#   
#   # Weather and Climate
#   
#   mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#     dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#     trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod2 <- ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#     dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#    
#   
#   mod3 <- ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#     dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#     dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#   
#   mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
#     dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
#   trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
#   
#     weather_climate <- systemfit(list(corn = mod1, 
#                                       cotton = mod2, 
#                                       hay = mod3, 
#                                       soybean = mod4,
#                                       wheat = mod5), data = train_dm, method = "SUR")
# 
#   # Baseline RMS
#   a <- predict(baseline, test)
#   a <- rowSums(a) - dep_var
#   b <- dep_var - a
#   head(b)
#   dmse$disagg_base[i] <- sqrt(mean(b^2))
#   
#   
#   # Trends and Fixed-effect RMS
#   fit <- predict(trend_fe, test_dm)
#   fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
#   fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
#   fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
#   fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
#   fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat
# 
#   fit <- rowSums(fit[1:5])
#   residuals <- dep_var - fit
#   dmse$disagg_t_fe[i] <- sqrt(mean(residuals^2))
#   
#   # Weather RMS
#   fit <- predict(weather, test_dm)
#   fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
#   fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
#   fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
#   fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
#   fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat
# 
#   fit <- rowSums(fit[1:5])
#   residuals <- dep_var - fit
#   dmse$disagg_weather_mse[i] <- sqrt(mean(residuals^2))
#   
#   # Climate RMS
#   fit <- predict(climate, test_dm)
#   fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
#   fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
#   fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
#   fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
#   fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat
# 
#   fit <- rowSums(fit[1:5])
#   residuals <- dep_var - fit
#   dmse$disagg_climate_mse[i] <- sqrt(mean(residuals^2))
#   
#   # Weather and Climate RMS
#   fit <- predict(weather_climate, test_dm)
#   fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
#   fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
#   fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
#   fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
#   fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat
# 
#   fit <- rowSums(fit[1:5])
#   residuals <- dep_var - fit
#   dmse$disagg_weather_climate_mse[i] <- sqrt(mean(residuals^2))
# 
#   dmse[i, ]  
# }
# 
# 
# stopCluster(cl)
# 
# 
# # Results
#  rep                disagg_base 
#                 500.500000                  17.160132 
#                disagg_t_fe         disagg_weather_mse 
#                   2.625683                   2.621718 
#         disagg_climate_mse disagg_weather_climate_mse 
#                   2.622624                   2.619858
#   