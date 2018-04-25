source("R/predictSUR.R")
source("R/predictFelm.R")

# Crop data
regdat <- readRDS("data/full_ag_data.rds")

#---------------------------------------------------------------------------------------------------
# Aggregate rev/acre
mse <- data.frame(year = 1950:2010,
                   agg_mse = 0,
                  disagg_mse = 0)
for(i in 1950:2010){
  test <- filter(regdat, year == i)
  dep_var <- test$ln_rev
  train <- filter(regdat, year != i)
  modten_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
                trend_lat + trend_long + trend_sq_lat + trend_sq_long | fips | 0 | state, 
            data = train, weights = train$w, psdef = FALSE)
  
  # Get fixed effects
  fe <- predictFelm(modten_1, newdata = train)
  fepdat <- data.frame(fips = train$fips,
                     effect = fe$effect)
  fepdat <- fepdat %>% 
    group_by(fips) %>% 
    summarise_all(mean)
  
  # Use test data
  test <- as.matrix(select(test, rownames(modten_1$coefficients)))
  coef <- as.matrix(modten_1$coefficients)
  fit <- test %*% coef
  
  test <- as.data.frame(test)
  pdat <- data.frame(fips = filter(regdat, year == i)$fips,
                     fit = as.numeric(fit))
  pdat <- left_join(pdat, fepdat, by = "fips")
  
  
  pdat$pred <- pdat$fit + pdat$effect
  residuals <- dep_var - pdat$pred
  mse$agg_mse[(i-1949)] <- (mean(residuals^2))
  print(i)
}  

#---------------------------------------------------------------------------------------------------
# Disaggregate rev/acre

mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
  trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod2 <- ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1
 

mod3 <- ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
   dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
  dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
trend_lat + trend_long + trend_sq_lat + trend_sq_long - 1


for(i in 1950:2010){
  test <- filter(regdat, year == i)
  test_dep <- filter(regdat, year == i)
  dep_var <- rowSums(test_dep[, c("ln_rev_corn", "ln_rev_cotton", "ln_rev_hay", "ln_rev_soybean", "ln_rev_wheat")])
  train <- filter(regdat, year != i)
  
  train_dm <- demeanlist(train, fl = list(fips = factor(train$fips)))
  test_dm <- demeanlist(test, fl = list(fips = factor(test$fips)))
  
  train_means <- demeanlist(train, fl = list(fips = factor(train$fips)), means = TRUE)
  test_means <- demeanlist(test, fl = list(fips = factor(test$fips)), means = TRUE)

  mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = train_dm, method = "SUR")

  fit <- predict(mod, data = test_dm)
  
  fit$corn.pred <- fit$corn.pred + test_means$ln_rev_corn
  fit$cotton.pred <- fit$cotton.pred + test_means$ln_rev_cotton
  fit$hay.pred <- fit$hay.pred + test_means$ln_rev_hay
  fit$soybean.pred <- fit$soybean.pred + test_means$ln_rev_soybean
  fit$wheat.pred <- fit$wheat.pred + test_means$ln_rev_wheat

  fit <- rowSums(fit[1:5])
  residuals <- dep_var - fit
  mse$disagg_mse[(i-1949)] <- (mean(residuals^2))
  print(i)
}
    
mean(sqrt(mse$agg_mse))
mean(sqrt(mse$disagg_mse))
  
sd(mse$agg_mse)
sd(mse$disagg_mse)  
  
  
  