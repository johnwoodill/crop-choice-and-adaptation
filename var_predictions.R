library(tidyverse)
library(RcppRoll)
library(lfe)
library(ggthemes)
library(doParallel)

source("R/predictFelm.R")

cl <- makeCluster(1)
registerDoParallel(cl)

# download.file("https://spideroak.com/share/NJXWQ3TXN5XWI2LMNQ/crop-choice-data/run/
#               media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/
#               data/full_weather_data.rds",
#               destfile = "data/full_weather_data.rds", method = "auto")

# Load data
regdat <- readRDS("data/full_weather_data.rds")
regdat$trend <- regdat$year - (min(regdat$year) - 1)
regdat$trend_sq <- regdat$trend^2

for (n in 1:30){
  lab1 <- paste0("dday0_10_lag", n)
  lab2 <- paste0("dday10_30_lag", n)
  lab3 <- paste0("dday30_lag", n)
  lab4 <- paste0("prec_lag", n)
  lab5 <- paste0("prec_sq_lag", n)
  
  
  regdat <- regdat %>% 
    group_by(fips) %>% 
    arrange(year) %>% 
    mutate(!!lab1 := lag(dday0_10, n),
           !!lab2 := lag(dday10_30, n),
           !!lab3 := lag(dday30, n),
           !!lab4 := lag(prec, n),
           !!lab5 := lag(prec, n)^2)
}
  



d <- foreach(i = 1932:1933, .combine = rbind, .packages = c("lfe", "dplyr")) %dopar% {
  
  outdat <- data.frame()
  
# for (i in seq(1932, 2012, 1)){
  for (j in 2:30){
    
    test <- filter(regdat, year == i)
    train <- filter(regdat, year < i & year >= (i - j))
                    
    
    form1 <- as.formula(paste0("dday0_10 ~ -1 ")) 
    form2 <- as.formula(paste0("dday10_30 ~ -1")) 
    form3 <- as.formula(paste0("dday30 ~ -1")) 
    form4 <- as.formula(paste0("prec ~ -1")) 
    
    for (k in 1:j){
      form1 <- update.formula(form1, paste0('~ .  + dday0_10_lag', k, ' + dday10_30_lag', k, ' + dday30_lag', k, ' + prec_lag', k, ' + prec_sq_lag', k))
      form2 <- update.formula(form2, paste0('~ .  + dday0_10_lag', k, ' + dday10_30_lag', k, ' + dday30_lag', k, ' + prec_lag', k, ' + prec_sq_lag', k))
      form3 <- update.formula(form3, paste0('~ .  + dday0_10_lag', k, ' + dday10_30_lag', k, ' + dday30_lag', k, ' + prec_lag', k, ' + prec_sq_lag', k))
      form4 <- update.formula(form4, paste0('~ .  + dday0_10_lag', k, ' + dday10_30_lag', k, ' + dday30_lag', k, ' + prec_lag', k, ' + prec_sq_lag', k))
      
    }
    
    # Add fixed effects and cluster
    form1 <- as.formula((paste0(form1[[2]], " ~ ", form1[[3]], " + trend + trend_sq | fips | 0 | fips"))[[2]])
    form2 <- as.formula((paste0(form2[[2]], " ~ ", form2[[3]], " + trend + trend_sq | fips | 0 | fips"))[[2]])
    form3 <- as.formula((paste0(form3[[2]], " ~ ", form3[[3]], " + trend + trend_sq | fips | 0 | fips"))[[2]])
    form4 <- as.formula((paste0(form4[[2]], " ~ ", form4[[3]], " + trend + trend_sq | fips | 0 | fips"))[[2]])
    
    # Train data
    mod1 <- felm(form1, data = train)
    mod2 <- felm(form2, data = train)
    mod3 <- felm(form3, data = train)
    mod4 <- felm(form4, data = train)
    
    # Add formula for predictions (predictFelm)
    mod1$call[[2]] <- form1
    mod2$call[[2]] <- form2
    mod3$call[[2]] <- form3
    mod4$call[[2]] <- form4
    
    # Predict using test data
    pmod1 <- predictFelm(mod1, newdata = test)
    pmod2 <- predictFelm(mod2, newdata = test)
    pmod3 <- predictFelm(mod3, newdata = test)
    pmod4 <- predictFelm(mod4, newdata = test)
    
    # Get RMSE
    mod1_mse <- sqrt(mean((test$dday0_10 - (pmod1$fit + pmod1$effect))^2))
    mod2_mse <- sqrt(mean((test$dday10_30 - (pmod2$fit + pmod2$effect))^2))
    mod3_mse <- sqrt(mean((test$dday30 - (pmod3$fit + pmod3$effect))^2))
    mod4_mse <- sqrt(mean((test$prec - (pmod4$fit + pmod4$effect))^2))
    
    # Bind data
    indat <- data.frame(predict_year = rep(i, 4),
                        lag = rep(j, 4),
                        dep = c("dday0_10", "dday10_30", "dday30", "prec"),
                        rmse = c(mod1_mse,
                                 mod2_mse,
                                 mod3_mse,
                                 mod4_mse))
  outdat <- rbind(outdat, indat)
  # print(j)  
  }
  # print(i)
  outdat
}

saveRDS(d, "data/VAR_output.rds")     

d <- readRDS("data/VAR_output.rds")
dd <- d %>% 
  group_by(dep, lag) %>% 
  summarise(mean_rmse = mean(rmse))
   
ddd <- dd %>% group_by(dep) %>% top_n(n = 1, -mean_rmse) %>% arrange(mean_rmse)
ddd

head(dd)

ggplot(dd, aes(lag, mean_rmse)) +   geom_point(size = .8) +
  theme_tufte(base_size = 12) +
  ylab("RMSE") +
  xlab("VAR Lags") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~dep, scales = "free")

ggsave("figures/var_predictions.pdf", width = 6, height = 4)
