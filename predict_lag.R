library(tidyverse)
library(RcppRoll)
library(lfe)
library(ggthemes)

# Load data
regdat <- readRDS("data/full_weather_data.rds")
regdat <- regdat %>% 
  group_by(fips) %>% 
  arrange(year) %>% 
  mutate(dday0_10_lag1 = lag(dday0_10, 1),
         dday10_30_lag1 = lag(dday10_30, 1),
         dday30_lag1 = lag(dday30, 1),
         prec_lag1 = lag(prec, 1))

# Trend variables
regdat$trend <- regdat$year - (min(regdat$year) - 1)
regdat$trend_sq <- regdat$trend^2
regdat$trend_lat <- regdat$trend*regdat$lat
regdat$trend_long <- regdat$trend*regdat$long
regdat$trend_sq_long <- regdat$trend_sq*regdat$long
regdat$trend_sq_lat <- regdat$trend_sq*regdat$lat

# View(regdat)
# Loop through 1 to 50 year right rolling mean
for (i in 1:50){
  
  # Create custom col labels
  lab1 <- paste0("dday0_10_rm_", i)
  lab2 <- paste0("dday10_30_rm_", i)
  lab3 <- paste0("dday30_rm_", i)
  lab4 <- paste0("prec_rm_", i)
  
  # Loop through each fips and calculate rollingmean
  regdat <- regdat %>%
      group_by(fips) %>%
      arrange(year) %>%
      mutate(!!lab1 := roll_mean(dday0_10_lag1, i, align = "right", fill = "NA"),
             !!lab2 := roll_mean(dday10_30_lag1, i, align = "right", fill = "NA"),
             !!lab3 := roll_mean(dday30_lag1, i, align = "right", fill = "NA"),
             !!lab4 := roll_mean(prec_lag1, i, align = "right", fill = "NA")) %>% 
    ungroup()
  
  # Progress bar for loop
  print(i)
}

# data.frame to store RMSE
# outdat <- data.frame(window = c(1:50), rmse = 0, coef =0 )
outdat <- data.frame()
for (i in 1:50){ # Formula
  form1 <- as.formula(paste0("dday0_10 ~ dday0_10_rm_", i, " | fips | 0 | fips")) # Regression
  form2 <- as.formula(paste0("dday10_30 ~ dday10_30_rm_", i, " | fips | 0 | fips")) # Regression
  form3 <- as.formula(paste0("dday30 ~ dday30_rm_", i, " | fips | 0 | fips")) # Regression
  form4 <- as.formula(paste0("prec ~ prec_rm_", i, " | fips | 0 | fips")) # Regression
  
  
  #  form <- as.formula(paste0('I(dday30 - dday30_rm_', i,") ~ -1 | fips")) # Regression
  
  mod1 <- felm(form1, data = regdat)
  mod2 <- felm(form2, data = regdat)
  mod3 <- felm(form3, data = regdat)
  mod4 <- felm(form4, data = regdat)
  
  # RMSE
  indat <- data.frame(window = rep(i, 4),
                      var = c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)", "Precipitation"),
                      rmse = c(sqrt(mean(mod1$residuals^2)),
                               sqrt(mean(mod2$residuals^2)),
                               sqrt(mean(mod3$residuals^2)),
                               sqrt(mean(mod4$residuals^2))),
                      coef = c(mod1$coefficients,
                               c(mod2$coefficients),
                               c(mod3$coefficients),
                               c(mod4$coefficients)),
                      se = c(mod1$cse,
                             mod2$cse,
                             mod3$cse,
                             mod4$cse))
  outdat <- rbind(outdat, indat)  
  print(i)
}

ggplot(outdat, aes(window, rmse)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  # annotate("text", x = 40, y = 135, label = "Degree Day (10-30C)", size = 3) +
  # annotate("text", x = 40, y = 55, label = "Degree Day (30C)", size = 3) +
  # annotate("text", x = 40, y = 35, label = "Degree Day (0-10C)", size = 3) +
  theme_tufte(base_size = 12) +
  ylab("RMSE") +
  xlab("Rollmean Window Size") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~var, scales = "free")
ggsave("figures/predict_lag_rmse.pdf", width = 6, height = 4)

ggplot(outdat, aes(window, coef)) + 
  geom_point(size = .8) +
  geom_errorbar(aes(ymin = (coef - se*1.96), ymax = (coef + se*1.96)), width = 0.1, alpha = 0.5) +
  theme_tufte(base_size = 12) +
  ylab("Coefficient \n (Clustered SE by County)") +
  xlab("Rollmean Window Size") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~var, scales = "free")
ggsave("figures/predict_lag_coef.pdf", width = 6, height = 4)



# Differences
outdat <- data.frame()
for (i in 1:50){ # Formula
  form1 <- as.formula(paste0("I(dday0_10 - dday0_10_rm_", i,") ~ -1 | fips | 0 | fips")) # Regression
  form2 <- as.formula(paste0("I(dday10_30 - dday10_30_rm_", i,") ~ -1 | fips | 0 | fips")) # Regression
  form3 <- as.formula(paste0("I(dday30 - dday30_rm_", i, ") ~ -1 | fips | 0 | fips")) # Regression
  form4 <- as.formula(paste0("I(prec - prec_rm_", i, ") ~ -1 | fips | 0 | fips")) # Regression
  
  
  #  form <- as.formula(paste0('I(dday30 - dday30_rm_', i,") ~ -1 | fips")) # Regression
  
  mod1 <- felm(form1, data = regdat)
  mod2 <- felm(form2, data = regdat)
  mod3 <- felm(form3, data = regdat)
  mod4 <- felm(form4, data = regdat)
  
  # RMSE
  indat <- data.frame(window = rep(i, 4),
                      var = c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)", "Precipitation"),
                      rmse = c(sqrt(mean(mod1$residuals^2)),
                               sqrt(mean(mod2$residuals^2)),
                               sqrt(mean(mod3$residuals^2)),
                               sqrt(mean(mod4$residuals^2))))
  outdat <- rbind(outdat, indat)  
  print(i)
}

ggplot(outdat, aes(window, rmse)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  # annotate("text", x = 40, y = 135, label = "Degree Day (10-30C)", size = 3) +
  # annotate("text", x = 40, y = 55, label = "Degree Day (30C)", size = 3) +
  # annotate("text", x = 40, y = 35, label = "Degree Day (0-10C)", size = 3) +
  theme_tufte(base_size = 12) +
  ylab("RMSE") +
  xlab("Rollmean Window Size") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  facet_wrap(~var, scales = "free")
ggsave("figures/predict_lag_rmse_diff.pdf", width = 6, height = 4)
