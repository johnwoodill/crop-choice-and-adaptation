regdat <- readRDS("data/full_weather_data.rds")

calcAIC <- function(residuals, df_residuals){
    n <- length(residuals)
    edf <- n - df_residuals
    RSS <- sum(residuals^2)
    dev <- n * log(RSS/n)
    c(edf, dev + 2 * edf)
}


intervals <- c("ten", "fifteen", "twenty", "thirty", "fourty")

retdat <- data.frame(fips = newdata$fips,
                     dday30 = newdata$dday30)
aicdat <- data.frame()

for (j in 1950:2010){
  for (i in intervals){
  newdata <- filter(regdat, year == j )
  form <- as.formula(paste0("dday30 ~", paste0("dday30_rm_",i), " + trend:(lat + long) + trend_sq:(lat + long) |fips"))
  form1 <- as.formula(paste0("dday30 ~ dday30_rm_", i, "+ trend:(lat + long) + trend_sq:(lat + long) - 1"))
  
  pmod <- felm(form, data = filter(regdat, year != j))
  moddat <- model.matrix(form1, data = newdata)
  
  coef <- as.matrix(pmod$coefficients)
  fit <- as.numeric(moddat %*% coef)
  pdat <- data.frame(fips = factor(newdata$fips),
                     fit = fit)
  fips_fe = data.frame(fips = getfe(pmod)[5],
                       getfe(pmod)[1])
  rownames(fips_fe) <- NULL
  names(fips_fe) <- c("fips", "effect")
  head(fips_fe)
  class(fips_fe$fips)
  
  pdat <- left_join(pdat, fips_fe, by = "fips")
  head(pdat)
  pdat$res <- newdata$dday30 - (pdat$fit + pdat$effect)
  rownames(pdat) <- NULL
  head(pdat)
  pdat$tfit <- pdat$fit + pdat$effect 
  pdat$real_dday30 <- newdata$dday30
  retdat[, paste0(`i`)] <- pdat$tfit
  retdat[, paste0(`i`, "_res")] <- pdat$res
  df_residuals <- length(pmod$coefficients) - length(unique(regdat$fips))
  SSE <- sum((newdata$dday30 - pdat$tfit)^2  )
  SST <- sum((newdata$dday30 - mean(newdata$dday30))^2)
  
  aic <- data.frame(interval = i, 
                    aic = calcAIC(residuals = pdat$res, df_residuals = df_residuals)[2], 
                    r2 = 1 - (SSE/SST),
                    mse = sum((newdata$dday30 - pdat$tfit)^2)/nrow(pdat))
  aicdat <- rbind(aicdat, aic)
  
}}
  
outdat <- aicdat %>% 
  group_by(interval) %>% 
  summarise_all(mean)
arrange(outdat, mse)
head(retdat)
arrange(aicdat, mse)

# with trend:(lat + long) + trend_sq:(lat + long)
#   interval      aic        r2
# 1      two 25922.78 0.9190320
# 2     five 29395.57 0.6702493
# 3      ten 29936.76 0.5895827
# 4  fifteen 29807.45 0.6104910
# 5   twenty 29912.30 0.5936220
# 6   thirty 29962.67 0.5852599
# 7   fourty 29988.93 0.5808313

# without trends
#   interval      aic        r2
# 1      two 26124.41 0.9124376
# 2     five 29616.97 0.6405308
# 3      ten 30261.78 0.5334481
# 4  fifteen 30139.10 0.5560279
# 5   twenty 30435.74 0.4994474
# 6   thirty 30570.09 0.4715014
# 7   fourty 30571.59 0.4711809

################
  modmap1_dat <- select(retdat, fips, ten)
  names(modmap1_dat) <- c("region", "value")
  
  modmap1 <- county_choropleth(modmap1_dat,
                   title      = NULL)
  
  modmap1 <- modmap1 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
    theme_tufte(base_size = 10)+ 
    xlab("Difference in Degree Day 30C 1950's to 2000's \n (30-year rolling average)") + ylab(NULL) + theme(legend.position = "none",
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.ticks.y = element_blank(),
                         panel.border = element_rect(fill = NA)) 
  modmap1
  
  
  modmap2_dat <- select(retdat, fips, real_dday30)
  names(modmap2_dat) <- c("region", "value")
  
  modmap2 <- county_choropleth(modmap2_dat,
                   title      = NULL)
  
  modmap2 <- modmap2 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
    theme_tufte(base_size = 10)+ 
    xlab("Difference in Degree Day 30C 1950's to 2000's \n (30-year rolling average)") + ylab(NULL) + theme(legend.position = "none",
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.ticks.y = element_blank(),
                         panel.border = element_rect(fill = NA)) 
  modmap2


library(cowplot)
plot_grid(modmap1, modmap2, labels = c("Predicted", "Real"))


# pdat$diff <- pdat$real_dday30 - pdat$fit_fe
# plot(density(pdat$diff))
