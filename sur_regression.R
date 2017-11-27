library(systemfit)
library(plm)
library(tidyverse)
library(lfe)
library(AER)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# Load regression data
csdat <- readRDS("data/cross_section_regression_data.rds")
lddat <- readRDS("data/long_difference_regression_data.rds")
pdat <- readRDS("data/panel_regression_data.rds")

corn <- tobit(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state) - 1, data = csdat)
cotton <- tobit(p_cotton_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state) - 1, data = csdat)
hay <- tobit(p_hay_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state) - 1, data = csdat)
soybean <- tobit(p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state) - 1, data = csdat)

pcorn <- predict(corn)
pcotton <- predict(cotton)
phay <- predict(hay)
psoybean <- predict(soybean)

pcorn <- tobit.ey(pcorn, corn$scale)
pcotton <- tobit.ey(pcotton, cotton$scale)
phay <- tobit.ey(phay, hay$scale)
psoybean <- tobit.ey(psoybean, soybean$scale)

preddat <- data.frame(corn = pcorn, 
                      cotton = pcotton,
                      hay = phay,
                      soybean = psoybean)
preddat$wheat <- 1 - rowSums(preddat[, 1:4])


View(preddat)

  tobit.ey <- function(mu, sigma){
    p0 <- pnorm(mu/sigma)
    lambda <- function(x) dnorm(x)/pnorm(x)
    ey0 <- mu + sigma * lambda(mu/sigma)
    ey <- p0 * ey0
    return(ey)
  }
  
tobit.ey(pcorn, corn$scale)
  
# Demean for fixed effects
femoddat <- as.data.frame(csdat)
femoddat <- select(femoddat, dday0_10, dday10_30, dday30C, prec, prec_sq)
fe <- factor(csdat$state)
class(femoddat$state)
femoddat <- demeanlist(femoddat, fl = list(fe))

surdat <- select(csdat, fips, state, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
surdat <- cbind(surdat, femoddat)

mod1b <- p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state) -1

mod2b <- p_cotton_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)   -1

mod3b <- p_hay_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)   -1

mod4b <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)   -1

mod5b <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)   -1

restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30C + corn_prec + corn_prec_sq  +
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30C + cotton_prec + cotton_prec_sq  +
              hay_dday0_10 + hay_dday10_30 + hay_dday30C + hay_prec + hay_prec_sq  +
              soybean_dday0_10 + soybean_dday10_30 + soybean_dday30C + soybean_prec + soybean_prec_sq +
              wheat_dday0_10 + wheat_dday10_30 + wheat_dday30C + wheat_prec + wheat_prec_sq = 0")

mod <- systemfit(list(corn = mod1b, 
                      cotton = mod2b, 
                      hay = mod3b, 
                      soybean = mod4b,
                      wheat = mod5b), data = csdat, method = "SUR")

summary(mod)
                 , restrict.matrix = restrict)

summary(mod)
sum(mod$coefficients)
test <- predict(mod)[1]
rowSums(test[, 1:5])
lm(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq , data = surdat)

mod1b_fe <- felm(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state, data = surdat)
mod1fe <- getfe(mod1b_fe)
mod1fe <- mod1fe[, c(1, 5)]
rownames(mod1fe) <- NULL
names(mod1fe) <- c("effect", "state")
mod1fee <- data.frame(state = csdat$state)
mod1fee <- left_join(mod1fee, mod1fe, by = "state")
mod1fee <- cbind(mod1fee, test)
mod1fee$fit <- mod1fee$test + mod1fee$effect
summary(mod1fee$fit)



y <- select(csdat, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
X <- select(csdat, dday0_10, dday10_30, dday30C, prec, prec_sq)

fm <- fmlogit(y, X, maxit = 1000)
summary(fm)


# Doesn't work stacking varaibles because of unbal panel not implemented
surdat <- select(csdat, fips, state, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a,
                 dday0_10, dday10_30, dday30C, prec, prec_sq)

surdat <- gather(surdat, key = crops, value = value, -fips, -state, -dday0_10, -dday10_30, -dday30C, -prec, -prec_sq)

surdat <- pdata.frame(surdat, c("state"))

mod <- systemfit(value ~ dday0_10 + dday10_30 + dday30C + prec, data = surdat, method = "SUR")
summary(mod)

# Demean for fixed effects
femoddat <- as.data.frame(moddat)
femoddat <- select(femoddat, dday0_10, dday10_30, dday30C, prec, prec_sq)

femoddat$fips <- factor(femoddat$fips)
class(femoddat)
femoddat <- demeanlist(femoddat, fl = moddat$fips)
femoddat

mod1b <- p_corn_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq 

mod2b <- p_cotton_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq 

mod3b <- p_hay_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq

mod4b <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq

mod5b <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq

restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30C + corn_prec + corn_prec_sq  +
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30C + cotton_prec + cotton_prec_sq  +
              hay_dday0_10 + hay_dday10_30 + hay_dday30C + hay_prec + hay_prec_sq  +
              soybean_dday0_10 + soybean_dday10_30 + soybean_dday30C + soybean_prec + soybean_prec_sq +
              wheat_dday0_10 + wheat_dday10_30 + wheat_dday30C + wheat_prec + wheat_prec_sq = 0")

mod <- systemfit(list(corn = mod1b, 
                      cotton = mod2b, 
                      hay = mod3b, 
                      soybean = mod4b,
                      wheat = mod5b), data = moddat, method = "SUR")


,
                 restrict.matrix = restrict)
