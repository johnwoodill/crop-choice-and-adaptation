library(systemfit)
library(plm)
library(tidyverse)
library(lfe)
library(AER)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

dummyCreator <- function(invec, prefix = NULL) {
     L <- length(invec)
     ColNames <- sort(unique(invec))
     M <- matrix(0L, ncol = length(ColNames), nrow = L,
                 dimnames = list(NULL, ColNames))
     M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
     if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
     M
} 

# Load regression data
pdat <- readRDS("data/panel_regression_data.rds")

dummystate <- dummyCreator(csdat$state, prefix = "state")

mod1b <- p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state) - 1   

mod2b <- p_cotton_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)  - 1

mod3b <- p_hay_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)  - 1

mod4b <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)  - 1

mod5b <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + factor(state)  - 1

mod <- systemfit(list(corn = mod1b, 
                      cotton = mod2b, 
                      hay = mod3b, 
                      soybean = mod4b), data = csdat, method = "SUR")


summary(mod)
sum(mod$coefficients)

# Build coefmatrix
coefmat <- matrix(mod$coefficients, ncol = 31, nrow = 4, byrow = TRUE)

# Solve for 5th equation
eq5 <- colSums(coefmat)
coefmat <- rbind(coefmat, eq5)
coefmat <- data.frame(coefmat)
names(coefmat) <- names(mod$coefficients)[1:31]
rownames(coefmat) <- c("Corn", "Cotton", "Hay", "Soybean", "Wheat")
names(coefmat) <-


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
csdat$latlong <- csdat$lat*csdat$long
surdat <- select(csdat, fips, state, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a,
                 dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)

surdat <- gather(surdat, key = crops, value = value, -fips, -state, -dday0_10, -dday10_30, -dday30C, -prec, -prec_sq,
                 -lat, -long, -latlong)

surdat <- pdata.frame(surdat, c("state"))

surdat <- make.pbalanced(surdat)


mod <- systemfit(value ~ dday0_10 + dday10_30 + dday30C + prec , data = surdat, method = "SUR",
                 pooled = TRUE)
summary(mod)

# Demean for fixed effects
femoddat <- as.data.frame(moddat)
femoddat <- select(femoddat, dday0_10, dday10_30, dday30C, prec, prec_sq)

femoddat$fips <- factor(femoddat$fips)
class(femoddat)
femoddat <- demeanlist(femoddat, fl = moddat$fips)
femoddat

mod1b <- p_corn_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1

mod2b <- p_cotton_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1

mod3b <- p_hay_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1

mod4b <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq  + lat + long + latlong -1

mod5b <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1

restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30C + corn_prec + corn_prec_sq + corn_lat + 
              corn_long + corn_latlong +              
              cotton_dday0_10 + cotton_dday10_30 + cotton_dday30C + cotton_prec + cotton_prec_sq  + 
              cotton_lat + cotton_long + cotton_latlong + 
              hay_dday0_10 + hay_dday10_30 + hay_dday30C + hay_prec + hay_prec_sq + hay_lat + 
              hay_long + hay_latlong + 
              soybean_dday0_10 + soybean_dday10_30 + soybean_dday30C + soybean_prec + 
              soybean_prec_sq  + soybean_lat + soybean_long + soybean_latlong + 
              wheat_dday0_10 + wheat_dday10_30 + wheat_dday30C + wheat_prec + wheat_prec_sq + 
              wheat_lat + wheat_long + wheat_latlong = 0")



mod <- systemfit(list(corn = mod1b, 
                      cotton = mod2b, 
                      hay = mod3b, 
                      soybean = mod4b,
                      wheat = mod5b), data = csdat, method = "SUR", restrict.matrix = restrict)

predict(mod)

, restrict.matrix = restrict)
summary(mod)
predict(mod)
,
                 restrict.matrix = restrict)
