library(systemfit)
library(plm)
library(tidyverse)
library(lfe)
library(AER)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")


# Crop data


# Download from Dropbox
# download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1", 
#               destfile = "full_ag_data.rds", method = "auto")

cropdat <- readRDS("data/full_ag_data.rds")

# coefnames <- c("corn_dday0_10","corn_dday10_30", "corn_dday30",           
# "corn_prec", "corn_prec_sq", "corn_dday0_10_five",    
# "corn_dday10_30_five", "corn_dday30_five", "corn_prec_five",        
# "corn_prec_sq_five", "cotton_dday0_10", "cotton_dday10_30",      
# "cotton_dday30", "cotton_prec", "cotton_prec_sq",        
# "cotton_dday0_10_five", "cotton_dday10_30_five", "cotton_dday30_five",    
# "cotton_prec_five", "cotton_prec_sq_five", "hay_dday0_10",          
# "hay_dday10_30", "hay_dday30", "hay_prec",              
# "hay_prec_sq", "hay_dday0_10_five", "hay_dday10_30_five",    
# "hay_dday30_five", "hay_prec_five", "hay_prec_sq_five",      
# "soybean_dday0_10", "soybean_dday10_30", "soybean_dday30",        
# "soybean_prec", "soybean_prec_sq", "soybean_dday0_10_five", 
# "soybean_dday10_30_five", "soybean_dday30_five", "soybean_prec_five",     
# "soybean_prec_sq_five" )

# Skeleton Model
# Models
# skmod1 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + - 1   
# 
# skmod2 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five   - 1
# 
# skmod3 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five   - 1
# 
# skmod4 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  - 1
# 
# skmod5 <- rnorm(nrow(cropdat), 100, 10) ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five   - 1
# 
# skelmod <- systemfit(list(corn = skmod1, 
#                       cotton = skmod2, 
#                       hay = skmod3, 
#                       soybean = skmod4,
#                       wheat = skmod5), data = cropdat, method = "SUR")
# 
# skelmod$coefficients
# 
# modmat <- model.matrix(~dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  + 
#                 factor(state) - 1, data = cropdat)
# modmat

# Convert to z-scores for linear regression

# First estimate between zero and 1
cropdat$p_corn_a <- (cropdat$p_corn_a + .01)/1.02
cropdat$p_cotton_a <- (cropdat$p_cotton_a + .01)/1.02
cropdat$p_hay_a <- (cropdat$p_hay_a + .01)/1.02
cropdat$p_soybean_a <- (cropdat$p_soybean_a + .01)/1.02
cropdat$p_wheat_a <- (cropdat$p_wheat_a + .01)/1.02

# Get mean and sd to convert back
# corn_m <- mean(cropdat$p_corn_a, na.rm = TRUE)
# corn_sd <- sd(cropdat$p_corn_a, na.rm = TRUE)
# 
# cotton_m <- mean(cropdat$p_cotton_a, na.rm = TRUE)
# cotton_sd <- sd(cropdat$p_cotton_a, na.rm = TRUE)
# 
# hay_m <- mean(cropdat$p_hay_a, na.rm = TRUE)
# hay_sd <- sd(cropdat$p_hay_a, na.rm = TRUE)
# 
# soybean_m <- mean(cropdat$p_soybean_a, na.rm = TRUE)
# soybean_sd <- sd(cropdat$p_soybean_a, na.rm = TRUE)
# 
# wheat_m <- mean(cropdat$p_wheat_a, na.rm = TRUE)
# wheat_sd <- sd(cropdat$p_wheat_a, na.rm = TRUE)


# Calc z-scores
cropdat$z_corn_a <- qnorm(cropdat$p_corn_a)
cropdat$z_cotton_a <- qnorm(cropdat$p_cotton_a)
cropdat$z_hay_a <- qnorm(cropdat$p_hay_a)
cropdat$z_soybean_a <- qnorm(cropdat$p_soybean_a)
cropdat$z_wheat_a <- qnorm(cropdat$p_wheat_a)

# Models
  
mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five - 1   

mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five - 1

mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  - 1

mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  - 1

mod5 <- z_wheat_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  - 1

# restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30 + corn_prec + corn_prec_sq + 
#               corn_dday0_10_five + corn_dday10_30_five + corn_dday30_five + corn_prec_five + corn_prec_sq_five +
#               cotton_dday0_10 + cotton_dday10_30 + cotton_dday30 + cotton_prec + cotton_prec_sq + 
#               cotton_dday0_10_five + cotton_dday10_30_five + cotton_dday30_five + cotton_prec_five + cotton_prec_sq_five +
#               hay_dday0_10 + hay_dday10_30 + hay_dday30 + hay_prec + hay_prec_sq + 
#               hay_dday0_10_five + hay_dday10_30_five + hay_dday30_five + hay_prec_five + hay_prec_sq_five +
#               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30 + soybean_prec + soybean_prec_sq + 
#               soybean_dday0_10_five + soybean_dday10_30_five + soybean_dday30_five + soybean_prec_five + soybean_prec_sq_five +
#               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30 + wheat_prec + wheat_prec_sq + 
#               wheat_dday0_10_five + wheat_dday10_30_five + wheat_dday30_five + wheat_prec_five + wheat_prec_sq_five = 0")

mod <- systemfit(list(corn = mod1, 
                      cotton = mod2, 
                      hay = mod3, 
                      soybean = mod4,
                      wheat = mod5), data = cropdat, method = "SUR")

summary(mod)
mod$coefCov
mod$coefficients
sum(mod$coefficients)

# (n) coefficients
ncoef <- length(mod$coefficients)
ncoef4 <- length(mod$coefficients)/4

# Keep main coefficients
coeff <- mod$coefficients

# Build coefmatrix
coefmat <- as.data.frame(matrix(mod$coefficients, ncol = ncoef4, nrow = 4, byrow = TRUE))

# Solve for 5th equation
eq5 <- as.data.frame(colSums(coefmat)*-1)
coefmat <- rbind(coefmat, t(eq5))
rownames(coefmat) <- NULL
rownames(coefmat) <- c("Corn", "Cotton", "Hay", "Soybean", "Wheat")
names(coefmat) <- names(mod$coefficients)[1:ncoef4]
coefmat
names(coefmat) <- substring(names(coefmat), 6)
coefmat
colSums(coefmat)
sum(coefmat)  
rowSums(coefmat)

# test <- as.numeric(c(coefmat[1, 1:ncoef4], coefmat[2, 1:ncoef4], coefmat[3, 1:ncoef4], coefmat[4, 1:ncoef4], coefmat[5, 1:ncoef4]))
# names(test) <- names(skelmod$coefficients)
# skelmod$coefficients <- test
# 
# pmod <- predict(skelmod)
# pmod
# summary(skelmod)
# 
# pmod[, 1] <- (pmod[,1]*corn_sd) + corn_m
# pmod[, 2] <- (pmod[,2]*cotton_sd) + cotton_m
# pmod[, 3] <- (pmod[,3]*hay_sd) + hay_m
# pmod[, 4] <- (pmod[,4]*soybean_sd) + soybean_m
# 
# pmod[, 1] <- (pmod[,1]*1.02) - 0.01
# pmod[, 2] <- (pmod[,2]*1.02) - 0.01
# pmod[, 3] <- (pmod[,3]*1.02) - 0.01
# pmod[, 4] <- (pmod[,4]*1.02) - 0.01
# 
# rowSums(pmod[, 1:4])
# which(rowSums(pmod[, 1:4]) > 1)
# 
# # Convert back to proportions
# coefmat[1, 1:10] <- (coefmat[1, 1:10]*corn_sd) + corn_m
# coefmat[2, 1:10] <- (coefmat[2, 1:10]*cotton_sd) + cotton_m
# coefmat[3, 1:10] <- (coefmat[3, 1:10]*hay_sd) + hay_m
# coefmat[4, 1:10] <- (coefmat[4, 1:10]*soybean_sd) + soybean_m
# 
# # Transform back to original acres
# coefmat[1, 1:10] <- (coefmat[1, 1:10]*1.02) - 0.01
# coefmat[2, 1:10] <- (coefmat[2, 1:10]*1.02) - 0.01
# coefmat[3, 1:10] <- (coefmat[3, 1:10]*1.02) - 0.01
# coefmat[4, 1:10] <- (coefmat[4, 1:10]*1.02) - 0.01
# 
# coefmat
# sum(coefmat$dday0_10)

predmat <- data.frame(corn = modmat %*% t(coefmat[1, 1:ncoef4]),
                       cotton = modmat %*% t(coefmat[2, 1:ncoef4]),
                       hay = modmat %*% t(coefmat[3, 1:ncoef4]),
                       soybean = modmat %*% t(coefmat[4, 1:ncoef4]))
predmat$Wheat <- 1 - rowSums(predmat[, 1:4])

rowSums(predmat[, 1:5])


,
                       wheat = modmat %*% t(coefmat[5, 1:ncoef4]))

coefmat[1, 1:ncoef4] %*% modmat

modmat %*% t(coefmat[1, 1:ncoef4]
predmat

# Convert back to proportions
predmat$Corn <- (predmat$Corn*corn_sd) + corn_m
predmat$Cotton <- (predmat$Cotton*cotton_sd) + cotton_m
predmat$Hay <- (predmat$Hay*hay_sd) + hay_m
predmat$Soybean <- (predmat$Soybean*soybean_sd) + soybean_m
predmat$Wheat <- (predmat$Wheat*wheat_sd) + wheat_m

rowSums(predmat[, 1:5])

# Transform back to original acres
predmat$Corn <- (predmat$Corn*1.02) - 0.01
predmat$Cotton <- (predmat$Cotton*1.02) - 0.01
predmat$Hay <- (predmat$Hay*1.02) - 0.01
predmat$Soybean <- (predmat$Soybean*1.02) - 0.01
predmat$Wheat <- (predmat$Wheat*1.02) - 0.01

rowSums(predmat[, 1:5])
range(predmat[, 1:5])

# Convert back to proportions
# cropdat$p_corn_a <- (cropdat$z_corn_a*corn_sd) + corn_m
# cropdat$p_cotton_a <- (cropdat$z_cotton_a*cotton_sd) + cotton_m
# cropdat$p_hay_a <- (cropdat$z_hay_a*hay_sd) + hay_m
# cropdat$p_soybean_a <- (cropdat$z_soybean_a*soybean_sd) + soybean_m


# Transform back to original acres
# cropdat$p_corn_a <- (cropdat$p_corn_a*1.02) - 0.01
# cropdat$p_cotton_a <- (cropdat$p_cotton_a*1.02) - 0.01
# cropdat$p_hay_a <- (cropdat$p_hay_a*1.02) - 0.01
# cropdat$p_soybean_a <- (cropdat$p_soybean_a*1.02) - 0.01


# 
# sum(mod$coefficients)
# test <- predict(mod)[1]
# rowSums(test[, 1:5])
# lm(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq , data = surdat)
# 
# mod1b_fe <- felm(p_corn_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq | state, data = surdat)
# mod1fe <- getfe(mod1b_fe)
# mod1fe <- mod1fe[, c(1, 5)]
# rownames(mod1fe) <- NULL
# names(mod1fe) <- c("effect", "state")
# mod1fee <- data.frame(state = csdat$state)
# mod1fee <- left_join(mod1fee, mod1fe, by = "state")
# mod1fee <- cbind(mod1fee, test)
# mod1fee$fit <- mod1fee$test + mod1fee$effect
# summary(mod1fee$fit)
# 
# 
# 
# y <- select(csdat, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
# X <- select(csdat, dday0_10, dday10_30, dday30C, prec, prec_sq)
# 
# fm <- fmlogit(y, X, maxit = 1000)
# summary(fm)
# 
# 
# # Doesn't work stacking varaibles because of unbal panel not implemented
# csdat$latlong <- csdat$lat*csdat$long
# surdat <- select(csdat, fips, state, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a,
#                  dday0_10, dday10_30, dday30C, prec, prec_sq, lat, long, latlong)
# 
# surdat <- gather(surdat, key = crops, value = value, -fips, -state, -dday0_10, -dday10_30, -dday30C, -prec, -prec_sq,
#                  -lat, -long, -latlong)
# 
# surdat <- pdata.frame(surdat, c("state"))
# 
# surdat <- make.pbalanced(surdat)
# 
# 
# mod <- systemfit(value ~ dday0_10 + dday10_30 + dday30C + prec , data = surdat, method = "SUR",
#                  pooled = TRUE)
# summary(mod)
# 
# # Demean for fixed effects
# femoddat <- as.data.frame(moddat)
# femoddat <- select(femoddat, dday0_10, dday10_30, dday30C, prec, prec_sq)
# 
# femoddat$fips <- factor(femoddat$fips)
# class(femoddat)
# femoddat <- demeanlist(femoddat, fl = moddat$fips)
# femoddat
# 
# mod1b <- p_corn_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# 
# mod2b <- p_cotton_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# 
# mod3b <- p_hay_a ~  dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# 
# mod4b <- p_soybean_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq  + lat + long + latlong -1
# 
# mod5b <- p_wheat_a ~ dday0_10 + dday10_30 + dday30C + prec + prec_sq + lat + long + latlong -1
# 
# restrict <- c("corn_dday0_10 + corn_dday10_30 + corn_dday30C + corn_prec + corn_prec_sq + corn_lat + 
#               corn_long + corn_latlong +              
#               cotton_dday0_10 + cotton_dday10_30 + cotton_dday30C + cotton_prec + cotton_prec_sq  + 
#               cotton_lat + cotton_long + cotton_latlong + 
#               hay_dday0_10 + hay_dday10_30 + hay_dday30C + hay_prec + hay_prec_sq + hay_lat + 
#               hay_long + hay_latlong + 
#               soybean_dday0_10 + soybean_dday10_30 + soybean_dday30C + soybean_prec + 
#               soybean_prec_sq  + soybean_lat + soybean_long + soybean_latlong + 
#               wheat_dday0_10 + wheat_dday10_30 + wheat_dday30C + wheat_prec + wheat_prec_sq + 
#               wheat_lat + wheat_long + wheat_latlong = 0")
# 
# 
# 
# mod <- systemfit(list(corn = mod1b, 
#                       cotton = mod2b, 
#                       hay = mod3b, 
#                       soybean = mod4b,
#                       wheat = mod5b), data = csdat, method = "SUR", restrict.matrix = restrict)
# 
# predict(mod)
# 
# , restrict.matrix = restrict)
# summary(mod)
# predict(mod)
# ,
#                  restrict.matrix = restrict)
