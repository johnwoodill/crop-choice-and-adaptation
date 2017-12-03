library(tidyverse)
library(systemfit)


setwd("/home/john/")

# Download from Dropbox
download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1", 
              destfile = "full_ag_data.rds", method = "auto")

cropdat <- readRDS("full_ag_data.rds")

coefnames <- c("corn_dday0_10","corn_dday10_30", "corn_dday30",           
"corn_prec", "corn_prec_sq", "corn_dday0_10_five",    
"corn_dday10_30_five", "corn_dday30_five", "corn_prec_five",        
"corn_prec_sq_five", "cotton_dday0_10", "cotton_dday10_30",      
"cotton_dday30", "cotton_prec", "cotton_prec_sq",        
"cotton_dday0_10_five", "cotton_dday10_30_five", "cotton_dday30_five",    
"cotton_prec_five", "cotton_prec_sq_five", "hay_dday0_10",          
"hay_dday10_30", "hay_dday30", "hay_prec",              
"hay_prec_sq", "hay_dday0_10_five", "hay_dday10_30_five",    
"hay_dday30_five", "hay_prec_five", "hay_prec_sq_five",      
"soybean_dday0_10", "soybean_dday10_30", "soybean_dday30",        
"soybean_prec", "soybean_prec_sq", "soybean_dday0_10_five", 
"soybean_dday10_30_five", "soybean_dday30_five", "soybean_prec_five",     
"soybean_prec_sq_five" )

# Models
mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + 
  factor(state) + factor(state_trend_sq) - 1   

mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  + 
  factor(state) + factor(state_trend_sq)  - 1

mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  + 
  factor(state) + factor(state_trend_sq)  - 1

mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five  + 
  factor(state) + factor(state_trend_sq)  - 1

# Boot-strapping regression to get coefficients and standard errors
outdat <- data.frame()

for (i in 1:10){
  
  bsdat <- cropdat
  bsdat$dday0_10 <- sample(bsdat$dday0_10, nrow(bsdat), replace = TRUE)
  bsdat$dday10_30 <- sample(bsdat$dday10_30, nrow(bsdat), replace = TRUE)
  bsdat$dday30 <- sample(bsdat$dday30, nrow(bsdat), replace = TRUE)
  bsdat$prec <- sample(bsdat$prec, nrow(bsdat), replace = TRUE)
  bsdat$prec_sq <- sample(bsdat$prec_sq, nrow(bsdat), replace = TRUE)
  bsdat$dday0_10_five <- sample(bsdat$dday0_10_five, nrow(bsdat), replace = TRUE)
  bsdat$dday10_30_five <- sample(bsdat$dday10_30_five, nrow(bsdat), replace = TRUE)
  bsdat$dday30_five <- sample(bsdat$dday30_five, nrow(bsdat), replace = TRUE)
  bsdat$prec_five <- sample(bsdat$prec_five, nrow(bsdat), replace = TRUE)
  bsdat$prec_sq_five <- sample(bsdat$prec_sq_five, nrow(bsdat), replace = TRUE)
  
  # Convert to z-scores for linear regression

  # First estimate between zero and 1
  bsdat$p_corn_a <- (bsdat$p_corn_a + .01)/1.02
  bsdat$p_cotton_a <- (bsdat$p_cotton_a + .01)/1.02
  bsdat$p_hay_a <- (bsdat$p_hay_a + .01)/1.02
  bsdat$p_soybean_a <- (bsdat$p_soybean_a + .01)/1.02
  bsdat$p_wheat_a <- (bsdat$p_wheat_a + .01)/1.02
  
  # Get mean and sd to convert back
  corn_m <- mean(bsdat$p_corn_a, na.rm = TRUE)
  corn_sd <- sd(bsdat$p_corn_a, na.rm = TRUE)
  
  cotton_m <- mean(bsdat$p_cotton_a, na.rm = TRUE)
  cotton_sd <- sd(bsdat$p_cotton_a, na.rm = TRUE)
  
  hay_m <- mean(bsdat$p_hay_a, na.rm = TRUE)
  hay_sd <- sd(bsdat$p_hay_a, na.rm = TRUE)
  
  soybean_m <- mean(bsdat$p_soybean_a, na.rm = TRUE)
  soybean_sd <- sd(bsdat$p_soybean_a, na.rm = TRUE)
  
  wheat_m <- mean(bsdat$p_wheat_a, na.rm = TRUE)
  wheat_sd <- sd(bsdat$p_wheat_a, na.rm = TRUE)
  
  # Calc z-scores
  bsdat$z_corn_a <- (bsdat$p_corn_a - mean(bsdat$p_corn_a, na.rm = TRUE))/sd(bsdat$p_corn_a, na.rm = TRUE)
  bsdat$z_cotton_a <- (bsdat$p_cotton_a - mean(bsdat$p_cotton_a, na.rm = TRUE))/sd(bsdat$p_cotton_a, na.rm = TRUE)
  bsdat$z_hay_a <- (bsdat$p_hay_a - mean(bsdat$p_hay_a, na.rm = TRUE))/sd(bsdat$p_hay_a, na.rm = TRUE)
  bsdat$z_soybean_a <- (bsdat$p_soybean_a - mean(bsdat$p_soybean_a, na.rm = TRUE))/sd(bsdat$p_soybean_a, na.rm = TRUE)
  bsdat$z_wheat_a <- (bsdat$p_wheat_a - mean(bsdat$p_wheat_a, na.rm = TRUE))/sd(bsdat$p_wheat_a, na.rm = TRUE)



mod <- systemfit(list(corn = mod1, 
                      cotton = mod2, 
                      hay = mod3, 
                      soybean = mod4), data = bsdat, method = "SUR")



# (n) coefficients
ncoef <- length(mod$coefficients)
ncoef4 <- length(mod$coefficients)/4

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
coefmat$run <- i
outdat <- rbind(outdat, coefmat)
print(i)
}
