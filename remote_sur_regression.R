library(tidyverse)
library(systemfit)


# setwd("/home/johnw/")

# Download from Dropbox
# download.file("https://www.dropbox.com/s/u0e0wah5jnmqtf9/full_ag_data.rds?raw=1", 
#               destfile = "full_ag_data.rds", method = "auto")

cropdat <- readRDS("full_ag_data.rds")

# coefnames <- c("corn_dday0_10","corn_dday10_30", "corn_dday30",           
# "corn_prec", "corn_prec_sq", "corn_dday0_10_thirty",    
# "corn_dday10_30_thirty", "corn_dday30_thirty", "corn_prec_thirty",        
# "corn_prec_sq_thirty", "cotton_dday0_10", "cotton_dday10_30",      
# "cotton_dday30", "cotton_prec", "cotton_prec_sq",        
# "cotton_dday0_10_thirty", "cotton_dday10_30_thirty", "cotton_dday30_thirty",    
# "cotton_prec_thirty", "cotton_prec_sq_thirty", "hay_dday0_10",          
# "hay_dday10_30", "hay_dday30", "hay_prec",              
# "hay_prec_sq", "hay_dday0_10_thirty", "hay_dday10_30_thirty",    
# "hay_dday30_thirty", "hay_prec_thirty", "hay_prec_sq_thirty",      
# "soybean_dday0_10", "soybean_dday10_30", "soybean_dday30",        
# "soybean_prec", "soybean_prec_sq", "soybean_dday0_10_thirty", 
# "soybean_dday10_30_thirty", "soybean_dday30_thirty", "soybean_prec_thirty",     
# "soybean_prec_sq_thirty" )

# Models
mod1 <- z_corn_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty - 1   

mod2 <- z_cotton_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty - 1

mod3 <- z_hay_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty - 1

mod4 <- z_soybean_a ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty - 1

# Boot-strapping regression to get coefficients and standard errors
cropdat <- as.data.frame(cropdat)
outdat <- data.frame()
  
sur_bs <- function(x){
  bsdat <- cropdat
  bdat <- data.frame()
  for (j in unique(bsdat$thirty)){
    drawdat <- filter(bsdat, thirty == j)
    sampdat <- sample_n(drawdat, nrow(drawdat), replace = TRUE)
    bdat <- rbind(bdat, sampdat)
  }
  bsdat <- bdat  
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
# rownames(coefmat) <- c("Corn", "Cotton", "Hay", "Soybean", "Wheat")
names(coefmat) <- names(mod$coefficients)[1:ncoef4]
coefmat
names(coefmat) <- substring(names(coefmat), 6)
coefmat$run <- x
coefmat$crop <- c("Corn", "Cotton", "Hay", "Soybean", "Wheat")
# outdat <- rbind(outdat, coefmat)
filename <- paste0("sur_thirty_", x, ".rds")
saveRDS(coefmat, paste0("/home/john/", filename))
# sink("/home/john/log.txt", append=TRUE)
# cat(paste("Starting iteration",x,"\n"))  
message(paste0("Process: (",x, ") complete"))
return(coefmat)

#print(x)
}
# d <- sur_bs(cropdat)

i <- 1:2

library(parallel)
cl <- makeCluster(2)
clusterExport(cl, c("cropdat"))
clusterExport(cl, c("mod1", "mod2", "mod3", "mod4"))
# clusterEvalQ(cl, "/home/john/output.txt")
clusterCall(cl, function() library(systemfit))
clusterCall(cl, function() library(dplyr))
d <- parLapply(cl, X = i, fun = sur_bs)
stopCluster(cl)

sur_thirty <- do.call("rbind", d)

