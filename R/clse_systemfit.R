clse_systemfit   <- function(systemfit.mod, cluster){
               
    # Find square matrix diagonals (vcov)
    sql <- length(systemfit.mod$coefficients)/length(systemfit.mod$eq)
    dg <- seq(1, nrow(mat), sql)
    retdat <- data.frame()
               
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- systemfit.mod$rank
    
    # Degree of freedom correction
    dfc <- (M/(M-1))*((N-1)/(N-K))
               
    for (i in 1:length(systemfit.mod$eq)){
      
      # Calculate estfun
      xmat <- model.frame(systemfit.mod$eq[[i]])
      xmat[, 1] <- NULL
      
      # xmat <- model.matrix(formula(systemfit.mod)[[i]], data = cropdat_dm)
      res <- residuals(systemfit.mod)[i]
      res <- as.vector(resid(systemfit.mod)[[i]])
      ef <- res * xmat
      
      # Calculate bread for sandwich            
      bread.df <- nrow(resid(systemfit.mod)) * vcov(systemfit.mod)[dg[i]:(dg[i] + 8), dg[i]:(dg[i] + 8)]

      # Correct estfun and calculate meat
      uj  <- apply(ef, 2, function(x) tapply(x, cluster, sum)) 
      meat <- (crossprod(uj)/N) 
      
      # Get adj clustered standard errors
      vcovCL <- sqrt(diag(dfc * 1/N * (bread.df %*% meat %*% bread.df) ))
      
      # Build data frame for return fn
      cldat <- data.frame(coef = names(vcovCL), se = c(vcovCL))
      rownames(cldat) <- NULL
      retdat <- rbind(retdat, cldat)
    }              
    retdat
}