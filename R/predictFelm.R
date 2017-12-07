# felm.fit : felm object from lfe package
# newdata  : new prediction data

# Custom predict funciton for Felm object models
predictFelm <- function(felm.fit, newdata = NULL, terms = NULL){
  felm.formula <- as.character(felm.fit$call[[2]])
  rhs          = felm.formula[3]
  last         = which(strsplit(rhs,"")[[1]]=="|")[1] - 1
  lm.formula   = paste( felm.formula[2], "~", substr(rhs, 1, last))
  dep.var <- felm.formula[2]
  exp.var <- attr(felm.fit$terms,"term.labels")
  w <<- felm.fit$weights
  

  # Get model data
  dat <- as.data.frame(cbind(felm.fit$response, felm.fit$X))
  
  # Get demeaned data from felm object
  dmdat <- as.data.frame(cbind(felm.fit$cY, felm.fit$cX))
  
  # No fixed-effects
     if( is.null(getfe( felm.fit )) ) {
      lm.fit    = lm( lm.formula, data=dat )

     }
  
  # W/o weights and fixed-effects
    if (is.null(w) & !is.null(getfe(felm.fit))){
      lm.formula   = paste(lm.formula, "- 1" )
       lm.fit       = lm( lm.formula, data=dmdat)
    }
    
  # With weights and fixed-effects
    if (!is.null(w) & !is.null(getfe(felm.fit))){
      lm.formula   = paste(lm.formula, "- 1" )
      lm.fit       = lm( lm.formula, data=dmdat, weights = w^2  )
    }   
  
  # # If no newdata, use model data
    if (is.null(newdata)){
      newdata <- dat
    }
    
    # Check for terms before predict
    # type_check <- ifelse(!is.null(terms), "terms", NULL)
    
    # Predict
  
    if(!is.null(terms)){
      pred <- predict(lm.fit, newdata = newdata, se.fit = TRUE, type = "terms", terms = terms)
      pred$fit <- rowSums(pred$fit)
    }
  
    if(is.null(terms)){
      pred <- predict(lm.fit, newdata = newdata, se.fit = TRUE)
    }
      
    
    pred$res <- felm.fit$residuals
    
    
    # Return pred if no fixed-effects
  	if( is.null(getfe( felm.fit )) ) {
      return(pred)
    }
  
    # Get fixed-effects
    if( !is.null(getfe( felm.fit )) ) {
      
      # data.frame
      fe           = getfe( felm.fit )
      eff.dat <- data.frame(fit = pred$fit)
    }

      # Get each fixed effect and merge
      for (j in names(felm.fit$fe)){
        eff.dat[, `j`] <- felm.fit$fe[[j]]
        fename <- as.character(j)
        mergedat <- filter(fe, fe == j) %>%
                 dplyr::select(1, 5) %>%
                 setNames(c(paste0(j, "_effect"), paste0(fename)))
        mergedat[, 2] <- as.character(mergedat[, 2])
        #eff.dat[, `j`] <- factor(eff.dat[, `j`])
        eff.dat <- left_join(eff.dat, mergedat, by = `j`)
      }
      
      # Store effect data in pred object
      effcol <- grep("_effect", colnames(eff.dat))
      if (length(effcol) == 1){
        pred$effect <- as.numeric(eff.dat[,effcol])
      } else {
        pred$effect <- as.numeric(rowSums(eff.dat[, grep("_effect", colnames(eff.dat))]))
      }
      pred$res <- as.numeric(felm.fit$residuals)
      pred$pred_data <- eff.dat
      return(pred)  
}

      

# cc.pred.se = function( xmat, vcovMat, w, block.size=300 ) {
#   xmat = as.matrix(xmat)
#   n    = nrow(xmat)
#   k    = ncol(xmat)
#   w    = matrix( w, ncol = 1, nrow=n )
#   sqrt( t(w)%*%xmat%*%vcovMat%*%t(xmat)%*%w / ( sum(w)^2 ) )
# }
# 
# 
# 
# boot.strap <- function(x, rep = 1000, sample = length(x), cluster = NULL){
#   if (is.null(cluster)){
#     newdat.sum <- c()
#     newdat.mean <- c()
#     for (r in 1:rep){
#       sampdat <- sample(x, size = sample, replace = TRUE)
#       newdat.sum[r] <- sum(sampdat)
#       newdat.mean[r] <- mean(sampdat)
#     }
#     retdat <- list(se.sum = sd(newdat.sum),
#                 se.mean = sd(newdat.mean))
#     return(retdat)
#   }
#   
#   if(!is.null(cluster)){
#     #dat <- cbind(as.numeric(x), cluster)
#     dat <- data.frame(x = x, cluster = cluster)
#     cl.unique <- unique(cluster)
#     newdat.sum <- c()
#     newdat.mean <- c()
#     for (i in cl.unique){
#       cl.dat.sum <- c()
#       cl.dat.mean <- c()
#       cl.dat <- filter(dat, cluster == i)
#       for (r in 1:rep){
#         sampdat <- sample(cl.dat[[1]], nrow(cl.dat),  replace = TRUE)
#         cl.dat.sum[r] <- sum(sampdat)
#         cl.dat.mean[r] <- mean(sampdat)
#       }
#       newdat.sum[i] <- sd(cl.dat.sum)
#       newdat.mean[i] <- sd(cl.dat.mean)
#     }
#       retdat <- list(se.sum = sd(newdat.sum),
#                 se.mean = sd(newdat.mean))
#     return(retdat)
#     }
#   #eff.dat[, grep("_effect", colnames(eff.dat)
#   }
#   


