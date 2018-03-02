
# systemfit.mod <- sur_thirty
# newdata <- cropdat
# terms <- NULL
# terms <- c("dday0_10", "dday10_30")

predictSUR <- function(systemfit.mod, 
                       newdata, 
                       fips = NULL,
                       var.terms = NULL, 
                       cons.terms = NULL, 
                       intercept = FALSE,
                       leave.one.out = FALSE){
  
  # var.terms = NULL
  # cons.terms = NULL
  #                      
  # n equations from system
  neq <- length(systemfit.mod$eq)
  
  # Get formula for each equation
  eqlist <- list()
  if(is.null(var.terms)){
    for (i in 1:neq){
      eqlist[[i]] <- as.formula(paste0("~", paste0(systemfit.mod[["eq"]][[i]][["terms"]][[3]]))[2])
      if(intercept == FALSE) eqlist[[i]] <- update(eqlist[[i]], ~. - 1)
  }}
  
  # Get formula with var.terms
  if(!is.null(var.terms)){
    eqlist <- list()
    for (j in 1:neq){
      eqlist[[j]] <- as.formula(paste0("~", paste0(var.terms, collapse = " + ")))
      if(intercept == FALSE) eqlist[[j]] <- update(eqlist[[j]], ~. - 1)
    }}
  
  # Get formula with cons.terms
  if(!is.null(cons.terms)){
    ceqlist <- list()
    for (j in 1:neq){
      ceqlist[[j]] <- as.formula(paste0("~", paste0(cons.terms, collapse = " + ")))
      if(intercept == FALSE) ceqlist[[j]] <- update(ceqlist[[j]], ~. - 1)
  }}
  
  # Get model.matrix and coefficients 
  modmat <- list()
  cmodmat <- list()
  coefmat <- list()
  ccoefmat <- list()
  retdat <- list()
  
  for(k in 1:neq){

    # No var.terms and no cons.terms
    if (is.null(var.terms) & is.null(cons.terms)){
      inmod <- model.matrix(eqlist[[k]], data = newdata)
  
      modmat[[k]] <- inmod
      
      # Set terms
      varterms <- colnames(modmat[[1]])
      
      # Get coefficients
      coefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
        
      # Remove coefficients not in model matrix
      locterms <- which(names(coefmat[[k]]) %in% varterms)
      coefmat[[k]] <- coefmat[[k]][locterms]
      
      # Return predict values
      dim(modmat[[k]])
      dim(as.matrix(coefmat[[k]]))
      head(modmat[[k]])
      head(as.matrix(coefmat[[k]]))
      as.matrix(coefmat[[k]])
      colnames(modmat[[k]])
      indat <- modmat[[k]] %*% as.matrix(coefmat[[k]])
      retdat <- c(retdat, list(indat))
    }
        
    # Variation terms and constant terms
    if (!is.null(var.terms) & !is.null(cons.terms)){
      inmod <- model.matrix(eqlist[[k]], data = newdata)
      cinmod <- as.data.frame(model.matrix(ceqlist[[k]], data = newdata))
      
      # cindat$fips <- fips
      # cindat <- cindat %>% 
      #   group_by(fips) %>% 
      #   mutate_all(mean)
      # head(cindat)
      # cinmod$fips <- NULL
      
      modmat[[k]] <- inmod
      cmodmat[[k]] <- as.matrix(cinmod)
      
      # Set terms
      varterms <- colnames(modmat[[1]])
      consterms <- colnames(cmodmat[[1]])
      
      # Get coefficients
      coefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
      ccoefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
        
      # Remove coefficients not in model matrix
      locterms <- which(names(coefmat[[k]]) %in% varterms)
      coefmat[[k]] <- coefmat[[k]][locterms]
      
      # Remove coefficients not cons.terms
      locterms <- which(names(ccoefmat[[k]]) %in% consterms)
      ccoefmat[[k]] <- ccoefmat[[k]][locterms]
        
      
      # Return predict values
      #dim(modmat[[k]])
      #dim(as.matrix(coefmat[[k]]))
      #head(modmat[[k]])
      #head(as.matrix(coefmat[[k]]))
      #as.matrix(coefmat[[k]])
      #colnames(modmat[[k]])
      indat <- modmat[[k]] %*% as.matrix(coefmat[[k]])
      cindat <- cmodmat[[k]] %*% as.matrix(ccoefmat[[k]])
      indat <- rowSums(cbind(indat, cindat))
      retdat <- c(retdat, list(indat))
    }
    
    # Variation terms and no constant terms
    if (!is.null(var.terms) & is.null(cons.terms)){
      inmod <- model.matrix(eqlist[[k]], data = newdata)
      # cinmod <- as.data.frame(model.matrix(ceqlist[[k]], data = newdata))
  
      # cinmod <- cinmod %>% 
        # mutate_all(mean)
      
      modmat[[k]] <- inmod
      # cmodmat[[k]] <- as.matrix(cinmod)
      
      # Set terms
      varterms <- colnames(modmat[[1]])
      # consterms <- colnames(cmodmat[[1]])
      
      # Get coefficients
      coefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
      # ccoefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
        
      # Remove coefficients not in model matrix
      locterms <- which(names(coefmat[[k]]) %in% varterms)
      coefmat[[k]] <- coefmat[[k]][locterms]
      
      # Remove coefficients not cons.terms
      # locterms <- which(names(ccoefmat[[k]]) %in% consterms)
      # ccoefmat[[k]] <- ccoefmat[[k]][locterms]
        
      
      # Return predict values
      #dim(modmat[[k]])
      #dim(as.matrix(coefmat[[k]]))
      #head(modmat[[k]])
      #head(as.matrix(coefmat[[k]]))
      #as.matrix(coefmat[[k]])
      #colnames(modmat[[k]])
      indat <- modmat[[k]] %*% as.matrix(coefmat[[k]])
      # cindat <- cmodmat[[k]] %*% as.matrix(ccoefmat[[k]])
      # indat <- rowSums(cbind(indat, cindat))
      retdat <- c(retdat, list(indat))
    }
  }
  
  # Convert list to data.frame
  retdat <- as.data.frame(matrix(unlist(retdat), ncol = neq))
  
  # if(isTRUE(leave.one.out)){
  #   for(i in 1:ncol(retdat)){
  #     corn <- pnorm(retdat[, 1] + resid(systemfit.mod)[[1]] + systemfit.mod$effects$corn.effect)*1.0002 - 0.0001
  #     head(corn)
  #     head(cropdat$p_corn_a)
  #     apply(retdat, 2, function(x) pnorm(x)*1.0002 - 0.0001)
  #     mdat <- retdat[, -i]
  #   }
  # }
  
  # Change col names to predict models
  for(p in 1:neq){
    names(retdat)[p] <- paste0(systemfit.mod$eq[[p]][["eqnLabel"]], "_predict")
  }
  
  
  
  # message(paste0("Predicted coefficients: ", paste0(terms, collapse = ", ")))
  return(retdat)
}  
  