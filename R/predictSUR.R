
# systemfit.mod <- sur_thirty
# newdata <- cropdat
# terms <- NULL
# terms <- c("dday0_10", "dday10_30")

predictSUR <- function(systemfit.mod, newdata, terms = NULL, intercept = FALSE){
  
  # n equations from system
  neq <- length(systemfit.mod$eq)
  
  # Get formula for each equation
  eqlist <- list()
  if(is.null(terms)){
    for (i in 1:neq){
      eqlist[[i]] <- as.formula(paste0("~", paste0(systemfit.mod[["eq"]][[i]][["terms"]][[3]]))[2])
      if(intercept == FALSE) eqlist[[i]] <- update(eqlist[[i]], ~. - 1)
  }}
  
  # Get formula with terms
  if(!is.null(terms)){
    eqlist <- list()
    for (j in 1:neq){
      eqlist[[j]] <- as.formula(paste0("~", paste0(terms, collapse = " + ")))
      if(intercept == FALSE) eqlist[[j]] <- update(eqlist[[j]], ~. - 1)
  }}
  
  # Get model.matrix for each equation
  modmat <- list()
  for(k in 1:neq){
    inmod <- model.matrix(eqlist[[k]], data = newdata)
    # inmod <- inmod[, 2:ncol(inmod)]
    modmat[[k]] <- inmod
  }
  
  # Set terms
  terms <- colnames(modmat[[1]])
  
  # Get coefficients
  coefmat <- list()
  for(l in 1:neq){
    coefmat[[l]] <- systemfit.mod$eq[[l]][["coefficients"]] 
  }
  
  # Remove coefficients not in model matrix
  for(m in 1:neq){
    locterms <- which(names(coefmat[[m]]) %in% terms)
    coefmat[[m]] <- coefmat[[m]][locterms]
  }
  
  # Return predict values
  retdat <- list()
  for(nn in 1:neq){
    dim(modmat[[nn]])
    dim(as.matrix(coefmat[[nn]]))
    head(modmat[[nn]])
    head(as.matrix(coefmat[[nn]]))
    as.matrix(coefmat[[nn]])
    colnames(modmat[[nn]])
    indat <- modmat[[nn]] %*% as.matrix(coefmat[[nn]])
    retdat <- c(retdat, list(indat))
  }
  
  # Convert list to data.frame
  retdat <- as.data.frame(matrix(unlist(retdat), ncol = neq))
  
  # Change col names to predict models
  for(p in 1:neq){
    names(retdat)[p] <- paste0(systemfit.mod$eq[[p]][["eqnLabel"]], "_predict")
  }
  

  head(retdat)
  message(paste0("Predicted coefficients: ", paste0(terms, collapse = ", ")))
  return(retdat)
}  
  
# a <- predict(sur_thirty)
# b <- predictSUR(sur_thirty, newdata = cropdat, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "dday0_10_thirty", 
# "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty", "factor(state)", "factor(thirty)", 
# "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
# "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", "trend2_ne", 
# "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi"))
# 
# 
# head(a)
# head(b)





# 
# # Model name
#   systemfit.mod$eq[[1]][["eqnLabel"]]
#   
#   # Model coefficients
#   systemfit.mod$eq[[1]][["coefficients"]]  
#   
#   systemfit.mod$eq[[1]][["terms"]]
#   
#   systemfit.mod$eq[[1]][[]]
#   
#   systemfit.mod$eq[[1]]
#   
#     
# term_form    
# # Keep terms before model.matrix
#   model.matrix(eqlist[[i]], data = newdata)
#   model.matrix(term_form, data = newdata)
# }
#     
#     
# as.formula(paste("~ ", paste(terms, collapse = "+")))
# 
# model.matrix(testform, data = newdata)
# test
# update(eqlist[[i]], formula=drop.terms(eqlist[[i]], grep( "x2", attr(eqlist[[i]], "term.labels") ), keep.response=TRUE) )
# update(eqlist[[1]], -list(dday0_10))
# 
# eqlist
# newform <- as.formula(as.character(eqlist[[1]])[[2]])
# newform
# paste0(eqlist[[i]])[[2]]
# as.formula(eqlist[[i]])
# 
# unclass(eqlist[[1]])
# 
# match.call(eqlist[[1]])
# as.formula(paste0(print(eqlist[[1]])))
# 
# paste(deparse(eqlist[[1]]), collapse= '')
# 
# attr(systemfit.mod$coefficients, "names")
# 
# str(systemfit.mod)
# attr(systemfit.mod$model)
