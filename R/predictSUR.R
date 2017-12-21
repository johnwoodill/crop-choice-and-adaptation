
# systemfit.mod <- sur_thirty
# newdata <- cropdat
# terms <- NULL
# terms <- c("dday0_10", "dday10_30")

predictSUR <- function(systemfit.mod, newdata, var.terms = NULL, cons.terms = NULL, intercept = FALSE){
  
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

    if (is.null(var.terms) & is.null(cons.terms)){
      inmod <- model.matrix(eqlist[[k]], data = newdata)
  
      modmat[[k]] <- inmod
      
      # Set terms
      var.terms <- colnames(modmat[[1]])
      
      # Get coefficients
      coefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
        
      # Remove coefficients not in model matrix
      locterms <- which(names(coefmat[[k]]) %in% var.terms)
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
        
    if (!is.null(var.terms) & !is.null(cons.terms)){
      inmod <- model.matrix(eqlist[[k]], data = newdata)
      cinmod <- as.data.frame(model.matrix(ceqlist[[k]], data = newdata))
  
      cinmod <- cinmod %>% 
        mutate_all(mean)
      
      modmat[[k]] <- inmod
      cmodmat[[k]] <- as.matrix(cinmod)
      
      # Set terms
      var.terms <- colnames(modmat[[1]])
      cons.terms <- colnames(cmodmat[[1]])
      
      # Get coefficients
      coefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
      ccoefmat[[k]] <- systemfit.mod$eq[[k]][["coefficients"]] 
        
      # Remove coefficients not in model matrix
      locterms <- which(names(coefmat[[k]]) %in% var.terms)
      coefmat[[k]] <- coefmat[[k]][locterms]
      
      # Remove coefficients not cons.terms
      locterms <- which(names(ccoefmat[[k]]) %in% cons.terms)
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
  }
  
  # Convert list to data.frame
  retdat <- as.data.frame(matrix(unlist(retdat), ncol = neq))
  
  # Change col names to predict models
  for(p in 1:neq){
    names(retdat)[p] <- paste0(systemfit.mod$eq[[p]][["eqnLabel"]], "_predict")
  }
  
  # message(paste0("Predicted coefficients: ", paste0(terms, collapse = ", ")))
  return(retdat)
}  
  
 # a <- predict(sur_thirty)
 # b <- predictSUR(sur_thirty, newdata = cropdat, terms = c("dday0_10", "dday10_30", "dday30", "prec", "prec_sq", "dday0_10_thirty", 
 # "dday10_30_thirty", "dday30_thirty", "prec_thirty", "prec_sq_thirty", "factor(state)", "factor(thirty)", 
 # "trend2_al", "trend2_ar", "trend2_de", "trend2_ga", "trend2_ia", "trend2_il", "trend2_in", "trend2_ks", "trend2_ky", 
 # "trend2_md", "trend2_mi", "trend2_mn", "trend2_mo", "trend2_ms", "trend2_mt", "trend2_nc", "trend2_nd", "trend2_ne", 
 # "trend2_oh", "trend2_ok", "trend2_sc", "trend2_sd", "trend2_tn", "trend2_va", "trend2_wi"))
 # 
# c <- predictSUR(sur_thirty, newdata = cropdat) 
#  head(a)
#  head(b)
# head(c)




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
