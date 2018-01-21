# mod = sur_five
# newdata_list = newdata_list_five
# terms = NULL
# type = "Five-year"
# effect = "Climate-effect"

predictSUR.clean <- function(mod, newdata_list, acres, var.terms = NULL, cons.terms = NULL, type, effect){
  
  p0 <- newdata_list[[1]]
  p1 <- newdata_list[[2]]
  p2 <- newdata_list[[3]]
  p3 <- newdata_list[[4]]
  p4 <- newdata_list[[5]]
  p5 <- newdata_list[[6]]
    
  indat0 <- predictSUR(systemfit.mod = mod, newdata = p0, var.terms = var.terms, cons.terms = cons.terms)
  indat1 <- predictSUR(mod, newdata = p1, var.terms = var.terms, cons.terms = cons.terms)
  indat2 <- predictSUR(mod, newdata = p2, var.terms = var.terms, cons.terms = cons.terms)
  indat3 <- predictSUR(mod, newdata = p3, var.terms = var.terms, cons.terms = cons.terms)
  indat4 <- predictSUR(mod, newdata = p4, var.terms = var.terms, cons.terms = cons.terms)
  indat5 <- predictSUR(mod, newdata = p5, var.terms = var.terms, cons.terms = cons.terms)
  
  corn0.pred <- pnorm(indat0$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
  corn1.pred <- pnorm(indat1$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
  corn2.pred <- pnorm(indat2$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
  corn3.pred <- pnorm(indat3$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
  corn4.pred <- pnorm(indat4$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
  corn5.pred <- pnorm(indat5$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
  
  cotton0.pred <- pnorm(indat0$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
  cotton1.pred <- pnorm(indat1$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
  cotton2.pred <- pnorm(indat2$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
  cotton3.pred <- pnorm(indat3$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
  cotton4.pred <- pnorm(indat4$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
  cotton5.pred <- pnorm(indat5$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
  
  hay0.pred <- pnorm(indat0$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
  hay1.pred <- pnorm(indat1$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
  hay2.pred <- pnorm(indat2$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
  hay3.pred <- pnorm(indat3$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
  hay4.pred <- pnorm(indat4$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
  hay5.pred <- pnorm(indat5$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
  
  soybean0.pred <- pnorm(indat0$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.00101 - 0.001
  soybean1.pred <- pnorm(indat1$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.00101 - 0.001
  soybean2.pred <- pnorm(indat2$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.00101 - 0.001
  soybean3.pred <- pnorm(indat3$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.00101 - 0.001
  soybean4.pred <- pnorm(indat4$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.00101 - 0.001
  soybean5.pred <- pnorm(indat5$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.00101 - 0.001
  
  wheat0.pred <- pnorm(indat0$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.00101 - 0.001
  wheat1.pred <- pnorm(indat1$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.00101 - 0.001
  wheat2.pred <- pnorm(indat2$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.00101 - 0.001
  wheat3.pred <- pnorm(indat3$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.00101 - 0.001
  wheat4.pred <- pnorm(indat4$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.00101 - 0.001
  wheat5.pred <- pnorm(indat5$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.00101 - 0.001
  
  temp0 <- data.frame(temp = 0,
                      corn.pred = corn0.pred,
                      cotton.pred = cotton0.pred,
                      hay.pred = hay0.pred,
                      soybean.pred = soybean0.pred,
                      wheat.pred = wheat0.pred)


  temp1 <- data.frame(temp = 1,
                      corn.pred = corn1.pred,
                      cotton.pred = cotton1.pred,
                      hay.pred = hay1.pred,
                      soybean.pred = soybean1.pred,
                      wheat.pred = wheat1.pred)
  
  temp2 <- data.frame(temp = 2,
                      corn.pred = corn2.pred,
                      cotton.pred = cotton2.pred,
                      hay.pred = hay2.pred,
                      soybean.pred = soybean2.pred,
                      wheat.pred = wheat2.pred)
    
  temp3 <- data.frame(temp = 3,
                      corn.pred = corn3.pred,
                      cotton.pred = cotton3.pred,
                      hay.pred = hay3.pred,
                      soybean.pred = soybean3.pred,
                      wheat.pred = wheat3.pred)
      
      
  temp4 <- data.frame(temp = 4,
                      corn.pred = corn4.pred,
                      cotton.pred = cotton4.pred,
                      hay.pred = hay4.pred,
                      soybean.pred = soybean4.pred,
                      wheat.pred = wheat4.pred)
        
  temp5 <- data.frame(temp = 5,
                      corn.pred = corn5.pred,
                      cotton.pred = cotton5.pred,
                      hay.pred = hay5.pred,
                      soybean.pred = soybean5.pred,
                      wheat.pred = wheat5.pred)
  
  temp0[temp0 <0 ] <- 0
  temp1[temp1 <0 ] <- 0
  temp2[temp2 <0 ] <- 0
  temp3[temp3 <0 ] <- 0
  temp4[temp4 <0 ] <- 0
  temp5[temp5 <0 ] <- 0
  
  temp0[, 2:6] <- temp0[, 2:6] / rowSums(temp0[, 2:6])
  temp1[, 2:6] <- temp1[, 2:6] / rowSums(temp1[, 2:6])
  temp2[, 2:6] <- temp2[, 2:6] / rowSums(temp2[, 2:6])
  temp3[, 2:6] <- temp3[, 2:6] / rowSums(temp3[, 2:6])
  temp4[, 2:6] <- temp4[, 2:6] / rowSums(temp4[, 2:6])
  temp5[, 2:6] <- temp5[, 2:6] / rowSums(temp5[, 2:6])
  
  indat0_corn <- sum(temp0$corn.pred*acres)
  indat1_corn <- sum(temp1$corn.pred*acres)
  indat2_corn <- sum(temp2$corn.pred*acres)
  indat3_corn <- sum(temp3$corn.pred*acres)
  indat4_corn <- sum(temp4$corn.pred*acres)
  indat5_corn <- sum(temp5$corn.pred*acres)
  
  indat0_cotton <- sum(temp0$cotton.pred*acres)
  indat1_cotton <- sum(temp1$cotton.pred*acres)
  indat2_cotton <- sum(temp2$cotton.pred*acres)
  indat3_cotton <- sum(temp3$cotton.pred*acres)
  indat4_cotton <- sum(temp4$cotton.pred*acres)
  indat5_cotton <- sum(temp5$cotton.pred*acres)
  
  indat0_hay <- sum(temp0$hay.pred*acres)
  indat1_hay <- sum(temp1$hay.pred*acres)
  indat2_hay <- sum(temp2$hay.pred*acres)
  indat3_hay <- sum(temp3$hay.pred*acres)
  indat4_hay <- sum(temp4$hay.pred*acres)
  indat5_hay <- sum(temp5$hay.pred*acres)
  
  indat0_soybean <- sum(temp0$soybean.pred*acres)
  indat1_soybean <- sum(temp1$soybean.pred*acres)
  indat2_soybean <- sum(temp2$soybean.pred*acres)
  indat3_soybean <- sum(temp3$soybean.pred*acres)
  indat4_soybean <- sum(temp4$soybean.pred*acres)
  indat5_soybean <- sum(temp5$soybean.pred*acres)
  
  indat0_wheat <- sum(temp0$wheat.pred*acres)
  indat1_wheat <- sum(temp1$wheat.pred*acres)
  indat2_wheat <- sum(temp2$wheat.pred*acres)
  indat3_wheat <- sum(temp3$wheat.pred*acres)
  indat4_wheat <- sum(temp4$wheat.pred*acres)
  indat5_wheat <- sum(temp5$wheat.pred*acres)
  
  
  
  
  pdat <- data.frame(temp = rep(c(0,1, 2, 3, 4, 5), 5),
                     crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), each = 6),
                     sum = c(indat0_corn, indat1_corn, indat2_corn, indat3_corn, indat4_corn, indat5_corn,
                             indat0_cotton, indat1_cotton, indat2_cotton, indat3_cotton, indat4_cotton, indat5_cotton,
                             indat0_hay, indat1_hay, indat2_hay, indat3_hay, indat4_hay, indat5_hay,
                             indat0_soybean, indat1_soybean, indat2_soybean, indat3_soybean, indat4_soybean, indat5_soybean,
                             indat0_wheat, indat1_wheat, indat2_wheat, indat3_wheat, indat4_wheat, indat5_wheat),
                     type = type,
                     effect = effect)
  
  temp <- bind_rows(temp0, temp1, temp2, temp3, temp4, temp5)
  retlist <- list(predictions = temp,
                  agg_predictions = pdat)
  return(retlist)
}

