# mod = sur_five
# newdata_list = newdata_list
# terms = climate_terms
# type = "Five-year"
# effect = "Climate-effect"

predictSUR.clean <- function(mod, newdata_list, terms = NULL, type, effect){
  
  p0 <- newdata_list[[1]]
  p1 <- newdata_list[[2]]
  p2 <- newdata_list[[3]]
  p3 <- newdata_list[[4]]
  p4 <- newdata_list[[5]]
  p5 <- newdata_list[[6]]
    
  indat0 <- predictSUR(mod, newdata = p0, terms = terms)
  indat1 <- predictSUR(mod, newdata = p1, terms = terms)
  indat2 <- predictSUR(mod, newdata = p2, terms = terms)
  indat3 <- predictSUR(mod, newdata = p3, terms = terms)
  indat4 <- predictSUR(mod, newdata = p4, terms = terms)
  indat5 <- predictSUR(mod, newdata = p5, terms = terms)
  
  indat0$corn.pred <- pnorm(indat0$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  indat1$corn.pred <- pnorm(indat1$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  indat2$corn.pred <- pnorm(indat2$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  indat3$corn.pred <- pnorm(indat3$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  indat4$corn.pred <- pnorm(indat4$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  indat5$corn.pred <- pnorm(indat5$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  
  indat0$cotton.pred <- pnorm(indat0$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  indat1$cotton.pred <- pnorm(indat1$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  indat2$cotton.pred <- pnorm(indat2$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  indat3$cotton.pred <- pnorm(indat3$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  indat4$cotton.pred <- pnorm(indat4$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  indat5$cotton.pred <- pnorm(indat5$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  
  indat0$hay.pred <- pnorm(indat0$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  indat1$hay.pred <- pnorm(indat1$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  indat2$hay.pred <- pnorm(indat2$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  indat3$hay.pred <- pnorm(indat3$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  indat4$hay.pred <- pnorm(indat4$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  indat5$hay.pred <- pnorm(indat5$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  
  indat0$soybean.pred <- pnorm(indat0$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  indat1$soybean.pred <- pnorm(indat1$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  indat2$soybean.pred <- pnorm(indat2$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  indat3$soybean.pred <- pnorm(indat3$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  indat4$soybean.pred <- pnorm(indat4$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  indat5$soybean.pred <- pnorm(indat5$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  
  indat0$wheat.pred <- pnorm(indat0$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  indat1$wheat.pred <- pnorm(indat1$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  indat2$wheat.pred <- pnorm(indat2$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  indat3$wheat.pred <- pnorm(indat3$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  indat4$wheat.pred <- pnorm(indat4$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  indat5$wheat.pred <- pnorm(indat5$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  
  indat0_corn <- sum(indat0$corn.pred)
  indat1_corn <- sum(indat1$corn.pred)
  indat2_corn <- sum(indat2$corn.pred)
  indat3_corn <- sum(indat3$corn.pred)
  indat4_corn <- sum(indat4$corn.pred)
  indat5_corn <- sum(indat5$corn.pred)
  
  indat0_cotton <- sum(indat0$cotton.pred)
  indat1_cotton <- sum(indat1$cotton.pred)
  indat2_cotton <- sum(indat2$cotton.pred)
  indat3_cotton <- sum(indat3$cotton.pred)
  indat4_cotton <- sum(indat4$cotton.pred)
  indat5_cotton <- sum(indat5$cotton.pred)
  
  indat0_hay <- sum(indat0$hay.pred)
  indat1_hay <- sum(indat1$hay.pred)
  indat2_hay <- sum(indat2$hay.pred)
  indat3_hay <- sum(indat3$hay.pred)
  indat4_hay <- sum(indat4$hay.pred)
  indat5_hay <- sum(indat5$hay.pred)
  
  indat0_soybean <- sum(indat0$soybean.pred)
  indat1_soybean <- sum(indat1$soybean.pred)
  indat2_soybean <- sum(indat2$soybean.pred)
  indat3_soybean <- sum(indat3$soybean.pred)
  indat4_soybean <- sum(indat4$soybean.pred)
  indat5_soybean <- sum(indat5$soybean.pred)
  
  indat0_wheat <- sum(indat0$wheat.pred)
  indat1_wheat <- sum(indat1$wheat.pred)
  indat2_wheat <- sum(indat2$wheat.pred)
  indat3_wheat <- sum(indat3$wheat.pred)
  indat4_wheat <- sum(indat4$wheat.pred)
  indat5_wheat <- sum(indat5$wheat.pred)
  
  
  
  
  pdat <- data.frame(temp = rep(c(0,1, 2, 3, 4, 5), 5),
                     crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), each = 6),
                     sum = c(indat0_corn, indat1_corn, indat2_corn, indat3_corn, indat4_corn, indat5_corn,
                             indat0_cotton, indat1_cotton, indat2_cotton, indat3_cotton, indat4_cotton, indat5_cotton,
                             indat0_hay, indat1_hay, indat2_hay, indat3_hay, indat4_hay, indat5_hay,
                             indat0_soybean, indat1_soybean, indat2_soybean, indat3_soybean, indat4_soybean, indat5_soybean,
                             indat0_wheat, indat1_wheat, indat2_wheat, indat3_wheat, indat4_wheat, indat5_wheat),
                     type = type,
                     effect = effect)

  return(pdat)
}

