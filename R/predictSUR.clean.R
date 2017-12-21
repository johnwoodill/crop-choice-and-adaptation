# mod = sur_five
# newdata_list = newdata_list_five
# terms = NULL
# type = "Five-year"
# effect = "Climate-effect"

predictSUR.clean <- function(mod, newdata_list, var.terms = NULL, cons.terms = NULL, type, effect){
  
  p0 <- newdata_list[[1]]
  p1 <- newdata_list[[2]]
  p2 <- newdata_list[[3]]
  p3 <- newdata_list[[4]]
  p4 <- newdata_list[[5]]
  p5 <- newdata_list[[6]]
    
  indat0 <- predictSUR(mod, newdata = p0, var.terms = var.terms, cons.terms = cons.terms)
  indat1 <- predictSUR(mod, newdata = p1, var.terms = var.terms, cons.terms = cons.terms)
  indat2 <- predictSUR(mod, newdata = p2, var.terms = var.terms, cons.terms = cons.terms)
  indat3 <- predictSUR(mod, newdata = p3, var.terms = var.terms, cons.terms = cons.terms)
  indat4 <- predictSUR(mod, newdata = p4, var.terms = var.terms, cons.terms = cons.terms)
  indat5 <- predictSUR(mod, newdata = p5, var.terms = var.terms, cons.terms = cons.terms)
  
  corn0.pred <- pnorm(indat0$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  corn1.pred <- pnorm(indat1$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  corn2.pred <- pnorm(indat2$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  corn3.pred <- pnorm(indat3$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  corn4.pred <- pnorm(indat4$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  corn5.pred <- pnorm(indat5$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.0002 - 0.0001
  
  cotton0.pred <- pnorm(indat0$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  cotton1.pred <- pnorm(indat1$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  cotton2.pred <- pnorm(indat2$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  cotton3.pred <- pnorm(indat3$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  cotton4.pred <- pnorm(indat4$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  cotton5.pred <- pnorm(indat5$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.0002 - 0.0001
  
  hay0.pred <- pnorm(indat0$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  hay1.pred <- pnorm(indat1$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  hay2.pred <- pnorm(indat2$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  hay3.pred <- pnorm(indat3$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  hay4.pred <- pnorm(indat4$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  hay5.pred <- pnorm(indat5$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.0002 - 0.0001
  
  soybean0.pred <- pnorm(indat0$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  soybean1.pred <- pnorm(indat1$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  soybean2.pred <- pnorm(indat2$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  soybean3.pred <- pnorm(indat3$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  soybean4.pred <- pnorm(indat4$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  soybean5.pred <- pnorm(indat5$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.0002 - 0.0001
  
  wheat0.pred <- pnorm(indat0$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  wheat1.pred <- pnorm(indat1$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  wheat2.pred <- pnorm(indat2$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  wheat3.pred <- pnorm(indat3$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  wheat4.pred <- pnorm(indat4$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  wheat5.pred <- pnorm(indat5$wheat_predict + resid(mod)[[5]] + mod$effects$wheat.effect)*1.0002 - 0.0001
  
  indat0_corn <- sum(corn0.pred)
  indat1_corn <- sum(corn1.pred)
  indat2_corn <- sum(corn2.pred)
  indat3_corn <- sum(corn3.pred)
  indat4_corn <- sum(corn4.pred)
  indat5_corn <- sum(corn5.pred)
  
  indat0_cotton <- sum(cotton0.pred)
  indat1_cotton <- sum(cotton1.pred)
  indat2_cotton <- sum(cotton2.pred)
  indat3_cotton <- sum(cotton3.pred)
  indat4_cotton <- sum(cotton4.pred)
  indat5_cotton <- sum(cotton5.pred)
  
  indat0_hay <- sum(hay0.pred)
  indat1_hay <- sum(hay1.pred)
  indat2_hay <- sum(hay2.pred)
  indat3_hay <- sum(hay3.pred)
  indat4_hay <- sum(hay4.pred)
  indat5_hay <- sum(hay5.pred)
  
  indat0_soybean <- sum(soybean0.pred)
  indat1_soybean <- sum(soybean1.pred)
  indat2_soybean <- sum(soybean2.pred)
  indat3_soybean <- sum(soybean3.pred)
  indat4_soybean <- sum(soybean4.pred)
  indat5_soybean <- sum(soybean5.pred)
  
  indat0_wheat <- sum(wheat0.pred)
  indat1_wheat <- sum(wheat1.pred)
  indat2_wheat <- sum(wheat2.pred)
  indat3_wheat <- sum(wheat3.pred)
  indat4_wheat <- sum(wheat4.pred)
  indat5_wheat <- sum(wheat5.pred)
  
  
  
  
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

