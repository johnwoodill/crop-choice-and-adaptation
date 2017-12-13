library(tidyverse)
library(lfe)
library(systemfit)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

cropdat <- readRDS("data/full_ag_data.rds")

# Get SUR out files
# sur_five <- readRDS("data/sur_out_five.rds")
# sur_ten <- readRDS("data/sur_out_ten.rds")
# sur_twenty <- readRDS("data/sur_out_twenty.rds")
# sur_thirty <- readRDS("data/sur_out_thirty.rds")

sur_five <- readRDS("models/sur_model_five.rds")
sur_ten <- readRDS("models/sur_model_ten.rds")
sur_twenty <- readRDS("models/sur_model_twenty.rds")
sur_thirty <- readRDS("models/sur_model_thirty.rds")
sur_sixty <- readRDS("models/sur_model_sixty.rds")

# 
# # Residuals
# mod_res <- readRDS("data/mod_residuals.rds")

# msd <- select(sur_thirty, crop_mean_m, crop_sd_sd)

# sur_thirty <- sur_thirty[, 2:62]
# names(sur_thirty) <- substr(names(sur_thirty), 1, nchar(names(sur_thirty))-2)

# mod_matrix <- model.matrix(~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#                dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
#                factor(state) + factor(thirty) +trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
#                trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
#                trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
#                trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
#                trend2_va + trend2_wi - 1, data = cropdat)

p1 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_1C.rds")
p2 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_2C.rds")
p3 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_3C.rds")
p4 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_4C.rds")
p5 <- readRDS("data/degree_day_changes/panel_adapt_regression_data_5C.rds")


# # 
# # corn_thirty <- sur_thirty[1, ]
# # cotton_thirty <- sur_thirty[2, ]
# # hay_thirty <- sur_thirty[3, ]
# # soybean_thirty <- sur_thirty[4, ]
# # wheat_thirty <- sur_thirty[5, ]
# # 
# # out1 <- mod_matrix %*% t(corn_thirty)
# # out2 <- mod_matrix %*% t(cotton_thirty)
# # out3 <- mod_matrix %*% t(hay_thirty)
# # out4 <- mod_matrix %*% t(soybean_thirty)
# # out5 <- mod_matrix %*% t(wheat_thirty)
# 
# range(out1)
# out1 <- (out1*sd(cropdat$p_corn_a, na.rm = TRUE)) + mean(cropdat$p_corn_a, na.rm = TRUE)
# out1 <- (out1*1.02) - 0.01
# range(out1)
# 
# range(out2)
# out2 <- (out2*sd(cropdat$p_cotton_a, na.rm = TRUE)) + mean(cropdat$p_cotton_a, na.rm = TRUE)
# out2 <- (out2*1.02) - 0.01
# range(out2)
# 
# range(out3)
# out3 <- (out3*sd(cropdat$p_hay_a, na.rm = TRUE)) + mean(cropdat$p_hay_a, na.rm = TRUE)
# out3 <- (out3*1.02) - 0.01
# range(out3)
# 
# range(out4)
# out4 <- (out4*sd(cropdat$p_soybean_a, na.rm = TRUE)) + mean(cropdat$p_soybean_a, na.rm = TRUE)
# out4 <- (out4*1.02) - 0.01
# range(out4)
# 
# range(out5)
# out5 <- (out5*sd(cropdat$p_wheat_a, na.rm = TRUE)) + mean(cropdat$p_wheat_a, na.rm = TRUE)
# out5 <- (out5*1.02) - 0.01
# range(out5)
# 
# 
# out <- cbind(out1, out2, out3, out4, out5)
# rowSums(out)
# range(rowSums(out))

pten0 <- predict(sur_ten$eq[[1]], type = c("terms"), terms = c("corn_dday0_10"))
pten0
predict.lm(sur_ten$eq[[1]])

pten1 <- predict(sur_ten, newdata = p1)
pten2 <- predict(sur_ten, newdata = p2)
pten3 <- predict(sur_ten, newdata = p3)
pten4 <- predict(sur_ten, newdata = p4)
pten5 <- predict(sur_ten, newdata = p5)

pten0$corn.pred <- pnorm(pten0$corn.pred + resid(sur_ten)[[1]])*1.02 - 0.01
pten1$corn.pred <- pnorm(pten1$corn.pred + resid(sur_ten)[[1]])*1.02 - 0.01
pten2$corn.pred <- pnorm(pten2$corn.pred + resid(sur_ten)[[1]])*1.02 - 0.01
pten3$corn.pred <- pnorm(pten3$corn.pred + resid(sur_ten)[[1]])*1.02 - 0.01
pten4$corn.pred <- pnorm(pten4$corn.pred + resid(sur_ten)[[1]])*1.02 - 0.01
pten5$corn.pred <- pnorm(pten5$corn.pred + resid(sur_ten)[[1]])*1.02 - 0.01

pten0$cotton.pred <- pnorm(pten0$cotton.pred + resid(sur_ten)[[2]])*1.02 - 0.01
pten1$cotton.pred <- pnorm(pten1$cotton.pred + resid(sur_ten)[[2]])*1.02 - 0.01
pten2$cotton.pred <- pnorm(pten2$cotton.pred + resid(sur_ten)[[2]])*1.02 - 0.01
pten3$cotton.pred <- pnorm(pten3$cotton.pred + resid(sur_ten)[[2]])*1.02 - 0.01
pten4$cotton.pred <- pnorm(pten4$cotton.pred + resid(sur_ten)[[2]])*1.02 - 0.01
pten5$cotton.pred <- pnorm(pten5$cotton.pred + resid(sur_ten)[[2]])*1.02 - 0.01

pten0$hay.pred <- pnorm(pten0$hay.pred + resid(sur_ten)[[3]])*1.02 - 0.01
pten1$hay.pred <- pnorm(pten1$hay.pred + resid(sur_ten)[[3]])*1.02 - 0.01
pten2$hay.pred <- pnorm(pten2$hay.pred + resid(sur_ten)[[3]])*1.02 - 0.01
pten3$hay.pred <- pnorm(pten3$hay.pred + resid(sur_ten)[[3]])*1.02 - 0.01
pten4$hay.pred <- pnorm(pten4$hay.pred + resid(sur_ten)[[3]])*1.02 - 0.01
pten5$hay.pred <- pnorm(pten5$hay.pred + resid(sur_ten)[[3]])*1.02 - 0.01

pten0$soybean.pred <- pnorm(pten0$soybean.pred + resid(sur_ten)[[4]])*1.02 - 0.01
pten1$soybean.pred <- pnorm(pten1$soybean.pred + resid(sur_ten)[[4]])*1.02 - 0.01
pten2$soybean.pred <- pnorm(pten2$soybean.pred + resid(sur_ten)[[4]])*1.02 - 0.01
pten3$soybean.pred <- pnorm(pten3$soybean.pred + resid(sur_ten)[[4]])*1.02 - 0.01
pten4$soybean.pred <- pnorm(pten4$soybean.pred + resid(sur_ten)[[4]])*1.02 - 0.01
pten5$soybean.pred <- pnorm(pten5$soybean.pred + resid(sur_ten)[[4]])*1.02 - 0.01

pten0$wheat.pred <- pnorm(pten0$wheat.pred + resid(sur_ten)[[5]])*1.02 - 0.01
pten1$wheat.pred <- pnorm(pten1$wheat.pred + resid(sur_ten)[[5]])*1.02 - 0.01
pten2$wheat.pred <- pnorm(pten2$wheat.pred + resid(sur_ten)[[5]])*1.02 - 0.01
pten3$wheat.pred <- pnorm(pten3$wheat.pred + resid(sur_ten)[[5]])*1.02 - 0.01
pten4$wheat.pred <- pnorm(pten4$wheat.pred + resid(sur_ten)[[5]])*1.02 - 0.01
pten5$wheat.pred <- pnorm(pten5$wheat.pred + resid(sur_ten)[[5]])*1.02 - 0.01

pten0_corn <- sum(pten0$corn.pred)
pten1_corn <- sum(pten1$corn.pred)
pten2_corn <- sum(pten2$corn.pred)
pten3_corn <- sum(pten3$corn.pred)
pten4_corn <- sum(pten4$corn.pred)
pten5_corn <- sum(pten5$corn.pred)

pten0_cotton <- sum(pten0$cotton.pred)
pten1_cotton <- sum(pten1$cotton.pred)
pten2_cotton <- sum(pten2$cotton.pred)
pten3_cotton <- sum(pten3$cotton.pred)
pten4_cotton <- sum(pten4$cotton.pred)
pten5_cotton <- sum(pten5$cotton.pred)

pten0_hay <- sum(pten0$hay.pred)
pten1_hay <- sum(pten1$hay.pred)
pten2_hay <- sum(pten2$hay.pred)
pten3_hay <- sum(pten3$hay.pred)
pten4_hay <- sum(pten4$hay.pred)
pten5_hay <- sum(pten5$hay.pred)

pten0_soybean <- sum(pten0$soybean.pred)
pten1_soybean <- sum(pten1$soybean.pred)
pten2_soybean <- sum(pten2$soybean.pred)
pten3_soybean <- sum(pten3$soybean.pred)
pten4_soybean <- sum(pten4$soybean.pred)
pten5_soybean <- sum(pten5$soybean.pred)

pten0_wheat <- sum(pten0$wheat.pred)
pten1_wheat <- sum(pten1$wheat.pred)
pten2_wheat <- sum(pten2$wheat.pred)
pten3_wheat <- sum(pten3$wheat.pred)
pten4_wheat <- sum(pten4$wheat.pred)
pten5_wheat <- sum(pten5$wheat.pred)

pdat <- data.frame(temp = rep(c(0,1, 2, 3, 4, 5), 5),
                   crop = rep(c("Corn", "Cotton", "Hay", "Soybean", "Wheat"), each = 6),
                   sum = c(pten0_corn, pten1_corn, pten2_corn, pten3_corn, pten4_corn, pten5_corn,
                           pten0_cotton, pten1_cotton, pten2_cotton, pten3_cotton, pten4_cotton, pten5_cotton,
                           pten0_hay, pten1_hay, pten2_hay, pten3_hay, pten4_hay, pten5_hay,
                           pten0_soybean, pten1_soybean, pten2_soybean, pten3_soybean, pten4_soybean, pten5_soybean,
                           pten0_wheat, pten1_wheat, pten2_wheat, pten3_wheat, pten4_wheat, pten5_wheat))

pdat

pdat <- pdat %>% 
  group_by(crop) %>% 
  mutate(change = (sum - first(sum))/first(sum))
pdat
pdat$change <- 100*pdat$change

ggplot(pdat, aes(temp, change)) + geom_line() + geom_point() + facet_wrap(~crop, scales = "free")

ggplot(pdat, aes(temp, sum, color = crop)) + geom_line() + geom_point()
