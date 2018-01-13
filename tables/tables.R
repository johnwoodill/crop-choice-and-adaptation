library(stargazer)
library(lfe)


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
  }

# Save models
corn1 <- readRDS("models/modtwenty_corn_base_1.rds")
corn2 <- readRDS("models/modtwenty_corn_base_2.rds")
corn3 <- readRDS("models/modtwenty_corn_base_3.rds")
corn4 <- readRDS("models/modtwenty_corn_base_4.rds")
corn5 <- readRDS("models/modtwenty_corn_base_5.rds")
corn6 <- readRDS("models/modtwenty_corn.rds")
corn7 <- readRDS("models/modfive_corn.rds")
corn8 <- readRDS("models/modten_corn.rds")
corn9 <- readRDS("models/modthirty_corn.rds")
corn10 <- readRDS("models/modsixty_corn.rds")

cotton1 <- readRDS("models/modtwenty_cotton_base_1.rds")
cotton2 <- readRDS("models/modtwenty_cotton_base_2.rds")
cotton3 <- readRDS("models/modtwenty_cotton_base_3.rds")
cotton4 <- readRDS("models/modtwenty_cotton_base_4.rds")
cotton5 <- readRDS("models/modtwenty_cotton_base_5.rds")
cotton6 <- readRDS("models/modtwenty_cotton.rds")
cotton7 <- readRDS("models/modfive_cotton.rds")
cotton8 <- readRDS("models/modten_cotton.rds")
cotton9 <- readRDS("models/modthirty_cotton.rds")
cotton10 <- readRDS("models/modsixty_cotton.rds")

hay1 <- readRDS("models/modtwenty_hay_base_1.rds")
hay2 <- readRDS("models/modtwenty_hay_base_2.rds")
hay3 <- readRDS("models/modtwenty_hay_base_3.rds")
hay4 <- readRDS("models/modtwenty_hay_base_4.rds")
hay5 <- readRDS("models/modtwenty_hay_base_5.rds")
hay6 <- readRDS("models/modtwenty_hay.rds")
hay7 <- readRDS("models/modfive_hay.rds")
hay8 <- readRDS("models/modten_hay.rds")
hay9 <- readRDS("models/modthirty_hay.rds")
hay10 <- readRDS("models/modsixty_hay.rds")

soybean1 <- readRDS("models/modtwenty_soybean_base_1.rds")
soybean2 <- readRDS("models/modtwenty_soybean_base_2.rds")
soybean3 <- readRDS("models/modtwenty_soybean_base_3.rds")
soybean4 <- readRDS("models/modtwenty_soybean_base_4.rds")
soybean5 <- readRDS("models/modtwenty_soybean_base_5.rds")
soybean6 <- readRDS("models/modtwenty_soybean.rds")
soybean7 <- readRDS("models/modfive_soybean.rds")
soybean8 <- readRDS("models/modten_soybean.rds")
soybean9 <- readRDS("models/modthirty_soybean.rds")
soybean10 <- readRDS("models/modsixty_soybean.rds")

wheat1 <- readRDS("models/modtwenty_wheat_base_1.rds")
wheat2 <- readRDS("models/modtwenty_wheat_base_2.rds")
wheat3 <- readRDS("models/modtwenty_wheat_base_3.rds")
wheat4 <- readRDS("models/modtwenty_wheat_base_4.rds")
wheat5 <- readRDS("models/modtwenty_wheat_base_5.rds")
wheat6 <- readRDS("models/modtwenty_wheat.rds")
wheat7 <- readRDS("models/modfive_wheat.rds")
wheat8 <- readRDS("models/modten_wheat.rds")
wheat9 <- readRDS("models/modthirty_wheat.rds")
wheat10 <- readRDS("models/modsixty_wheat.rds")

# mod1 <- readRDS("models/modfive5.rds")
# mod2 <- readRDS("models/modten5.rds")
# mod3 <- readRDS("models/modtwenty5.rds")
# mod4 <- readRDS("models/modthirty5.rds")
# mod5 <- readRDS("models/modsixty5.rds")
# 
# mod4a <- readRDS("models/modthirty5a.rds")
# mod4b <- readRDS("models/modthirty5b.rds")
# mod4c <- readRDS("models/modthirty5c.rds")
# mod4d <- readRDS("models/modthirty5d.rds")
# mod4e <- readRDS("models/modthirty5e.rds")

# Corn
attr(corn1$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(corn2$beta, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn3$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn4$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(corn5$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn6$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn7$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn8$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn9$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn10$beta, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(corn1$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(corn2$coefficients, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn3$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn4$coefficients, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(corn5$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn6$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn7$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn8$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn9$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(corn10$coefficients, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

# Cotton
attr(cotton1$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(cotton2$beta, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton3$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton4$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(cotton5$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton6$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton7$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton8$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton9$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton10$beta, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(cotton1$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(cotton2$coefficients, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton3$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton4$coefficients, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(cotton5$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton6$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton7$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton8$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton9$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(cotton10$coefficients, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

# hay
attr(hay1$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(hay2$beta, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay3$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay4$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(hay5$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay6$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay7$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay8$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay9$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay10$beta, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(hay1$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(hay2$coefficients, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay3$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay4$coefficients, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(hay5$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay6$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay7$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay8$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay9$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(hay10$coefficients, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")


# soybean
attr(soybean1$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(soybean2$beta, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean3$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean4$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(soybean5$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean6$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean7$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean8$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean9$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean10$beta, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(soybean1$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(soybean2$coefficients, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean3$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean4$coefficients, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(soybean5$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean6$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean7$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean8$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean9$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(soybean10$coefficients, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")


# wheat
attr(wheat1$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(wheat2$beta, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat3$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat4$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(wheat5$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat6$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat7$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat8$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat9$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat10$beta, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(wheat1$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(wheat2$coefficients, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat3$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat4$coefficients, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(wheat5$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat6$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat7$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat8$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat9$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(wheat10$coefficients, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")


# Corn
star1 <- stargazer(corn1, corn2, corn3, corn4, corn5, corn6, corn7, corn8, corn9, corn10,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Corn Revenue per Acre",
                  column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                                    "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared ",
                               "Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "--", "--", "County", "County", "County x Interval", "County x Interval", "County x Interval", "County x Interval", "County x Interval"),
                           c("Quad. County-trend", "--", "--", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "--", "State", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")

# Fix Column heading
star_1 <- star1[1:12]
star1_last <- star1[13:length(star1)]
star1_headinga <- paste("\\\\[-1.8ex] &  & (30-year) & (30-year) & (30-year) & (30-year) & (30-year) & (5-year) & (10-year) & (20-year) & (60-year)\\\\") 
star1 <- c(star_1, star1_headinga, star1_last)

star_1 <- star1[1:7]
star1_last <- star1[8:length(star1)]
star1_shrink <- paste("\\resizebox{\\columnwidth}{!}{%")
star1 <- c(star_1, star1_shrink, star1_last)

star_1 <- star1[1:16]
star1_last <- star1[17:length(star1)]
# star1_climate <- paste("\\textbf{Climate-effect}\\\\")
star1 <- c(star_1, "\\textbf{Weather-effect}\\\\", "\\\\[-1.8ex]", star1_last)

star_1 <- star1[1:33]
star1_last <- star1[34:length(star1)]
# star1_climate <- paste("\\textbf{Climate-effect}\\\\")
star1 <- c(star_1, "\\hline \\\\[-1.8ex]", "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star1_last)


loc <- which(star1 == "\\end{tabular} ")
star1 <- star1[1:loc-1]
star1notes <- paste("\\parbox{10in}{Notes: Table reports regression coefficients for log corn revenue per acre using weather (year-to-year) and climate (average of interval) degree day and precipitation variables from 1950-2010. Corn revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Climate variables are averaged over intervals from 5 to 60-years. Regression estimates are weighted by total county-level corn acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star1 <- c(star1, "\\end{tabular}%", "}", star1notes)
star1 <- c(star1, "\\end{table}")




# cotton
star2 <- stargazer(cotton1, cotton2, cotton3, cotton4, cotton5, cotton6, cotton7, cotton8, cotton9, cotton10,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Cotton Revenue per Acre",
                  column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                                    "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared ",
                               "Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "--", "--", "County", "County", "County x Interval", "County x Interval", "County x Interval", "County x Interval", "County x Interval"),
                           c("Quad. County-trend", "--", "--", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "--", "State", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")

# Fix Column heading
star_1 <- star2[1:12]
star2_last <- star2[13:length(star2)]
star2_headinga <- paste("\\\\[-1.8ex] &  & (30-year) & (30-year) & (30-year) & (30-year) & (30-year) & (5-year) & (10-year) & (20-year) & (60-year)\\\\") 
star2 <- c(star_1, star2_headinga, star2_last)

star_1 <- star2[1:7]
star2_last <- star2[8:length(star2)]
star2_shrink <- paste("\\resizebox{\\columnwidth}{!}{%")
star2 <- c(star_1, star2_shrink, star2_last)

star_1 <- star2[1:16]
star2_last <- star2[17:length(star2)]
# star2_climate <- paste("\\textbf{Climate-effect}\\\\")
star2 <- c(star_1, "\\textbf{Weather-effect}\\\\", "\\\\[-1.8ex]", star2_last)

star_1 <- star2[1:33]
star2_last <- star2[34:length(star2)]
# star2_climate <- paste("\\textbf{Climate-effect}\\\\")
star2 <- c(star_1, "\\hline \\\\[-1.8ex]", "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star2_last)


loc <- which(star2 == "\\end{tabular} ")
star2 <- star2[1:loc-1]
star2notes <- paste("\\parbox{10in}{Notes: Table reports regression coefficients for log cotton revenue per acre using weather (year-to-year) and climate (average of interval) degree day and precipitation variables from 1950-2010. cotton revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Climate variables are averaged over intervals from 5 to 60-years. Regression estimates are weighted by total county-level cotton acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star2 <- c(star2, "\\end{tabular}%", "}", star2notes)
star2 <- c(star2, "\\end{table}")
#star2




# hay
star3 <- stargazer(hay1, hay2, hay3, hay4, hay5, hay6, hay7, hay8, hay9, hay10,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Hay Revenue per Acre",
                  column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                                    "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared ",
                               "Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "--", "--", "County", "County", "County x Interval", "County x Interval", "County x Interval", "County x Interval", "County x Interval"),
                           c("Quad. County-trend", "--", "--", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "--", "State", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")

# Fix Column heading
star_1 <- star3[1:12]
star3_last <- star3[13:length(star3)]
star3_headinga <- paste("\\\\[-1.8ex] &  & (30-year) & (30-year) & (30-year) & (30-year) & (30-year) & (5-year) & (10-year) & (20-year) & (60-year)\\\\") 
star3 <- c(star_1, star3_headinga, star3_last)

star_1 <- star3[1:7]
star3_last <- star3[8:length(star3)]
star3_shrink <- paste("\\resizebox{\\columnwidth}{!}{%")
star3 <- c(star_1, star3_shrink, star3_last)

star_1 <- star3[1:16]
star3_last <- star3[17:length(star3)]
# star3_climate <- paste("\\textbf{Climate-effect}\\\\")
star3 <- c(star_1, "\\textbf{Weather-effect}\\\\", "\\\\[-1.8ex]", star3_last)

star_1 <- star3[1:33]
star3_last <- star3[34:length(star3)]
# star3_climate <- paste("\\textbf{Climate-effect}\\\\")
star3 <- c(star_1, "\\hline \\\\[-1.8ex]", "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star3_last)


loc <- which(star3 == "\\end{tabular} ")
star3 <- star3[1:loc-1]
star3notes <- paste("\\parbox{10in}{Notes: Table reports regression coefficients for log hay revenue per acre using weather (year-to-year) and climate (average of interval) degree day and precipitation variables from 1950-2010. hay revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Climate variables are averaged over intervals from 5 to 60-years. Regression estimates are weighted by total county-level hay acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star3 <- c(star3, "\\end{tabular}%", "}", star3notes)
star3 <- c(star3, "\\end{table}")
#star3

# soybean
star4 <- stargazer(soybean1, soybean2, soybean3, soybean4, soybean5, soybean6, soybean7, soybean8, soybean9, soybean10,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Soybean Revenue per Acre",
                  column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                                    "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared ",
                               "Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "--", "--", "County", "County", "County x Interval", "County x Interval", "County x Interval", "County x Interval", "County x Interval"),
                           c("Quad. County-trend", "--", "--", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "--", "State", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")

# Fix Column heading
star_1 <- star4[1:12]
star4_last <- star4[13:length(star4)]
star4_headinga <- paste("\\\\[-1.8ex] &  & (30-year) & (30-year) & (30-year) & (30-year) & (30-year) & (5-year) & (10-year) & (20-year) & (60-year)\\\\") 
star4 <- c(star_1, star4_headinga, star4_last)

star_1 <- star4[1:7]
star4_last <- star4[8:length(star4)]
star4_shrink <- paste("\\resizebox{\\columnwidth}{!}{%")
star4 <- c(star_1, star4_shrink, star4_last)

star_1 <- star4[1:16]
star4_last <- star4[17:length(star4)]
# star4_climate <- paste("\\textbf{Climate-effect}\\\\")
star4 <- c(star_1, "\\textbf{Weather-effect}\\\\", "\\\\[-1.8ex]", star4_last)

star_1 <- star4[1:33]
star4_last <- star4[34:length(star4)]
# star4_climate <- paste("\\textbf{Climate-effect}\\\\")
star4 <- c(star_1, "\\hline \\\\[-1.8ex]", "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star4_last)


loc <- which(star4 == "\\end{tabular} ")
star4 <- star4[1:loc-1]
star4notes <- paste("\\parbox{10in}{Notes: Table reports regression coefficients for log soybean revenue per acre using weather (year-to-year) and climate (average of interval) degree day and precipitation variables from 1950-2010. soybean revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Climate variables are averaged over intervals from 5 to 60-years. Regression estimates are weighted by total county-level soybean acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star4 <- c(star4, "\\end{tabular}%", "}", star4notes)
star4 <- c(star4, "\\end{table}")
#star4



# wheat
star5 <- stargazer(wheat1, wheat2, wheat3, wheat4, wheat5, wheat6, wheat7, wheat8, wheat9, wheat10,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Wheat Revenue per Acre",
                  column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                                    "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared ",
                               "Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "--", "--", "County", "County", "County x Interval", "County x Interval", "County x Interval", "County x Interval", "County x Interval"),
                           c("Quad. County-trend", "--", "--", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "--", "State", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")

# Fix Column heading
star_1 <- star5[1:12]
star5_last <- star5[13:length(star5)]
star5_headinga <- paste("\\\\[-1.8ex] &  & (30-year) & (30-year) & (30-year) & (30-year) & (30-year) & (5-year) & (10-year) & (20-year) & (60-year)\\\\") 
star5 <- c(star_1, star5_headinga, star5_last)

star_1 <- star5[1:7]
star5_last <- star5[8:length(star5)]
star5_shrink <- paste("\\resizebox{\\columnwidth}{!}{%")
star5 <- c(star_1, star5_shrink, star5_last)

star_1 <- star5[1:16]
star5_last <- star5[17:length(star5)]
# star5_climate <- paste("\\textbf{Climate-effect}\\\\")
star5 <- c(star_1, "\\textbf{Weather-effect}\\\\", "\\\\[-1.8ex]", star5_last)

star_1 <- star5[1:33]
star5_last <- star5[34:length(star5)]
# star5_climate <- paste("\\textbf{Climate-effect}\\\\")
star5 <- c(star_1, "\\hline \\\\[-1.8ex]", "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star5_last)


loc <- which(star5 == "\\end{tabular} ")
star5 <- star5[1:loc-1]
star5notes <- paste("\\parbox{10in}{Notes: Table reports regression coefficients for log wheat revenue per acre using weather (year-to-year) and climate (average of interval) degree day and precipitation variables from 1950-2010. wheat revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Climate variables are averaged over intervals from 5 to 60-years. Regression estimates are weighted by total county-level wheat acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star5 <- c(star5, "\\end{tabular}%", "}", star5notes)
star5 <- c(star5, "\\end{table}")
#star5





setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "tables.tex")
cat("\\begin{landscape}", file = "tables.tex", sep = "\n", append = TRUE)
cat(star1, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star2, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star3, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star4, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star5, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\end{landscape}", file = "tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "tables.tex", append = TRUE)
# Compile pdf
system("pdflatex tables.tex")

}

system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' tables.tex > tables_out.tex")
system("pdflatex tables_out.tex")

