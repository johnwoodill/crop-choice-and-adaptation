regdat <- readRDS("data/full_weather_data.rds")

calcAIC <- function(residuals, df_residuals){
    n <- length(residuals)
    edf <- n - df_residuals
    RSS <- sum(residuals^2)
    dev <- n * log(RSS/n)
    c(edf, dev + 2 * edf)
}


intervals <- c("ten", "fifteen", "twenty", "thirty", "fourty")

retdat <- data.frame(fips = newdata$fips,
                     dday30 = newdata$dday30)
aicdat <- data.frame()

for (j in 1950:2010){
  for (i in intervals){
  newdata <- filter(regdat, year == j )
  form <- as.formula(paste0("dday30 ~", paste0("dday30_rm_",i), "  |fips"))
  form1 <- as.formula(paste0("dday30 ~ dday30_rm_", i, " - 1"))
  
  pmod <- felm(form, data = filter(regdat, year != j))
  moddat <- model.matrix(form1, data = newdata)
  
  coef <- as.matrix(pmod$coefficients)
  fit <- as.numeric(moddat %*% coef)
  pdat <- data.frame(fips = factor(newdata$fips),
                     fit = fit)
  fips_fe = data.frame(fips = getfe(pmod)[5],
                       getfe(pmod)[1])
  rownames(fips_fe) <- NULL
  names(fips_fe) <- c("fips", "effect")
  head(fips_fe)
  class(fips_fe$fips)
  
  pdat <- left_join(pdat, fips_fe, by = "fips")
  head(pdat)
  pdat$res <- newdata$dday30 - (pdat$fit + pdat$effect)
  rownames(pdat) <- NULL
  head(pdat)
  pdat$tfit <- pdat$fit + pdat$effect 
  pdat$real_dday30 <- newdata$dday30
  retdat[, paste0(`i`)] <- pdat$tfit
  retdat[, paste0(`i`, "_res")] <- pdat$res
  df_residuals <- length(pmod$coefficients) - length(unique(regdat$fips))
  SSE <- sum((newdata$dday30 - pdat$tfit)^2  )
  SST <- sum((newdata$dday30 - mean(newdata$dday30))^2)
  
  aic <- data.frame(interval = i, 
                    aic = calcAIC(residuals = pdat$res, df_residuals = df_residuals)[2], 
                    r2 = 1 - (SSE/SST),
                    mse = sum((newdata$dday30 - pdat$tfit)^2)/nrow(pdat))
  aicdat <- rbind(aicdat, aic)
  
}}
  
outdat <- aicdat %>% 
  group_by(interval) %>% 
  summarise_all(mean)
arrange(outdat, mse)
head(retdat)
arrange(aicdat, mse)

library(stargazer)
stargazer(outdat, summary = FALSE)

# with trend:(lat + long) + trend_sq:(lat + long)
#   interval   aic    r2   mse
#   <fctr>   <dbl> <dbl> <dbl>
# 1 ten      24897 0.679   658
# 2 twenty   25057 0.666   685
# 3 fifteen  25058 0.662   692
# 4 thirty   25164 0.656   696
# 5 fourty   25147 0.655   704

# without trends
#   interval   aic    r2   mse
#   <fctr>   <dbl> <dbl> <dbl>
# 1 ten      24933 0.689   637
# 2 twenty   25103 0.668   674
# 3 fifteen  25080 0.667   678
# 4 thirty   25205 0.656   689
# 5 fourty   25226 0.649   712

################
  modmap1_dat <- select(retdat, fips, ten)
  names(modmap1_dat) <- c("region", "value")
  
  modmap1 <- county_choropleth(modmap1_dat,
                   title      = NULL)
  
  modmap1 <- modmap1 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
    theme_tufte(base_size = 10)+ 
    xlab("Difference in Degree Day 30C 1950's to 2000's \n (30-year rolling average)") + ylab(NULL) + theme(legend.position = "none",
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.ticks.y = element_blank(),
                         panel.border = element_rect(fill = NA)) 
  modmap1
  
  
  modmap2_dat <- select(retdat, fips, real_dday30)
  names(modmap2_dat) <- c("region", "value")
  
  modmap2 <- county_choropleth(modmap2_dat,
                   title      = NULL)
  
  modmap2 <- modmap2 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
    theme_tufte(base_size = 10)+ 
    xlab("Difference in Degree Day 30C 1950's to 2000's \n (30-year rolling average)") + ylab(NULL) + theme(legend.position = "none",
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.ticks.y = element_blank(),
                         panel.border = element_rect(fill = NA)) 
  modmap2


library(cowplot)
plot_grid(modmap1, modmap2, labels = c("Predicted", "Real"))


# pdat$diff <- pdat$real_dday30 - pdat$fit_fe
# plot(density(pdat$diff))

########################################
# s.d check
#####
# download.file("https://www.dropbox.com/s/dxfxlfmhirooqil/eastern_weather_data.rds?raw=1", destfile = "regdat.rds", method = "auto")

library(zoo)
library(dplyr)
library(ggplot2)
# download.file("https://www.dropbox.com/s/7illfcto1xxtixg/full_weather_data.rds?raw=1", 
              # destfile = "full_weather_data.rds", method = "auto")
download.file("https://www.dropbox.com/s/dxfxlfmhirooqil/eastern_weather_data.rds?raw=1", 
              destfile = "regdat.rds", method = "auto")
# regdat <- readRDS("full_weather_data.rds")

regdat <- readRDS("regdat.rds")
joe <- regdat
joe$yr = joe$year - (min(joe$year) - 1)
# fips.index = 1001
fips.index = unique(joe$fips)
# nfip = length(1001)
nfip = length(fips.index)

rollMean = function(vec, len){
        n = length(vec)
        n2 = n - len + 1
        for( i in 1:n2 ) {
                if (i==1) x = sum(vec[1:len])/len
                else x = c(x, sum(vec[i:(len+(i-1))])/len)
        }
        x
}

allFipsRM = function(varName, len){
  y = c()
  for( i in 1:nfip){
    z = joe[joe$fips==fips.index[i], varName]
    x = rollMean(z, len)
    lenx = length(x)
    x = cbind( rep(fips.index[i], length(x)), x, 0:(lenx-1) )
    if(i == 1) y = x
    else y = rbind(y, x)
  }
  y = data.frame(y)
  colnames(y) = c("fips",paste("rm",len,sep=""),"yr")
  y
}

outdat <- joe[, c("fips", "yr", "dday0_10", "dday10_30", "dday30", "prec")]
for (j in 1:2){
  mdat1 <- allFipsRM("dday0_10", j)
  names(mdat1)[2] <- paste0("dday0_10_rm_", j)
  mdat2 <- allFipsRM("dday10_30", j)
  names(mdat2)[2] <- paste0("dday10_30_rm_", j)
  mdat3 <- allFipsRM("dday30", j)
  names(mdat3)[2] <- paste0("dday30_rm_", j)
  mdat4 <- allFipsRM("prec", j)
  names(mdat4)[2] <- paste0("prec_rm_", j)
  outdat <- merge(outdat, mdat1, by=c("fips","yr"))
  outdat <- merge(outdat, mdat2, by=c("fips","yr"))
  outdat <- merge(outdat, mdat3, by=c("fips","yr"))
  outdat <- merge(outdat, mdat4, by=c("fips","yr"))
  print(j)
}

saveRDS(outdat, "data/outdat.rds")
outdat$yr <- outdat$yr + 1949

rm1 = allFipsRM("dday30",1)
rm2 = allFipsRM("dday30",2)
rm3 = allFipsRM("dday30",3)
rm4 = allFipsRM("dday30",4)
rm5 = allFipsRM("dday30",5)
rm6 = allFipsRM("dday30",6)
rm7 = allFipsRM("dday30",7)
rm8 = allFipsRM("dday30",8)
rm9 = allFipsRM("dday30",9)
rm10 = allFipsRM("dday30",10)
rm11 = allFipsRM("dday30",11)
rm12 = allFipsRM("dday30",12)
rm13 = allFipsRM("dday30",13)
rm14 = allFipsRM("dday30",14)
rm15 = allFipsRM("dday30",15)
rm16 = allFipsRM("dday30",16)
rm17 = allFipsRM("dday30",17)
rm18 = allFipsRM("dday30",18)
rm19 = allFipsRM("dday30",19)
rm20 = allFipsRM("dday30",20)
rm21 = allFipsRM("dday30",21)
rm22 = allFipsRM("dday30",22)
rm23 = allFipsRM("dday30",23)
rm24 = allFipsRM("dday30",24)
rm25 = allFipsRM("dday30",25)
rm26 = allFipsRM("dday30",26)
rm27 = allFipsRM("dday30",27)
rm28 = allFipsRM("dday30",28)
rm29 = allFipsRM("dday30",29)
rm30 = allFipsRM("dday30",30)
rm31 = allFipsRM("dday30",31)
rm32 = allFipsRM("dday30",32)
rm33 = allFipsRM("dday30",33)
rm34 = allFipsRM("dday30",34)
rm35 = allFipsRM("dday30",35)
rm36 = allFipsRM("dday30",36)
rm37 = allFipsRM("dday30",37)
rm38 = allFipsRM("dday30",38)
rm39 = allFipsRM("dday30",39)
rm40 = allFipsRM("dday30",40)
rm41 = allFipsRM("dday30",41)
rm42 = allFipsRM("dday30",42)
rm43 = allFipsRM("dday30",43)
rm44 = allFipsRM("dday30",44)
rm45 = allFipsRM("dday30",45)
rm46 = allFipsRM("dday30",46)
rm47 = allFipsRM("dday30",47)
rm48 = allFipsRM("dday30",48)
rm49 = allFipsRM("dday30",49)
rm50 = allFipsRM("dday30",50)

rmTestDat = merge( joe[, c("fips","dday30","yr")], rm1, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm2, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm3, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm4, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm5, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm6, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm7, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm8, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm9, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm10, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm11, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm12, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm13, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm14, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm15, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm16, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm17, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm18, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm19, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm20, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm21, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm22, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm23, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm24, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm25, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm26, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm27, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm28, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm29, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm30, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm31, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm32, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm33, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm34, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm35, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm36, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm37, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm38, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm39, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm40, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm41, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm42, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm43, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm44, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm45, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm46, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm47, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm48, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm49, by=c("fips","yr") )
rmTestDat = merge( rmTestDat, rm50, by=c("fips","yr") )

saveRDS(rmTestDat, "data/rmTestDat.rds")
test <- rmTestDat
test$dday30 <- NULL
test$year <- test$yr + 1949
test$yr <- NULL
cropdat <- left_join(cropdat, test, by = c("year", "fips"))
outdat <- data.frame(rollmean = seq(1,50,1),
                     var = "dday30",
                     sd = rep(0, 50))

# regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in 1:50){
  outdat$sd[j] <- sd(rmTestDat$dday30 - rmTestDat[, j+3])
}

ggplot(outdat, aes(rollmean, sd)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  ylab("sd(dday - dday_rollmean)") +
  xlab("Rollmean") +
  theme_tufte(base_size = 8) +
  # ylim(0, 40) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 

ggsave("figures/predict_lag1.pdf", width = 6, height = 4)


tdat <- outdat
tdat$sd <- round(tdat$sd, 2)
# tdat$n <- 1:150
# tdat <- spread(tdat, key = var, value = round(sd, 5), -rollmean)
rownames(tdat) <- NULL
star1 <- stargazer(tdat, summary = FALSE, rownames = FALSE)
setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "predict_lag1.tex")
cat(star1, file = "predict_lag1.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "predict_lag1.tex", append = TRUE)
# Compile pdf
system("pdflatex predict_lag1.tex")
}

# lag check with dplyr
regdat <- readRDS("data/full_weather_data.rds")
regdat <- regdat %>% 
  group_by(fips) %>% 
  arrange(-year) %>% 
  mutate(dday0_10_lag1 = lag(dday0_10),
         dday10_30_lag1 = lag(dday10_30),
         dday30_lag1 = lag(dday30))
         
# View(regdat)
# Loop through 1 to 50 year right rolling mean
for (i in 1:50){
  
  # Create custom col labels
  lab1 <- paste0("dday0_10_rm_", i)
  lab2 <- paste0("dday10_30_rm_", i)
  lab3 <- paste0("dday30_rm_", i)
  
  # Loop through each fips and calculate rollingmean
  regdat <- regdat %>%
      group_by(fips) %>%
      arrange(year) %>%
      mutate(!!lab1 := roll_mean(dday0_10_lag1, i, align = "left", fill = "NA"),
             !!lab2 := roll_mean(dday10_30_lag1, i, align = "left", fill = "NA"),
             !!lab3 := roll_mean(dday30_lag1, i, align = "left", fill = "NA")) %>% 
    ungroup()
  
  # Progress bar for loop
  print(i)
}
head(regdat$dday0_10_rm_1)
# Create data.frame for calculating sd
outdat <- data.frame(rollmean = rep(seq(1,50, 1), each = 3),
                     var = rep(c("dday0_10", "dday10_30", "dday30"), 50),
                     sd = rep(0, 150))
regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in seq(19, 168, 3)){
  outdat[j - 18, 3] <- sd((regdat$dday0_10 - regdat[, j]), na.rm = TRUE)
  outdat[j + 1 - 18, 3] <- sd((regdat$dday10_30 - regdat[, j + 1]), na.rm = TRUE)
  outdat[j + 2 - 18, 3] <- sd((regdat$dday30 - regdat[, j + 2]), na.rm = TRUE)  
  
}
head(outdat)
# Find 5 lowest sd after 10 year lag
sorder <- outdat %>% 
  filter(rollmean >= 10) %>% 
  group_by(var) %>% 
  arrange(sd) %>% 
  slice(1:5) 
sorder$order <- rep(seq(1, 5, 1), 3)
  
# Plot results
ggplot(outdat, aes(rollmean, sd, color = factor(var))) + 
  geom_point(size = .8) +
  geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  ylab("sd(dday - dday_rollmean)") +
  xlab("Rollmean") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey")

+ggsave("figures/predict_lag1.pdf", width = 6, height = 4)




tdat <- outdat
tdat$sd <- round(tdat$sd, 2)
# tdat$n <- 1:150
tdat <- spread(tdat, key = var, value = round(sd, 5), -rollmean)
rownames(tdat) <- NULL
star1 <- stargazer(tdat, summary = FALSE, rownames = FALSE)
setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "predict_lag1.tex")
cat(star1, file = "predict_lag1.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "predict_lag1.tex", append = TRUE)
# Compile pdf
system("pdflatex predict_lag1.tex")

}


# Best predictor
data(zip_codes)
zip_codes <- select(zip_codes, fips, latitude, longitude)
zip_codes <- zip_codes[!duplicated(zip_codes[,1:3]),]
names(zip_codes) <- c("fips", "lat", "long")
zip_codes <- zip_codes %>%
  group_by(fips) %>%
  summarise(lat = mean(lat, na.rm = TRUE),
            long = mean(long, na.rm = TRUE))

# newregdat <- left_join(regdat, zip_codes, by="fips")
newregdat <- regdat
newregdat$trend <- newregdat$year - (min(newregdat$year) - 1)
newregdat$trend_sq <- newregdat$trend^2
retdat <- data.frame(fips = newregdat$fips,
                     dday30 = newregdat$dday30)
aicdat <- data.frame()

for (k in 1900:2010){
  for (l in 1:50){
  newdata <- filter(newregdat, year == k )
  form <- as.formula(paste0("dday30 ~", paste0("dday30_rm_",l), " + trend:lat + trend_sq:lat + trend:long + trend_sq:long  |fips"))
  form1 <- as.formula(paste0("dday30 ~ dday30_rm_", l, " - 1 + trend:lat + trend_sq:lat + trend:long + trend_sq:long"))
  
  pmod <- felm(form, data = filter(newregdat, year != k))
  moddat <- model.matrix(form1, data = newdata)
  
  coef <- as.matrix(pmod$coefficients)
  fit <- as.numeric(moddat %*% coef)
  pdat <- data.frame(fips = factor(newdata$fips),
                     fit = fit)
  fips_fe = data.frame(fips = getfe(pmod)[5],
                       getfe(pmod)[1])
  rownames(fips_fe) <- NULL
  names(fips_fe) <- c("fips", "effect")
  head(fips_fe)
  class(fips_fe$fips)
  
  pdat <- left_join(pdat, fips_fe, by = "fips")
  head(pdat)
  pdat$res <- newdata$dday30 - (pdat$fit + pdat$effect)
  rownames(pdat) <- NULL
  head(pdat)
  pdat$tfit <- pdat$fit + pdat$effect 
  pdat$real_dday30 <- newdata$dday30
  retdat[, paste0(`l`)] <- pdat$tfit
  retdat[, paste0(`l`, "_res")] <- pdat$res
  df_residuals <- length(pmod$coefficients) - length(unique(regdat$fips))
  SSE <- sum((newdata$dday30 - pdat$tfit)^2  )
  SST <- sum((newdata$dday30 - mean(newdata$dday30))^2)
  
  aic <- data.frame(rollmean = l, 
                    aic = calcAIC(residuals = pdat$res, df_residuals = df_residuals)[2], 
                    r2 = 1 - (SSE/SST),
                    mse = sum((newdata$dday30 - pdat$tfit)^2)/nrow(pdat))
  aicdat <- rbind(aicdat, aic)
  print(k)
}}
  
outdat <- aicdat %>% 
  group_by(interval) %>% 
  summarise_all(mean)
arrange(outdat, mse)
head(retdat)
arrange(aicdat, mse)



