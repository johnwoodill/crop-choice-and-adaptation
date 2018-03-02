regdat <- readRDS("data/full_weather_data.rds")
library(tidyverse)
library(zoo)
library(RcppRoll)
# regdat <- readRDS("data/degree_day_changes/fips_degree_days_1C_1900-2013.rds")
regdat$fips <- as.numeric(as.character(regdat$fips))
class(regdat$fips)
head(regdat$fips)
joe <- regdat
joe$yr = joe$year - (min(joe$year) - 1)
# fips.index = 1001
fips.index = unique(joe$fips)
# nfip = length(1001)
nfip = length(fips.index)

rollMean = function(vec, len){
        nn = length(vec)
        n2 = nn - len + 1
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
  colnames(y) = c("fips",paste(varName, "_rm",len,sep=""),"yr")
  y
}

allFipsRM3 = function(dat, varName, len){
  do.call(rbind, lapply(split(dat, dat$fips), function(x) {
    all.rm <- as.data.frame(sapply(len, function(l) c(roll_meanr(x[,varName], l), rep(NA, l-1))))
    colnames(all.rm) <- paste0(varName, "_rm", len)
    cbind(data.frame(fips=x$fips[1]), all.rm, data.frame(year=seq_len(nrow(x))-1))
  }))
}

test <- joe %>% 
  group_by(fips) %>% 
  mutate(dday0_10_rm10 = roll_mean(dday0_10, 10, align = "left", fill = "NA")) %>% 
  filter(!is.na(dday0_10_rm10) ) %>% 
  arrange(fips) %>% 
  ungroup()

dday0_10_rm10 = allFipsRM("dday0_10",10)
all.equal(test$dday0_10_rm10, dday0_10_rm10$dday0_10_rm10)
head(test)
head(dday0_10_rm10)
head(test$dday0_10_rm10)
head(dday0_10_rm10$dday0_10_rm10)


which((test$dday0_10_rm10 - dday0_10_rm10$dday0_10_rm10) != 0)
nrow(test)

mean(test$dday0_10_rm10 - dday0_10_rm10$dday0_10_rm10)
nrow(dday0_10_rm10)

# dday0_10_rm1 = allFipsRM2(joe, "dday0_10",1)
# dday0_10_rm2 = allFipsRM2(joe, "dday0_10",2)
dday0_10_rm33 = allFipsRM3(joe, "dday30", 10)
# sd(regdat$dday0_10 - dday0_10_rm33$dday0_10_rm2, na.rm = TRUE)
head(dday0_10_rm33)
dday0_10_rm33

dday0_10_rm1 = allFipsRM("dday0_10",1)
dday0_10_rm2 = allFipsRM("dday0_10",2)
dday0_10_rm3 = allFipsRM("dday0_10",3)
dday0_10_rm4 = allFipsRM("dday0_10",4)
dday0_10_rm5 = allFipsRM("dday0_10",5)
dday0_10_rm6 = allFipsRM("dday0_10",6)
dday0_10_rm7 = allFipsRM("dday0_10",7)
dday0_10_rm8 = allFipsRM("dday0_10",8)
dday0_10_rm9 = allFipsRM("dday0_10",9)
dday0_10_rm10 = allFipsRM("dday0_10",10)
dday0_10_rm11 = allFipsRM("dday0_10",11)
dday0_10_rm12 = allFipsRM("dday0_10",12)
dday0_10_rm13 = allFipsRM("dday0_10",13)
dday0_10_rm14 = allFipsRM("dday0_10",14)
dday0_10_rm15 = allFipsRM("dday0_10",15)
dday0_10_rm16 = allFipsRM("dday0_10",16)
dday0_10_rm17 = allFipsRM("dday0_10",17)
dday0_10_rm18 = allFipsRM("dday0_10",18)
dday0_10_rm19 = allFipsRM("dday0_10",19)
dday0_10_rm20 = allFipsRM("dday0_10",20)
dday0_10_rm21 = allFipsRM("dday0_10",21)
dday0_10_rm22 = allFipsRM("dday0_10",22)
dday0_10_rm23 = allFipsRM("dday0_10",23)
dday0_10_rm24 = allFipsRM("dday0_10",24)
dday0_10_rm25 = allFipsRM("dday0_10",25)
dday0_10_rm26 = allFipsRM("dday0_10",26)
dday0_10_rm27 = allFipsRM("dday0_10",27)
dday0_10_rm28 = allFipsRM("dday0_10",28)
dday0_10_rm29 = allFipsRM("dday0_10",29)
dday0_10_rm30 = allFipsRM("dday0_10",30)
dday0_10_rm31 = allFipsRM("dday0_10",31)
dday0_10_rm32 = allFipsRM("dday0_10",32)
dday0_10_rm33 = allFipsRM("dday0_10",33)
dday0_10_rm34 = allFipsRM("dday0_10",34)
dday0_10_rm35 = allFipsRM("dday0_10",35)
dday0_10_rm36 = allFipsRM("dday0_10",36)
dday0_10_rm37 = allFipsRM("dday0_10",37)
dday0_10_rm38 = allFipsRM("dday0_10",38)
dday0_10_rm39 = allFipsRM("dday0_10",39)
dday0_10_rm40 = allFipsRM("dday0_10",40)
dday0_10_rm41 = allFipsRM("dday0_10",41)
dday0_10_rm42 = allFipsRM("dday0_10",42)
dday0_10_rm43 = allFipsRM("dday0_10",43)
dday0_10_rm44 = allFipsRM("dday0_10",44)
dday0_10_rm45 = allFipsRM("dday0_10",45)
dday0_10_rm46 = allFipsRM("dday0_10",46)
dday0_10_rm47 = allFipsRM("dday0_10",47)
dday0_10_rm48 = allFipsRM("dday0_10",48)
dday0_10_rm49 = allFipsRM("dday0_10",49)
dday0_10_rm50 = allFipsRM("dday0_10",50)

dday0_10_rm = merge( joe[, c("fips","dday0_10","yr")], dday0_10_rm1, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm2, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm3, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm4, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm5, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm6, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm7, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm8, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm9, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm10, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm11, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm12, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm13, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm14, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm15, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm16, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm17, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm18, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm19, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm20, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm21, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm22, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm23, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm24, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm25, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm26, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm27, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm28, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm29, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm30, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm31, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm32, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm33, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm34, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm35, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm36, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm37, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm38, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm39, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm40, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm41, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm42, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm43, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm44, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm45, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm46, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm47, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm48, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm49, by=c("fips","yr") )
dday0_10_rm = merge( dday0_10_rm, dday0_10_rm50, by=c("fips","yr") )
names(dday0_10_rm)[4:53] <- paste0("dday0_10_", names(dday0_10_rm)[4:53])


dday10_30_rm1 = allFipsRM("dday10_30",1)
dday10_30_rm2 = allFipsRM("dday10_30",2)
dday10_30_rm3 = allFipsRM("dday10_30",3)
dday10_30_rm4 = allFipsRM("dday10_30",4)
dday10_30_rm5 = allFipsRM("dday10_30",5)
dday10_30_rm6 = allFipsRM("dday10_30",6)
dday10_30_rm7 = allFipsRM("dday10_30",7)
dday10_30_rm8 = allFipsRM("dday10_30",8)
dday10_30_rm9 = allFipsRM("dday10_30",9)
dday10_30_rm10 = allFipsRM("dday10_30",10)
dday10_30_rm11 = allFipsRM("dday10_30",11)
dday10_30_rm12 = allFipsRM("dday10_30",12)
dday10_30_rm13 = allFipsRM("dday10_30",13)
dday10_30_rm14 = allFipsRM("dday10_30",14)
dday10_30_rm15 = allFipsRM("dday10_30",15)
dday10_30_rm16 = allFipsRM("dday10_30",16)
dday10_30_rm17 = allFipsRM("dday10_30",17)
dday10_30_rm18 = allFipsRM("dday10_30",18)
dday10_30_rm19 = allFipsRM("dday10_30",19)
dday10_30_rm20 = allFipsRM("dday10_30",20)
dday10_30_rm21 = allFipsRM("dday10_30",21)
dday10_30_rm22 = allFipsRM("dday10_30",22)
dday10_30_rm23 = allFipsRM("dday10_30",23)
dday10_30_rm24 = allFipsRM("dday10_30",24)
dday10_30_rm25 = allFipsRM("dday10_30",25)
dday10_30_rm26 = allFipsRM("dday10_30",26)
dday10_30_rm27 = allFipsRM("dday10_30",27)
dday10_30_rm28 = allFipsRM("dday10_30",28)
dday10_30_rm29 = allFipsRM("dday10_30",29)
dday10_30_rm30 = allFipsRM("dday10_30",30)
dday10_30_rm31 = allFipsRM("dday10_30",31)
dday10_30_rm32 = allFipsRM("dday10_30",32)
dday10_30_rm33 = allFipsRM("dday10_30",33)
dday10_30_rm34 = allFipsRM("dday10_30",34)
dday10_30_rm35 = allFipsRM("dday10_30",35)
dday10_30_rm36 = allFipsRM("dday10_30",36)
dday10_30_rm37 = allFipsRM("dday10_30",37)
dday10_30_rm38 = allFipsRM("dday10_30",38)
dday10_30_rm39 = allFipsRM("dday10_30",39)
dday10_30_rm40 = allFipsRM("dday10_30",40)
dday10_30_rm41 = allFipsRM("dday10_30",41)
dday10_30_rm42 = allFipsRM("dday10_30",42)
dday10_30_rm43 = allFipsRM("dday10_30",43)
dday10_30_rm44 = allFipsRM("dday10_30",44)
dday10_30_rm45 = allFipsRM("dday10_30",45)
dday10_30_rm46 = allFipsRM("dday10_30",46)
dday10_30_rm47 = allFipsRM("dday10_30",47)
dday10_30_rm48 = allFipsRM("dday10_30",48)
dday10_30_rm49 = allFipsRM("dday10_30",49)
dday10_30_rm50 = allFipsRM("dday10_30",50)

dday10_30_rm = merge( joe[, c("fips","dday10_30","yr")], dday10_30_rm1, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm2, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm3, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm4, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm5, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm6, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm7, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm8, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm9, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm10, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm11, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm12, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm13, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm14, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm15, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm16, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm17, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm18, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm19, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm20, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm21, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm22, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm23, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm24, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm25, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm26, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm27, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm28, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm29, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm30, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm31, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm32, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm33, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm34, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm35, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm36, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm37, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm38, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm39, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm40, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm41, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm42, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm43, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm44, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm45, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm46, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm47, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm48, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm49, by=c("fips","yr") )
dday10_30_rm = merge( dday10_30_rm, dday10_30_rm50, by=c("fips","yr") )
names(dday10_30_rm)[4:53] <- paste0("dday10_30_", names(dday10_30_rm)[4:53])




dday30_rm1 = allFipsRM("dday30",1)
dday30_rm2 = allFipsRM("dday30",2)
dday30_rm3 = allFipsRM("dday30",3)
dday30_rm4 = allFipsRM("dday30",4)
dday30_rm5 = allFipsRM("dday30",5)
dday30_rm6 = allFipsRM("dday30",6)
dday30_rm7 = allFipsRM("dday30",7)
dday30_rm8 = allFipsRM("dday30",8)
dday30_rm9 = allFipsRM("dday30",9)
dday30_rm10 = allFipsRM("dday30",10)
dday30_rm11 = allFipsRM("dday30",11)
dday30_rm12 = allFipsRM("dday30",12)
dday30_rm13 = allFipsRM("dday30",13)
dday30_rm14 = allFipsRM("dday30",14)
dday30_rm15 = allFipsRM("dday30",15)
dday30_rm16 = allFipsRM("dday30",16)
dday30_rm17 = allFipsRM("dday30",17)
dday30_rm18 = allFipsRM("dday30",18)
dday30_rm19 = allFipsRM("dday30",19)
dday30_rm20 = allFipsRM("dday30",20)
dday30_rm21 = allFipsRM("dday30",21)
dday30_rm22 = allFipsRM("dday30",22)
dday30_rm23 = allFipsRM("dday30",23)
dday30_rm24 = allFipsRM("dday30",24)
dday30_rm25 = allFipsRM("dday30",25)
dday30_rm26 = allFipsRM("dday30",26)
dday30_rm27 = allFipsRM("dday30",27)
dday30_rm28 = allFipsRM("dday30",28)
dday30_rm29 = allFipsRM("dday30",29)
dday30_rm30 = allFipsRM("dday30",30)
dday30_rm31 = allFipsRM("dday30",31)
dday30_rm32 = allFipsRM("dday30",32)
dday30_rm33 = allFipsRM("dday30",33)
dday30_rm34 = allFipsRM("dday30",34)
dday30_rm35 = allFipsRM("dday30",35)
dday30_rm36 = allFipsRM("dday30",36)
dday30_rm37 = allFipsRM("dday30",37)
dday30_rm38 = allFipsRM("dday30",38)
dday30_rm39 = allFipsRM("dday30",39)
dday30_rm40 = allFipsRM("dday30",40)
dday30_rm41 = allFipsRM("dday30",41)
dday30_rm42 = allFipsRM("dday30",42)
dday30_rm43 = allFipsRM("dday30",43)
dday30_rm44 = allFipsRM("dday30",44)
dday30_rm45 = allFipsRM("dday30",45)
dday30_rm46 = allFipsRM("dday30",46)
dday30_rm47 = allFipsRM("dday30",47)
dday30_rm48 = allFipsRM("dday30",48)
dday30_rm49 = allFipsRM("dday30",49)
dday30_rm50 = allFipsRM("dday30",50)

dday30_rm = merge( joe[, c("fips","dday30","yr")], dday30_rm1, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm2, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm3, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm4, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm5, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm6, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm7, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm8, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm9, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm10, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm11, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm12, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm13, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm14, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm15, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm16, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm17, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm18, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm19, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm20, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm21, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm22, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm23, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm24, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm25, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm26, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm27, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm28, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm29, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm30, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm31, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm32, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm33, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm34, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm35, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm36, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm37, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm38, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm39, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm40, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm41, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm42, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm43, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm44, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm45, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm46, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm47, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm48, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm49, by=c("fips","yr") )
dday30_rm = merge( dday30_rm, dday30_rm50, by=c("fips","yr") )
names(dday30_rm)[4:53] <- paste0("dday30_", names(dday30_rm)[4:53])



prec_rm1 = allFipsRM("prec",1)
prec_rm2 = allFipsRM("prec",2)
prec_rm3 = allFipsRM("prec",3)
prec_rm4 = allFipsRM("prec",4)
prec_rm5 = allFipsRM("prec",5)
prec_rm6 = allFipsRM("prec",6)
prec_rm7 = allFipsRM("prec",7)
prec_rm8 = allFipsRM("prec",8)
prec_rm9 = allFipsRM("prec",9)
prec_rm10 = allFipsRM("prec",10)
prec_rm11 = allFipsRM("prec",11)
prec_rm12 = allFipsRM("prec",12)
prec_rm13 = allFipsRM("prec",13)
prec_rm14 = allFipsRM("prec",14)
prec_rm15 = allFipsRM("prec",15)
prec_rm16 = allFipsRM("prec",16)
prec_rm17 = allFipsRM("prec",17)
prec_rm18 = allFipsRM("prec",18)
prec_rm19 = allFipsRM("prec",19)
prec_rm20 = allFipsRM("prec",20)
prec_rm21 = allFipsRM("prec",21)
prec_rm22 = allFipsRM("prec",22)
prec_rm23 = allFipsRM("prec",23)
prec_rm24 = allFipsRM("prec",24)
prec_rm25 = allFipsRM("prec",25)
prec_rm26 = allFipsRM("prec",26)
prec_rm27 = allFipsRM("prec",27)
prec_rm28 = allFipsRM("prec",28)
prec_rm29 = allFipsRM("prec",29)
prec_rm30 = allFipsRM("prec",30)
prec_rm31 = allFipsRM("prec",31)
prec_rm32 = allFipsRM("prec",32)
prec_rm33 = allFipsRM("prec",33)
prec_rm34 = allFipsRM("prec",34)
prec_rm35 = allFipsRM("prec",35)
prec_rm36 = allFipsRM("prec",36)
prec_rm37 = allFipsRM("prec",37)
prec_rm38 = allFipsRM("prec",38)
prec_rm39 = allFipsRM("prec",39)
prec_rm40 = allFipsRM("prec",40)
prec_rm41 = allFipsRM("prec",41)
prec_rm42 = allFipsRM("prec",42)
prec_rm43 = allFipsRM("prec",43)
prec_rm44 = allFipsRM("prec",44)
prec_rm45 = allFipsRM("prec",45)
prec_rm46 = allFipsRM("prec",46)
prec_rm47 = allFipsRM("prec",47)
prec_rm48 = allFipsRM("prec",48)
prec_rm49 = allFipsRM("prec",49)
prec_rm50 = allFipsRM("prec",50)

prec_rm = merge( joe[, c("fips","prec","yr")], prec_rm1, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm2, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm3, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm4, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm5, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm6, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm7, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm8, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm9, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm10, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm11, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm12, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm13, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm14, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm15, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm16, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm17, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm18, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm19, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm20, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm21, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm22, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm23, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm24, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm25, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm26, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm27, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm28, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm29, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm30, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm31, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm32, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm33, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm34, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm35, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm36, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm37, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm38, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm39, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm40, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm41, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm42, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm43, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm44, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm45, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm46, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm47, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm48, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm49, by=c("fips","yr") )
prec_rm = merge( prec_rm, prec_rm50, by=c("fips","yr") )
names(prec_rm)[4:53] <- paste0("prec_", names(prec_rm)[4:53])

prec_rm$prec_sq_rm1 <- prec_rm$prec_rm1^2
prec_rm$prec_sq_rm2 <- prec_rm$prec_rm2^2
prec_rm$prec_sq_rm3 <- prec_rm$prec_rm3^2
prec_rm$prec_sq_rm4 <- prec_rm$prec_rm4^2
prec_rm$prec_sq_rm5 <- prec_rm$prec_rm5^2
prec_rm$prec_sq_rm6 <- prec_rm$prec_rm6^2
prec_rm$prec_sq_rm7 <- prec_rm$prec_rm7^2
prec_rm$prec_sq_rm8 <- prec_rm$prec_rm8^2
prec_rm$prec_sq_rm9 <- prec_rm$prec_rm9^2
prec_rm$prec_sq_rm10 <- prec_rm$prec_rm10^2
prec_rm$prec_sq_rm11 <- prec_rm$prec_rm11^2
prec_rm$prec_sq_rm12 <- prec_rm$prec_rm12^2
prec_rm$prec_sq_rm13 <- prec_rm$prec_rm13^2
prec_rm$prec_sq_rm14 <- prec_rm$prec_rm14^2
prec_rm$prec_sq_rm15 <- prec_rm$prec_rm15^2
prec_rm$prec_sq_rm16 <- prec_rm$prec_rm16^2
prec_rm$prec_sq_rm17 <- prec_rm$prec_rm17^2
prec_rm$prec_sq_rm18 <- prec_rm$prec_rm18^2
prec_rm$prec_sq_rm19 <- prec_rm$prec_rm19^2
prec_rm$prec_sq_rm20 <- prec_rm$prec_rm20^2
prec_rm$prec_sq_rm21 <- prec_rm$prec_rm21^2
prec_rm$prec_sq_rm22 <- prec_rm$prec_rm22^2
prec_rm$prec_sq_rm23 <- prec_rm$prec_rm23^2
prec_rm$prec_sq_rm24 <- prec_rm$prec_rm24^2
prec_rm$prec_sq_rm25 <- prec_rm$prec_rm25^2
prec_rm$prec_sq_rm26 <- prec_rm$prec_rm26^2
prec_rm$prec_sq_rm27 <- prec_rm$prec_rm27^2
prec_rm$prec_sq_rm28 <- prec_rm$prec_rm28^2
prec_rm$prec_sq_rm29 <- prec_rm$prec_rm29^2
prec_rm$prec_sq_rm30 <- prec_rm$prec_rm30^2
prec_rm$prec_sq_rm31 <- prec_rm$prec_rm31^2
prec_rm$prec_sq_rm32 <- prec_rm$prec_rm32^2
prec_rm$prec_sq_rm33 <- prec_rm$prec_rm33^2
prec_rm$prec_sq_rm34 <- prec_rm$prec_rm34^2
prec_rm$prec_sq_rm35 <- prec_rm$prec_rm35^2
prec_rm$prec_sq_rm36 <- prec_rm$prec_rm36^2
prec_rm$prec_sq_rm37 <- prec_rm$prec_rm37^2
prec_rm$prec_sq_rm38 <- prec_rm$prec_rm38^2
prec_rm$prec_sq_rm39 <- prec_rm$prec_rm39^2
prec_rm$prec_sq_rm40 <- prec_rm$prec_rm40^2
prec_rm$prec_sq_rm41 <- prec_rm$prec_rm41^2
prec_rm$prec_sq_rm42 <- prec_rm$prec_rm42^2
prec_rm$prec_sq_rm43 <- prec_rm$prec_rm43^2
prec_rm$prec_sq_rm44 <- prec_rm$prec_rm44^2
prec_rm$prec_sq_rm45 <- prec_rm$prec_rm45^2
prec_rm$prec_sq_rm46 <- prec_rm$prec_rm46^2
prec_rm$prec_sq_rm47 <- prec_rm$prec_rm47^2
prec_rm$prec_sq_rm48 <- prec_rm$prec_rm48^2
prec_rm$prec_sq_rm49 <- prec_rm$prec_rm49^2
prec_rm$prec_sq_rm50 <- prec_rm$prec_rm50^2
prec_merge <- joe[, c("fips", "year", "prec_sq")]
prec_merge <- prec_merge[prec_merge$year >= 1950, ]
prec_merge$year <- prec_merge$year - 1949
names(prec_merge)[2] <- "yr"


library(dplyr)
library(ggplot2)
library(ggthemes)
prec_rm <- left_join(prec_rm, prec_merge, by = c("fips", "yr"))
outdat <- left_join(dday0_10_rm, dday10_30_rm, by = c("fips", "yr"))
outdat <- left_join(outdat, dday30_rm, by = c("fips", "yr"))
outdat <- left_join(outdat, prec_rm, by = c("fips", "yr"))
names(outdat)[2] <- "year"
outdat$year <- outdat$year + 1949
saveRDS(outdat, "data/full_rollmean_lag_variables.rds")
outdat <- readRDS("data/full_rollmean_lag_variables.rds")

# dday0_10
dday0_10_outdat <- data.frame(rollmean = seq(1,50,1),
                     var = "dday0_10",
                     sd = rep(0, 50))

# regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in 1:50){
  dday0_10_outdat$sd[j] <- sd(dday0_10_rm$dday0_10 - dday0_10_rm[, j+3])
}

p1 <- ggplot(dday0_10_outdat, aes(rollmean, sd)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  ylab("sd(dday - dday_rollmean)") +
  xlab("Rollmean") +
  theme_tufte(base_size = 8) +
  # ylim(0, 40) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("text", x=25, y=max(dday0_10_outdat$sd), label = "Degree Day 0-10 (C)") +
  theme(legend.position = "top",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 

ggsave("figures/rollmean_dday0_10.pdf",plot = p1, width = 6, height = 4)

# dday10_30
dday10_30_outdat <- data.frame(rollmean = seq(1,50,1),
                     var = "dday10_30",
                     sd = rep(0, 50))

# regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in 1:50){
  dday10_30_outdat$sd[j] <- sd(dday10_30_rm$dday10_30 - dday10_30_rm[, j+3])
}

p2 <- ggplot(dday10_30_outdat, aes(rollmean, sd)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  ylab("sd(dday - dday_rollmean)") +
  xlab("Rollmean") +
  theme_tufte(base_size = 8) +
  # ylim(0, 40) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
    annotate("text", x=25, y=max(dday10_30_outdat$sd), label = "Degree Day 10-30 (C)") +
  theme(legend.position = "top",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 

ggsave("figures/rollmean_dday10_30.pdf", plot = p2, width = 6, height = 4)

# dday30
dday30_outdat <- data.frame(rollmean = seq(1,50,1),
                     var = "dday30",
                     sd = rep(0, 50))

# regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in 1:50){
  dday30_outdat$sd[j] <- sd(dday30_rm$dday30 - dday30_rm[, j+3])
}

p3 <- ggplot(dday30_outdat, aes(rollmean, sd)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  ylab("sd(dday - dday_rollmean)") +
  xlab("Rollmean") +
  theme_tufte(base_size = 8) +
  # ylim(0, 40) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("text", x=25, y=max(dday30_outdat$sd), label = "Degree Day 30 (C)") +
  theme(legend.position = "top",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 

ggsave("figures/rollmean_dday30.pdf", plot = p3, width = 6, height = 4)

# prec
prec_outdat <- data.frame(rollmean = seq(1,50,1),
                     var = "prec",
                     sd = rep(0, 50))

# regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in 1:50){
  prec_outdat$sd[j] <- sd(prec_rm$prec - prec_rm[, j+3])
}

p4 <- ggplot(prec_outdat, aes(rollmean, sd)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  ylab("sd(prec - prec_rollmean)") +
  xlab("Rollmean") +
  theme_tufte(base_size = 8) +
  # ylim(0, 40) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
    annotate("text", x=25, y=max(prec_outdat$sd), label = "Precipitation") +
  theme(legend.position = "top",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 

ggsave("figures/rollmean_prec.pdf", plot = p4, width = 6, height = 4)

# prec_sq
prec_sq_outdat <- data.frame(rollmean = seq(1,50,1),
                     var = "prec_sq",
                     sd = rep(0, 50))

# regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in 1:50){
  prec_sq_outdat$sd[j] <- sd(prec_rm$prec_sq - prec_rm[, j+53])
}

ggplot(prec_sq_outdat, aes(rollmean, sd)) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  ylab("sd(dday - dday_rollmean)") +
  xlab("Rollmean") +
  theme_tufte(base_size = 8) +
  # ylim(0, 40) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +    
  annotate("text", x=25, y=max(prec_sq_outdat$sd), label = "Precipitation Sq.") +
  theme(legend.position = "top",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) 

ggsave("figures/rollmean_prec_sq.pdf", width = 6, height = 4)

library(cowplot)
plot_grid(p1, p2, p3, p4, ncol = 2)
ggsave("figures/rollmean_all_plots.pdf", width = 6, height = 4)
