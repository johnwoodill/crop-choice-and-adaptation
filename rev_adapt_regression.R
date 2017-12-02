library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

# Crop data
cropdat <- readRDS("data/full_ag_data.rds")

#---------------------------------------------------------------------------------------------
# Regressions

# Five year differences 1950-1980 & 1980-2010
modfive5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five
            | state + state_trend_five | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modfive5)


# Ten year differences 1950-1980 & 1980-2010
modten5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten
            | state + state_trend_ten| 0 | state, 
            data = regdat, weights = regdat$w)
summary(modten5)


# Twenty year differences 1950-1980 & 1980-2010
modthirty5 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            | state + state_trend_thirty| 0 | state, 
            data = regdat, weights = regdat$w)
summary(modthirty5)

# 60-year
modsixty5 <- felm(ln_rev ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty
            | state  | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modsixty5)



# Save robust model (5)

saveRDS(modfive5, "models/modfive5.rds")
saveRDS(modten5, "models/modten5.rds")
saveRDS(modtwenty5, "models/modtwenty5.rds")
saveRDS(modthirty5, "models/modthirty5.rds")
saveRDS(modsixty5, "models/modsixty5.rds")

# Build up regression
modthirty5a <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq, data = regdat, weights = regdat$w)
summary(modthirty5a)

modthirty5b <- felm(ln_rev ~ dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty,
            data = regdat, weights = regdat$w)
summary(modthirty5b)

modthirty5c <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty,
            data = regdat, weights = regdat$w)
summary(modthirty5c)

modthirty5d <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            | state  | 0 | 0, 
            data = regdat, weights = regdat$w)
summary(modthirty5d)

modthirty5e <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty
            | state + thirty | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modthirty5e)

saveRDS(modthirty5a, "models/modthirty5a.rds")
saveRDS(modthirty5b, "models/modthirty5b.rds")
saveRDS(modthirty5c, "models/modthirty5c.rds")
saveRDS(modthirty5d, "models/modthirty5d.rds")
saveRDS(modthirty5e, "models/modthirty5e.rds")