library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

source("R/predictFelm.R")

dummyCreator <- function(invec, prefix = NULL) {
     L <- length(invec)
     ColNames <- sort(unique(invec))
     M <- matrix(0L, ncol = length(ColNames), nrow = L,
                 dimnames = list(NULL, ColNames))
     M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
     if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
     M
} 

# Crop data
regdat <- readRDS("data/full_ag_data.rds")
regdat <- as.data.frame(regdat)

#---------------------------------------------------------------------------------------------

# Regressions with state fe and trends

# Five year differences 1950-1980 & 1980-2010
modfive_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_five + dday10_30_five + dday30_five + prec_five + prec_sq_five + 
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi
            | state + five | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modfive_1)

# Ten year differences 1950-1980 & 1980-2010

modten_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten + 
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi
            | state + ten | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modten_1)

# Twenty year differences 1950-1980 & 1980-2010
modtwenty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi
            | state + twenty | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modtwenty_1)

# Thirty year differences 1950-1980 & 1980-2010
modthirty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty + 
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi
            | state + thirty| 0 | state, 
            data = regdat, weights = regdat$w)
summary(modthirty_1)

# 60-year
modsixty_1 <- felm(ln_rev ~ dday0_10_sixty + dday10_30_sixty + dday30_sixty + prec_sixty + prec_sq_sixty 
            | state | 0 | state, 
            data = regdat, weights = regdat$w)
summary(modsixty_1)



# Save robust model (5)
saveRDS(modfive_1, "models/modfive_1.rds")
saveRDS(modten_1, "models/modten_1.rds")
saveRDS(modtwenty_1, "models/modtwenty_1.rds")
saveRDS(modthirty_1, "models/modthirty_1.rds")
saveRDS(modsixty_1, "models/modsixty_1.rds")
# 


# # Build up regression
mod_base_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq, data = regdat, weights = regdat$w)

mod_base_2 <- felm(ln_rev ~ dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty,
            data = regdat, weights = regdat$w)

mod_base_3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty,
            data = regdat, weights = regdat$w)

mod_base_4 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty +
              trend2_al +trend2_ar + trend2_de +trend2_ga + trend2_ia  +         
              trend2_il +trend2_in + trend2_ks + trend2_ky + trend2_md + trend2_mi +         
              trend2_mn+ trend2_mo + trend2_ms +  trend2_mt + trend2_nc + trend2_nd +         
              trend2_ne +trend2_oh + trend2_ok +  trend2_sc + trend2_sd + trend2_tn +         
              trend2_va + trend2_wi
            | state  | 0 | 0,
            data = regdat, weights = regdat$w)

saveRDS(mod_base_1, "models/mod_base_1.rds")
saveRDS(mod_base_2, "models/mod_base_2.rds")
saveRDS(mod_base_3, "models/mod_base_3.rds")
saveRDS(mod_base_4, "models/mod_base_4.rds")
