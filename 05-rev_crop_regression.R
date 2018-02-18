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
regdat$state <- factor(regdat$state)

get_trends <- function(x){
  trends <- ""
  for (i in x){
    strend <- paste0("trend1_", i)
    trends <- paste0(trends, " + ", strend)
  }
  trends <- strsplit(trends, 2, length(x))
  return(trends)
}

x <- levels(regdat$state)
get_trends(x)

regdat <- regdat %>% 
  group_by(fips) %>% 
  arrange(-year) %>% 
  mutate(dday0_10_rm_thirty = lag(dday0_10_rm_thirty),
         dday10_30_rm_thirty = lag(dday10_30_rm_thirty),
         dday30_rm_thirty = lag(dday30_rm_thirty),
         prec_rm_thirty = lag(prec_rm_thirty),
         prec_sq_rm_thirty = lag(prec_sq_rm_thirty))

regdat <- filter(regdat, year >= 1950 & year <= 2009)

# trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + 
#   trend1_ky + trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + 
#   trend1_nc + trend1_nd + trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + 
#   trend1_tn + trend1_tx + trend1_va + trend1_wi + trend1_wv

#---------------------------------------------------------------------------------------------
# Regressions with state fe and trends

# Ten year differences 1950-1980 & 1980-2010

modten_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm_ten + dday10_30_rm_ten + dday30_rm_ten + prec_rm_ten + prec_sq_rm_ten +
                factor(ers_region):trend + factor(ers_region):trend_sq
            | state | 0 | state, 
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modten_1)

saveRDS(modten_1, "models/rev_crop_modten.rds")

# Twenty year differences 1950-1980 & 1980-2010
modtwenty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm_twenty + dday10_30_rm_twenty + dday30_rm_twenty + prec_rm_twenty + prec_sq_rm_twenty +
  trend1_al + trend1_ar + trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + trend1_ky + 
  trend1_md + trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + trend1_nc + trend1_nd + 
  trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_tx + trend1_va + 
  trend1_wi + trend1_wv +
  trend2_al + trend2_ar + trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + 
  trend2_md + trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
  trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + trend2_va + 
  trend2_wi + trend2_wv
            | state  | 0 | state, 
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modtwenty_1)

saveRDS(modtwenty_1, "models/rev_crop_modtwenty.rds")

# Thirty year differences 1950-1980 & 1980-2010

modthirty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm_thirty + dday10_30_rm_thirty + dday30_rm_thirty + prec_rm_thirty + prec_sq_rm_thirty
+ trend:(lat+long) | fips | 0 | state, 
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modthirty_1)

t(modthirty_1$coefficients)

sum(modthirty_1$coefficients[1:10])

saveRDS(modthirty_1, "models/rev_crop_modthirty.rds")

# Build up regression

mod_base_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten,
            data = regdat, weights = regdat$w)

mod_base_2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten | fips | 0 | 0,
            data = regdat, weights = regdat$w)

mod_base_3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten | fips + ten | 0 | 0,
            data = regdat, weights = regdat$w)

saveRDS(mod_base_1, "models/rev_crop_mod_base_1.rds")
saveRDS(mod_base_2, "models/rev_crop_mod_base_2.rds")
saveRDS(mod_base_3, "models/rev_crop_mod_base_3.rds")
