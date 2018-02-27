library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# dummyCreator <- function(invec, prefix = NULL) {
#      L <- length(invec)
#      ColNames <- sort(unique(invec))
#      M <- matrix(0L, ncol = length(ColNames), nrow = L,
#                  dimnames = list(NULL, ColNames))
#      M[cbind(seq_len(L), match(invec, ColNames))] <- 1L
#      if (!is.null(prefix)) colnames(M) <- paste(prefix, colnames(M), sep = "_")
#      M
# } 

# Crop data

regdat <- readRDS("data/full_ag_data.rds")
rm <- readRDS("data/full_rollmean_lag_variables.rds")
rm <- select(rm, year, fips, dday0_10_rm10, dday10_30_rm10, dday30_rm10, prec_rm10, prec_sq_rm10,
             dday0_10_rm20, dday10_30_rm20, dday30_rm20, prec_rm20, prec_sq_rm20,
             dday0_10_rm30, dday10_30_rm30, dday30_rm30, prec_rm30, prec_sq_rm30)
regdat <- left_join(regdat, rm, by = c("year", "fips"))
# (regdat$year)
# regdat <- as.data.frame(regdat)
# regdat$state <- factor(regdat$state)
# regdat$fips <- factor(regdat$fips)
# 
# get_trends <- function(x){
#   trends <- ""
#   for (i in x){
#     strend <- paste0("trend1_", i)
#     trends <- paste0(trends, " + ", strend)
#   }
#   trends <- strsplit(trends, 2, length(x))
#   return(trends)
# }
# 
# x <- levels(regdat$state)
# get_trends(x)
# 

#---------------------------------------------------------------------------------------------
# Regressions with state fe and trends

# Ten year differences 1950-1980 & 1980-2010

modten_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
                state:trend + state:trend_sq | fips | 0 | state, 
            data = regdat, weights = regdat$acres, psdef = FALSE)
summary(modten_1)

saveRDS(modten_1, "models/rev_crop_modten.rds")

# Twenty year differences 1950-1980 & 1980-2010
modtwenty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm20 + dday10_30_rm20 + dday30_rm20 + prec_rm20 + prec_sq_rm20 | fips | 0 | state, 
            data = regdat, weights = regdat$acres, psdef = FALSE)
summary(modtwenty_1)

saveRDS(modtwenty_1, "models/rev_crop_modtwenty.rds")

# Thirty year differences 1950-1980 & 1980-2010

modthirty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
              dday0_10_rm30 + dday10_30_rm30 + dday30_rm30 + prec_rm30 + prec_sq_rm30 | fips | 0 | state, 
            data = regdat, weights = regdat$acres, psdef = FALSE)
summary(modthirty_1)

saveRDS(modthirty_1, "models/rev_crop_modthirty.rds")

# Build up regression

mod_base_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10,
            data = regdat, weights = regdat$w)

mod_base_2 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 | fips | 0 | 0,
            data = regdat, weights = regdat$w)

mod_base_3 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
              dday0_10_rm10 + dday10_30_rm10 + dday30_rm10 + prec_rm10 + prec_sq_rm10 +
                trend + trend_sq | fips | 0 | 0,
            data = regdat, weights = regdat$w)

saveRDS(mod_base_1, "models/rev_crop_mod_base_1.rds")
saveRDS(mod_base_2, "models/rev_crop_mod_base_2.rds")
saveRDS(mod_base_3, "models/rev_crop_mod_base_3.rds")
