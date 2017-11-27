library(tidyverse)
library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

# Load regression data
csdat <- readRDS("data/cross_section_regression_data.rds")
lddat <- readRDS("data/long_difference_regression_data.rds")
pdat <- readRDS("data/panel_regression_data.rds")

# Cross-section estimates
csmod0 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | 0 | 0 | 0, 
            data = csdat, weights = csdat$w)
summary(csmod0)

csmod1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | state | 0 | state, 
            data = csdat, weights = csdat$w)
summary(csmod1)

# Long-difference estimates
ldmod0 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | 0 | 0 | 0, 
            data = lddat, weights = lddat$w)
summary(ldmod0)

ldmod1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | state + decade| 0 | state, 
            data = lddat, weights = lddat$w)
summary(ldmod1)

# Panel estimates
pmod0 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | 0 | 0 | 0, 
            data = pdat, weights = pdat$w)
summary(pmod0)

pmod1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq | fips + year | 0 | state, 
            data = pdat, weights = pdat$w)

summary(pmod1)


# Save models
saveRDS(csmod0, "models/cs_rev.rds")
saveRDS(csmod1, "models/cs_rev_fe.rds")

saveRDS(ldmod0, "models/ld_rev.rds")
saveRDS(ldmod1, "models/ld_rev_fe.rds")

saveRDS(pmod0, "models/p_rev.rds")
saveRDS(pmod1, "models/p_rev_fe.rds")

