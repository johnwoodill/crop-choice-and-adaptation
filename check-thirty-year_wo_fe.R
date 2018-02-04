library(tidyverse)
library(lfe)
library(choroplethr)
library(cowplot)
library(ggthemes)

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


# 
# 
# 
# # Ten year differences 1950-1980 & 1980-2010
# 
# modten_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_ten + dday10_30_ten + dday30_ten + prec_ten + prec_sq_ten + 
#  trend1_al + trend1_ar + 
#   trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + trend1_ky + trend1_md + 
#   trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + trend1_nc + trend1_nd + 
#   trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_tx + 
#   trend1_va + trend1_wi  +
#     trend2_al + trend2_ar + 
#   trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
#   trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
#   trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
#   trend2_va + trend2_wi  
# 
#             | fips + ten| 0 | state, 
#             data = regdat, weights = regdat$w, psdef = FALSE)
# summary(modten_1)
# 
# 
# # Twenty year differences 1950-1980 & 1980-2010
# modtwenty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_twenty + dday10_30_twenty + dday30_twenty + prec_twenty + prec_sq_twenty + 
#  trend1_al + trend1_ar + 
#   trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + trend1_ky + trend1_md + 
#   trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + trend1_nc + trend1_nd + 
#   trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_tx + 
#   trend1_va + trend1_wi +
#        trend2_al + trend2_ar + 
#   trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
#   trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
#   trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
#   trend2_va + trend2_wi 
#  
#             | fips + twenty| 0 | state, 
#             data = regdat, weights = regdat$w, psdef = FALSE)
# summary(modtwenty_1)
# 
# 
# # Thirty year differences 1950-1980 & 1980-2010
# modthirty_1 <- felm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
#               dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty + 
#  trend1_al + trend1_ar + 
#   trend1_ga + trend1_ia + trend1_il + trend1_in + trend1_ks + trend1_ky + trend1_md + 
#   trend1_mi + trend1_mn + trend1_mo + trend1_ms + trend1_mt + trend1_nc + trend1_nd + 
#   trend1_ne + trend1_oh + trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_tx + 
#   trend1_va + trend1_wi  +
#        trend2_al + trend2_ar + 
#   trend2_ga + trend2_ia + trend2_il + trend2_in + trend2_ks + trend2_ky + trend2_md + 
#   trend2_mi + trend2_mn + trend2_mo + trend2_ms + trend2_mt + trend2_nc + trend2_nd + 
#   trend2_ne + trend2_oh + trend2_ok + trend2_sc + trend2_sd + trend2_tn + trend2_tx + 
#   trend2_va + trend2_wi 
# 
#  
#             | fips + thirty| 0 | state, 
#             data = regdat, weights = regdat$w, psdef = FALSE)
# summary(modthirty_1)

#---------------------------------------------------------------------------------------------------
# Climate 30-year
# Residual checks
modthirty_1 <- felm(dday0_10_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq, 
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modthirty_1)

modthirty_res1 <- data.frame(value = residuals(modthirty_1))
modthirty_res1$region <- regdat$fips

modthirty_res1 <- modthirty_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_thirty))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

thirty_map1 <- county_choropleth(modthirty_res1,
                 title      = NULL, state_zoom = states)
     
                
thirty_map1 <- thirty_map1 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
thirty_map1

# Climate
# Residual checks
modthirty_2 <- felm(dday10_30_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq, data = regdat, weights = regdat$w, psdef = FALSE)
summary(modthirty_2)

modthirty_res2<- data.frame(value = residuals(modthirty_2))
modthirty_res2$region <- regdat$fips

modthirty_res2<- modthirty_res2%>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_thirty))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

thirty_map2 <- county_choropleth(modthirty_res2,
                 title      = NULL, state_zoom = states)
     
                
thirty_map2 <- thirty_map2+ scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Climate Degree Day 10-30C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
thirty_map2

# Climate
# Residual checks
modthirty_3 <- felm(dday30_thirty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modthirty_3)

modthirty_res3 <- data.frame(value = residuals(modthirty_3))
modthirty_res3$region <- regdat$fips

modthirty_res3 <- modthirty_res3 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_thirty))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

thirty_map3 <- county_choropleth(modthirty_res3,
                 title      = NULL, state_zoom = states)
     
                
thirty_map3 <- thirty_map3 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Climate Degree Day 30C Residuals (30-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
thirty_map3

plot_grid(thirty_map1, thirty_map2, thirty_map3, ncol = 3)

#---------------------------------------------------------------------------------------
# Climate 20-year
# Residual checks
modtwenty_1 <- felm(dday0_10_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modtwenty_1)

modtwenty_res1 <- data.frame(value = residuals(modtwenty_1))
modtwenty_res1$region <- regdat$fips

modtwenty_res1 <- modtwenty_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_twenty))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

twenty_map1 <- county_choropleth(modtwenty_res1,
                 title      = NULL, state_zoom = states)
     
                
twenty_map1 <- twenty_map1 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
twenty_map1

# Climate
# Residual checks
modtwenty_2 <- felm(dday10_30_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modtwenty_2)

modtwenty_res2<- data.frame(value = residuals(modtwenty_2))
modtwenty_res2$region <- regdat$fips

modtwenty_res2<- modtwenty_res2%>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_twenty))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

twenty_map2 <- county_choropleth(modtwenty_res2,
                 title      = NULL, state_zoom = states)
     
                
twenty_map2 <- twenty_map2+ scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Climate Degree Day 10-30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
twenty_map2

# Climate
# Residual checks
modtwenty_3 <- felm(dday30_twenty ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modtwenty_3)

modtwenty_res3 <- data.frame(value = residuals(modtwenty_3))
modtwenty_res3$region <- regdat$fips

modtwenty_res3 <- modtwenty_res3 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_twenty))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

twenty_map3 <- county_choropleth(modtwenty_res3,
                 title      = NULL, state_zoom = states)
     
                
twenty_map3 <- twenty_map3 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Climate Degree Day 30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
twenty_map3

plot_grid(twenty_map1, twenty_map2, twenty_map3, ncol = 3)



# Climate 20-year
# Residual checks
modten_1 <- felm(dday0_10_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modten_1)

modten_res1 <- data.frame(value = residuals(modten_1))
modten_res1$region <- regdat$fips

modten_res1 <- modten_res1 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday0_10_ten))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

ten_map1 <- county_choropleth(modten_res1,
                 title      = NULL, state_zoom = states)
     
                
ten_map1 <- ten_map1 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Degree Day 0-10C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
ten_map1

# Climate
# Residual checks
modten_2 <- felm(dday10_30_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modten_2)

modten_res2<- data.frame(value = residuals(modten_2))
modten_res2$region <- regdat$fips

modten_res2<- modten_res2%>% 
  group_by(region) %>% 
  summarise(value = mean(dday10_30_ten))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

ten_map2 <- county_choropleth(modten_res2,
                 title      = NULL, state_zoom = states)
     
                
ten_map2 <- ten_map2+ scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Climate Degree Day 10-30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
ten_map2

# Climate
# Residual checks
modten_3 <- felm(dday30_ten ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq,
            data = regdat, weights = regdat$w, psdef = FALSE)
summary(modten_3)

modten_res3 <- data.frame(value = residuals(modten_3))
modten_res3$region <- regdat$fips

modten_res3 <- modten_res3 %>% 
  group_by(region) %>% 
  summarise(value = mean(dday30_ten))

regdat$state <- toupper(factor(regdat$state))
states <- tolower(unique(state.name[match(regdat$state, state.abb)]))
states <- states[!is.na(states)]  

ten_map3 <- county_choropleth(modten_res3,
                 title      = NULL, state_zoom = states)
     
                
ten_map3 <- ten_map3 + scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  theme_tufte(base_size = 10)+ 
  xlab("Climate Degree Day 30C Residuals (20-year)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA)) 
  
ten_map3

plot_grid(ten_map1, ten_map2, ten_map3, ncol = 3)

#-----------------------------------------------------------------------------
# Differences 30-year - 10-year
# Degree Day 0 - 10C
# #fips and states
fs <- data.frame(region = regdat$fips, state = regdat$state, ln_rev = regdat$ln_rev)

fs <- fs %>%
  group_by(region, state) %>%
  summarise(ln_rev = mean(ln_rev, na.rm = TRUE))
fs$ln_rev <- NULL
head(fs)

mdat1 <- data.frame(region = modthirty_res1$region,
                    value = modthirty_res1$value - modten_res1$value)


mdat1_map <- county_choropleth(mdat1,
                 title      = NULL, state_zoom = states)

mdat1_map <- mdat1_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  theme_tufte(base_size = 10)+
  xlab("Differences in Climate and Weather (Day 0-10C Residuals)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
mdat1_map

mdat1_state <- left_join(mdat1, fs, by = c("region"))
mdat1_state <- mdat1_state %>%
  group_by(state) %>%
  summarise(sum = sum(value))
mdat1_state
total <- data.frame(state = "Total", sum = sum(mdat1_state$sum))
mdat1_state <- rbind(mdat1_state, total)

mdat1_state_p <- ggplot(mdat1_state, aes(state, sum)) + geom_bar(stat = "identity") +
  geom_text(data = filter(mdat1_state, sum >0), aes(x=state, y=sum, label=state), vjust=-1) +
  geom_text(data = filter(mdat1_state, sum < 0), aes(x=state, y=sum, label=state), vjust=1.5) +
  ylab("Sum of differences \n (mean state 0-10C residuals)") +
  xlab(NULL) +
  theme_tufte(base_size = 10) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())
mdat1_state_p


# Degree Day 10 - 30C
# #fips and states

mdat2 <- data.frame(region = modthirty_res2$region,
                    value = modthirty_res2$value - modten_res2$value)


mdat2_map <- county_choropleth(mdat2,
                 title      = NULL, state_zoom = states)

mdat2_map <- mdat2_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  theme_tufte(base_size = 10)+
  xlab("Differences in Climate and Weather (Day 10-30C Residuals)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
mdat2_map

mdat2_state <- left_join(mdat2, fs, by = c("region"))
mdat2_state <- mdat2_state %>%
  group_by(state) %>%
  summarise(sum = sum(value))

mdat2_state
total <- data.frame(state = "Total", sum = sum(mdat2_state$sum))
mdat2_state <- rbind(mdat2_state, total)

mdat2_state_p <- ggplot(mdat2_state, aes(state, sum)) + geom_bar(stat = "identity") +
  geom_text(data = filter(mdat2_state, sum >0), aes(x=state, y=sum, label=state), vjust=-1) +
  geom_text(data = filter(mdat2_state, sum < 0), aes(x=state, y=sum, label=state), vjust=1.5) +
  ylab("Sum of differences \n (mean state DD10-30C residuals)") +
  xlab(NULL) +
  theme_tufte(base_size = 10) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())
mdat2_state_p

# Degree Day 30C

mdat3 <- data.frame(region = modthirty_res3$region,
                    value = modthirty_res3$value - modten_res3$value)


mdat3_map <- county_choropleth(mdat3,
                 title      = NULL, state_zoom = states)

mdat3_map <- mdat3_map + scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  theme_tufte(base_size = 10)+
  xlab("Differences in Climate and Weather (Day 30C Residuals)") + ylab(NULL) + theme(legend.position = "none",
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank(),
                       panel.border = element_rect(fill = NA))
mdat3_map

mdat3_state <- left_join(mdat3, fs, by = c("region"))
mdat3_state <- mdat3_state %>%
  group_by(state) %>%
  summarise(sum = sum(value))

mdat3_state
total <- data.frame(state = "Total", sum = sum(mdat3_state$sum))
mdat3_state <- rbind(mdat3_state, total)

mdat3_state_p <- ggplot(mdat3_state, aes(state, sum)) + geom_bar(stat = "identity") +
  geom_text(data = filter(mdat3_state, sum >0), aes(x=state, y=sum, label=state), vjust=-1) +
  geom_text(data = filter(mdat3_state, sum < 0), aes(x=state, y=sum, label=state), vjust=1.5) +
  ylab("Sum of differences \n (mean state DD30C residuals)") +
  xlab(NULL) +
   ylim(ymin, ymax) +
  theme_tufte(base_size = 10) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", alpha = 0.5) +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())
mdat3_state_p

ymax <- max(mdat1_state$sum, mdat2_state$sum, mdat3_state$sum) + 10
ymin <- min(mdat1_state$sum, mdat2_state$sum, mdat3_state$sum) -10

mdat1_state[nrow(mdat1_state), 2] + mdat2_state[nrow(mdat2_state), 2] + mdat3_state[nrow(mdat3_state), 2]

mdat1_state_p <- mdat1_state_p + ylim(ymin, ymax)
mdat2_state_p <- mdat2_state_p + ylim(ymin, ymax)
mdat3_state_p <- mdat3_state_p + ylim(ymin, ymax)

mdat2_state_p <- mdat2_state_p + ylab(NULL)
mdat3_state_p <- mdat3_state_p + ylab(NULL)



plot_grid(mdat1_map, mdat2_map, mdat3_map, mdat1_state_p, mdat2_state_p, mdat3_state_p, ncol = 3)
plot_grid(mdat1_map, mdat2_map, mdat3_map, mdat1_state_p, mdat2_state_p, mdat3_state_p, ncol = 3)
