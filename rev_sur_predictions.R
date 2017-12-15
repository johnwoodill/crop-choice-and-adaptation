library(tidyverse)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

rdat <- readRDS("data/rev_predictions.rds")
sdat <- readRDS("data/sur_predictions.rds")
cropdat <- readRDS("data/full_ag_data.rds")

sdat <- sdat %>% 
  group_by(temp, type, effect) %>% 
  summarise(sum = sum(sum))

rdat
head(rdat)
head(sdat)

rdat <- select(rdat, temp, model, effect, sum)
names(rdat)[2] <- "type"

pdat <- data.frame(temp = sdat$temp,
                   type = sdat$type,
                   effect = sdat$effect,
                   a_sum = sdat$sum)



head(pdat)
pdat <- left_join(pdat, rdat, by = c("temp", "type", "effect"))
names(pdat)[5] <- "r_sum"
 
cropdat <- filter(cropdat, year >= 1980)
c_acres <- mean(cropdat$acres, na.rm = TRUE)
 
# cdat <- data.frame(temp = rep(unique(pdat$temp), 6),
#                   type = rep(unique(pdat$type), 7))
# 
# cdat

# cdat <- pdat
cdat <- as.data.frame(filter(pdat, effect == "Total-effect"))
newcdat <- as.data.frame(filter(pdat, effect == "Climate-effect"))
newcdat <- filter(newcdat, type == "60-year")
cdat <- rbind(cdat, newcdat)

cdat$effect <- "w/o Adaptation"
cdat$a_sum <- c_acres

cdat$sum <- cdat$a_sum*cdat$r_sum

cdat <- cdat %>% 
  group_by(type, effect) %>% 
  mutate(change = (sum - first(sum))/first(sum))

cdat$change <- cdat$change*100
# 
# cdat$type <- rep(unique(pdat$type), 6)
# cdat$a_sum <- c_acres
# cdat$effect <- "w/o Adaptation"
# cdat
# 
# pdat <- rbind(pdat, cdat)
# pdat





pdat$sum <- pdat$a_sum*pdat$r_sum

pdat
pdat <- pdat %>% 
  group_by(type, effect) %>% 
  mutate(change = (sum - first(sum))/first(sum))

pdat$change <- pdat$change*100
pdat
pdat <- rbind(pdat, cdat)

pdat$effect <- factor(pdat$effect, levels = c("Weather-effect", "Climate-effect", "Total-effect", "w/o Adaptation"))

ggplot(pdat, aes(temp, change, color = effect)) + 
  geom_point(size = 1, alpha = 0.5) + 
  geom_line() + 
  facet_wrap(~type, ) +
  theme_tufte(base_size = 12) +
  ylab("% Change in Total Revenue") +
  xlab("Change in Temperature (C)") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  theme(legend.position = "top", 
       #legend.justification = c("left", "top"), 
       legend.box.background = element_rect(colour = "grey"), 
       legend.title = element_blank(), legend.key = element_blank()) +
  #theme(legend.position = c(.85,1), 
  #     legend.justification = c("left", "top"), 
  #     legend.box.background = element_rect(colour = "grey"), 
  #     legend.title = element_blank(), legend.key = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")
