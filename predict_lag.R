library(tidyverse)
library(RcppRoll)
library(lfe)

# Load data
regdat <- readRDS("data/full_weather_data.rds")
regdat <- regdat %>% 
  group_by(fips) %>% 
  arrange(-year) %>% 
  mutate(dday0_10_lag1 = lag(dday0_10, 1),
         dday10_30_lag1 = lag(dday10_30, 1),
         dday30_lag1 = lag(dday30, 1))

# Trend variables
regdat$trend <- regdat$year - (min(regdat$year) - 1)
regdat$trend_sq <- regdat$trend^2
regdat$trend_lat <- regdat$trend*regdat$lat
regdat$trend_long <- regdat$trend*regdat$long
regdat$trend_sq_long <- regdat$trend_sq*regdat$long
regdat$trend_sq_lat <- regdat$trend_sq*regdat$lat

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

outdat <- data.frame(window = c(1:50), 
                        rmse = 0)
for (i in 1:50){
  form <- as.formula(paste0('dday30 ~ dday30_rm_', i))
  mod <- lm(form, data = regdat)
  outdat$rmse[i] = sqrt(mean(mod$residuals^2))
  print(i)
}

plot(outdat$window, outdat$rmse)
saveRDS(outdat, "doc/outdat_left.rds")


# Create data.frame for calculating sd
outdat <- data.frame(rollmean = rep(seq(1,50, 1), each = 3),
                     var = rep(c("Degree Day (0-10C)", "Degree Day (10-30C)", "Degree Day (30C)"), 50),
                     sd = rep(0, 150))
regdat <- as.data.frame(regdat)

# Loop through each degree day var and sd differences
for (j in seq(20, 169, 3)){
  outdat[j - 19, 3] <- sd((regdat$dday0_10 - regdat[, j]), na.rm = TRUE)
  outdat[j + 1 - 19, 3] <- sd((regdat$dday10_30 - regdat[, j + 1]), na.rm = TRUE)
  outdat[j + 2 - 19, 3] <- sd((regdat$dday30 - regdat[, j + 2]), na.rm = TRUE)  
  
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
ggplot(outdat, aes(rollmean, sd, shape = factor(var))) + 
  geom_point(size = .8) +
  # geom_text(data = sorder, aes(x = rollmean, y = sd, label = order), vjust = -1.5, show.legend = FALSE) +
  # geom_point(data = sorder, aes(x = rollmean, y = sd), shape = 1, size = 5) +
  annotate("text", x = 40, y = 135, label = "Degree Day (10-30C)", size = 3) +
  annotate("text", x = 40, y = 55, label = "Degree Day (30C)", size = 3) +
  annotate("text", x = 40, y = 35, label = "Degree Day (0-10C)", size = 3) +
  theme_tufte(base_size = 12) +
  ylab("MSE") +
  xlab("Rollmean Window Size") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "none",
    legend.box.background = element_rect(colour = "grey"),
    legend.title = element_blank(), legend.key = element_blank()) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey")

ggsave("figures/predict_lag.pdf", width = 6, height = 4)

# 
# 
# 
# tdat <- outdat
# tdat$sd <- round(tdat$sd, 2)
# # tdat$n <- 1:150
# tdat <- spread(tdat, key = var, value = round(sd, 5), -rollmean)
# rownames(tdat) <- NULL
# star1 <- stargazer(tdat, summary = FALSE, rownames = FALSE)
# setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
# {
# cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "predict_lag1.tex")
# cat(star1, file = "predict_lag1.tex", sep = "\n", append = TRUE)
# cat("\\end{document}", file = "predict_lag1.tex", append = TRUE)
# # Compile pdf
# system("pdflatex predict_lag1.tex")
# 
# }
# 
