library(cowplot)
library(tidyverse)
library(ggthemes)

x <- -40:40
x

wheat <- data.frame(y = -5*x^2 , temp = 1:81)
wheat <- wheat[40:81, ]
wheat$temp <- 0:41

x <- -40:40
corn <- data.frame(y = -5*x^2 , temp = 1:81)
corn$y <- corn$y - 800

x <- -40:60
cotton <- data.frame(y = -5*(x-20)^2 , temp = 1:101)
cotton$y <- cotton$y - 2000

wheat_outline <- filter(wheat, temp < 24)
corn_outline <- filter(corn, temp > 22 & temp < 58)
cotton_outline <- filter(cotton, temp > 56)

p1 <- ggplot(NULL) + geom_line(data = wheat, aes(temp, y), color = 'grey') +
  geom_line(data = corn, aes(temp, y), color = 'grey')  +
  geom_line(data = cotton, aes(temp, y), color = 'grey') +
  geom_line(data = wheat_outline, aes(temp, y)) +
  geom_line(data = corn_outline, aes(temp, y)) +
  geom_line(data = cotton_outline, aes(temp, y)) +
  ylim(-7500, 1000) +
  xlim(0, 105) + 
  theme_tufte(base_size = 12) +
  ylab("Value of Activity") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  annotate("text", x = 8, y = 200, label = "Wheat") +
  annotate("text", x = 40, y = -450, label = "Corn") +
  annotate("text", x = 70, y = -1800, label = "Cotton") +
  
  # Wheat
  geom_segment(aes(x = 16, xend = 16, y = -1100, yend = -7500), linetype = 'dashed', size = 0.25) +
  geom_point(aes(x = 16, y = -1100)) +  
  
  # Corn
  geom_segment(aes(x = 23, xend = 23, y = -2350, yend = -7500), linetype = 'dashed', size = 0.25) +
  geom_point(aes(x = 23, y = -2380)) +  
  
  # Cotton
  geom_segment(aes(x = 30, xend = 30, y = -1380, yend = -7500), linetype = 'dashed', size = 0.25) +
  geom_point(aes(x = 30, y = -1380)) +
  
  geom_point(aes(x = 30, y = -4200)) +
  
  annotate("text", x = 16, y = -700, label = "A") +
  annotate("text", x = 30, y = -1000, label = "B") +
  annotate("text", x = 23, y = -1980, label = "C") +
  annotate("text", x = 32, y = -4200, label = "D") +
  
  # Value of adapatation label
  geom_segment(aes(x = 35, xend = 35, y = -1400, yend = -4100), size = 0.25) +
  geom_segment(aes(x = 35, xend = 34, y = -1400, yend = -1400), size = 0.25) +
  geom_segment(aes(x = 35, xend = 34, y = -4100, yend = -4100), size = 0.25) +
  geom_segment(aes(x = 35, xend = 36, y = -2750, yend = -2750), size = 0.25) +
  annotate("text", x = 42, y = -2800, label = "Value of \n Adaptation", size = 3) +
  
  
  
  # geom_text(data = filter(pdat, temp == 3 & effect == "Weather-climate-effect" & panel == 1), aes(label = effect), 
  #           vjust = -3, size = 2) + 
  # geom_text(data = filter(pdat, temp == 3 & effect == "Weather-effect" & panel == 1), aes(label = effect), 
  #           vjust = 3, size = 2) + 
  # geom_text(data = filter(pdat, temp == 3 & effect == "Weather-climate-effect" & panel == 2), aes(label = effect), 
  #           vjust = -1.5, size = 2, hjust = .3) + 
  # geom_text(data = filter(pdat, temp == 3 & effect == "Weather-effect" & panel == 2), aes(label = effect), 
  #           vjust = 3, size = 2) + 
  # scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  # ylim(-60, 60) +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 1,
  #                              override.aes = list(linetype = c(1, 1),
  #                                                  size = 1.5,
  #                                                  shape = c(NA, NA)))) +
    theme(legend.position = "none",
          # axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=8),
        plot.margin = unit(c(0, 0, 3, 0), "cm"))
p1  

dens <- density(cropdat$tavg)
dens <- data.frame(x = dens$x, y = dens$y)
dens <- filter(dens, x > 13 & x < 23)
ggplot(dens, aes(x, y)) + geom_line() + ylim(.010, 0.3)

dens_plot <- ggplot(cropdat, aes(tavg)) + geom_density(fill = "grey95") + xlim(13, 23) + ylim(0, 0.15) +
  theme_tufte(base_size = 12) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
    xlab("Temperature") +
  ylab("Density") +
      theme(legend.position = "none",
        # axis.title.x=element_blank(),
        # axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=8)) + 
  geom_segment(aes(x = 15.2, xend = 15.2, y = 0, yend = .15), linetype = 'dashed', size = .25) 
dens_plot

ggdraw() + draw_plot(p1, 0.005, width = 0.98) +
  draw_plot(dens_plot, -0.010, height = .30, width = 1) 

ggsave("figures/mendelsohn_plot.pdf", width = 6, height = 4)

  
