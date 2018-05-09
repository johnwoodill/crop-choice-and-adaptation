library(cowplot)
library(tidyverse)
library(ggthemes)
library(latex2exp)

cropdat <- readRDS("data/full_ag_data.rds")

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
  annotate("text", x = 50, y = -650, label = "Corn") +
  annotate("text", x = 70, y = -1800, label = "Cotton") +
  
  # Derivative lines
  
  # Point C derivative
  # annotate("segment", x=23, xend=35, y=-2380, yend=-300, color = "red") +
  # annotate("segment", x=23, xend=11, y=-2380, yend=-4460, color = "red") +
  
  # PointB past derivative
  # annotate("segment", x=20, xend=, y=-1300, yend=-300, color = "red") +
  # annotate("segment", x=23, xend=11, y=-2380, yend=-4460, color = "red") +
  
  # Delta
  annotate("text", x = 26, y = -7000, label=TeX("$\\mathbf{\\Delta t}", output = "character"), parse=TRUE, color = 'blue') +
  annotate("text", x = 37, y = -7000, label=TeX("$\\mathbf{\\Delta t}", output = "character"), parse=TRUE, color = 'blue') +
  annotate("text", x = 82, y = -7000, label=TeX("$\\mathbf{\\Delta t}", output = "character"), parse=TRUE, color = 'blue') +
  # annotate("text", x = 23, y = -700, label="df/dt", color = 'red', size = 4) +
  annotate("text", x = 100, y = -7000, label="f(x*(t), t)", size = 4) +
  
  # Arrows
  annotate("segment", x = 24, xend = 28, y = -7500, yend = -7500, colour = "blue", size=1, arrow=arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("segment", x = 35, xend = 39, y = -7500, yend = -7500, colour = "blue", size=1, arrow=arrow(length = unit(0.1, "cm"), type = "closed")) +
  annotate("segment", x = 80, xend = 85, y = -7500, yend = -7500, colour = "blue", size=1, arrow=arrow(length = unit(0.1, "cm"), type = "closed")) +
  
  # Wheat
  # geom_segment(aes(x = 16, xend = 16, y = -1100, yend = -7500), linetype = 'dashed', size = 0.25) +
  # geom_point(aes(x = 16, y = -1100)) +  
  
  # Corn
  # geom_segment(aes(x = 23, xend = 23, y = -2350, yend = -7500), linetype = 'dashed', size = 0.25) +
  geom_point(aes(x = 23, y = -2380)) +
  geom_point(aes(x = 41, y =-800)) +
  
  # Cotton
  # geom_segment(aes(x = 30, xend = 30, y = -1380, yend = -7500), linetype = 'dashed', size = 0.25) +
  geom_point(aes(x = 30, y = -1380)) +
  
  # geom_point(aes(x = 30, y = -4200)) +
  
  # annotate("text", x = 16, y = -700, label = "A") +
  annotate("text", x = 30, y = -1000, label = "A'") +
  annotate("text", x = 23, y = -1980, label = "A") +
  # annotate("text", x = 28, y = -4200, label = "A'") +
  
  annotate("text", x = 33, y = -700, label = "B") +
  geom_point(aes(x = 33, y = -1100)) +
  
  annotate("text", x = 41, y = -400, label = "B'") +
  
  annotate("text", x = 79, y = -3000, label = "C") +
  geom_point(aes(x = 79, y = -3600)) +
  
  annotate("text", x = 86, y = -4500, label = "C'") +
  geom_point(aes(x = 86, y = -5100)) +
  # Value of adapatation label
  # geom_segment(aes(x = 35, xend = 35, y = -1400, yend = -2760), size = 0.25) +
  # geom_segment(aes(x = 35, xend = 34, y = -1400, yend = -1400), size = 0.25) +
  # geom_segment(aes(x = 35, xend = 36, y = -2080, yend = -2080), size = 0.25) +
  # geom_segment(aes(x = 35, xend = 34, y = -4100, yend = -4100), size = 0.25) +
  # geom_segment(aes(x = 35, xend = 34, y = -2750, yend = -2750), size = 0.25) +
  # annotate("text", x = 42, y = -2000, label = "Value of \n Adaptation", size = 3) +
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

dens_plot <- ggplot(cropdat, aes(tavg)) + 
  geom_density(fill = "grey95") + 
  # xlim(13, 23) + 
  ylim(0, 0.15) +
  theme_tufte(base_size = 12) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  xlab("Climate (t)") +
  ylab("Density") +
      theme(legend.position = "none",
        # axis.title.x=element_blank(),
        # axis.title.y=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=8)) + 
  # geom_segment(aes(x = 15.2, xend = 15.2, y = 0, yend = .15), linetype = 'dashed', size = .25) +
  annotate("text", x = 22.5, y = .10, label=TeX("$\\mathbf{\\phi}", output = "character"), parse=TRUE, size = 4) +
  annotate("text", x = 22.7, y = .10, label=c("(t)"), parse=TRUE, size = 3) +
  scale_x_continuous(breaks = c(15.23, 16.2, 20.6), labels = c(expression(t[1]), expression(t[2]), expression(t[3])), limits = c(13, 23)) 
  
dens_plot

ggdraw() + draw_plot(p1, 0.005, width = 0.98) +
  draw_plot(dens_plot, -0.010, height = .30, width = 1) 


ggsave("figures/mendelsohn_plot.pdf", width = 6, height = 4)
ggsave("figures/mendelsohn_plot.jpg", width = 6, height = 4)

  
