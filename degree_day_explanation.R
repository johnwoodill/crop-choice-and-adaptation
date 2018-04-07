library(ggplot2)
library(scales)
library(ggthemes)

t = seq(0, 4*pi, 0.01)
y = 30*sin(t)
dat <- data.frame(t = t, y = y)
pi_scales <- math_format(.x * pi, format = function(x) x / pi)

gp <- ggplot(dat, aes(t,y)) + geom_line()
gp+ scale_x_continuous(labels = pi_scales, breaks = seq(-pi / 2, pi / 2,  3*pi / 2))
plot(t,y,type="l", xlab="time", ylab="Sine wave")



pi_scales <- math_format(.x * pi, format = function(x) x / pi)

time <- seq(-pi/2, 3*pi/2, by = 0.01)
signal <- 20*sin(time/2-40.05)^2  + 10
df <- data.frame(time,signal)

gg <- ggplot(data = df, aes(x = time, y = signal)) + geom_line() +  scale_x_continuous(labels = pi_scales, breaks = seq(-pi / 2, 7 * pi / 2, pi / 2)) + theme_bw() + ylab("Temperature (C)") + xlab("Time")
gg <- gg + geom_segment(aes(x = -0.5*pi, xend = 1.5*pi, y = 20, yend = 20), linetype = 1) + ylim(5,35) +
  geom_segment(aes(x = -0.5*pi, xend = 1.5*pi, y = 10, yend = 10), linetype = 9) +
  geom_segment(aes(x = -0.5*pi, xend = .5*pi, y = 30, yend = 30), linetype = 9) +
  geom_segment(aes(x = 0.5*pi, xend = 0.5*pi, y = 30, yend = 10), linetype = 9) +
  annotate("text", x = -0.1, y = 21, label = "W") +
  geom_point(aes(x=0, y = 20)) +
  geom_point(aes(x=0.5*pi, y = 30)) +
  annotate("text", x = 0, y = 31, label = "Max Temp") +
  annotate("text", x = 0, y = 11, label = "Min Temp") +
  annotate("text", x = pi+1, y = 21, label = "Threshold")

gg + theme(axis.line=element_blank(), axis.text=element_text(size=10),axis.ticks=element_blank(), plot.title = element_text(size=10, face = "plain"),panel.border = element_rect(colour = "black", size=1, fill = NA))
  

ggsave("figures/degree_day_explanatin.pdf", width=6, height=4)
