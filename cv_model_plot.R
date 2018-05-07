library(tidyverse)
library(ggthemes)
library(cowplot)

outdat <- 
structure(list(dep_var = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 
4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 
5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 
6L, 6L), .Label = c("ln_rev", "ln_corn_mrev", "ln_cotton_mrev", 
"ln_hay_mrev", "ln_soybean_mrev", "ln_wheat_mrev"), class = "factor"), 
    climate_var = c("davg", "davg", "davg", "davg", "iv", "iv", 
    "iv", "iv", "rm", "rm", "rm", "rm", "davg", "davg", "davg", 
    "davg", "iv", "iv", "iv", "iv", "rm", "rm", "rm", "rm", "davg", 
    "davg", "davg", "davg", "iv", "iv", "iv", "iv", "rm", "rm", 
    "rm", "rm", "davg", "davg", "davg", "davg", "iv", "iv", "iv", 
    "iv", "rm", "rm", "rm", "rm", "davg", "davg", "davg", "davg", 
    "iv", "iv", "iv", "iv", "rm", "rm", "rm", "rm", "davg", "davg", 
    "davg", "davg", "iv", "iv", "iv", "iv", "rm", "rm", "rm", 
    "rm"), measure = c("agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse", "agg_climate_mse", "agg_t_fe", "agg_weather_climate_mse", 
    "agg_weather_mse"), value = c(0.363339221226832, 0.367994239059493, 
    0.356656088697654, 0.358956129993227, 0.366605539742177, 
    0.368046693923984, 0.359628597327453, 0.358925661818108, 
    0.363543514605746, 0.364378496349895, 0.353201005648017, 
    0.35405336002506, 0.255869568376581, 0.256859137710178, 0.221720926399699, 
    0.222006191256676, 0.250752596087051, 0.259539358739514, 
    0.223390317669006, 0.222948359007898, 0.257524616451471, 
    0.257262900463002, 0.221992775917025, 0.221732808216579, 
    0.32858045110331, 0.330478371383399, 0.323527329421675, 0.323387954243397, 
    0.330184746592057, 0.327042098472024, 0.322948333871163, 
    0.319314890778529, 0.317831269806592, 0.327963744658079, 
    0.311008563461413, 0.319951490851594, 0.189488302954209, 
    0.190563594426519, 0.175650161004777, 0.176413301364997, 
    0.188368017963801, 0.190023669539962, 0.176222301097269, 
    0.17600105439952, 0.188109775683171, 0.188871993448439, 0.174981002146456, 
    0.17550531988825, 0.222999684389702, 0.224966067893078, 0.19432951492362, 
    0.193969976699526, 0.219276865427174, 0.226927848981678, 
    0.195750970221297, 0.195031159235029, 0.225849497792274, 
    0.226081651068898, 0.194444299077374, 0.194350978184283, 
    0.211372056353849, 0.211556949857821, 0.209044790417847, 
    0.20986085834544, 0.212046983572084, 0.212324229846746, 0.211263496784344, 
    0.210790493945176, 0.212640316040391, 0.212474369116903, 
    0.210944492309237, 0.210919492713729), change = c(0, 1.28117680688136, 
    -1.83936446679553, -1.20633583646842, 0, 0.393107584468288, 
    -1.90311974544366, -2.09486139502151, 0, 0.229678624594437, 
    -2.84491636962508, -2.61045905081764, 0, 0.386747568253487, 
    -13.3461130972098, -13.2346247092839, 0, 3.50415620399482, 
    -10.9120618669671, -11.0883147425126, 0, -0.101627561696896, 
    -13.7974540158734, -13.8984026956649, 0, 0.577612050174043, 
    -1.53786436918806, -1.5802817369316, 0, -0.951784766700986, 
    -2.19162538414726, -3.29205268435924, 0, 3.18800439543062, 
    -2.14664414528189, 0.667090134426417, 0, 0.567471160776797, 
    -7.30290035516101, -6.90016290471043, 0, 0.878945159617771, 
    -6.44786572467236, -6.56532021622587, 0, 0.405198380838865, 
    -6.97931486496892, -6.70058520305202, 0, 0.881787572371347, 
    -12.8565964317596, -13.0178245631258, 0, 3.48918867460064, 
    -10.7288542090597, -11.0571200226307, 0, 0.102791141398906, 
    -13.9053657510388, -13.9466856981732, 0, 0.0874730118832238, 
    -1.10102819461892, -0.714946920835538, 0, 0.130747568294339, 
    -0.369487353482496, -0.592552464431191, 0, -0.0780411384717993, 
    -0.797508093823394, -0.809264846246451)), class = c("tbl_df", 
"tbl", "data.frame"), .Names = c("dep_var", "climate_var", "measure", 
"value", "change"), row.names = c(NA, -72L))


outdat <- filter(outdat, measure %in% c("agg_weather_mse", "agg_weather_climate_mse"))
outdat$measure <- factor(outdat$measure)
outdat$measure <- factor(outdat$measure, levels = c("agg_weather_mse", "agg_weather_climate_mse"),
                         labels = c("Weather", "Weather-Climate"))

outdat$dep_var <- factor(outdat$dep_var, 
                         levels = c("ln_corn_mrev", "ln_soybean_mrev", "ln_hay_mrev",
                                            "ln_cotton_mrev", "ln_wheat_mrev", "ln_rev"),
                         labels = c("Corn Rev.", "Soybean Rev.", "Hay Rev.", 
                                    "Cotton Rev.", "Wheat Rev.", "Agg. Rev."))

outdat$climate_var <- factor(outdat$climate_var,
                         levels = c("rm", "davg", "iv"),
                         labels = c("Rolling Mean", "Decade Average", "Inst. Variable"))

outdat2 <- filter(outdat, measure == "Weather")

p1 <- ggplot(outdat2, aes(dep_var, abs(change))) + 
  geom_bar(stat = "identity", position="dodge", width = .5) +
  theme_tufte(base_size = 10) +
  ylab("Percentage Reduction in RMSE") +
  xlab(NULL) +
  ylim(0, 15) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  # ylim(-60, 60) +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 1,
  #                              override.aes = list(linetype = c(1, 1),
  #                                                  size = 1.5,
  #                                                  shape = c(NA, NA)))) +
    theme(legend.position = c(.9,.9),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=4)) +
  facet_wrap(~measure)
p1
p2 <- ggplot(filter(outdat, measure == "Weather-Climate"), aes(dep_var, abs(change), fill = climate_var)) + 
  geom_bar(stat = "identity", position="dodge") +
  theme_tufte(base_size = 10) +
  # ylab(NULL) +
  ylab("Percentage Reduction in RMSE") +
  xlab(NULL) +
  ylim(0, 15) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  # ylim(-60, 60) +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 1,
  #                              override.aes = list(linetype = c(1, 1),
  #                                                  size = 1.5,
  #                                                  shape = c(NA, NA)))) +
    theme(legend.position = c(.85,.8),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=7)) +
  facet_wrap(~measure)
p2
plot_grid(p1, p2, ncol = 1)

ggsave("figures/cv_model_plot.pdf", width = 6, height = 6 )


############################ DO NOT USE ###################################

outdat <- readRDS("data/cv_model_outdat_thirtyyear.rds")
outdat$n <- NULL
outdat$rep <- NULL
outdat <- gather(outdat, key = measure, value = value, -dep_var, -climate_var)
outdat <- filter(outdat, climate_var != 'n')

outdat <- filter(outdat, measure %in% c("agg_weather_mse", "agg_weather_climate_mse"))
outdat$measure <- factor(outdat$measure)
outdat$measure <- factor(outdat$measure, levels = c("agg_weather_mse", "agg_weather_climate_mse"),
                         labels = c("Weather", "Weather-Climate"))

outdat$climate_var <- factor(outdat$climate_var,
                         levels = c("rm", "thirty", "iv"),
                         labels = c("Rolling Mean", "30-year Average", "Inst. Variable"))
outdat$dep_var <- factor(outdat$dep_var, 
                         levels = c("ln_corn_mrev", "ln_soybean_mrev", "ln_hay_mrev",
                                            "ln_cotton_mrev", "ln_wheat_mrev", "ln_rev"),
                         labels = c("Corn Rev.", "Soybean Rev.", "Hay Rev.", 
                                    "Cotton Rev.", "Wheat Rev.", "Agg. Rev."))

outdat <- outdat %>% 
  group_by(dep_var, climate_var, measure) %>% 
  summarise(value = mean(value)) %>% 
  group_by(dep_var, climate_var) %>% 
  mutate(change = 100*(value - first(value))/first(value)) %>% 
  ungroup()

dput(outdat2)
outdat2 <- filter(outdat, measure == "Weather")

p1 <- ggplot(outdat2, aes(dep_var, abs(change))) + 
  geom_bar(stat = "identity", position="dodge", width = .5) +
  theme_tufte(base_size = 10) +
  ylab("Percentage Reduction in RMSE") +
  xlab(NULL) +
  ylim(0, 15) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  # ylim(-60, 60) +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 1,
  #                              override.aes = list(linetype = c(1, 1),
  #                                                  size = 1.5,
  #                                                  shape = c(NA, NA)))) +
    theme(legend.position = c(.9,.9),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=4)) 
  # facet_wrap(~measure)
p1
p2 <- ggplot(filter(outdat, measure == "Weather-Climate"), aes(dep_var, abs(change), fill = climate_var)) + 
  geom_bar(stat = "identity", position="dodge") +
  theme_tufte(base_size = 10) +
  # ylab(NULL) +
  ylab("Percentage Reduction in RMSE") +
  xlab(NULL) +
  ylim(0, 15) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  # scale_x_continuous(breaks = 0:5, labels = c("+0C", "+1C", "+2C", "+3C", "+4C", "+5C")) +
  # ylim(-60, 60) +
  #guides(color = guide_legend(keywidth = 1.5, keyheight = 1,
  #                              override.aes = list(linetype = c(1, 1),
  #                                                  size = 1.5,
  #                                                  shape = c(NA, NA)))) +
    theme(legend.position = c(.85,.8),
        legend.box.background = element_rect(colour = "grey"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text=element_text(size=7)) +
  facet_wrap(~measure)
p2
plot_grid(p1, p2, ncol = 1)

ggsave("figures/cv_model_plot.pdf", width = 6, height = 6 )
plot_grid(p1, p2, ncol = 1)

ggsave("figures/cv_model_plot.pdf", width = 6, height = 6 )

