mod7_res1 <- data.frame(res = rep(0, nrow(dd_dat)),
                        region = rep(0, nrow(dd_dat)),
                        year = rep(0, nrow(dd_dat)))

mod7_res1$ln_rev <- dd_dat$ln_rev

mod7_res1$res <- as.numeric(mod7$residuals)
mod7_res1$region <- as.numeric(as.character(dd_dat$fips))
mod7_res1$year <- dd_dat$year

# mod7_res50_00 <- filter(mod7_res1, (year >= 1960 & year <= 1979) | (year >= 1980 & year <= 2010))
mod7_res50_00 <- mod7_res1
mod7_res50_00$decade <- ifelse(mod7_res1$year <= 1970, 1, 2)

# mod7_res1$corn_grain_a <- dd_dat$corn_grain_a
# mod7_res1$cotton_a <- dd_dat$cotton_a
# mod7_res1$hay_a <- dd_dat$hay_a
# mod7_res1$soybean_a <- dd_dat$soybean_a
# mod7_res1$wheat_a <- dd_dat$wheat_a
mod7_res1$p_corn_a <- dd_dat$p_corn_a
mod7_res1$p_cotton_a <- dd_dat$p_cotton_a
mod7_res1$p_hay_a <- dd_dat$p_hay_a
mod7_res1$p_soybean_a <- dd_dat$p_soybean_a
mod7_res1$p_wheat_a <- dd_dat$p_wheat_a

head(mod7_res1)

mod7_res1$ers <- dd_dat$region

test <- mod7_res1
test$group <- as.numeric(cut_number(test$res, 3))
test$group <- factor(test$group, labels = c("Coolest", "Neutral", "Warmest"))

test <- filter(test, ers %in% c("Heartland", "Southern Seaboard", "Northern Great Plains", "Mississipi Portal"))

test <- select(test, group, ers, res, p_corn_a, p_cotton_a, p_hay_a, p_soybean_a, p_wheat_a)
test <- gather(test, key = crop, value = value, -group, -ers, -res)
test$crop <- factor(test$crop, labels = c("Corn", "Cotton", "Hay", "Soybean", "Wheat"))
head(test)
# test <- test %>%
#   group_by(region, ers, group) %>%
#   summarise(ln_rev = mean(ln_rev),
#             value = mean(value),
#             corn_grain_a = mean(corn_grain_a),
#             cotton_a = mean(cotton_a),
#             hay_a = mean(hay_a),
#             soybean_a = mean(soybean_a),
#             wheat_a = mean(wheat_a),
#             p_corn_a = mean(p_corn_a),
#             p_)
# head(test)
# ggplot(test, aes(value, p_corn_a, color = factor(group))) + 
#   geom_point(alpha = 0.3) + 
#   geom_smooth(method = "lm") +
#   scale_colour_manual(values = c("blue", "orange", "red")) + 
#   theme_tufte(base_size = 8) +
#   ylab("Crop Share of Total Acres (%)") +
#   xlab(NULL) +
#   annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
#   annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
#   theme(legend.position = "top",
#         legend.title = element_blank()) + 
#   facet_wrap(~ers, scales = "free")

ggplot(test, aes(res, value, color = factor(group))) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_manual(values = c("blue", "orange", "red")) + 
  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  facet_wrap(ers~crop, scales = "free", ncol = 5)


ggplot(test, aes(res, ln_rev, color = factor(group))) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_manual(values = c("blue", "orange", "red")) + 
  theme_tufte(base_size = 8) +
  ylab("Crop Share of Total Acres (%)") +
  xlab(NULL) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "grey") +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "grey") +
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  facet_wrap(ers~crop, scales = "free", ncol = 5)
