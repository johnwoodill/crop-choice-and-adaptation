library(stargazer)
library(lfe)


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))}


# Save models
mod1 <- readRDS("models/rev_crop_mod_base_1.rds")
mod2 <- readRDS("models/rev_crop_mod_base_2.rds")
mod3 <- readRDS("models/rev_crop_mod_base_3.rds")
mod4 <- readRDS("models/rev_crop_modten.rds")
mod5 <- readRDS("models/rev_crop_modtwenty.rds")
mod6 <- readRDS("models/rev_crop_modthirty.rds")


attr(mod1$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod2$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod3$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod4$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod5$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod6$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")


# Coefficients
attr(mod1$coefficients, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod2$coefficients, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod3$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod4$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                      "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod5$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod6$coefficients, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")



star1 <- stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression Model explaining Crop Revenue per Acre",
                  column.labels = c("10-year", "10-year", "10-year", "10-year", "11-year", "12-year"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared",
                               "Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County", "County", "County", "County", "County"),
                           c("National Quad. Trend", "--", "--", "Yes", "--", "--", "--"),
                           c("Lat/Long Quad. Trend", "--", "--", "--", "Yes", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")



# star_1 <- star1[1:7]
# star1_last <- star1[8:length(star1)]
# star1_shrink <- paste("\\resizebox{\\columnwidth}{!}{%")
# star1 <- c(star_1, star1_shrink, star1_last)

star_1 <- star1[1:14]
star1_last <- star1[15:length(star1)]
# star1_climate <- paste("\\textbf{Climate-effect}\\\\")
star1 <- c(star_1, "\\textbf{Weather-effect}\\\\", "\\\\[-1.8ex]", star1_last)

star_1 <- star1[1:31]
star1_last <- star1[32:length(star1)]
# star1_climate <- paste("\\textbf{Climate-effect}\\\\")
star1 <- c(star_1, "\\hline \\\\[-1.8ex]", "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star1_last)


loc <- which(star1 == "\\end{tabular} ")
star1 <- star1[1:loc-1]
star1notes <- paste("\\parbox{5in}{Notes: Table reports regression coefficients for log crop revenue per acre using weather (year-to-year) and climate (rolling mean) degree day and precipitation variables from 1950-2010.
Crop revenue per acre is calculated by summing production (lbs) per acre times average crop price for corn, cotton, hay, soybean, and wheat. Climate variables are 10, 11, and 12-year 'right' rolling mean windows. Regression
estimates are weighted by total county-level total acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star1 <- c(star1, "\\end{tabular}", star1notes)
star1 <- c(star1, "\\end{table}")


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "agg_crop_rev.tex")
cat(star1, file = "agg_crop_rev.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "agg_crop_rev.tex", append = TRUE)
# Compile pdf
system("pdflatex agg_crop_rev.tex")

}

system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' agg_crop_rev.tex > agg_crop_rev_out.tex")
system("pdflatex agg_crop_rev_out.tex")
# 