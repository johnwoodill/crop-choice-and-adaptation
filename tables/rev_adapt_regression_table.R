library(stargazer)
library(lfe)


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
  }

# Save models
base1 <- readRDS("models/mod_base_1.rds")
base2 <- readRDS("models/mod_base_2.rds")
base3 <- readRDS("models/mod_base_3.rds")
base4 <- readRDS("models/mod_base_4.rds")
thirty1 <- readRDS("models/modthirty_1.rds")

# mod1 <- readRDS("models/modfive5.rds")
# mod2 <- readRDS("models/modten5.rds")
# mod3 <- readRDS("models/modtwenty5.rds")
# mod4 <- readRDS("models/modthirty5.rds")
# mod5 <- readRDS("models/modsixty5.rds")
# 
# mod4a <- readRDS("models/modthirty5a.rds")
# mod4b <- readRDS("models/modthirty5b.rds")
# mod4c <- readRDS("models/modthirty5c.rds")
# mod4d <- readRDS("models/modthirty5d.rds")
# mod4e <- readRDS("models/modthirty5e.rds")

attr(base1$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(base2$beta, "dimnames")[[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(base3$beta, "dimnames")[[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(base4$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(thirty1$beta, "dimnames")[[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(base1$coefficients,"dimnames") [[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq")
attr(base2$coefficients,"dimnames") [[1]] <- c("(Intercept)", "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(base3$coefficients,"dimnames") [[1]] <- c("(Intercept)", "weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(base4$coefficients,"dimnames") [[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(thirty1$coefficients,"dimnames") [[1]][1:10] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")


star1 <- stargazer(base1, base2, base3, base4, thirty1, 
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression Model explaining Crop Revenue per Acre",
                  column.labels = c("Weather", "Climate", "Weather-Climate (30-year)", "Weather-Climate (30-year)",  
                                    "Weather-Climate (30-year)"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) - Weather", "Degree Days (10-30C) - Weather", "Degree Days (30C) - Weather",
                               "Precipitation - Weather", "Precipitation Squared - Weather",
                               "Degree Days (0-10C) - Climate", "Degree Days (10-30C) - Climate", "Degree Days (30C) - Climate",
                               "Precipitation - Climate", "Precipitation Squared - Climate"),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "--", "--", "State", "State x Climate Interval"),
                           c("Quad. State-trend", "--", "--", "--", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "State", "State")),
          notes.append = FALSE, notes.align = "l")

# star2 <- stargazer(mod4a, mod4b, mod4c, mod4d, mod4e, 
#                   align = FALSE, no.space = FALSE, 
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year", "state"), 
#                   omit.stat = c("ser", "f"),
#                   title = "Regression Model explaining Crop Revenue per Acre", 
#                   column.labels = c("Weather", "Climate", "Weather-Climate", "Weather-Climate (30-year)",  "Weather-Climate (30-year)"),
#           dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
#                              "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"), 
#           covariate.labels = c("Degree Days (0-10C) - Weather", "Degree Days (10-30C) - Weather", "Degree Days (30C) - Weather", 
#                                "Precipitation - Weather", "Precipitation Squared - Weather",
#                                "Degree Days (0-10C) - Climate", "Degree Days (10-30C) - Climate", "Degree Days (30C) - Climate", 
#                                "Precipitation - Climate", "Precipitation Squared - Climate"),
#           model.names = FALSE,  omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Fixed-effect", "--", "--", "--", "State", "State, Year Interval"),
#                            c("Clusterd SE", "--", "--", "--", "--", "State")),
#           notes.append = FALSE, notes.align = "l") 


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "rev_adapt_regression_tables.tex")
cat(star1, file = "rev_adapt_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
# cat(star2, file = "rev_adapt_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star3, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star4, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "rev_adapt_regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex rev_adapt_regression_tables.tex")

}

system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' rev_adapt_regression_tables.tex > rev_adapt_regression_tables_out.tex") 
system("pdflatex rev_adapt_regression_tables_out")      

