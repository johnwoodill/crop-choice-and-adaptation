library(stargazer)
library(lfe)


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
  }

# Save models
# mod1 <- readRDS("models/modfive5.rds")
# mod2 <- readRDS("models/modten5.rds")
# mod3 <- readRDS("models/modtwenty5.rds")
# mod4 <- readRDS("models/modthirty5.rds")
# mod5 <- readRDS("models/modsixty5.rds")

mod1 <- readRDS("models/modfive5.rds")
mod2 <- readRDS("models/modten5.rds")
mod3 <- readRDS("models/modtwenty5.rds")
mod4 <- readRDS("models/modthirty5.rds")
mod5 <- readRDS("models/modsixty5.rds")

attr(mod1$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(mod2$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(mod3$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(mod4$beta, "dimnames")[[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")
attr(mod5$beta, "dimnames")[[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod1$coefficients,"dimnames") [[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod2$coefficients,"dimnames") [[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod3$coefficients,"dimnames") [[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod4$coefficients,"dimnames") [[1]] <- c("weather_dday0_10", "weather_dday10_30", "weather_dday30", "weather_prec", "weather_prec_sq",
                                "climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")

attr(mod5$coefficients,"dimnames") [[1]] <- c("climate_dday0_10", "climate_dday10_30", "climate_dday30", "climate_prec", "climate_prec_sq")


star1 <- stargazer(mod1, mod2, mod3, mod4, mod5, 
                  align = FALSE, no.space = FALSE, 
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state"), 
                  omit.stat = c("ser", "f"),
                  title = "Regression Model explaining Crop Revenue per Acre", 
                  column.labels = c("5-year", "10-year", "20-year", "30-year",  "60-year"),
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
          add.lines = list(c("Fixed-effect", "State, Year Interval", "State, Year Interval", "State, Year Interval", "State, Year Interval", "State, Year Interval"),
                           c("Clusterd SE", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l") 




setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "rev_adapt_regression_tables.tex")
cat(star1, file = "rev_adapt_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star2, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
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

