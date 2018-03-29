# Trend specifications
library(stargazer)
library(lfe)

multiply.100 <- function(x) (x * 100)

regdat <- readRDS("data/full_ag_data.rds")


mod1 <- readRDS("models/sur_rev_model1.rds")
mod2 <- readRDS("models/sur_rev_model2.rds")
mod3 <- readRDS("models/sur_rev_model3.rds")
mod4 <- readRDS("models/sur_rev_model.rds")

skelmod <- lm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq - 1,
              data = regdat)
ncoef <- length(sur_thirty$coefficients)
ncoef/5

star1 <- stargazer(skelmod, skelmod, skelmod, skelmod, skelmod, skelmod, skelmod, skelmod,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f", "rsq", "adj.rsq"),
                  title = "Seemingly Unrelated Regression (SUR) Model explaining Crop Rev/Acre",
                  column.labels = c("Corn", "Corn", "Corn", "Corn", "Cotton", "Hay", "Soybean", "Wheat"),
          dep.var.labels = c("Crop Z-score (quantiles)", "Crop Z-score (quantiles)", "Crop Z-score (quantiles)", "Crop Z-score (quantiles)", "Crop Z-score (quantiles)"),
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)",
                               "Precipitation", "Precipitation Squared"),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          coef = list(skelmod = as.numeric(mod1$coefficients[2:6]),
                      skelmod = as.numeric(mod2$coefficients[1:5]),
                      skelmod = as.numeric(mod3$coefficients[1:5]),
                      skelmod = as.numeric(mod4$coefficients[1:5]), # Corn
                      skelmod = as.numeric(mod4$coefficients[10:14]), #COtton
                      skelmod = as.numeric(mod4$coefficients[19:23]),
                      skelmod = as.numeric(mod4$coefficients[28:32]),
                      skelmod = as.numeric(mod4$coefficients[37:41])),
          se = list(skelmod = as.numeric(mod1$bs_se[2:6, 1]),
                      skelmod = as.numeric(mod2$bs_se[1:5, 1]),
                      skelmod = as.numeric(mod3$bs_se[1:5, 1]),
                      skelmod = as.numeric(mod4$bs_se[1:5, 1]), # Corn
                      skelmod = as.numeric(mod4$bs_se[6:10, 1]), #COtton
                      skelmod = as.numeric(mod4$bs_se[11:15, 1]),
                      skelmod = as.numeric(mod4$bs_se[16:20, 1]),
                      skelmod = as.numeric(mod4$bs_se[20:24, 1])),
          
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County", "County", "County", "County", "County", "County", "County"),
                           c("Lat/Long Linear Trend", "--", "--", "Yes", "--", "--", "--", "--", "--"),
                           c("Lat/Long Quad. Trend", "--", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("R Sq.", "0.09", "0.05", "0.08", "0.17", "0.11", "0.15", "0.21", "0.27")),
          notes.append = FALSE, notes.align = "l")


star_1 <- star1[1:14]
star1_last <- star1[15:length(star1)]
star1 <- c(star_1, "\\textbf{Weather-effect}\\\\", "\\\\[-1.8ex]", star1_last)

loc <- which(star1 == "\\end{tabular} ")
star1 <- star1[1:loc-1]
star1notes <- paste("\\parbox{6.3in}{Notes: Table reports regression coefficients for log crop revenue per acre using weather (year-to-year) degree days and precipitation variables from 1950-2010.
Crop revenue per acre equals crop yield per acre times average state-level crop price for corn, cotton, hay, soybean, and wheat. Standard errors are bootstrapped by strata state.
Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star1 <- c(star1, "\\end{tabular}", star1notes)
star1 <- c(star1, "\\end{table}")

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "sur_rev_tables.tex")
cat(star1, file = "sur_rev_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "sur_rev_tables.tex", append = TRUE)
#cat(star2, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star3, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star4, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "sur_rev_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex sur_rev_tables.tex")

}

system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' sur_rev_tables.tex > sur_rev_tables_out.tex")
system("pdflatex sur_rev_tables_out.tex")
