library(tidyverse)
library(lmtest)
library(stargazer)
library(lfe)
library(systemfit)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
}

cropdat <- readRDS("data/full_ag_data.rds")

# Get SUR out files
sur_five <- readRDS("models/sur_model_five.rds")
sur_ten <- readRDS("models/sur_model_ten.rds")
sur_twenty <- readRDS("models/sur_model_twenty.rds")
sur_thirty <- readRDS("models/sur_model_thirty.rds")
sur_sixty <- readRDS("models/sur_model_sixty.rds")

skelmod <- lm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq + 
               dday0_10_thirty + dday10_30_thirty + dday30_thirty + prec_thirty + prec_sq_thirty - 1,
              data = cropdat)
ncoef <- length(sur_thirty$coefficients)
ncoef/5

modsum_thirty <- summary(sur_thirty)
modsum_thirty$coefficients[1, 2]

star1 <- stargazer(skelmod, skelmod, skelmod, skelmod, skelmod,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Seemingly Unrelated Regression (SUR) Model explaining Crop Shares",
                  column.labels = c("Corn (30-year)", "Cotton (30-year)", "Hay (30-year)", "Soybean (30-year)",  
                                    "Wheat (30-year)"),
          dep.var.labels = c("Crop Z-score (quantiles)", "Crop Z-score (quantiles)", "Crop Z-score (quantiles)", "Crop Z-score (quantiles)", "Crop Z-score (quantiles)"),
          covariate.labels = c("Degree Days (0-10C) - Weather", "Degree Days (10-30C) - Weather", "Degree Days (30C) - Weather",
                               "Precipitation - Weather", "Precipitation Squared - Weather",
                               "Degree Days (0-10C) - Climate", "Degree Days (10-30C) - Climate", "Degree Days (30C) - Climate",
                               "Precipitation - Climate", "Precipitation Squared - Climate"),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          coef = list(skelmod = as.numeric(modsum_thirty$coefficients[1:10, 1]),
                      skelmod = as.numeric(modsum_thirty$coefficients[62:71, 1]),
                      skelmod = as.numeric(modsum_thirty$coefficients[123:131, 1]),
                      skelmod = as.numeric(modsum_thirty$coefficients[184:193, 1]),
                      skelmod = as.numeric(modsum_thirty$coefficients[245:254, 1])),
          se = list(skelmod = as.numeric(modsum_thirty$coefficients[1:10, 2]),
                      skelmod = as.numeric(modsum_thirty$coefficients[62:71, 2]),
                      skelmod = as.numeric(modsum_thirty$coefficients[123:131, 2]),
                      skelmod = as.numeric(modsum_thirty$coefficients[184:193, 2]),
                      skelmod = as.numeric(modsum_thirty$coefficients[245:254, 2])),
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(#c("Boostrap Regression", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Fixed-effect", "State", "State", "State", "State", "State"),
                           c("Quad. State-trend", "Yes", "Yes", "Yes", "Yes", "Yes")),
                           #c("Bootstrap Clustered SE", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "sur_regression_tables.tex")
cat(star1, file = "sur_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star2, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star3, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star4, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "sur_regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex sur_regression_tables.tex")

}

system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' sur_regression_tables.tex > sur_regression_tables_out.tex") 
system("pdflatex sur_regression_tables_out.tex")      
