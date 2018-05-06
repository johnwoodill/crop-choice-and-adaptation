# Trend specifications
library(stargazer)
library(lfe)

multiply.100 <- function(x) (x * 100)

regdat <- readRDS("data/full_ag_data.rds")

mod1 <- readRDS("models/sur_share_ten_model1.rds")
mod2 <- readRDS("models/sur_share_ten_model2.rds")
# mod3 <- readRDS("models/sur_share_ten_model3.rds")
mod4 <- readRDS("models/sur_share_model_ten.rds")
# mod5 <- readRDS("models/sur_share_model_twenty.rds")
# mod6 <- readRDS("models/sur_share_model_thirty.rds")

mod4$coefficients <- mod4$coefficients[-c(grep("trend", names(mod4$coefficients)))]

skelmod <- lm(ln_rev ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq - 1,
              data = regdat)


star1 <- stargazer(skelmod, skelmod, skelmod, skelmod, skelmod, skelmod, skelmod,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f", "rsq", "adj.rsq"),
                  title = "Seemingly Unrelated Regression (SUR) Model explaining Crop Share",
                  column.labels = c("Corn", "Corn", "Corn", "Cotton", "Hay", "Soybean", "Wheat"),
          dep.var.labels = c("Z-score", "Z-score", "Z-score", "Z-score", "Z-score"),
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)",
                               "Precipitation", "Precipitation Squared"),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          coef = list(skelmod = as.numeric(mod1$coefficients[2:6]),
                      skelmod = as.numeric(mod2$coefficients[1:5]),
                      # skelmod = as.numeric(mod3$coefficients[1:5]),
                      skelmod = as.numeric(mod4$coefficients[1:5]), # Corn
                      skelmod = as.numeric(mod4$coefficients[6:10]), #COtton
                      skelmod = as.numeric(mod4$coefficients[11:15]),
                      skelmod = as.numeric(mod4$coefficients[16:20]),
                      skelmod = as.numeric(mod4$coefficients[20:24])),
          se = list(skelmod = as.numeric(mod1$bs_se[2:6, 1]),
                      skelmod = as.numeric(mod2$bs_se[1:5, 1]),
                      # skelmod = as.numeric(mod3$bs_se[1:5, 1]),
                      skelmod = as.numeric(mod4$bs_se[1:5, 1]), # Corn
                      skelmod = as.numeric(mod4$bs_se[6:10, 1]), #COtton
                      skelmod = as.numeric(mod4$bs_se[11:15, 1]),
                      skelmod = as.numeric(mod4$bs_se[16:20, 1]),
                      skelmod = as.numeric(mod4$bs_se[20:24, 1])),
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County", "County", "County", "County", "County", "County", "County"),
                           c("State Trend", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Bootstrap SE", "--", "--", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
                           # c("R Sq.", "0.09", "0.07", "0.09", "0.09", "0.25", "0.08", "0.29", "0.15")),
          notes.append = FALSE, notes.align = "l")

# star_1 <- star1[1:12]
# star1_last <- star1[13:length(star1)]
# star1_headinga <- paste("\\\\[-1.8ex]  & (10-year) & (10-year) & (10-year) & (10-year) & (10-year) & (10-year) & (10-year) & (10-year) \\\\")
# star1 <- c(star_1, star1_headinga, star1_last)


star_1 <- star1[1:14]
star1_last <- star1[15:length(star1)]
star1 <- c(star_1, "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star1_last)

loc <- which(star1 == "\\end{tabular} ")
star1 <- star1[1:loc-1]
star1notes <- paste("\\parbox{5.6in}{Notes: Table reports regression coefficients for a seemingly unrelated regression (SUR) using transformed crop shares to z-scores for climate (rolling mean) degree days and precipitation variables from 1950-2010.
Z-scores are calculated using individual crop shares as a proportion of total acres. Climate effects use a right rolling mean window. 
Standard errors are bootstrapped by strata state.
Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star1 <- c(star1, "\\end{tabular}", star1notes)
star1 <- c(star1, "\\end{table}")


# 
# 
# star2 <- stargazer(skelmod, skelmod, skelmod, skelmod, skelmod, skelmod, skelmod, skelmod, skelmod, skelmod,
#                   align = FALSE, no.space = FALSE,
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year", "state", "trend"),
#                   omit.stat = c("ser", "f", "rsq", "adj.rsq"),
#                   title = "Seemingly Unrelated Regression (SUR) Model explaining Crop Share",
#                   column.labels = c("Corn", "Cotton", "Hay", "Soybean", "Wheat", "Corn", "Cotton", "Hay", "Soybean", "Wheat"),
#           dep.var.labels = c("Z-score", "Z-score", "Z-score", "Z-score", "Z-score"),
#           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)",
#                                "Precipitation", "Precipitation Squared"),
#           model.names = FALSE,  omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           coef = list(skelmod = as.numeric(mod5$coefficients[1:5]),
#                       skelmod = as.numeric(mod5$coefficients[10:14]),
#                       skelmod = as.numeric(mod5$coefficients[19:23]),
#                       skelmod = as.numeric(mod5$coefficients[28:32]), # Corn
#                       skelmod = as.numeric(mod5$coefficients[37:41]), #COtton
#                       skelmod = as.numeric(mod6$coefficients[1:5]),
#                       skelmod = as.numeric(mod6$coefficients[10:14]),
#                       skelmod = as.numeric(mod6$coefficients[19:23]),
#                       skelmod = as.numeric(mod6$coefficients[28:32]),
#                       skelmod = as.numeric(mod6$coefficients[37:41])),
#           se = list(skelmod = as.numeric(mod5$bs_se[1:5, 1]),
#                       skelmod = as.numeric(mod5$bs_se[6:10, 1]),
#                       skelmod = as.numeric(mod5$bs_se[11:15, 1]),
#                       skelmod = as.numeric(mod5$bs_se[16:20, 1]), # Corn
#                       skelmod = as.numeric(mod5$bs_se[20:24, 1]), #COtton
#                       skelmod = as.numeric(mod6$bs_se[1:5, 1]),
#                       skelmod = as.numeric(mod6$bs_se[6:10, 1]),
#                       skelmod = as.numeric(mod6$bs_se[11:15, 1]),
#                       skelmod = as.numeric(mod6$bs_se[16:20, 1]),
#                       skelmod = as.numeric(mod6$bs_se[20:24, 1])),
#           
# 
#           
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Fixed-effect", "County", "County", "County", "County", "County", "County", "County", "County", "County", "County"),
#                            c("Lat/Long Linear Trend", "--", "--", "--", "--", "--", "--", "--", "--", "--", "--"),
#                            c("Lat/Long Quad. Trend", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
#                            c("R Sq.", "0.09", "0.25", "0.08", "0.29", "0.15", "0.09", "0.25", "0.08", "0.29", "0.15")),
#           notes.append = FALSE, notes.align = "l")
# 
# star_2 <- star2[1:12]
# star2_last <- star2[13:length(star2)]
# star2_headinga <- paste("\\\\[-1.8ex]  & (11-year) & (11-year) & (11-year) & (11-year) & (11-year) & (12-year) & (12-year) & (12-year) & (12-year) & (12-year) \\\\")
# star2 <- c(star_2, star2_headinga, star2_last)
# 
# 
# star_2 <- star2[1:15]
# star2_last <- star2[16:length(star2)]
# star2 <- c(star_2, "\\textbf{Climate-effect}\\\\", "\\\\[-1.8ex]", star2_last)
# 
# loc <- which(star2 == "\\end{tabular} ")
# star2 <- star2[1:loc-1]
# star2notes <- paste("\\parbox{8.3in}{Notes: Table reports regression coefficients for a seemingly unrelated regression (SUR) using transformed crop shares to z-scores for climate (rolling mean) degree days and precipitation variables from 1950-2010.
# Z-scores are calculated using individual crop shares as a proportion of total acres. Climate effects use a right rolling mean windows from 11-12 year. 
# Standard errors are bootstrapped by strata state.
# Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")
# 
# star2 <- c(star2, "\\end{tabular}", star2notes)
# star2 <- c(star2, "\\end{table}")


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "sur_share_tables.tex")
cat(star1, file = "sur_share_tables.tex", sep = "\n", append = TRUE)
# cat("\\newpage", file = "sur_share_tables.tex", append = TRUE)
# cat("\\begin{landscape}", file = "sur_share_tables.tex", sep = "\n", append = TRUE)
# cat("\\mbox{}\\vfill", file = "sur_share_tables.tex", sep = "\n", append = TRUE)
# cat(star2, file = "sur_share_tables.tex", sep = "\n", append = TRUE)
# cat("\\vfill", file = "sur_share_tables.tex", append = TRUE)
# cat("\\end{landscape}", file = "sur_share_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "sur_share_tables.tex", append = TRUE)
#cat(star3, file = "sur_share_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "sur_share_tables.tex", append = TRUE)
#cat(star4, file = "sur_share_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "sur_share_tables.tex", append = TRUE)
# Compile pdf
# system("pdflatex sur_share_tables.tex")

}

system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' sur_share_tables.tex > sur_share_tables_out.tex")
system("pdflatex sur_share_tables_out.tex")

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation")     
