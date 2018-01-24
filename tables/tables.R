library(stargazer)
library(lfe)


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
}



# Save models
corn1 <- readRDS("models/mod_corn_base_1.rds")
corn2 <- readRDS("models/mod_corn_base_2.rds")
corn3 <- readRDS("models/mod_corn_base_3.rds")
corn4 <- readRDS("models/mod_corn_base_4.rds")
corn5 <- readRDS("models/mod_corn.rds")

cotton1 <- readRDS("models/mod_cotton_base_1.rds")
cotton2 <- readRDS("models/mod_cotton_base_2.rds")
cotton3 <- readRDS("models/mod_cotton_base_3.rds")
cotton4 <- readRDS("models/mod_cotton_base_4.rds")
cotton5 <- readRDS("models/mod_cotton.rds")

hay1 <- readRDS("models/mod_hay_base_1.rds")
hay2 <- readRDS("models/mod_hay_base_2.rds")
hay3 <- readRDS("models/mod_hay_base_3.rds")
hay4 <- readRDS("models/mod_hay_base_4.rds")
hay5 <- readRDS("models/mod_hay.rds")

soybean1 <- readRDS("models/mod_soybean_base_1.rds")
soybean2 <- readRDS("models/mod_soybean_base_2.rds")
soybean3 <- readRDS("models/mod_soybean_base_3.rds")
soybean4 <- readRDS("models/mod_soybean_base_4.rds")
soybean5 <- readRDS("models/mod_soybean.rds")

wheat1 <- readRDS("models/mod_wheat_base_1.rds")
wheat2 <- readRDS("models/mod_wheat_base_2.rds")
wheat3 <- readRDS("models/mod_wheat_base_3.rds")
wheat4 <- readRDS("models/mod_wheat_base_4.rds")
wheat5 <- readRDS("models/mod_wheat.rds")



# Corn
star1 <- stargazer(corn1, corn2, corn4, corn5,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Corn Revenue per Acre",
                  # column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                  # "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(Corn Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County", "County", "County"),
                           c("Quad. State-trend", "--",  "--", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "State")),
          notes.append = FALSE, notes.align = "l")


loc <- which(star1 == "\\end{tabular} ")
star1 <- star1[1:loc-1]
star1notes <- paste("\\parbox{3.7in}{Notes: Table reports regression coefficients for log corn revenue per acre using degree day variables and precipitation variables from 1950-2010. Corn revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Regression estimates are weighted by total county-level corn acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star1 <- c(star1, "\\end{tabular}", "\\begin{center}", star1notes, "\\end{center}")
star1 <- c(star1, "\\end{table}")


# cotton
star2 <- stargazer(cotton1, cotton2, cotton4, cotton5,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Cotton Revenue per Acre",
                  # column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                  # "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(cotton Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County", "County", "County"),
                           c("Quad. State-trend", "--", "--", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "State")),
          notes.append = FALSE, notes.align = "l")


loc <- which(star2 == "\\end{tabular} ")
star2 <- star2[1:loc-1]
star2notes <- paste("\\parbox{3.7in}{Notes: Table reports regression coefficients for log cotton revenue per acre using degree day variables and precipitation variables from 1950-2010. Cotton revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Regression estimates are weighted by total county-level cotton acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star2 <- c(star2, "\\end{tabular}", "\\begin{center}", star2notes, "\\end{center}")
star2 <- c(star2, "\\end{table}")


# hay
star3 <- stargazer(hay1, hay2,  hay4, hay5,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Hay Revenue per Acre",
                  # column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                  # "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(hay Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County", "County",  "County"),
                           c("Quad. State-trend", "--", "--", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "State")),
          notes.append = FALSE, notes.align = "l")


loc <- which(star3 == "\\end{tabular} ")
star3 <- star3[1:loc-1]
star3notes <- paste("\\parbox{3.7in}{Notes: Table reports regression coefficients for log hay revenue per acre using degree day variables and precipitation variables from 1950-2010. Hay revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Regression estimates are weighted by total county-level hay acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star3 <- c(star3, "\\end{tabular}", "\\begin{center}", star3notes, "\\end{center}")
star3 <- c(star3, "\\end{table}")


# soybean
star4 <- stargazer(soybean1, soybean2, soybean4, soybean5,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Soybean Revenue per Acre",
                  # column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                  # "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(soybean Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County", "County", "County"),
                           c("Quad. State-trend", "--", "--", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "State")),
          notes.append = FALSE, notes.align = "l")


loc <- which(star4 == "\\end{tabular} ")
star4 <- star4[1:loc-1]
star4notes <- paste("\\parbox{3.7in}{Notes: Table reports regression coefficients for log soybean revenue per acre using degree day variables and precipitation variables from 1950-2010. Soybean revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Regression estimates are weighted by total county-level soybean acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star4 <- c(star4, "\\end{tabular}", "\\begin{center}", star4notes, "\\end{center}")
star4 <- c(star4, "\\end{table}")


# wheat
star5 <- stargazer(wheat1, wheat2, wheat4, wheat5,
                  align = FALSE, no.space = FALSE,
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state", "trend"),
                  omit.stat = c("ser", "f"),
                  title = "Regression explaining Wheat Revenue per Acre",
                  # column.labels = c("Weather", "Climate ", "Weather-Climate ", "Weather-Climate ",  
                  # "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Weather-Climate", "Climate"),
          dep.var.labels = c("Log(wheat Revenue per Acre)"),
          covariate.labels = c("Degree Days (0-10C) ", "Degree Days (10-30C) ", "Degree Days (30C) ",
                               "Precipitation ", "Precipitation Squared "),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "County",  "County", "County"),
                           c("Quad. State-trend", "--", "--",  "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "--", "State")),
          notes.append = FALSE, notes.align = "l")


loc <- which(star5 == "\\end{tabular} ")
star5 <- star5[1:loc-1]
star5notes <- paste("\\parbox{3.7in}{Notes: Table reports regression coefficients for log wheat revenue per acre using degree day variables and precipitation variables from 1950-2010. Wheat revenue per acre is calculated by multiplying production (lbs) per acre times average crop price from 1950-2010. Regression estimates are weighted by total county-level wheat acres (smoothed using a spline). Estimates in \\textbf{bold} are statistically significant at 95\\%. Coefficients have been multiplied by 100.}")

star5 <- c(star5, "\\end{tabular}", "\\begin{center}", star5notes, "\\end{center}")
star5 <- c(star5, "\\end{table}")


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}\n", file = "tables.tex")
cat(star1, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star2, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star3, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star4, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "tables.tex", append = TRUE)
cat(star5, file = "tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "tables.tex", append = TRUE)
# Compile pdf
system("pdflatex tables.tex")

}
# 
system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' tables.tex > tables_out.tex")
system("pdflatex tables_out.tex")

