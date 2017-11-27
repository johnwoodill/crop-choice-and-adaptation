library(stargazer)
library(lfe)


setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
  }

# Save models
csmod0 <- readRDS("models/cs_rev.rds")
csmod1 <- readRDS("models/cs_rev_fe.rds")

ldmod0 <- readRDS("models/ld_rev.rds")
ldmod1 <- readRDS("models/ld_rev_fe.rds")

pmod0 <- readRDS("models/p_rev.rds")
pmod1 <- readRDS("models/p_rev_fe.rds")



star1 <- stargazer(csmod0, csmod1, ldmod0, ldmod1, pmod0, pmod1,
                  align = FALSE, no.space = FALSE, 
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state"), 
                  omit.stat = c("ser", "f"),
                  title = "Regression Model explaining Crop Revenue per Acre", 
                  column.labels = c("Cross-section", "Cross-section", "Decade", 
                                    "Decade",  "Panel", "Panel"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"), 
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
                               "Precipitation", "Precipitation Squared"),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Fixed-effect", "--", "State", "--", "State, Decade", "--", "County, Year"),
                           c("Clusterd SE", "--", "State", "--", "State", "--", "State")),
          notes.append = FALSE, notes.align = "l") 

# star1
# 
# star2 <- stargazer(cmod0, cmod1, cmod2, cmod3, cmod4, cmod5,
#                   align = FALSE, no.space = FALSE, 
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year", "state"), 
#                   omit.stat = c("ser", "f"),
#                   title = "Difference-in-Difference Regression Model explaining Crop Revenue per Acre (constant acres)", 
#                   column.labels = c("Basic Model", "Basic Model", "Basic Model", 
#                                     "Climate Model",  "Climate Model", "Climate Model"),
#           dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
#                              "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"), 
#           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
#                                "Precipitation", "Precipitation Squared", "Post - 0:1950-1980/1:1980-2010", 
#                                "Treat - 0:cooled/1:warmed the most", "Treatment-effect"),
#           model.names = FALSE,  omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Weights", "--", "Crop Acres", "Crop Acres", "--", "Crop Acres", "Crop Acres"),
#                            c("Fixed-effect", "--", "County", "County", "--", "County", "County"),
#                            c("State-trend", "--", "Yes", "Yes", "--", "Yes", "Yes"),
#                            c("Clusterd SE", "--", "--", "State", "", "--", "State")),
#           notes.append = FALSE, notes.align = "l") 


# star2 <- stargazer(moda, modb, modc, modd, mode, modf,
#                    align = FALSE, no.space = FALSE,
#                    style = "aer", digits = 2,
#                    omit = c("fips", "year", "state"),
#                    omit.stat = c("ser", "f"),
#                    title = "Difference-in-Difference Regression Model explaining Crop Acres",
#                    column.labels = c("Basic Model", "Basic Model", "Basic Model",
#                                      "Climate Model",  "Climate Model", "Climate Model"),
#            dep.var.labels = c("Log(Crop Acre)", "Log(Crop Acre)", "Log(Crop Acre)", "Log(Crop Acre)",
#                               "Log(Crop Acre)", "Log(Crop Acre)"),
#            covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)",
#                                 "Precipitation", "Precipitation Squared", "Post - 0:1950-1980/1:1980-2010",
#                                 "Treat - 0:cooled/1:warmed the most", "Treatment-effect"),
#            model.names = FALSE,  omit.table.layout = "n",
#          apply.coef = multiply.100, apply.se = multiply.100,
#            table.layout ="=dcm#-t-as=n",
#            font.size = "footnotesize",
#            add.lines = list(c("Fixed-effect", "--", "County", "County", "--", "County", "County"),
#                             c("Clusterd SE", "--", "State", "State", "", "State", "State"),
#                             c("State-trend", "--", "--", "Yes", "--", "--", "Yes")),
#            notes.append = FALSE, notes.align = "l")
# 
# star3 <- stargazer(mod1a, mod2a, mod3a, mod4a, mod5a, 
#                   align = FALSE, no.space = FALSE,
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year", "state"),
#                   omit.stat = c("ser", "f"),
#                   title = "Difference-in-Difference Regression Model explaining Crop Revenues",
#                   #column.labels = c("Climate Model", "Climate Model", "Climate Model",
#                   #                  "Climate Model",  "Climate Model", "Climate Model"),
#           dep.var.labels = c("Log(Corn Rev)", "Log(Cotton Rev)", "Log(Hay Rev)", "Log(Soybean Rev)",
#                              "Log(Wheat Rev)"),
#           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)",
#                                "Precipitation", "Precipitation Squared", "Post - 0:1950-1980/1:1980-2010",
#                                "Treat - 0:cooled/1:warmed the most", "Treatment-effect"),
#           model.names = FALSE,  omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Fixed-effect", "County", "County", "County", "County", "County"),
#                            c("Clusterd SE", "State", "State", "State", "State", "State"),
#                            c("State-trend", "Yes", "Yes", "Yes", "Yes", "Yes")),
#           notes.append = FALSE, notes.align = "l")
# #star2
# 
# star4 <- stargazer(mod1b, mod2b, mod3b, mod4b, mod5b, 
#                   align = FALSE, no.space = FALSE,
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year", "state"),
#                   omit.stat = c("ser", "f"),
#                   title = "Difference-in-Difference Regression Model explaining Crop Shares",
#                   #column.labels = c("Climate Model", "Climate Model", "Climate Model",
#                   #                  "Climate Model",  "Climate Model", "Climate Model"),
#           dep.var.labels = c("Corn Share", "Cotton Share", "Hay Share", "Soybean Share",
#                              "Wheat Share"),
#           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)",
#                                "Precipitation", "Precipitation Squared", "Post - 0:1950-1980/1:1980-2010",
#                                "Treat - 0:cooled/1:warmed the most", "Treatment-effect"),
#           model.names = FALSE,  omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Fixed-effect", "County", "County", "County", "County", "County"),
#                            c("Clusterd SE", "State", "State", "State", "State", "State"),
#                            c("State-trend", "Yes", "Yes", "Yes", "Yes", "Yes")),
#           notes.append = FALSE, notes.align = "l")



# star1 <- stargazer(bs_mod1, bs_mod2, bs_mod3, bs_mod4,
#                   align = FALSE, no.space = FALSE, 
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year"), 
#                   omit.stat = c("ser", "f"),
#                   title = "Difference-in-Difference Regression Model explaining Crop Revenue per Acre", 
#                   column.labels = c("Basic Model", "Basic Model", "Climate Model", "Climate Model"),
#           dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"), 
#           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
#                                "Precipitation", "Precipitation Squared", "Post - 0:1950-1980 - 1:1980-2010", 
#                                "Treat - 0: w/o adapt - 1:w/ adapt", "Treatment-effect"),
#           model.names = FALSE,  omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Fixed-effect", "", "State", "", "State", "State \\& Year"),
#                            c("Clusterd SE", "", "State", "", "State", "State \\& Year")),
#           notes.append = FALSE, notes.align = "l") 


# Regression data includes only those counties that warmed the most from 1950-1980 and 1980-2010. 
# Regression estimates were bootstrapped by resampling half of data set and set crop acres equal to 
# the mean in each county (no adaptation). For other half, acres remain same (with adaptation).
# Coefficients are multipled by 100.



# star2 <- stargazer(modc1, modco1, modh1, mods1, modw1,
#                   align = FALSE, no.space = FALSE, 
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year"), 
#                   omit.stat = c("ser", "f"),
#                   title = "Difference-in-Difference Regression Model explaining Crop Acres", 
#                   #column.labels = c("Basic Model", "Basic Model", "Climate Model", "Climate Model", "Climate Model"),
#           dep.var.labels = c("Log(Corn Acres)",  "Log(Cotton Acres)", "Log(Hay Acres)", "Log(Soybeans Acres)", "Log(Wheat Acres)"),
#           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
#                               "Precipitation", "Precipitation Squared", "State-by-Year Trend", "Post - 0:1950-1980/1:1980-2010", 
#                                "Treat - County 0:cooled/1:warmed the most", "Treatment-effect"
#                                ),
#           model.names = FALSE, omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Mean of Dep Variable", "9.30", "8.05",  "9.64", "9.17", "9.18"),
#                            c("Fixed-effect", "State", "State", "State", "State", "State"),
#                            c("Clusterd SE", "State", "State", "State", "State", "State")),
#           notes.append = FALSE, notes.align = "l",
#           notes = "asdf")
# #star1
# 
# star3 <- stargazer(modc2, modco2, modh2, mods2, modw2, 
#                   align = FALSE, no.space = FALSE, 
#                   style = "aer", digits = 2,
#                   omit = c("fips", "year"), 
#                   omit.stat = c("ser", "f"),
#                   title = "Difference-in-Difference Regression Model explaining Crop Acres", 
#                   #column.labels = c("Basic Model", "Basic Model", "Climate Model", "Climate Model", "Climate Model"),
#           dep.var.labels = c("Corn Share", "Cotton Share", "Hay Share", "Soybeans Share", "Wheat Share"), 
#           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
#                               "Precipitation", "Precipitation Squared", "State-by-Year Trend", "Post - 0:1950-1980/1:1980-2010", 
#                                "Treat - County 0:cooled/1:warmed the most", "Treatment-effect"
#                                ),
#           model.names = FALSE, omit.table.layout = "n",
#           apply.coef = multiply.100, apply.se = multiply.100,
#           table.layout ="=dcm#-t-as=n",
#           font.size = "footnotesize",
#           add.lines = list(c("Mean of Dep Variable", "0.21", "0.11",  "0.22", "0.20", "0.11"),
#                              c("Fixed-effect", "State", "State", "State", "State", "State"),
#                            c("Clusterd SE", "State", "State", "State", "State", "State")),
#           notes.append = FALSE, notes.align = "l",
#           notes = "asdf")
# #star1

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/tables")          
{
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "rev_regression_tables.tex")
cat(star1, file = "rev_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star2, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star3, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star4, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "rev_regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex rev_regression_tables.tex")

}

#system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' rev_regression_tables.tex > new.tex") 
                  
