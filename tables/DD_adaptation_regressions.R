library(stargazer)
#library(lfe)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")

multiply.100 <- function(x) (x * 100)
multiply.1000 <- function(x) (x * 1000)

adj_ln <- function(x){
  ((exp(x) - 1))
  }

# 30-year Treatment log rev
mod0 <- readRDS("models/dd_mod0.rds")
mod1 <- readRDS("models/dd_mod1.rds")
mod2 <- readRDS("models/dd_mod2.rds")
mod3 <- readRDS("models/dd_mod3.rds")
mod4 <- readRDS("models/dd_mod4.rds")
mod5 <- readRDS("models/dd_mod5.rds")

# 30-year Treatment log rev with constant acres
cmod0 <- readRDS("models/dd_cmod0.rds")
cmod1 <- readRDS("models/dd_cmod1.rds")
cmod2 <- readRDS("models/dd_cmod2.rds")
cmod3 <- readRDS("models/dd_cmod3.rds")
cmod4 <- readRDS("models/dd_cmod4.rds")
cmod5 <- readRDS("models/dd_cmod5.rds")

# SUR Panel estimates
sur_corn <- readRDS("models/sur_corn.rds")
sur_cotton <- readRDS("models/sur_cotton.rds")
sur_hay <- readRDS("models/sur_hay.rds")
sur_soybean <- readRDS("models/sur_soybean.rds")
sur_wheat <- readRDS("models/sur_wheat.rds")

# # 30-year Treatment log acres
# moda <- readRDS("models/dd_moda.rds")
# modb <- readRDS("models/dd_modb.rds")
# modc <- readRDS("models/dd_modc.rds")
# modd <- readRDS("models/dd_modd.rds")
# mode <- readRDS("models/dd_mode.rds")
# modf <- readRDS("models/dd_modf.rds")
# 
# # 30-year Treatment Individual Crop Yields
# mod1a <- readRDS("models/dd_mod1a.rds")
# mod2a <- readRDS("models/dd_mod2a.rds")
# mod3a <- readRDS("models/dd_mod3a.rds")
# mod4a <- readRDS("models/dd_mod4a.rds")
# mod5a <- readRDS("models/dd_mod5a.rds")
# 
# # 30-year Treatment Individual Crop Shares
# mod1b <- readRDS("models/dd_mod1b.rds")
# mod2b <- readRDS("models/dd_mod2b.rds")
# mod3b <- readRDS("models/dd_mod3b.rds")
# mod4b <- readRDS("models/dd_mod4b.rds")
# mod5b <- readRDS("models/dd_mod5b.rds")

# Bootstrapped Revenue Per Acre
# bs_mod1 <- readRDS("models/bs_mod1.rds")
# bs_mod2 <- readRDS("models/bs_mod2.rds")
# bs_mod3 <- readRDS("models/bs_mod3.rds")
# bs_mod4 <- readRDS("models/bs_mod4.rds")

  # mod3$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")] <- 
  #   multiply.100(mod3$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")])
  # 
  # mod4$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")] <- 
  # multiply.100(mod3$coefficients[c("dday0_10", "dday10_30", "dday30C", "prec", "prec_sq")])
  

  
  mod0$coefficients[2:4] <- adj_ln(mod0$coefficients[2:4])
  mod1$coefficients[2:4] <- adj_ln(mod1$coefficients[2:4])
  mod2$coefficients[2:4] <- adj_ln(mod2$coefficients[2:4])
  mod3$coefficients[7:9] <- adj_ln(mod3$coefficients[7:9])
  mod4$coefficients[7:9] <- adj_ln(mod4$coefficients[7:9])
  mod5$coefficients[7:9] <- adj_ln(mod5$coefficients[7:9])
  
  cmod0$coefficients[2:4] <- adj_ln(cmod0$coefficients[2:4])
  cmod1$coefficients[2:4] <- adj_ln(cmod1$coefficients[2:4])
  cmod2$coefficients[2:4] <- adj_ln(cmod2$coefficients[2:4])
  cmod3$coefficients[7:9] <- adj_ln(cmod3$coefficients[7:9])
  cmod4$coefficients[7:9] <- adj_ln(cmod4$coefficients[7:9])
  cmod5$coefficients[7:9] <- adj_ln(cmod5$coefficients[7:9])
  # 
  # moda$coefficients[2:4] <- adj_ln(moda$coefficients[2:4])
  # modb$coefficients[2:4] <- adj_ln(modb$coefficients[2:4])
  # modc$coefficients[2:4] <- adj_ln(modc$coefficients[2:4])
  # modd$coefficients[7:9] <- adj_ln(modd$coefficients[7:9])
  # mode$coefficients[7:9] <- adj_ln(mode$coefficients[7:9])
  # modf$coefficients[7:9] <- adj_ln(modf$coefficients[7:9])
  # 
  # moda$coefficients[2:4] <- adj_ln(moda$coefficients[2:4])
  # modb$coefficients[2:4] <- adj_ln(modb$coefficients[2:4])
  # modc$coefficients[2:4] <- adj_ln(modc$coefficients[2:4])
  # modd$coefficients[7:9] <- adj_ln(modd$coefficients[7:9])
  # mode$coefficients[7:9] <- adj_ln(mode$coefficients[7:9])
  # modf$coefficients[7:9] <- adj_ln(modf$coefficients[7:9])
  # 
  # mod1a$coefficients[7:9] <- adj_ln(mod1a$coefficients[7:9])
  # mod2a$coefficients[7:9] <- adj_ln(mod2a$coefficients[7:9])
  # mod3a$coefficients[7:9] <- adj_ln(mod3a$coefficients[7:9])
  # mod4a$coefficients[7:9] <- adj_ln(mod4a$coefficients[7:9])
  # mod5a$coefficients[7:9] <- adj_ln(mod5a$coefficients[7:9])
  # 
  # mod1b$coefficients[7:9] <- adj_ln(mod1b$coefficients[7:9])
  # mod2b$coefficients[7:9] <- adj_ln(mod2b$coefficients[7:9])
  # mod3b$coefficients[7:9] <- adj_ln(mod3b$coefficients[7:9])
  # mod4b$coefficients[7:9] <- adj_ln(mod4b$coefficients[7:9])
  # mod5b$coefficients[7:9] <- adj_ln(mod5b$coefficients[7:9])
  # 
  # bs_mod1$coefficients[2:4] <- adj_ln(bs_mod1$coefficients[2:4])
  # bs_mod2$coefficients[1:3] <- adj_ln(bs_mod2$coefficients[1:3])
  # bs_mod3$coefficients[7:9] <- adj_ln(bs_mod3$coefficients[7:9])
  # bs_mod4$coefficients[6:8] <- adj_ln(bs_mod4$coefficients[6:8])
  
  # 
  # modc1$coefficients[7:9] <- adj_ln(modc1$coefficients[7:9])
  # modco1$coefficients[7:9] <- adj_ln(modco1$coefficients[7:9])
  # modh1$coefficients[7:9] <- adj_ln(modh1$coefficients[7:9])
  # mods1$coefficients[7:9] <- adj_ln(mods1$coefficients[7:9])
  # modw1$coefficients[7:9] <- adj_ln(modw1$coefficients[7:9])

    # mod0$coefficients[c("tau", "omega", "did")] <- adj_ln(mod0$coefficients[c("tau", "omega", "did")])
  # mod1$coefficients[c("tau", "omega", "did")] <- adj_ln(mod1$coefficients[c("tau", "omega", "did")])
  # mod2$coefficients[c("tau", "omega", "did")] <- adj_ln(mod2$coefficients[c("tau", "omega", "did")])
  # mod3$coefficients[c("tau", "omega", "did")] <- adj_ln(mod3$coefficients[c("tau", "omega", "did")])
  # mod4$coefficients[c("tau", "omega", "did")] <- adj_ln(mod4$coefficients[c("tau", "omega", "did")])
  # mod5$coefficients[c("tau", "omega", "did")] <- adj_ln(mod5$coefficients[c("tau", "omega", "did")])

star1 <- stargazer(mod0, mod1, mod2, mod3, mod4, mod5,
                  align = FALSE, no.space = FALSE, 
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state"), 
                  omit.stat = c("ser", "f"),
                  title = "Difference-in-Difference Regression Model Explaining Crop Revenue per Acre", 
                  column.labels = c("Basic Model", "Basic Model", "Basic Model", 
                                    "Climate Model",  "Climate Model", "Climate Model"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"), 
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
                               "Precipitation", "Precipitation Squared", "Post - 0:1950-1980/1:1980-2010", 
                               "Treat - 0:cooled/1:warmed the most", "Treatment-effect"),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Weights", "--", "Acres", "Acres", "--", "Acres", "Acres"),
                           c("Fixed-effect", "--", "County", "County", "--", "County", "County"),
                           c("State-trend", "--", "Yes", "Yes", "--", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "State", "--", "--", "State")),
          notes.append = TRUE, notes.align = "l") 

#star1
notes1 <- "\\parbox{7.5in}{Note: Crop revenue is calculated as the total sum of individual crop revenue for five 
field crops where revenue per acre equals yield (per acre lbs) times average state-level crop prices. 
Dependent variables is the difference in the log of crop revenue per acre from 1950-1980 and 1980-2010 for those counties that warmed the most versus cooled the most. 
Treatment indicates the top third counties that warmed the most witin state from 1950-1980 and 1980-2010. Estimates in 
bold are statistically significant at 95\\%. Coefficients have been multiplied by 100. }"

loc1 <- which(star1 == "\\end{tabular} ") 
top1 <- star1[1:loc1]
bot1 <- star1[(loc1+1):length(star1)]
star1 <- c(top1, "\\vspace{1ex} ", notes1, bot1)
star1

star2 <- stargazer(cmod0, cmod1, cmod2, cmod3, cmod4, cmod5,
                  align = FALSE, no.space = FALSE, 
                  style = "aer", digits = 2,
                  omit = c("fips", "year", "state"), 
                  omit.stat = c("ser", "f"),
                  title = "Difference-in-Difference Regression Model Explaining Crop Revenue per Acre (constant acres)", 
                  column.labels = c("Basic Model", "Basic Model", "Basic Model", 
                                    "Climate Model",  "Climate Model", "Climate Model"),
          dep.var.labels = c("Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)",
                             "Log(Crop Revenue per Acre)", "Log(Crop Revenue per Acre)"), 
          covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
                               "Precipitation", "Precipitation Squared", "Post - 0:1950-1980/1:1980-2010", 
                               "Treat - 0:cooled/1:warmed the most", "Treatment-effect"),
          model.names = FALSE,  omit.table.layout = "n",
          apply.coef = multiply.100, apply.se = multiply.100,
          table.layout ="=dcm#-t-as=n",
          font.size = "footnotesize",
          add.lines = list(c("Weights", "--", "Acres", "Acres", "--", "Acres", "Acres"),
                           c("Fixed-effect", "--", "County", "County", "--", "County", "County"),
                           c("State-trend", "--", "Yes", "Yes", "--", "Yes", "Yes"),
                           c("Clusterd SE", "--", "--", "State", "--", "--", "State")),
          notes.append = FALSE, notes.align = "l") 

notes2 <- "\\parbox{7.5in}{Note: Crop revenue is calculated as the total sum of individual crop revenue for five 
field crops where revenue per acre equals yield (per acre lbs) times average state-level crop prices. 
Dependent variables is the difference in the log of crop revenue per acre from 1950-1980 and 1980-2010 for those counties that warmed the most versus cooled the most. 
Treatment indicates the top third counties that warmed the most witin state from 1950-1980 and 1980-2010. Estimates in 
bold are statistically significant at 95\\%. Coefficients have been multiplied by 100. }"

loc2 <- which(star2 == "\\end{tabular} ") 
top2 <- star2[2:loc2]
bot2 <- star2[(loc2+1):length(star2)]
star2 <- c(top2, "\\vspace{1ex} ", notes2, bot2)
star2


# SUR model results 
star3 <- stargazer(sur_corn, sur_cotton, sur_hay, sur_soybean, sur_wheat,
                   align = FALSE, no.space = FALSE, 
                   style = "aer", digits = 2,
                   omit = c("fips", "year"), 
                   omit.stat = c("ser", "f"),
                   title = "Seemingly Unrelated Regression (SUR) Model Explaining Crop Share", 
                   #column.labels = c("Basic Model", "Basic Model", "Climate Model", "Climate Model", "Climate Model"),
           dep.var.labels = c("Corn Share",  "Cotton Share", "Hay Share", "Soybean Share", "Wheat Share"),
           covariate.labels = c("Degree Days (0-10C)", "Degree Days (10-30C)", "Degree Days (30C)", 
                               "Precipitation", "Precipitation Squared"),
           model.names = FALSE, omit.table.layout = "n",
           apply.coef = multiply.100, apply.se = multiply.100,
           table.layout ="=dcm#-t-as=n",
           font.size = "footnotesize",
           #add.lines = list(c("Mean of Dep Variable", "9.30", "8.05",  "9.64", "9.17", "9.18"),
          #                  c("Fixed-effect", "State", "State", "State", "State", "State"),
          #                  c("Clusterd SE", "State", "State", "State", "State", "State")),
          notes.append = FALSE, notes.align = "l")
star3

notes3 <- "\\parbox{5.5in}{Note: Table reports SUR regression results where coefficients have been 
restricted to sum to zero across models (sum of predicted values across models equal one). Crop share is calculated 
as crop acres divided by sum of field crop acres in sample (corn, cotton, hay, soybean, and wheat). 
Dependent variables is the proportion change in crop acres.
Estimates in bold are statistically significant at 95\\%. 
Coefficients have been multiplied by 100. }"

loc3 <- which(star3 == "\\end{tabular} ") 
top3 <- star3[1:loc3]
bot3 <- star3[(loc3+1):length(star3)]
star3 <- c(top3, "\\vspace{1ex} ", notes3, bot3)
star3

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
cat("\\documentclass[10pt]{article}\n\\usepackage{graphicx}\n\\usepackage{pdflscape}\n\\usepackage{dcolumn}\n\\usepackage[a4paper, total={8in, 10in}]{geometry}\n\\begin{document}", file = "dd_adaptation_regression_tables.tex")
cat(star1, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
cat(star2, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
cat(star3, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
#cat("\\newpage", file = "dd_adaptation_regression_tables.tex", append = TRUE)
#cat(star4, file = "dd_adaptation_regression_tables.tex", sep = "\n", append = TRUE)
cat("\\end{document}", file = "dd_adaptation_regression_tables.tex", append = TRUE)
# Compile pdf
system("pdflatex dd_adaptation_regression_tables.tex")
system("sed -r 's/([0-9\\.]+)\\$\\^\\{\\*{1,3}\\}\\$/\\\\textbf{\\1}/g' dd_adaptation_regression_tables.tex > DD_tables.tex") 
system("pdflatex DD_tables.tex")
}

                  
