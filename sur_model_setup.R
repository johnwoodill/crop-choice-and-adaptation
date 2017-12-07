library(tidyverse)
library(lmtest)

setwd("/run/media/john/1TB/SpiderOak/Projects/crop-choice-and-adaptation/")
cropdat <- readRDS("data/full_ag_data.rds")

# Get SUR out files
sur_five <- readRDS("data/sur_out_five.rds")
sur_ten <- readRDS("data/sur_out_ten.rds")
sur_twenty <- readRDS("data/sur_out_twenty.rds")
sur_thirty <- readRDS("data/sur_out_thirty.rds")





