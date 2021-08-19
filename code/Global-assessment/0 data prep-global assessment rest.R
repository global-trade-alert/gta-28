
##GTA 28, Chapter: Global assessment on resort to subsidies
#Request SE: 
# Refer to L inward as subsidies where “Affects competition in domestic markets” and all other subsidies as subsidies where “Affects competition in foreign markets”. 
# For all three jurisdictions, calculate for each year 2009-2020 what was the share of world goods trade covered by (a) all subsidies, (b) L inward and (c) all other subsidies (all harmfull).   
# Plot these findings on a single line chart. 
#Author: Silvan
#Date 19.8.2021

library(gtalibrary)
library(tidyverse)

rm(list = ls())
gta_setwd()

#parameters


#subsidy.intervention.types <- subset(gtalibrary::int.mast.types, mast.subchapter.id %in% c("L","P7","P8","P9"))$intervention.type

affects.domestic.markets = "L"
affects.foreign.markets = c("P7","P8","P9")

eu.countries = countries[countries$is.eu == 1, "name"]
relevant.juristictions = c(eu.countries, "United States of America", "China")

################################################################################
#1. get data -------------------------------------------------------------------

gta_trade_coverage()