
##GTA 28, Chapter: Global assessment on resort to subsidies
#Request SE: 
# Refer to L inward as subsidies where “Affects competition in domestic markets” and all other subsidies as subsidies where “Affects competition in foreign markets”. 
# For all three jurisdictions, calculate for each year 2009-2020 what was the share of world goods trade covered by (a) all subsidies, (b) L inward and (c) all other subsidies (all harmfull).   
# Plot these findings on a single line chart. 
#Author: Silvan
#Date 19.8.2021

library(gtalibrary)
library(tidyverse)
library(rJava)
install.packages("rJava")

rm(list = ls())
gta_setwd()

#parameters

#paths
data.path = "0 dev/gta-28-sh/data/Global assessment/"


#define relevant intervention types
subsidy.intervention.types <- subset(gtalibrary::int.mast.types, mast.subchapter.id %in% c("L","P7","P8","P9"))$intervention.type
subsidy.intervention.types = subsidy.intervention.types[!subsidy.intervention.types == "Export-related non-tariff measure, nes"]
affects.domestic.markets = subset(gtalibrary::int.mast.types, mast.subchapter.id %in% c("L"))$intervention.type
affects.foreign.markets = subset(gtalibrary::int.mast.types, mast.subchapter.id %in% c("P7","P8","P9"))$intervention.type
affects.foreign.markets = affects.foreign.markets[!affects.foreign.markets == "Export-related non-tariff measure, nes"]
relevant.intervention.types = list("All subsidy types" = subsidy.intervention.types, "Affects competition in domestic markets" = affects.domestic.markets, "Affects competition in foreign markets" = affects.foreign.markets)


#define relevant juristictions
eu.countries = gtalibrary::country.names[country.names$is.eu == 1, "name"]
relevant.juristictions = list("EU" = eu.countries, "USA" = "United States of America", "China" = "China")


#inclue unpublished interventions?
include.unpublished = T


#get different goods types
# cpc.goods.codes = gtalibrary::cpc.names[cpc.names$cpc <500 & cpc.names$cpc.digit.level == 3, ]$cpc #get all product cpc codes
# cpc.goods.codes.agricultural = gtalibrary::cpc.names[cpc.names$cpc <100 & cpc.names$cpc.digit.level == 3, ]$cpc
# cpc.goods.codes.not.agricultural = gtalibrary::cpc.names[cpc.names$cpc >= 100 & cpc.names$cpc <500 & cpc.names$cpc.digit.level == 3, ]$cpc
# relevant.goods = list("Only Agricultural" = cpc.goods.codes.agricultural, "Not Agricultural" = cpc.goods.codes.not.agricultural)
# 

#use HS codes
agricultural.hs = gtalibrary::hs.codes[hs.codes$hs.code < 250000,"hs.code" ]
non.agricultural.hs = gtalibrary::hs.codes[hs.codes$hs.code >= 250000, "hs.code"] 
relevant.goods = list("Only Agricultural" = agricultural.hs, "Not Agricultural" = non.agricultural.hs)




#relevant years
years.to.include = c(2009, 2020)


#find interventions to exclude
gta_data_slicer(affected.flows = "outward subsidy",
                keep.affected = T, 
                eligible.firms = "firm-specific", 
                keep.firms = T,
                add.unpublished = include.unpublished
                )

ids.to.exclude = as.character(unique(master.sliced$intervention.id))



################################################################################
#1. get data -------------------------------------------------------------------


#Task 6 ------------------------------------------------------------------------


#initiate dataframe to fill
data6 = data.frame("Country"  = NA, 
                   "Subsidy category"  = NA, 
                   "Importing country"  = NA ,
                   "Exporting country"  = NA,
                   "Number of interventions affecting exported product"  = NA,
                   "Trade coverage estimate for 2009"  = NA,
                   "Trade coverage estimate for 2010"  = NA,
                   "Trade coverage estimate for 2011"  = NA,
                   "Trade coverage estimate for 2012" = NA ,
                   "Trade coverage estimate for 2013" = NA, 
                   "Trade coverage estimate for 2014" = NA,
                   "Trade coverage estimate for 2015" = NA,
                   "Trade coverage estimate for 2016" = NA,
                   "Trade coverage estimate for 2017" = NA,                
                   "Trade coverage estimate for 2018" = NA,
                   "Trade coverage estimate for 2019" = NA,
                   "Trade coverage estimate for 2020" = NA, 
                   check.names = F)
  

#to fill the dataframe                 
line = 0


#loop through all combinations of jurisdiction and intervention types 
for(i in 1:length(relevant.juristictions)){
  for(y in 1:length(relevant.intervention.types)){
    
    line = line + 1 
    
    gta_trade_coverage(
      gta.evaluation = c("Red","Amber") ,
      implementers = relevant.juristictions[[i]], 
      keep.implementer = T, 
      intervention.types = relevant.intervention.types[[y]], 
      keep.type = T, 
      add.unpublished = include.unpublished, 
      coverage.period = years.to.include, 
      intervention.ids =  ids.to.exclude ,
      keep.interventions = F
    )
    
    data6 = rbind(data6, cbind("Country" = names(relevant.juristictions)[i],
                           "Subsidy category" = names(relevant.intervention.types[y]) , 
                           trade.coverage.estimates[1, ]
                        )
                  )
                  
   print(line) 
  }

  line = line + 1
}

data6 = data6[2:nrow(data6),]



#Task 7 ------------------------------------------------------------------------


#initiate dataframe to fill
data7 = data.frame("Country"  = NA, 
                   "Subsidy category"  = NA, 
                   "Good type" = NA,
                   "Importing country"  = NA ,
                   "Exporting country"  = NA,
                   "Number of interventions affecting exported product"  = NA,
                   "Trade coverage estimate for 2009"  = NA,
                   "Trade coverage estimate for 2010"  = NA,
                   "Trade coverage estimate for 2011"  = NA,
                   "Trade coverage estimate for 2012" = NA ,
                   "Trade coverage estimate for 2013" = NA, 
                   "Trade coverage estimate for 2014" = NA,
                   "Trade coverage estimate for 2015" = NA,
                   "Trade coverage estimate for 2016" = NA,
                   "Trade coverage estimate for 2017" = NA,                
                   "Trade coverage estimate for 2018" = NA,
                   "Trade coverage estimate for 2019" = NA,
                   "Trade coverage estimate for 2020" = NA,                 
                   check.names = F)

#to fill the dataframe                 
line = 0

#loop through all combinations of jurisdiction and intervention types 
for(i in 1:length(relevant.juristictions)) {
  for (y in 1:length(relevant.intervention.types)) {
    for (z in 1:length(relevant.goods)) {
      
      line = line + 1
      
      gta_trade_coverage(
        gta.evaluation = c("Red","Amber"),
        cpc.sectors = relevant.goods[[z]],
        keep.cpc = T,
        implementers = relevant.juristictions[[i]],
        keep.implementer = T,
        intervention.types = relevant.intervention.types[[y]],
        keep.type = T, 
        add.unpublished = include.unpublished, 
        coverage.period = years.to.include, 
        intervention.ids =  ids.to.exclude ,
        keep.interventions = F
      )
      
      
      data7 = rbind(
        data7,
        cbind(
          "Country" = names(relevant.juristictions)[i],
          "Subsidy category" = names(relevant.intervention.types[y]) ,
          "Good type" = names(relevant.goods[z]),
          trade.coverage.estimates[1,]
        )
      )
      
      print(line)
    }
    
  }
  
}
#delete unnecessary NA row
data7 = data7[2:nrow(data7),]

################################################################################
#2. save data ------------------------------------------------------------------

saveRDS(data6, paste0(data.path, "Task.6.subsidy.trade.share.undifferentiated.RData"))
saveRDS(data7, paste0(data.path, "Task.7.subsidy.trade.share.differentiated.RData"))
