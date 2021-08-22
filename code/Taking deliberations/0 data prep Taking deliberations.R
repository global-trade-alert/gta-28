# 1. 
# Go to the WTO website (https://www.wto.org/english/tratop_e/scm_e/scm_e.htm)  and download the statistics for the number of countervailing duty investigations launched each year since 2000. 
# Please produce a stacked bar chart showing the number of investigations initiated by China, EU28, and USA, and rest of the world. 
# Please compare this stacked bar chart with a comparable on produced for the years 2009 on using the GTA’s own data (Intervention type “anti-subsidy”; use announcement date for init year; ask Josse to confirm correspondence.). 
# Hopefully our coverage is better.

#mail by SE: You are right to write that the WTO statistics count the number of investigations. 
#So the parallel in our case is the number of anti-subsidy measures of any type (including those that did not result in duties). 
#For the moment, I would include the duty extensions as they typically follow a further investigation.

#3.
# Please take a look at the charts on pages 3 and 4 of the following document: https://docs.wto.org/dol2fe/Pages/SS/directdoc.aspx?filename=q:/G/SCM/W546R12.pdf&Open=True. 
# Please suggest an interesting way of highlighting how few WTO members have made subsidy notifications. 
# Maybe a stacked bar chart (based on the data on page 4) makes sense plus an extra line indicating the number of WTO members in each year 
# (this number grows and is the number reported above the stacked bars in the figure on page 3)?






#Author: Silvan
#Date: 17.8.2021
#Supervisor Piotr

library(openxlsx)
library(gtalibrary)
library(lubridate)
library(tidyverse)
library(splitstackshape)

rm(list = ls())
gta_setwd()

#parameters
path = "0 dev/gta-28-sh/code/taking deliberations/"
path.data = "0 dev/gta-28-sh/data/Taking deliberations/"

#relevant juristictopms
juristictions = c("European Union2", "United States", "China")
to.exclud = c("Botswana1", "Eswatini1", "Lesotho1", "Namibia1", "Kazakhstan3") #excluded since they would be double reports (e.g. Botswana is reported together with South Africa, but both Botswana and South Africa are listed seperately)
years = as.character(c(2000:2020))
eu = gtalibrary::country.names[country.names$is.eu, ]$name
implementing.juristiction = c("United States of America", "China", eu)


#include unpublished
include.unpublished = F

#parameters for 
import.restrictions = int.mast.types$intervention.type[int.mast.types$is.at.the.border==1]
subsidy.intervention.types <- subset(gtalibrary::int.mast.types, mast.subchapter.id %in% c("L","P7","P8","P9"))$intervention.type

################################################################################
#1. Get data -------------------------------------------------------------------

data1 = readxl::read_xls(paste0(path.data, "Kopie von CV_InitiationsByRepMem.xls"), skip = 1)



################################################################################
#2. prepare data ---------------------------------------------------------------

################################################################################
#2.1. data for first chart ------------------------------------------------------




#reduce to relevant data
names(data1) = c("reporting.member", 1995:2020, "total")
data1 = data.frame(data1[1:31,] %>% select(c(reporting.member, years))) %>%
                                      filter(!reporting.member %in% to.exclud)

#data1 = data.frame(lapply(data1, function(x) gsub("[*]", "", x)))
data1[, 2:ncol(data1)] = as.numeric(unlist(data1[, 2:ncol(data1)]))
names(data1) = c("reporting.member", 2000:2020)

#calculate rest of world numers
rest.of.the.world = colSums(subset(data1, !reporting.member %in% juristictions)[,2:ncol(data1) ], na.rm = T)
rest.of.the.world = c("rest.of.the.world", rest.of.the.world)
data1 = rbind(data1[data1$reporting.member %in% juristictions,], rest.of.the.world)
data1[data1$reporting.member == "European Union2", "reporting.member"] = "EU"
data1[data1$reporting.member == "United States", "reporting.member"] = "United States of America"

data1.wto = data1


#gather and adjust to later add to other data
data1.wto = gather(data1.wto, key = "years", value = "investigations", 2:ncol(data1.wto))
colnames(data1.wto) = c("Country", "Year", "Investigations")
data1.wto$Data = "WTO"


#get GTA data
gta_data_slicer(announcement.period = c("2009-01-01", NA), 
                intervention.types = "Anti-subsidy", 
                keep.type = T, 
                add.unpublished = include.unpublished
                 
)

#adjust implementer to EU and rest of world
eu.members = gtalibrary::country.names[country.names$is.eu == TRUE, ]$name
master.sliced$implementing.jurisdiction = as.character(master.sliced$implementing.jurisdiction)
master.sliced[master.sliced$implementing.jurisdiction %in% eu.members ,"implementing.jurisdiction" ] = "EU"
master.sliced = unique(master.sliced)
master.sliced[!master.sliced$implementing.jurisdiction %in% c("EU", "United States of America", "China") ,"implementing.jurisdiction" ] = "rest.of.the.world"


#reduce to relevant columns
master.sliced = master.sliced %>% select(c("implementing.jurisdiction", "date.announced", ))
master.sliced$date.announced = year(master.sliced$date.announced)

#aggregate
data1.gta = data.frame(table(master.sliced))
colnames(data1.gta) = c("Country", "Year", "Investigations")
data1.gta$Data = "GTA"

#put data together
data1 = rbind(data1.gta, data1.wto)


################################################################################
#2.2. data for second chart ----------------------------------------------------


#2.2.1 get all subsidy interventions of the relevant countries
#get all L subsidy interventions
gta_data_slicer(implementing.country = implementing.juristiction, 
                keep.implementer = T, 
                mast.chapters = "L", 
                keep.mast = T, 
                affected.country = implementing.juristiction, 
                keep.affected = T, 
                add.unpublished = include.unpublished
                
)

#reduce to relevant interventions
data2 = master.sliced[master.sliced$affected.jurisdiction %in% implementing.juristiction, ] #only get right affecte juristictions
data2$affected.jurisdiction = ifelse(data2$affected.jurisdiction %in% eu, "EU28", data2$affected.jurisdiction) #change EU member states to EU
data2$implementing.jurisdiction = ifelse(data2$implementing.jurisdiction %in% eu, "EU28", data2$implementing.jurisdiction)
data2 = data2 %>% select( - c("affected.sector","a.un", "i.un")) %>%
                    filter(!data2$implementing.jurisdiction == data2$affected.jurisdiction) #filter so we do not have multiple intervention doubled 
data2 = unique(data2)
length(data2$intervention.id[table(data2$intervention.id)>2]) #no more than 2 interventios per intervention id 

#find single HS codes
data2 = cSplit(data2, "affected.product", direction = "long")
data2$affected.product = ifelse(nchar(data2$affected.product)<6, paste0("0", data2$affected.product) ,data2$affected.product ) #cSplit transforms column to numeric and erases 0

#quick check
min(nchar(na.omit(data2$affected.product)))
max(nchar(na.omit(data2$affected.product)))




#get date/product pairs 
china.subsidy.dates = na.omit(unique(data2[data2$implementing.jurisdiction == 42, c("date.announced", "affected.product")]))
usa.subsidy.dates = unique(data2[data2$implementing.jurisdiction == 221, c("date.announced", "affected.product")])
eu.subsidy.dates = unique(data2[data2$implementing.jurisdiction == "EU28",c("date.announced", "affected.product") ])




#2.2.2 get all harmful subsidy interventions for the relevant countries and calculate if they are inacted after the mentioned timeframes

different.intervention.type.lists = list(subsidy.intervention.types, import.restrictions)


#calculate all the percentages for both intervention type lists to avoid basically copying code
for (a in different.intervention.type.lists) {
  #get all harmfull subsidy interventions of any type
  gta_data_slicer(
    implementing.country = implementing.juristiction,
    keep.implementer = T,
    intervention.types = a,
    keep.type = T,
    affected.country = implementing.juristiction,
    keep.affected = T,
    gta.evaluation = c("Red", "Amber")
  )
  
  
  
  #some cleaning
  data2 = master.sliced[master.sliced$affected.jurisdiction %in% implementing.juristiction,] #only get right affecte juristictions
  data2$affected.jurisdiction = ifelse(data2$affected.jurisdiction %in% eu,
                                       "EU28",
                                       data2$affected.jurisdiction) #change EU member states to EU
  data2$implementing.jurisdiction = ifelse(data2$implementing.jurisdiction %in% eu,
                                           "EU28",
                                           data2$implementing.jurisdiction)
  data2 = data2 %>% select(-c("affected.sector", "a.un", "i.un")) %>%
    filter(!data2$implementing.jurisdiction == data2$affected.jurisdiction) #filter so we do not have multiple intervention doubled
  data2 = unique(data2)
  length(data2$intervention.id[table(data2$intervention.id) > 2]) #no more than 2 interventios per intervention id
  
  #find single HS codes
  data2 = cSplit(data2, "affected.product", direction = "long")
  data2$affected.product = ifelse(
    nchar(data2$affected.product) < 6,
    paste0("0", data2$affected.product) ,
    data2$affected.product
  ) #cSplit transforms column to numeric and erases 0
  
  #quick check
  min(nchar(na.omit(data2$affected.product)))
  max(nchar(na.omit(data2$affected.product)))
  
  
  
  
  #get all the dates on which china implemented a subsidy that hurts the US and for EU
  china.us.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 42 &
                                                   (data2$affected.jurisdiction == 221), c("date.announced", "affected.product")])
  china.eu.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 42 &
                                                   (data2$affected.jurisdiction == "EU28"), c("date.announced", "affected.product")])
  
  #same for the US
  usa.china.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 221 &
                                                    (data2$affected.jurisdiction == 42), c("date.announced", "affected.product")])
  usa.eu.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 221 &
                                                 (data2$affected.jurisdiction == "EU28"), c("date.announced", "affected.product")])
  
  #and for EU
  eu.china.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == "EU28" &
                                                   (data2$affected.jurisdiction == 42), c("date.announced", "affected.product")])
  eu.usa.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == "EU28" &
                                                 (data2$affected.jurisdiction == 221), c("date.announced", "affected.product")])
  
  
  
  
  
  #create tables with intervals after announcement of subsidies
  china.table = data.frame(
    "subsidy.dates" = china.subsidy.dates$date.announced,
    "affected.product" = china.subsidy.dates$affected.product,
    "6m.interval" = interval(
      ymd(china.subsidy.dates$date.announced),
      china.subsidy.dates$date.announced  %m+%  months(6)
    ),
    "12m.interval" = interval(
      ymd(china.subsidy.dates$date.announced),
      china.subsidy.dates$date.announced  %m+%  months(12)
    ),
    "24m.interval" = interval(
      ymd(china.subsidy.dates$date.announced),
      china.subsidy.dates$date.announced  %m+%  months(24)
    )
  )
  
  usa.table = data.frame(
    "subsidy.dates" = usa.subsidy.dates$date.announced,
    "affected.product" = usa.subsidy.dates$affected.product,
    "6m.interval" = interval(
      ymd(usa.subsidy.dates$date.announced),
      usa.subsidy.dates$date.announced  %m+%  months(6)
    ),
    "12m.interval" = interval(
      ymd(usa.subsidy.dates$date.announced),
      usa.subsidy.dates$date.announced  %m+%  months(12)
    ),
    "24m.interval" = interval(
      ymd(usa.subsidy.dates$date.announced),
      usa.subsidy.dates$date.announced  %m+%  months(24)
    )
  )
  
  eu.table = data.frame(
    "subsidy.dates" = eu.subsidy.dates$date.announced,
    "affected.product" = eu.subsidy.dates$affected.product,
    "6m.interval" = interval(
      ymd(eu.subsidy.dates$date.announced),
      eu.subsidy.dates$date.announced  %m+%  months(6)
    ),
    "12m.interval" = interval(
      ymd(eu.subsidy.dates$date.announced),
      eu.subsidy.dates$date.announced  %m+%  months(12)
    ),
    "24m.interval" = interval(
      ymd(eu.subsidy.dates$date.announced),
      eu.subsidy.dates$date.announced  %m+%  months(24)
    )
  )
  
  #discard of not used HS to not waste time in calculations
  china.us.subsidy.response.dates = china.us.subsidy.response.dates[china.us.subsidy.response.dates$affected.product %in% usa.table$affected.product,] #
  china.eu.subsidy.response.dates = china.eu.subsidy.response.dates[china.eu.subsidy.response.dates$affected.product %in% eu.table$affected.product,] #
  
  usa.china.subsidy.response.dates = usa.china.subsidy.response.dates[usa.china.subsidy.response.dates$affected.product %in% china.table$affected.product,] #
  usa.eu.subsidy.response.dates = usa.eu.subsidy.response.dates[usa.eu.subsidy.response.dates$affected.product %in% eu.table$affected.product,] #
  
  eu.china.subsidy.response.dates = eu.china.subsidy.response.dates[eu.china.subsidy.response.dates$affected.product %in% china.table$affected.product,] #
  eu.usa.subsidy.response.dates = eu.usa.subsidy.response.dates[eu.usa.subsidy.response.dates$affected.product %in% china.table$affected.product,] #
  
  
  
  
  
  #go through all all combinations to check if there is a pair for which HS codes matches and date is in interval)
  
  ##China with retaliation usa
  
  #6m
  for (i in 1:nrow(china.table)) {
    possible.matches = usa.china.subsidy.response.dates[usa.china.subsidy.response.dates$date.announced %within% china.table[i, "X6m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% china.table[i, "affected.product"], ] #select by product
    
    if (nrow(possible.matches) > 0) {
      china.table$subsidy.usa.china.yes.6m[i] = 1
    }
    else
      china.table$subsidy.usa.china.yes.6m[i] = 0
  }
  
  #12m
  for (i in 1:nrow(china.table)) {
    possible.matches = usa.china.subsidy.response.dates[usa.china.subsidy.response.dates$date.announced %within% china.table[i, "X12m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% china.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      china.table$subsidy.usa.china.yes.12m[i] = 1
    }
    else
      china.table$subsidy.usa.china.yes.12m[i] = 0
  }
  
  #24m
  for (i in 1:nrow(china.table)) {
    possible.matches = usa.china.subsidy.response.dates[usa.china.subsidy.response.dates$date.announced %within% china.table[i, "X24m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% china.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      china.table$subsidy.usa.china.yes.24m[i] = 1
    }
    else
      china.table$subsidy.usa.china.yes.24m[i] = 0
  }
  
  ##China with retaliation EU
  
  #6m
  for (i in 1:nrow(china.table)) {
    possible.matches = eu.china.subsidy.response.dates[eu.china.subsidy.response.dates$date.announced %within% china.table[i, "X6m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% china.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      china.table$subsidy.eu.china.yes.6m[i] = 1
    }
    else
      china.table$subsidy.eu.china.yes.6m[i] = 0
  }
  
  #12m
  for (i in 1:nrow(china.table)) {
    possible.matches = eu.china.subsidy.response.dates[eu.china.subsidy.response.dates$date.announced %within% china.table[i, "X12m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% china.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      china.table$subsidy.eu.china.yes.12m[i] = 1
    }
    else
      china.table$subsidy.eu.china.yes.12m[i] = 0
  }
  
  #24m
  for (i in 1:nrow(china.table)) {
    possible.matches = eu.china.subsidy.response.dates[eu.china.subsidy.response.dates$date.announced %within% china.table[i, "X24m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% china.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      china.table$subsidy.eu.china.yes.24m[i] = 1
    }
    else
      china.table$subsidy.eu.china.yes.24m[i] = 0
  }
  
  
  
  
  ##USA with retaliation China
  
  #6m
  for (i in 1:nrow(usa.table)) {
    possible.matches = china.us.subsidy.response.dates[china.us.subsidy.response.dates$date.announced %within% usa.table[i, "X6m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% usa.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      usa.table$subsidy.china.usa.yes.6m[i] = 1
    }
    else
      usa.table$subsidy.china.usa.yes.6m[i] = 0
  }
  
  #12m
  for (i in 1:nrow(usa.table)) {
    possible.matches = china.us.subsidy.response.dates[china.us.subsidy.response.dates$date.announced %within% usa.table[i, "X12m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% usa.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      usa.table$subsidy.china.usa.yes.12m[i] = 1
    }
    else
      usa.table$subsidy.china.usa.yes.12m[i] = 0
  }
  
  #24m
  for (i in 1:nrow(usa.table)) {
    possible.matches = china.us.subsidy.response.dates[china.us.subsidy.response.dates$date.announced %within% usa.table[i, "X24m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% usa.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      usa.table$subsidy.china.usa.yes.24m[i] = 1
    }
    else
      usa.table$subsidy.china.usa.yes.24m[i] = 0
  }
  
  ##USA with retaliation EU
  
  #6m
  for (i in 1:nrow(usa.table)) {
    possible.matches = eu.usa.subsidy.response.dates[eu.usa.subsidy.response.dates$date.announced %within% usa.table[i, "X6m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% usa.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      usa.table$subsidy.eu.usa.yes.6m[i] = 1
    }
    else
      usa.table$subsidy.eu.usa.yes.6m[i] = 0
  }
  
  #12m
  for (i in 1:nrow(usa.table)) {
    possible.matches = eu.usa.subsidy.response.dates[eu.usa.subsidy.response.dates$date.announced %within% usa.table[i, "X12m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% usa.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      usa.table$subsidy.eu.usa.yes.12m[i] = 1
    }
    else
      usa.table$subsidy.eu.usa.yes.12m[i] = 0
  }
  
  #24m
  for (i in 1:nrow(usa.table)) {
    possible.matches = eu.usa.subsidy.response.dates[eu.usa.subsidy.response.dates$date.announced %within% usa.table[i, "X24m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% usa.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      usa.table$subsidy.eu.usa.yes.24m[i] = 1
    }
    else
      usa.table$subsidy.eu.usa.yes.24m[i] = 0
  }
  
  
  ##EU with retaliation USA
  
  #6m
  for (i in 1:nrow(eu.table)) {
    possible.matches = usa.eu.subsidy.response.dates[usa.eu.subsidy.response.dates$date.announced %within% eu.table[i, "X6m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% eu.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      eu.table$subsidy.usa.eu.yes.6m[i] = 1
    }
    else
      eu.table$subsidy.usa.eu.yes.6m[i] = 0
  }
  
  #12m
  for (i in 1:nrow(eu.table)) {
    possible.matches = usa.eu.subsidy.response.dates[usa.eu.subsidy.response.dates$date.announced %within% eu.table[i, "X12m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% eu.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      eu.table$subsidy.usa.eu.yes.12m[i] = 1
    }
    else
      eu.table$subsidy.usa.eu.yes.12m[i] = 0
  }
  
  #24m
  for (i in 1:nrow(eu.table)) {
    possible.matches = usa.eu.subsidy.response.dates[usa.eu.subsidy.response.dates$date.announced %within% eu.table[i, "X24m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% eu.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      eu.table$subsidy.usa.eu.yes.24m[i] = 1
    }
    else
      eu.table$subsidy.usa.eu.yes.24m[i] = 0
  }
  
  ##EU with retaliation China
  
  #6m
  for (i in 1:nrow(eu.table)) {
    possible.matches = eu.china.subsidy.response.dates[eu.china.subsidy.response.dates$date.announced %within% eu.table[i, "X6m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% eu.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      eu.table$subsidy.china.eu.yes.6m[i] = 1
    }
    else
      eu.table$subsidy.china.eu.yes.6m[i] = 0
  }
  
  #12m
  for (i in 1:nrow(eu.table)) {
    possible.matches = eu.china.subsidy.response.dates[eu.china.subsidy.response.dates$date.announced %within% eu.table[i, "X12m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% eu.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      eu.table$subsidy.china.eu.yes.12m[i] = 1
    }
    else
      eu.table$subsidy.china.eu.yes.12m[i] = 0
  }
  
  #24m
  for (i in 1:nrow(eu.table)) {
    possible.matches = eu.china.subsidy.response.dates[eu.china.subsidy.response.dates$date.announced %within% eu.table[i, "X24m.interval", ], ] #select by interval
    possible.matches = possible.matches[possible.matches$affected.product %in% eu.table[i, "affected.product"], ]
    
    if (nrow(possible.matches) > 0) {
      eu.table$subsidy.china.eu.yes.24m[i] = 1
    }
    else
      eu.table$subsidy.china.eu.yes.24m[i] = 0
  }
  

  
  
  #calculate percentages
  
  china.percentages.subsidies = data.frame(
    "region" = c("USA", "EU"),
    "6 Months" = c(
      sum(china.table$subsidy.usa.china.yes.6m) / nrow(china.table),
      sum(china.table$subsidy.eu.china.yes.6m) / nrow(china.table)
    ),
    "12 Months" = c(
      sum(china.table$subsidy.usa.china.yes.12m) / nrow(china.table),
      sum(china.table$subsidy.eu.china.yes.12m) / nrow(china.table)
    ),
    "24 Months" = c(
      sum(china.table$subsidy.usa.china.yes.24m) / nrow(china.table),
      sum(china.table$subsidy.eu.china.yes.24m) / nrow(china.table)
    )
  )
  
  usa.percentages.subsidies = data.frame(
    "region" = c("China", "EU"),
    "6 Months" = c(
      sum(usa.table$subsidy.china.usa.yes.6m) / nrow(usa.table),
      sum(usa.table$subsidy.eu.usa.yes.6m) / nrow(usa.table)
    ),
    "12 Months" = c(
      sum(usa.table$subsidy.china.usa.yes.12m) / nrow(usa.table),
      sum(usa.table$subsidy.eu.usa.yes.12m) / nrow(usa.table)
    ),
    "24 Months" = c(
      sum(usa.table$subsidy.china.usa.yes.24m) / nrow(usa.table),
      sum(usa.table$subsidy.eu.usa.yes.24m) / nrow(usa.table)
    )
  )
  
  eu.percentages.subsidies = data.frame(
    "region" = c("USA", "China"),
    "6 Months" = c(
      sum(eu.table$subsidy.usa.eu.yes.6m) / nrow(eu.table),
      sum(eu.table$subsidy.china.eu.yes.6m) / nrow(eu.table)
    ),
    "12 Months" = c(
      sum(eu.table$subsidy.usa.eu.yes.12m) / nrow(eu.table),
      sum(eu.table$subsidy.china.eu.yes.12m) / nrow(eu.table)
    ),
    "24 Months" = c(
      sum(eu.table$subsidy.usa.eu.yes.24m) / nrow(eu.table),
      sum(eu.table$subsidy.china.eu.yes.24m) / nrow(eu.table)
    )
  )
  
  
  assign(paste0(a, ".", "eu.percentages"), eu.percentages.subsidies)
  assign(paste0(a, ".", "usa.percentages"), usa.percentages.subsidies)
  assign(paste0(a, ".", "china.percentages"), china.percentages.subsidies)
  
  

}


#rename for clarity
china.percentages.subsidies = `Capital injection and equity stakes (including bailouts).china.percentages`
usa.percentages.subsidies = `Capital injection and equity stakes (including bailouts).usa.percentages`
eu.percentages.subsidies = `Capital injection and equity stakes (including bailouts).eu.percentages`
  
china.percentages.restrictions = `Anti-circumvention.china.percentages`
usa.percentages.restrictions = `Anti-circumvention.usa.percentages`
eu.percentages.restrictions = `Anti-circumvention.eu.percentages`
  


#2.2.3 prepare for finale table
#idea is to make one large table, therefore add tables together

#change names 
names(china.percentages.subsidies) = c("Region", "china subsidies within 6 Months", "china subsidies within 12 Months", "china subsidies within 24 Months")
names(usa.percentages.subsidies) = c("Region", "usa subsidies within 6 Months", "usa subsidies within 12 Months", "usa subsidies within 24 Months")
names(eu.percentages.subsidies) = c("Region", "eu subsidies within 6 Months", "eu subsidies within 12 Months", "eu subsidies within 24 Months")

names(china.percentages.restrictions) = c("Region", "china restrictions within 6 Months", "china restrictions within 12 Months", "china restrictions within 24 Months")
names(usa.percentages.restrictions) = c("Region", "usa restrictions within 6 Months", "usa restrictions within 12 Months", "usa restrictions within 24 Months")
names(eu.percentages.restrictions) = c("Region", "eu restrictions within 6 Months", "eu restrictions within 12 Months", "eu restrictions within 24 Months")

#bind bind tables together and make some adjustments to make them compatible
china.table = cbind(china.percentages.subsidies, china.percentages.restrictions)
china.table = rbind(c("China", rep(NA, ncol(china.table)-1)), china.table)
china.table = china.table %>% select(-c("Region"))
china.table = china.table %>% select(-c("Region"))

usa.table = cbind(usa.percentages.subsidies, usa.percentages.restrictions)
usa.table = rbind(usa.table[1, ], c("USA", rep(NA, ncol(usa.table)-1)), usa.table[2, ])
usa.table = usa.table %>% select(-c("Region"))
usa.table = usa.table %>% select(-c("Region"))

eu.table = cbind(eu.percentages.subsidies, eu.percentages.restrictions)
eu.table = rbind(eu.table, c("EU", rep(NA, ncol(eu.table)-1)))
eu.table = rbind(eu.table[2, ], eu.table[1, ], eu.table[3, ])
eu.table = eu.table %>% select(-c("Region"))
eu.table = eu.table %>% select(-c("Region"))

#create overall table
total.table = cbind(china.table, usa.table, eu.table)
total.table = apply(total.table,MARGIN = 2,  FUN = function(x)round(as.numeric(x), 3))
total.table = cbind(Regions = c("China", "USA", "EU"), total.table)


data2 = data.frame(total.table)


####### old---------------------
# ###china
# #we check if in the intervals between the time china announced a subsidy that hurt the US and 6/12/24 months the US also announced a subsidy that hurts china
# #USA
# subsidy.usa.china.yes.6m = ifelse(as.numeric(unlist(lapply(china.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(usa.china.subsidy.response.dates$date.announced) %within% x), "1", "0")))))
#
#
# subsidy.usa.china.yes.12m = as.numeric(unlist(lapply(china.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(usa.china.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.usa.china.yes.24m = as.numeric(unlist(lapply(china.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(usa.china.subsidy.response.dates) %within% x), "1", "0")))))
#
# #EU
# subsidy.eu.china.yes.6m = as.numeric(unlist(lapply(china.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(eu.china.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.eu.china.yes.12m = as.numeric(unlist(lapply(china.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(eu.china.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.eu.china.yes.24m = as.numeric(unlist(lapply(china.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(eu.china.subsidy.response.dates) %within% x), "1", "0")))))
#
# #make a table
# china.table = cbind(china.table, subsidy.usa.china.yes.6m, subsidy.usa.china.yes.12m, subsidy.usa.china.yes.24m, subsidy.eu.china.yes.6m, subsidy.eu.china.yes.12m, subsidy.eu.china.yes.24m )
#
#
# ###usa
# #we check if in the intervals between the time china announced a subsidy that hurt the US and 6/12/24 months the US also announced a subsidy that hurts china
# #China
# subsidy.china.usa.yes.6m = as.numeric(unlist(lapply(usa.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(china.us.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.china.usa.yes.12m = as.numeric(unlist(lapply(usa.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(china.us.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.china.usa.yes.24m = as.numeric(unlist(lapply(usa.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(china.us.subsidy.response.dates) %within% x), "1", "0")))))
#
# #EU
# subsidy.eu.usa.yes.6m = as.numeric(unlist(lapply(usa.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(eu.usa.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.eu.usa.yes.12m = as.numeric(unlist(lapply(usa.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(eu.usa.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.eu.usa.yes.24m = as.numeric(unlist(lapply(usa.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(eu.usa.subsidy.response.dates) %within% x), "1", "0")))))
#
# #make a table
# usa.table = cbind(usa.table, subsidy.china.usa.yes.6m, subsidy.china.usa.yes.12m, subsidy.china.usa.yes.24m, subsidy.eu.usa.yes.6m, subsidy.eu.usa.yes.12m, subsidy.eu.usa.yes.24m )
#
#
# ###EU
# #we check if in the intervals between the time china announced a subsidy that hurt the US and 6/12/24 months the US also announced a subsidy that hurts china
# #USA
# subsidy.usa.eu.yes.6m = as.numeric(unlist(lapply(eu.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(usa.eu.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.usa.eu.yes.12m = as.numeric(unlist(lapply(eu.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(usa.eu.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.usa.eu.yes.24m = as.numeric(unlist(lapply(eu.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(usa.eu.subsidy.response.dates) %within% x), "1", "0")))))
#
# #China
# subsidy.china.eu.yes.6m = as.numeric(unlist(lapply(eu.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(china.eu.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.china.eu.yes.12m = as.numeric(unlist(lapply(eu.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(china.eu.subsidy.response.dates) %within% x), "1", "0")))))
# subsidy.china.eu.yes.24m = as.numeric(unlist(lapply(eu.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(china.eu.subsidy.response.dates) %within% x), "1", "0")))))
#
# #make a table
# eu.table = cbind(eu.table, subsidy.usa.eu.yes.6m, subsidy.usa.eu.yes.12m, subsidy.usa.eu.yes.24m, subsidy.china.eu.yes.6m, subsidy.china.eu.yes.12m, subsidy.china.eu.yes.24m )


################################################################################
#2.3. data for third chart ------------------------------------------------------



#get data from website
year = c(1995, 1998, seq(2001, 2019, 2))
no.notification = c(28,60,58,60,60,62,54,50,52,57,67,80)
nil.notification = c(28,21,22,21,19,17,26,31,29,28,21,11)
notification = c(56,52,63,65,70,72,73,72,78,77,76,73)

data3 = data.frame("year" = year, 
                   "no.notification" = no.notification, 
                   "nil.notification" = nil.notification, 
                   "notification" = notification)

#calculate percentages (creates data from page 4)
data3$total.members = data3$no.notification + data3$nil.notification + data3$notification
data3$perc.nil.notification = round(data3$nil.notification/ data3$total.members, 2)
data3$perc.notification = round(data3$notification/ data3$total.members, 2)
data3$perc.no.notification = 1 - (data3$perc.nil.notification + data3$perc.notification)
#only discrepancy to table on page 4 is in year 2019 and just by 1 % (due to rounding)




################################################################################
#3. save data ------------------------------------------------------------------


saveRDS(data1, paste0(path.data, "wto.countervailing.initiations.reported.RData"))
saveRDS(data2, paste0(path.data, "landscape.table.percent.of.responses.RData"))
saveRDS(data3, paste0(path.data, "wto.members.reporting.subsidies.RData"))

