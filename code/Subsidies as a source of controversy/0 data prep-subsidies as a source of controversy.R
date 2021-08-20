##GTA 28, Chapter: Global assessment on resort to subsidies
#Request SE: 
# Go to https://data.imf.org/?sk=3C005430-5FDC-4A07-9474-64D64F1FB3DC. 
# Extract information for each G20 government (SE:Please build the data up from the EU member state level. In general, this IMF database is not great but it is the only one I know that tries to track government subsidies.) 
# on the total value of subsidies paid (note that there is the option to extract subsidies paid by central and non-central governments , SE:pick both, where available. Again, it varies across countries. ). 
# Where possible, extract subsidies paid to public corporations and private firms and not subsidies paid to other levels of government or to individuals. 
# Data availability will vary across countries (please make a note of any major concerns in this regard). Collect data for years 2000 to 2020 (or 2019 if that is the last year available). 
# Convert amounts in local currencies into USD using the nominal exchange rate(JF: Take average annual exchange rate as supplied by IMF or WB. Create help file CSV if you can’t pull it from an R library.)  for the year in question. 
# Produce a chart over time showing US, Chinese, EU (SE:Please include the UK throughout as in the EU. For almost all of the period of study the UK was a member of the EU.)   and rest of G20 subsidies as a stacked bar chart. Alternatively, produce the same chart as a line graph. 
# We can choose which chart one looks best. 



#Author: Silvan
#Date 19.8.2021

library(gtalibrary)
library(tidyverse)
library(readxl)
library(IMFData)

rm(list = ls())
gta_setwd()

#parameters
data.path = "0 dev/gta-28-sh/data/Subsidies as a source of controversy/"

################################################################################
#1. load data ------------------------------------------------------------------

data1 <- read_excel("0 dev/gta-28-sh/data/Subsidies as a source of controversy/IMF.govt.subsidy.data.xlsx", skip = 1)



################################################################################
#1. clean data ------------------------------------------------------------------

#delete every second column (was unit) and standardize to billions 
for (i in seq(ncol(data1),3, -2)) {
  if(any(na.omit(data1[, i]) == "M")){
   data1[, i-1] = data1[, i-1]/1000 #get from Millions to Billions
  }
  if(any(na.omit(data1[, i]) == "Tr")){
    data1[, i-1] = data1[, i-1]*1000 #get from trillions to billions
    
  }
  data1 = data1[, -i]
}


exchange.rates = gta_get_imf_data(start.date="2000-01-01",
                             end.date="2021-01-01",
                             frequency="A",
                             series="fx",
                             countries="all")


data1 = data.frame(t(data1))
data1 = rownames_to_column(data1)
data1[1, 1] = "Countries"
colnames(data1) = as.character(data1[1, ])
data1 = data1[-1, ]


#change country names to fit GTA 
data.known = data1[ data1$Countries %in% countries$name, ]
data.unknow = data1[!data1$Countries %in% data.known$Countries,]

data.unknow[data.unknow$Countries == "Croatia, Republic of","Countries"] = "Croatia"
data.unknow[data.unknow$Countries == "Czech Republic","Countries"] = "Croatia"
data.unknow[data.unknow$Countries == "Estonia, Republic of","Countries"] = "Estonia"
data.unknow[data.unknow$Countries == "Republic of Latvia","Countries"] = "Latvia"
data.unknow[data.unknow$Countries == "Republic of Lithuania","Countries"] = "Lithuania"
data.unknow[data.unknow$Countries == "Netherlands, The","Countries"] = "Netherlands"
data.unknow[data.unknow$Countries == "Poland, Republic of","Countries"] = "Poland"
data.unknow[data.unknow$Countries == "Russian Federation","Countries"] = "Russia"
data.unknow[data.unknow$Countries == "Slovak Republic","Countries"] = "Slovakia"
data.unknow[data.unknow$Countries == "Slovenia, Republic of","Countries"] = "Slovenia"
data.unknow[data.unknow$Countries== "United States","Countries"] = "United States of America"

data1 = rbind(data.known, data.unknow)
exchange.rates = pivot_wider(exchange.rates,names_from = "date", values_from  = "lcu.per.usd" )



