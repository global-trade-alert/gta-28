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
library(readxl)
rm(list = ls())
gta_setwd()

options(scipen = 999)

#parameters
data.path = "0 dev/gta-28-sh/data/Subsidies as a source of controversy/"

################################################################################
#1. load data ------------------------------------------------------------------

data1 <- read_excel("0 dev/gta-28-sh/data/Subsidies as a source of controversy/IMF.gov.subsidy.data2.xlsx", skip = 1)

countries = gtalibrary::country.names
countries = gtalibrary::country.names

exchange.rates = gta_get_imf_data(start.date="2000-01-01",
                                  end.date="2020-01-01",
                                  frequency="A",
                                  series="fx",
                                  countries="all")

 
#WB_exchange_rates <- read_excel("0 dev/gta-28-sh/data/Subsidies as a source of controversy/WB exchange rates.xlsx")
OECD_exchange_rates = read_csv(paste0(data.path, "DP_LIVE_21082021164157160.csv"))
################################################################################
#1. Task 1 ------------------------------------------------------------------

#1.1 Transform data to make it usable




#reduce to relevant data
data1 = data1[, c(1:43)]



#create unit vector with all units (M, B, T)
unit = data1$...41
unit[2] = "M"
unit[28] = "M"
unit[1] = "unit"
data1 = cbind(unit, data1)

#delete every second column (units)
for (i in seq(ncol(data1),4, -2)) {
  data1 = data1[, -i]
}

#adjust columnnames to the years
colnames(data1) = data1[1, ]
data1 = data1[-1,]

#transform to numeric
data1[,3:(ncol(data1))] <- sapply(data1[,3:(ncol(data1))],as.numeric)

#make everything to Billions
data1[data1$unit == "M", 3:ncol(data1)] = data1[data1$unit == "M", 3:ncol(data1)]/1000
data1[data1$unit == "Tr", 3:ncol(data1)] = data1[data1$unit == "M", 3:ncol(data1)]*1000

names(data1)[2] = "Countries"

#old
# for (i in seq(ncol(data1),4, -2)) {
#   if(any(na.omit(data1[, i]) == "M")){
#    data1[, i-1] = data1[, i-1]/1000 #get from Millions to Billions
#   }
#   if(any(na.omit(data1[, i]) == "Tr")){
#     data1[, i-1] = data1[, i-1]*1000 #get from trillions to billions
#     
#   }
#   data1 = data1[, -i]
# }
# 
# data1[,2:(ncol(data1))] <- sapply(data1[,2:(ncol(data1))],as.numeric)
# data1 = cbind(data1[, 1], (data1[, 2:22]+ data1[, 23:43]))
# data1[1,2:ncol(data1)] = round(data1[1,2:ncol(data1) ]/2)
# 
# 
# #transform data
# data1 = data.frame(t(data1))
# data1 = rownames_to_column(data1)
# data1[1, 1] = "Countries"
# colnames(data1) = as.character(data1[1, ])
# data1 = data1[-1, ]


#1.2 Adjust names and codes to standardise and make compatible with exchange rates

#change country names to fit GTA 
data.known = data1[ data1$Countries %in% countries$name, ]
data.unknow = data1[!data1$Countries %in% data.known$Countries,]

data.unknow[data.unknow$Countries == "Croatia, Republic of","Countries"] = "Croatia"
data.unknow[data.unknow$Countries == "Czech Republic","Countries"] = "Czechia"
data.unknow[data.unknow$Countries == "Estonia, Republic of","Countries"] = "Estonia"
data.unknow[data.unknow$Countries == "Republic of Latvia","Countries"] = "Latvia"
data.unknow[data.unknow$Countries == "Republic of Lithuania","Countries"] = "Lithuania"
data.unknow[data.unknow$Countries == "Netherlands, The","Countries"] = "Netherlands"
data.unknow[data.unknow$Countries == "Poland, Republic of","Countries"] = "Poland"
data.unknow[data.unknow$Countries == "Russian Federation","Countries"] = "Russia"
data.unknow[data.unknow$Countries == "Slovak Republic","Countries"] = "Slovakia"
data.unknow[data.unknow$Countries == "Slovenia, Republic of","Countries"] = "Slovenia"
data.unknow[data.unknow$Countries == "United States","Countries"] = "United States of America"
data.unknow[data.unknow$Countries == "Korea, Republic of", "Countries"] = "Republic of Korea"




data1 = rbind(data.known, data.unknow)

#Take all EU countries and change the ones not using the Euro
data1[data1$Countries %in% countries[countries$is.eu, ]$name, "Currency"] = "EUR"
data1[data1$Countries == "Sweden", "Currency"] = "SEK"
data1[data1$Countries == "Poland", "Currency"] = "PLN"
data1[data1$Countries == "Czechia", "Currency"] = "CZK"
data1[data1$Countries == "Hungary", "Currency"] = "HUF"
data1[data1$Countries == "Romania", "Currency"] = "RON"
data1[data1$Countries == "Denmark", "Currency"] = "DKK"
data1[data1$Countries == "Croatia", "Currency"] = "HRK"
data1[data1$Countries == "Bulgaria", "Currency"] = "BGN"
data1[data1$Countries == "United Kingdom", "Currency"] = "GBP"

#add other currencies 
data1[data1$Countries == "Argentina", "Currency"] = "ARS"
data1[data1$Countries == "Australia", "Currency"] = "AUD"
data1[data1$Countries == "Brazil", "Currency"] = "BRL"
data1[data1$Countries == "Canada", "Currency"] = "CAD"
data1[data1$Countries == "Indonesia", "Currency"] = "IDR"
data1[data1$Countries == "Japan", "Currency"] = "JPY"
data1[data1$Countries == "Mexico", "Currency"] = "MXN"
data1[data1$Countries == "Saudi Arabia", "Currency"] = "SAR"
data1[data1$Countries == "South Africa", "Currency"] = "ZAR"
data1[data1$Countries == "Turkey", "Currency"] = "TRY"
data1[data1$Countries == "Russia", "Currency"] = "RUB"
data1[data1$Countries == "United States of America", "Currency"] = "USD"

data1 = merge(data1, cbind("name" = countries$name, "ISO" = countries$iso_code), by.x = "Countries", by.y = "name")


#1.3 make exchange tables usable

#tidy up exchange rates
exchange.rates = pivot_wider(exchange.rates,names_from = "date", values_from  = "lcu.per.usd" )
exchange.rates[, 2:ncol(exchange.rates)] = sapply(exchange.rates[,2:(ncol(exchange.rates))],as.numeric)

OECD_exchange_rates = pivot_wider(OECD_exchange_rates,names_from = "TIME", values_from  = "Value" )
OECD_exchange_rates = OECD_exchange_rates[, -c(2:6)]
names(OECD_exchange_rates)[2:ncol(OECD_exchange_rates)] = 2000:2020

# WB_exchange_rates = WB_exchange_rates[, -c(2:4)]
# colnames(WB_exchange_rates) = c(colnames(WB_exchange_rates[, 1]), 2000:2020)
# WB_exchange_rates = WB_exchange_rates[!is.na(WB_exchange_rates$Country), ]
# WB_exchange_rates[, 2:ncol(WB_exchange_rates)] = sapply(WB_exchange_rates[,2:(ncol(WB_exchange_rates))],as.numeric)


#1.4 convert to nominal rate: loop through all countries and first convert with OECD data 

#Please note: using OECD and IMF data because they each did not have all relevant exchange rates (or not conveniently availble
round(OECD_exchange_rates[OECD_exchange_rates$LOCATION == "GBR", as.character(c(2000:2020))], 4) == round(exchange.rates[exchange.rates$currency == "GBP", as.character(c(2000:2020))], 4) #check to see that GBP data is same in both sets if rounds to 4 digits, is TRUE

i = 7
for(i in 1:nrow(data1)) {
  rate = OECD_exchange_rates[OECD_exchange_rates$LOCATION == data1[i, "ISO"],]
  if (nrow(rate) != 0) {
    data1[i, as.character(c(2000:2020))] = as.numeric(data1[i, as.character(c(2000:2020))]) / as.numeric(rate[2:ncol(rate)])
    rate = rate[-1, ]
    data1[i, "ISO"] = 0
  }
  else{
    print(paste("row", i, "could not convert, check if exchange rates are available and are correctly specified"))
  }
  
}


#1.5 Convert not yet converted data (because not avaible in OECD) with data from IMF, this time wie currency name


for(i in 1:nrow(data1)) {
  if (data1[i, "ISO"] != 0)
  {
    rate = exchange.rates[exchange.rates$currency == data1[i, "Currency"], ]
    if (nrow(rate) != 0) {
      data1[i, as.character(c(2000:2020))] = data1[i, as.character(c(2000:2020))] / rate[2:ncol(rate)]
      rate = rate[-1,]
      data1[i, "ISO"] = 0
    }
    else{
      print(
        paste(
          "row",
          i,
          "could not convert, check if exchange rates are available and are correctly specified"
        )
      )
    }
  }
}

#1.6 aggregate

data1[data1$Countries %in% countries[countries$is.eu, ]$name, "Currency"] = "EUR" #make also EU countries not using EUR to EUR for convenience


data1.eu = colSums(data1[data1$Currency == "EUR", as.character(c(2000:2020))], na.rm = T)
data1.us = colSums(data1[data1$Countries == "United States of America",as.character(c(2000:2020)) ], na.rm = T)
data1.cn = colSums(data1[data1$Countries == "China", as.character(c(2000:2020))], na.rm = T)
data1.rest = colSums( data1[data1$Currency != "EUR" & data1$Currency != "USD"& data1$Currency != "CNY", as.character(c(2000:2020))], na.rm = T)

data1 = t(data.frame("EU" = data1.eu, 
                   "USA" = data1.us, 
                   "China" = data1.cn, 
                   "Rest.G20" = data1.rest))


################################################################################
#3. aggregate data ------------------------------------------------------------------

saveRDS(data1, paste0(data.path, "govt.subsidy.G20.RData"))
