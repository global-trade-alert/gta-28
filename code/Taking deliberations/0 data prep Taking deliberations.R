# 1. 
# Go to the WTO website (https://www.wto.org/english/tratop_e/scm_e/scm_e.htm)  and download the statistics for the number of countervailing duty investigations launched each year since 2000. 
# Please produce a stacked bar chart showing the number of investigations initiated by China, EU28, and USA, and rest of the world. 
# Please compare this stacked bar chart with a comparable on produced for the years 2009 on using the GTA’s own data (Intervention type “anti-subsidy”; use announcement date for init year; ask Josse to confirm correspondence.). 
# Hopefully our coverage is better.

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

rm(list = ls())
gta_setwd()

#parameters
path = "0 dev/gta-28-sh/code/taking deliberations/"
path.data = "0 dev/gta-28-sh/data/Taking deliberations/"



juristictions = c("European Union2", "United States", "China")
to.exclud = c("Botswana1", "Eswatini1", "Lesotho1", "Namibia1", "Kazakhstan3") #excluded since they would be double reports (e.g. Botswana is reported together with South Africa, but both Botswana and South Africa are listed seperately)
years = as.character(c(2000:2020))

eu = gtalibrary::country.names[country.names$is.eu, ]$name
implementing.juristiction = c("United States of America", "China", eu)



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

gta_data_slicer(implementing.country = implementing.juristiction, 
                keep.implementer = T, 
                announcement.period = c("2009-01-01", NA), 
                intervention.types = "Anti-subsidy", 
                keep.type = T
                 
)




################################################################################
#2.2. data for second chart ----------------------------------------------------


#2.2.1 get all subsidy interventions of the relevant countries
#get all L subsidy interventions
gta_data_slicer(implementing.country = implementing.juristiction, 
                keep.implementer = T, 
                mast.chapters = "L", 
                keep.mast = T, 
                affected.country = implementing.juristiction, 
                keep.affected = T
                
)

#reduce to relevant interventions
data2 = master.sliced[master.sliced$affected.jurisdiction %in% implementing.juristiction, ] #only get right affecte juristictions
data2$affected.jurisdiction = ifelse(data2$affected.jurisdiction %in% eu, "EU28", data2$affected.jurisdiction) #change EU member states to EU
data2$implementing.jurisdiction = ifelse(data2$implementing.jurisdiction %in% eu, "EU28", data2$implementing.jurisdiction)
data2 = data2 %>% select( - c("affected.sector", "affected.product", "a.un", "i.un")) %>%
                    filter(!data2$implementing.jurisdiction == data2$affected.jurisdiction) #filter so we do not have multiple intervention doubled 
data2 = unique(data2)
length(data2$intervention.id[table(data2$intervention.id)>2]) #no more than 2 interventios per intervention id 

#get dates when subsidies were announced
china.subsidy.dates = unique(data2[data2$implementing.jurisdiction == 42, ]$date.announced)
usa.subsidy.dates = unique(data2[data2$implementing.jurisdiction == 221, ]$date.announced)
eu.subsidy.dates = unique(data2[data2$implementing.jurisdiction == "EU28", ]$date.announced)




#2.2.2 get all harmful subsidy interventions for the relevant countries and calculate if they are inacted after the mentioned timeframes

#get all harmfull subsidy interventions of any type
subsidy.intervention.types <- subset(gtalibrary::int.mast.types, mast.subchapter.id %in% c("L","P7","P8","P9"))$intervention.type
gta_data_slicer(implementing.country = implementing.juristiction, 
                keep.implementer = T, 
                intervention.types = subsidy.intervention.types,
                keep.type = T, 
                affected.country = implementing.juristiction, 
                keep.affected = T, 
                gta.evaluation = "Red"
)

#some cleaning
data2 = master.sliced[master.sliced$affected.jurisdiction %in% implementing.juristiction, ] #only get right affecte juristictions
data2$affected.jurisdiction = ifelse(data2$affected.jurisdiction %in% eu, "EU28", data2$affected.jurisdiction) #change EU member states to EU
data2$implementing.jurisdiction = ifelse(data2$implementing.jurisdiction %in% eu, "EU28", data2$implementing.jurisdiction)
data2 = data2 %>% select( - c("affected.sector", "affected.product", "a.un", "i.un")) %>%
  filter(!data2$implementing.jurisdiction == data2$affected.jurisdiction) #filter so we do not have multiple intervention doubled 
data2 = unique(data2)
length(data2$intervention.id[table(data2$intervention.id)>2]) #no more than 2 interventios per intervention id 




#get all the dates on which china implemented a subsidy that hurts the US and for EU
china.us.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 42& (data2$affected.jurisdiction == 221), ]$date.announced)
china.eu.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 42& (data2$affected.jurisdiction == "EU28"), ]$date.announced)

#same for the US
usa.china.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 221 & (data2$affected.jurisdiction == 42), ]$date.announced)
usa.eu.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == 221 & (data2$affected.jurisdiction == "EU28"), ]$date.announced)

#and for EU
eu.china.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == "EU28"& (data2$affected.jurisdiction == 42), ]$date.announced)
eu.usa.subsidy.response.dates = unique(data2[data2$implementing.jurisdiction == "EU28"& (data2$affected.jurisdiction == 221), ]$date.announced)




#create tables with intervals after announcement of subsidies
china.table = data.frame("subsidy.dates" = china.subsidy.dates,
                         "6m.interval" = interval(ymd(china.subsidy.dates), china.subsidy.dates  %m+%  months(6)),
                         "12m.interval" = interval(ymd(china.subsidy.dates), china.subsidy.dates  %m+%  months(12)),
                         "24m.interval" = interval(ymd(china.subsidy.dates), china.subsidy.dates  %m+%  months(24))
                         )

usa.table = data.frame("subsidy.dates" = usa.subsidy.dates,
                         "6m.interval" = interval(ymd(usa.subsidy.dates), usa.subsidy.dates  %m+%  months(6)),
                         "12m.interval" = interval(ymd(usa.subsidy.dates), usa.subsidy.dates  %m+%  months(12)),
                         "24m.interval" = interval(ymd(usa.subsidy.dates), usa.subsidy.dates  %m+%  months(24))
                         )

eu.table = data.frame("subsidy.dates" = eu.subsidy.dates,
                         "6m.interval" = interval(ymd(eu.subsidy.dates), eu.subsidy.dates  %m+%  months(6)),
                         "12m.interval" = interval(ymd(eu.subsidy.dates), eu.subsidy.dates  %m+%  months(12)),
                         "24m.interval" = interval(ymd(eu.subsidy.dates), eu.subsidy.dates  %m+%  months(24))
                         )




#check if in these intervals were subsidies of other countries


###china
#we check if in the intervals between the time china announced a subsidy that hurt the US and 6/12/24 months the US also announced a subsidy that hurts china
#USA
subsidy.usa.china.yes.6m = as.numeric(unlist(lapply(china.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(usa.china.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.usa.china.yes.12m = as.numeric(unlist(lapply(china.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(usa.china.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.usa.china.yes.24m = as.numeric(unlist(lapply(china.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(usa.china.subsidy.response.dates) %within% x), "1", "0")))))

#EU
subsidy.eu.china.yes.6m = as.numeric(unlist(lapply(china.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(eu.china.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.eu.china.yes.12m = as.numeric(unlist(lapply(china.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(eu.china.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.eu.china.yes.24m = as.numeric(unlist(lapply(china.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(eu.china.subsidy.response.dates) %within% x), "1", "0")))))

#make a table
china.table = cbind(china.table, subsidy.usa.china.yes.6m, subsidy.usa.china.yes.12m, subsidy.usa.china.yes.24m, subsidy.eu.china.yes.6m, subsidy.eu.china.yes.12m, subsidy.eu.china.yes.24m )



###usa
#we check if in the intervals between the time china announced a subsidy that hurt the US and 6/12/24 months the US also announced a subsidy that hurts china
#China
subsidy.china.usa.yes.6m = as.numeric(unlist(lapply(usa.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(china.us.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.china.usa.yes.12m = as.numeric(unlist(lapply(usa.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(china.us.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.china.usa.yes.24m = as.numeric(unlist(lapply(usa.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(china.us.subsidy.response.dates) %within% x), "1", "0")))))

#EU
subsidy.eu.usa.yes.6m = as.numeric(unlist(lapply(usa.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(eu.usa.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.eu.usa.yes.12m = as.numeric(unlist(lapply(usa.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(eu.usa.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.eu.usa.yes.24m = as.numeric(unlist(lapply(usa.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(eu.usa.subsidy.response.dates) %within% x), "1", "0")))))

#make a table
usa.table = cbind(usa.table, subsidy.china.usa.yes.6m, subsidy.china.usa.yes.12m, subsidy.china.usa.yes.24m, subsidy.eu.usa.yes.6m, subsidy.eu.usa.yes.12m, subsidy.eu.usa.yes.24m )



###china
#we check if in the intervals between the time china announced a subsidy that hurt the US and 6/12/24 months the US also announced a subsidy that hurts china
#USA
subsidy.usa.eu.yes.6m = as.numeric(unlist(lapply(eu.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(usa.eu.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.usa.eu.yes.12m = as.numeric(unlist(lapply(eu.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(usa.eu.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.usa.eu.yes.24m = as.numeric(unlist(lapply(eu.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(usa.eu.subsidy.response.dates) %within% x), "1", "0")))))

#China
subsidy.china.eu.yes.6m = as.numeric(unlist(lapply(eu.table$X6m.interval, FUN = function(x)(ifelse(any(ymd(china.eu.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.china.eu.yes.12m = as.numeric(unlist(lapply(eu.table$X12m.interval, FUN = function(x)(ifelse(any(ymd(china.eu.subsidy.response.dates) %within% x), "1", "0")))))
subsidy.china.eu.yes.24m = as.numeric(unlist(lapply(eu.table$X24m.interval, FUN = function(x)(ifelse(any(ymd(china.eu.subsidy.response.dates) %within% x), "1", "0")))))

#make a table
eu.table = cbind(eu.table, subsidy.usa.eu.yes.6m, subsidy.usa.eu.yes.12m, subsidy.usa.eu.yes.24m, subsidy.china.eu.yes.6m, subsidy.china.eu.yes.12m, subsidy.china.eu.yes.24m )

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
saveRDS(data3, paste0(path.data, "wto.members.reporting.subsidies.RData"))

