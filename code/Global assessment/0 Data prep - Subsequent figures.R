####################################################################

# DATA PREP for chapter 4: Global assessment on resort to subsidies
# Part 2
####################################################################

library(gtalibrary)
library(tidyverse)
library(janitor)


rm(list = ls())

gta_setwd()


data.path2 = "0 dev/gta-28-ad/data/Global assessment/data master.Rdata"

load(data.path2)

########################## Figure 2: ############################### 

#Map of the number of times each country’s commercial interests have been harmed in 2019 by 
#L inward policies implemented by China, EU28, and USA (together) in effect that year. 
#Here the number of times a country’s commercial interests relates to the number of HS codes 
#that it exports that are affected in 2019.

harmed.l.inward = master.l.inward[!is.na(master.l.inward$affected.product),]%>%
  filter((date.removed>as.Date("2019-01-01")|is.na(date.removed)) & date.implemented<as.Date("2020-01-01") & (gta.evaluation=="Red"|gta.evaluation=="Amber"))%>%
  separate_rows(affected.product, sep = ", ")%>%
  group_by(affected.jurisdiction)%>% 
  summarize(products = paste(unique(affected.product), collapse = ", "))

harmed.l.inward = harmed.l.inward%>%
  separate_rows(products, sep = ", ")

harmed.l.inward = harmed.l.inward%>%
  group_by(affected.jurisdiction)%>%
  summarize(count = n())

########################## Figure 3: ############################### 

#Map of the number of times each country’s commercial interests have been HARMED in 2019 by 
#L outward and P policies implemented by China, EU28, and USA (together) in effect that year. 
#Here the number of times a country’s commercial interests relates to the number of HS codes 
#that it exports that are affected in 2019.

harmed.p.outward = master.p.outward[!is.na(master.p.outward$affected.product),]%>%
  filter((date.removed>as.Date("2019-01-01")|is.na(date.removed)) & date.implemented<as.Date("2020-01-01") & (gta.evaluation=="Red"|gta.evaluation=="Amber"))%>%
  separate_rows(affected.product, sep = ", ")%>%
  group_by(affected.jurisdiction)%>% 
  summarize(products = paste(unique(affected.product), collapse = ", "))

harmed.p.outward = harmed.p.outward%>%
  separate_rows(products, sep = ", ")

harmed.p.outward = harmed.p.outward%>%
  group_by(affected.jurisdiction)%>%
  summarize(count = n())

########################## Figure 4: ############################### 

#Map of the number of times each country’s commercial interests have BENEFITED in 2019 from 
#L outward and P policies  implemented by China, EU28, and USA (together) in effect that year. 
#Here the number of times a country’s commercial interests relates to the number of HS codes 
#that it imports that are affected in 2019.

benefited.p.outward = master.p.outward[!is.na(master.p.outward$affected.product),]%>%
  filter((date.removed>as.Date("2019-01-01")|is.na(date.removed)) & date.implemented<as.Date("2020-01-01") & gta.evaluation=="Green")%>%
  separate_rows(affected.product, sep = ", ")%>%
  group_by(affected.jurisdiction)%>%
  summarize(products = paste(unique(affected.product), collapse = ", "))

benefited.p.outward = benefited.p.outward%>%
  separate_rows(products, sep = ", ")

benefited.p.outward = benefited.p.outward%>%
  group_by(affected.jurisdiction)%>%
  summarize(count = n())


########################## SILVAN'S TASKS ########################
# Figures 5 to 7 are treated by Silvan separately

########################## Figure 8: ############################### 

#Prepare a figure in landscape format with 6 panels. 
#The panels refer separately to Chinese, EU28, and USA inward L subsidy interventions. 
#The rows refer to agriculture and non-agricultural goods. 
#Each panel should plot the share of the implementing country’s imports of the goods in question
#affected by L inward subsidies. Those import shares should cover the years 2009 to 2020.


# gta_trade_coverage(coverage.period = c(2009:2020),
#                    implementers = c("China", "United States of America", "eu28"),
#                    gta.evaluation = c("red", "amber"),
#                    affected.flows = c("inward"),
#                    importers = c("China", "United States of America", "eu28"),
#                    keep.importers = T,
#                    
#                    affected.flows = "inward",
#                    mast.chapters = "L",
#                    keep.mast = T)





