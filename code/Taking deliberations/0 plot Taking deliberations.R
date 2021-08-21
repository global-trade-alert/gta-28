# 1. 
# Go to the WTO website (https://www.wto.org/english/tratop_e/scm_e/scm_e.htm)  and download the statistics for the number of countervailing duty investigations launched each year since 2000. 
# Please produce a stacked bar chart showing the number of investigations initiated by China, EU28, and USA, and rest of the world. 
# Please compare this stacked bar chart with a comparable on produced for the years 2009 on using the GTA’s own data (Intervention type “anti-subsidy”; use announcement date for init year; ask Josse to confirm correspondence.). 
# Hopefully our coverage is better.
#Author: Silvan
#Date: 17.8.2021
#Supervisor Piotr

library(ggplot2)
library(tidyverse)
library(gtalibrary)
library(gt)
gta_setwd()

rm(list = ls())


#parameters
path = "0 dev/gta-28-sh/code/taking deliberations/"
path.data = "0 dev/gta-28-sh/data/Taking deliberations/"




data1 = readRDS(paste0(path.data, "wto.countervailing.initiations.reported.RData"))
data2 = readRDS(paste0(path.data, "landscape.table.percent.of.responses.RData"))
data3 = readRDS(paste0(path.data, "wto.members.reporting.subsidies.RData"))


################################################################################
#1. prep data ------------------------------------------------------------------

#1.
data1[is.na(data1)] = 0
data1[2:ncol(data1)] = as.numeric(unlist(data1[2:ncol(data1)]))
data1 = gather(data1, key = "year", value = "investigations", 2:ncol(data1))
data1$year = as.factor(data1$year)


#2

data2 = data.frame(data2)
is.na(data2) = NULL



#3.
data3.1 = gather(data3, key = "kind.of.notification", value = "percentage", 6:ncol(data3) ) #

data3.2 = gather(data3[, 1:4], key = "kind.of.notification", value = "number", 2:4 ) #





################################################################################
#2. Plot -----------------------------------------------------------------------

#1.
ggplot(data1, aes(x = reporting.member, y = investigations, fill = year))+ 
  geom_col()


#2.

#quick and dirty approach, at later stage add gta colors, maybe make heatmap
table.2 = gt(data2, rowname_col = "Regions")

table.2 = table.2 %>%
  tab_header(title = "Reactions to subsidies")%>%
  tab_spanner(label = "China", columns = names(data2)[2:7])%>%
  tab_spanner(label = "USA", columns = names(data2)[8:13])%>%
  tab_spanner(label = "EU", columns = names(data2)[14:19])
  
  
table.2



#3. 
ggplot(data3.1, aes(x = as.factor(year), y = percentage, fill = kind.of.notification))+ 
  geom_col()+
  geom_text(aes(label = percentage), size = 3, position = position_stack(vjust = 0.5))


data3.2[data3.2$kind.of.notification == "no.notification", "number" ] = data3.2[data3.2$kind.of.notification == "no.notification", "number" ] *-1
data3.2[data3.2$kind.of.notification == "nil.notification", "number" ] = data3.2[data3.2$kind.of.notification == "nil.notification", "number" ] *-1

data3.2 = unique(data3.2)

ggplot(data3.2, aes(x = as.factor(year), y = number, fill = kind.of.notification))+ 
  geom_col()+
  geom_text(aes(label = number), size = 3, position = position_stack(vjust = 0.5))


