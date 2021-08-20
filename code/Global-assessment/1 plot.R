library(gtalibrary)
library(tidyverse)

rm(list = ls())
gta_setwd()

#parameters
data.path = "0 dev/gta-28-sh/data/Global assessment/"


################################################################################
#1. load data ------------------------------------------------------------------

data6 = readRDS(paste0(data.path, "Task.6.subsidy.trade.share.undifferentiated.RData"))
data7 = readRDS(paste0(data.path, "Task.7.subsidy.trade.share.differentiated.RData"))



################################################################################
#2. prep data ------------------------------------------------------------------

#6
data6 = data6 %>% select( - c("Importing country", "Exporting country", "Number of interventions affecting exported product"))
colnames(data6) = c("Country","Category", 2009:2021)
data6 = pivot_longer(names_to = "Year", values_to = "Trade Share", data6, cols = 3:ncol(data6))
data6$`Trade Share` = round(data6$`Trade Share`, 3)
data6$Year = as.numeric(data6$Year)


#7
data7 = data7 %>% select( - c("Importing country", "Exporting country", "Number of interventions affecting exported product"))
colnames(data7) = c("Country","Category", "Goods", 2009:2021)
data7 = pivot_longer(names_to = "Year", values_to = "Trade Share", data7, cols = 4:ncol(data7))
data7$`Trade Share` = round(data7$`Trade Share`, 3)
data7$Year = as.numeric(data7$Year)


################################################################################
#3. plot data ------------------------------------------------------------------

#SH: SE suggests plotting everything on a single graph, I would split it, otherwise really crowded, what do you think?

#6
ggplot(data = data6, aes(x = Year, y = `Trade Share`, color= Category))+
  geom_line()+
  facet_wrap(vars(Country))+
  gta_theme()



#7
ggplot(data = data7, aes(x = Year, y = `Trade Share`, color= Category))+
  geom_line()+
  facet_wrap(vars(Goods,Country ))+
  gta_theme()
