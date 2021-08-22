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
data6 = data6[data6$Year != 2021, ]


#7
data7 = data7 %>% select( - c("Importing country", "Exporting country", "Number of interventions affecting exported product"))
colnames(data7) = c("Country","Category", "Goods", 2009:2021)
data7 = pivot_longer(names_to = "Year", values_to = "Trade Share", data7, cols = 4:ncol(data7))
data7$`Trade Share` = round(data7$`Trade Share`, 3)
data7$Year = as.numeric(data7$Year)
data7 = data7[data7$Year != 2021, ]
data7[data7$Goods == "Not Agricultural","Goods"] = "Non Agricultural"
data7[data7$Goods == "Only Agricultural","Goods"] = "Agricultural"

################################################################################
#3. plot data ------------------------------------------------------------------

#SH: SE suggests plotting everything on a single graph, I would split it, otherwise really crowded, what do you think?

#6
figure.6 = ggplot(data = data6 )+
  geom_point(data = data6[data6$Year == 2020, ], aes(x = Year, y = `Trade Share`, color = Category))+
  geom_line(aes(x = Year, y = `Trade Share`, color= Category))+
  geom_label(data = data6[data6$Year == 2020, ], aes(x = Year, y = `Trade Share`, label = `Trade Share`), nudge_x = -1, nudge_y = 0.01)+
  facet_wrap(vars(Country))+
  ylab("Share of world goods trade")+
  gta_theme()

figure.6

#7
figure.7 = ggplot(data = data7, aes(x = Year, y = `Trade Share`, color= Category, label = `Trade Share`))+
  geom_point(data = data7[data7$Year == 2020, ], aes(x = Year, y = `Trade Share`, color = Category))+
  geom_label(data = data7[data7$Year == 2020, ], aes(x = Year, y = `Trade Share`, label = `Trade Share`), nudge_x = -1.5, nudge_y = 0.03)+
  geom_line()+
  facet_wrap(vars(Goods,Country ))+
  ylab("Share of world goods trade")+
  gta_theme()

figure.7

gta_plot_saver(figure.6, 
               path = data.path, 
               name= "Chapter.4.Figure.6", 
               png = T,
               width = 30
               )

gta_plot_saver(figure.7, 
               path = data.path, 
               name= "Chapter.4.Figure.7", 
               png = T,
               width = 30
               )  
  
  
write.xlsx(data6, paste0(data.path, "figure.6.xlsx"))  
write.xlsx(data7, paste0(data.path, "figure.7.xlsx"))  
