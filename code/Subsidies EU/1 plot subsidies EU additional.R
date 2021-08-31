#SE:
# Overnight I thought of the following chart, which should be added for each of the three large jurisdictions. 
# Restrict attention to the harmful inward subsidies in effect in a jurisdiction in 2019. 
# For each goods sector please calculate (a) the share of national goods exports accounted for by a sector X and (b) the share of total number of national harmful inward subsidies in effect in 2019 applied in sector X. 
# Please plot (a) on the vertical axis against (b). 
# It will be interesting to see if there is a negative correlation.

#Addition PL:
# Hier noch paar Kommentare dazu:
# Mit sector ist sicher 3-digit CPC codes gemeint.
# Für die share national goods exports nimm bitte folgenden Code: gtalibrary::gta_trade_value_bilateral(exporting.country = "China",keep.exporter = T, trade.data = 2019) und aggregate dann's über alle importer hinweg
# Der Scatterplot sollte 197 Punkte haben (nrow(subset(gtalibrary::cpc.names, cpc<500 & cpc.digit.level==3)))


rm(list = ls())


library(readr)
library(gtalibrary)
library(ggplot2)
library(openxlsx)


gta_setwd()

path.in = "0 dev/gta-28-sh/data/Subsidies China/"
path.gta = "0 report production/GTA 28/tables & figures/China/"

################################################################################
# 1. get data  -----------------------------------------------------------------


data = readRDS(paste0(path.in, "Shares.China.RData"))


################################################################################
# 2. plot data  ----------------------------------------------------------------




p1 = ggplot(data, aes(x = share.of.export.value, y = share.of.interventions))+
  geom_point()+
  scale_y_continuous(trans = "log10")+
  scale_x_continuous(trans = "log10")+
  labs(title =  "China share of exports to share of number of subsidies per sector (3 digit CPC)", 
       x = "share of total goods export", 
       y = "share of total number of inward subsidy interventions"
  )

gta_plot_saver(p1, 
               path = path.gta, 
               name = "Figure.extra", 
               png = T
)
