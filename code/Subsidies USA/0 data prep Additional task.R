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


gta_setwd()


subsidy.intervention.types <- subset(gtalibrary::int.mast.types, mast.subchapter.id %in% c("L","P7","P8","P9"))$intervention.type
subsidy.intervention.types = subsidy.intervention.types[subsidy.intervention.types != "Export-related non-tariff measure, nes"]

subsidy.chapter = "L"
cut.off = "2019-12-31"


################################################################################
# 1. Get data-------------------------------------------------------------------

#get conversions
hs.to.cpc <- read_delim("definitions/cpc-to-hs/hs 2012 to cpc 2_1.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#get trade data
gtalibrary::gta_trade_value_bilateral(exporting.country = "United States of America",keep.exporter = T, trade.data = 2019)

#get intervention data
gta_data_slicer(implementing.country = "United States of America", 
                keep.implementer = T, 
                gta.evaluation = c("Red", "Amber"),
                implementation.period = c(NA, cut.off), 
                keep.implementation.na = F, 
                mast.chapters = subsidy.chapter, 
                keep.mast = T
)


################################################################################
# 2. Prep data -----------------------------------------------------------------

#convert HS to CPC
data = merge(trade.base.bilateral, hs.to.cpc, by.x = "hs6", by.y = "hs", all.x = T)

length(unique(data$cpc)) #just 185



#aggregate over all country exported to and 
data = aggregate(data = data, trade.value ~  cpc, FUN = sum)
data$share.of.export.value = data$trade.value/sum(data$trade.value)

#prevent NA problem
help = subset(master.sliced, is.na(date.removed))

#remove all interventions removed before 2019
master.sliced = subset(master.sliced, !is.na(date.removed))
master.sliced = master.sliced[!master.sliced$date.removed < "2019-01-01", ]
master.sliced = rbind(master.sliced, help)


length(unique(master.sliced$intervention.id)) #798 interventions

#reduct to relevant variables and split all affected sector to get intervention cpc pairs
master.sliced = master.sliced[, c("intervention.id", "affected.sector")]
master.sliced = unique(cSplit(master.sliced, splitCols = "affected.sector", sep = ",", direction = "long"))

#add aggregate to get number of intervntions meatninging cpc
data.sector = aggregate(data = master.sliced, intervention.id ~ affected.sector, FUN = length)

#only take goods sectors
data.sector = data.sector[data.sector$affected.sector < 500, ]

#calculate share
data.sector$share.of.interventions = data.sector$intervention.id / length(unique(master.sliced$intervention.id))

data = merge(data, data.sector, by.x = "cpc", by.y = "affected.sector", all = T)

data[is.na(data)] = 0

ggplot(data, aes(x = share.of.export.value, y = share.of.interventions))+
  geom_point()+
  scale_y_continuous(trans = "log10")+
  scale_x_continuous(trans = "log10")
