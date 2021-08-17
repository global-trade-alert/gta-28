rm(list = ls())


library(openxlsx)
library(gtalibrary)
library(tidyr)

gta_setwd()

gta_colour_palette()

chapters <- c("D","E","F","G","I","L","M","P","TARIFF","X")


gta27.path = "0 report production/GTA 27/"
data.path = "1 data analysis/code/99 annexes/"




#### Investigating jumps for Russia & Saudi Arabia (03.05.2021)
### Simon: " I would not be surprised if the jumps are to do with the Canadian measures last year to finance the overseas 
### operations of their oil companies. It may be entirely appropriate that these export shares have jumped so much"

#### Russia: increase by 11% in MAST chapter L
#### Saudi Arabia: increase by 9% in MAST chapter L & almost doubling from 48 to 83% in MAST chapter P


#Russia L
## 1 Identify the 2020 measures (potentially already 2019) which could cause this jump


gta_data_slicer(affected.country = "Russia", keep.affected = T, 
                           implementation.period = c(as.Date("2019-07-01"),Sys.Date()), keep.implementation.na = F, 
                           keep.revocation.na = T, 
                           mast.chapters = "L", keep.mast = T,
                           affected.flows = c("inward","outward subsidy"),
                           gta.evaluation = c('red','amber'))
length(unique(master.sliced$intervention.id))
russia.l = unique(subset(master.sliced, select = c("title","intervention.id")))


## 2 Calculate trade share of each intervention (takes long but is thorough)
russia.l$share.2019 = as.numeric()
russia.l$share.2020 = as.numeric()
russia.l$share.2021 = as.numeric()

for(intrvn in russia.l$intervention.id){
  gta_trade_coverage(exporters = "Russia", keep.exporters = T, 
                     intervention.ids = intrvn, keep.interventions = T, 
                     coverage.period = c(2019,2021),trade.statistic = 'share')
  russia.l$share.2019[russia.l$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2019`
  russia.l$share.2020[russia.l$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2020`
  russia.l$share.2021[russia.l$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2021`
  print(intrvn)
}
russia.l <- russia.l[order(-russia.l$share.2021),]
write.xlsx(russia.l, file=paste0(gta27.path, data.path,"russia.l.xlsx"))

largest.russi.l.ids = russia.l$intervention.id[1:4]

## 3 Calculate trade coverage impact of excluding a particular intervention
gta_trade_coverage(exporters = "Russia", keep.exporters = T, 
                   intervention.ids = largest.russi.l.ids, keep.interventions = F, 
                   coverage.period = c(2019,2021),trade.statistic = 'share')

#these four interventions together have a trade coverage of 67.96651, 68.42574, 63.28572 percent in 2019/20/21

#SAUDIA ARABIA L

## 1 Identify the 2020 measures (potentially already 2019) which could cause this jump


gta_data_slicer(affected.country = "Saudi Arabia", keep.affected = T, 
                implementation.period = c(as.Date("2019-07-01"),Sys.Date()), keep.implementation.na = F, 
                keep.revocation.na = T, 
                mast.chapters = "L", keep.mast = T,
                affected.flows = c("inward","outward subsidy"),
                gta.evaluation = c('red','amber'))
length(unique(master.sliced$intervention.id))
saudi.arabia.l = unique(subset(master.sliced, select = c("title","intervention.id")))


## 2 Calculate trade share of each intervention (takes long but is thorough)
saudi.arabia.l$share.2019 = as.numeric()
saudi.arabia.l$share.2020 = as.numeric()
saudi.arabia.l$share.2021 = as.numeric()

for(intrvn in saudi.arabia.l$intervention.id){
  gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                     intervention.ids = intrvn, keep.interventions = T, 
                     coverage.period = c(2019,2021),trade.statistic = 'share')
  saudi.arabia.l$share.2019[saudi.arabia.l$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2019`
  saudi.arabia.l$share.2020[saudi.arabia.l$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2020`
  saudi.arabia.l$share.2021[saudi.arabia.l$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2021`
  print(intrvn)
}
saudi.arabia.l <- saudi.arabia.l[order(-saudi.arabia.l$share.2021),]

write.xlsx(saudi.arabia.l, file=paste0(gta27.path, data.path,"saudi.arbia.l.xlsx"))

largest.saudi.arabia.l.ids = saudi.arabia.l$intervention.id[1:4]

## 3 Calculate trade coverage impact of excluding a particular intervention
gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                   intervention.ids = largest.saudi.arabia.l.ids, keep.interventions = F, 
                   coverage.period = c(2019,2021),trade.statistic = 'share')

#these four interventions together have a trade coverage of 0.6744425 0.6745626 0.918409 in 2019/20/21

countries = gtalibrary::country.names


#SAUDI ARABIA P


gta_data_slicer(affected.country = "Saudi Arabia", keep.affected = T, 
                implementation.period = c(as.Date("2019-07-01"),Sys.Date()), keep.implementation.na = F, 
                keep.revocation.na = T, 
                mast.chapters = "P", keep.mast = T,
                affected.flows = c("inward","outward subsidy"),
                gta.evaluation = c('red','amber'))
length(unique(master.sliced$intervention.id))
saudi.arabia.p = unique(subset(master.sliced, select = c("title","intervention.id")))


## 2 Calculate trade share of each intervention (takes long but is thorough)
saudi.arabia.p$share.2019 = as.numeric()
saudi.arabia.p$share.2020 = as.numeric()
saudi.arabia.p$share.2021 = as.numeric()

for(intrvn in saudi.arabia.p$intervention.id){
  gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                     intervention.ids = intrvn, keep.interventions = T, 
                     coverage.period = c(2019,2021),trade.statistic = 'share')
  saudi.arabia.p$share.2019[saudi.arabia.p$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2019`
  saudi.arabia.p$share.2020[saudi.arabia.p$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2020`
  saudi.arabia.p$share.2021[saudi.arabia.p$intervention.id==intrvn] <- trade.coverage.estimates$`Trade coverage estimate for 2021`
  print(intrvn)
}
saudi.arabia.p <- saudi.arabia.p[order(-saudi.arabia.p$share.2021),]

write.xlsx(saudi.arabia.p, file=paste0(gta27.path, data.path,"saudi.arbia.p.xlsx"))

largest.saudi.arabia.l.ids = saudi.arabia.l$intervention.id[1:3]

## 3 Calculate trade coverage impact of excluding a particular intervention
gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                   intervention.ids = 82701, keep.interventions = T, 
                   coverage.period = c(2019,2021),trade.statistic = 'share')

gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                   intervention.ids = 82701, keep.interventions = F, 
                   coverage.period = c(2019,2021),trade.statistic = 'share')

#these three interventions together have a trade coverage of 0.6744425, 0.7102854, 0.9184094 in 2019/20/21

countries = gtalibrary::country.names






##### PL investigation
gta_trade_coverage(exporters = "Russia", keep.exporters = T, 
                   intervention.ids = 73035, keep.interventions = F,
                   implementer.role = c("importer","3rd country"),
                   mast.chapters = "L",
                   keep.mast = T,
                   group.mast = F,
                   coverage.period = c(2019,2021),trade.statistic = 'share')
View(trade.coverage.estimates)
## Russia - L: without intervention 73035, 2021 share drops to 39% instead of increasing to 57%


# gta_trade_coverage(exporters = "Russia", keep.exporters = T, 
#                    intervention.ids = 79111, keep.interventions = F,
#                    implementer.role = c("importer","3rd country"),
#                    mast.chapters = "L",
#                    keep.mast = T,
#                    group.mast = F,
#                    coverage.period = c(2019,2021),trade.statistic = 'share')
# View(trade.coverage.estimates)
## Russia - L: without intervention 79111, no impact at all

# gta_trade_coverage(exporters = "Russia", keep.exporters = T, 
#                    intervention.ids = 82312, keep.interventions = F,
#                    implementer.role = c("importer","3rd country"),
#                    mast.chapters = "L",
#                    keep.mast = T,
#                    group.mast = F,
#                    coverage.period = c(2019,2021),trade.statistic = 'share')
# View(trade.coverage.estimates)
## Russia - L: without intervention 82312, no impact at all

gta_trade_coverage(exporters = "Russia", keep.exporters = T, 
                   intervention.ids = 81550, keep.interventions = F,
                   implementer.role = c("importer","3rd country"),
                   mast.chapters = "L",
                   keep.mast = T,
                   group.mast = F,
                   coverage.period = c(2019,2021),trade.statistic = 'share')
View(trade.coverage.estimates)
## Russia - L: without intervention 81550, 2020 share is 31 instead of 46%


gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                   intervention.ids = 82701, keep.interventions = F,
                   implementer.role = c("importer","3rd country"),
                   mast.chapters = "P",
                   keep.mast = T,
                   group.mast = F,
                   coverage.period = c(2020,2021),trade.statistic = 'share')
View(trade.coverage.estimates)
## Saudi - P: without intervention 82701, 2021 share is 46 instead of 83%

# gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
#                    intervention.ids = 82519, keep.interventions = F,
#                    implementer.role = c("importer","3rd country"),
#                    mast.chapters = "P",
#                    keep.mast = T,
#                    group.mast = F,
#                    coverage.period = c(2020,2021),trade.statistic = 'share')
# View(trade.coverage.estimates)
# ## Saudi - P: without intervention 82519, no change
# 
# gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
#                    intervention.ids = 72651, keep.interventions = F,
#                    implementer.role = c("importer","3rd country"),
#                    mast.chapters = "P",
#                    keep.mast = T,
#                    group.mast = F,
#                    coverage.period = c(2020,2021),trade.statistic = 'share')
# View(trade.coverage.estimates)
# ## Saudi - P: without intervention 72651, no change



gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                   intervention.ids = 81550, keep.interventions = F,
                   implementer.role = c("importer","3rd country"),
                   mast.chapters = "L",
                   keep.mast = T,
                   group.mast = F,
                   coverage.period = c(2019,2021),trade.statistic = 'share')
View(trade.coverage.estimates)
## Saudi - L: without intervention 81550, 2020 share is 29 instead of 51%

gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                   intervention.ids = 84482, keep.interventions = F,
                   implementer.role = c("importer","3rd country"),
                   mast.chapters = "L",
                   keep.mast = T,
                   group.mast = F,
                   coverage.period = c(2019,2021),trade.statistic = 'share')
View(trade.coverage.estimates)
## Saudi - L: without intervention 84482, no change

gta_trade_coverage(exporters = "Saudi Arabia", keep.exporters = T, 
                   intervention.ids = 73035, keep.interventions = F,
                   implementer.role = c("importer","3rd country"),
                   mast.chapters = "L",
                   keep.mast = T,
                   group.mast = F,
                   coverage.period = c(2019,2021),trade.statistic = 'share')
View(trade.coverage.estimates)
## Saudi - L: without intervention 73035, 2021 share is 58% instead of 60%

#################################################################################
#Criterion Charts

gta_data_slicer(implementation.period = c(as.Date("2019-12-31"),c(as.Date("2021-04-30"))), 
                keep.implementation.na = F, 
                gta.evaluation = c('red')
)

argentina = subset(master.sliced, a.un==as.numeric(32))
num.hs.codes = na.omit(nchar(argentina$affected.product)) #
max(num.hs.codes) #286, so the longest string is 286 symbols, meaning divided by around 8 to get pr hs the maximum us +_ 38 hs codes for one intervention 


master.reduced = na.omit(unique(data.frame(master.sliced[, c(6, 18)])))
string2 = separate(master.reduced, affected.product, into= c( as.character(seq(1, 3000, 1))))
string3 = string2[, c(1,2,50)]

string3 = unique(na.omit(gather(string2, key = "code", "value")))



argentina = subset(master.sliced, a.un==as.numeric(32))

string = paste(unique(argentina$affected.product), collapse = ", ")
string.reduced = data.frame(argentina[, 18])
string2 = separate(string.reduced, `argentina...18.`, into= c( as.character(seq(1, 50, 1))))
string3 = unique(na.omit(gather(string2, key = "code", "value")))

products<-aggregate(affected.product ~ a.un + protect + currently.in.force, data=subset(g20.TL, prior==1), function(x) length(unique(x)))