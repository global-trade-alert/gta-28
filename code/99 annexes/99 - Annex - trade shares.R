rm(list = ls())

### Request:
# I have no major changes in mind. However instead of highlighting 2017-2019 in the export exposure table, we should highlight only 2020.

library(openxlsx)
library(gtalibrary)

gta_setwd()
gta26.path = "0 report production/GTA 28/"
data.path = "data/99 annex - title tables/"
out.path = "tables & figures/99 - Annex - title tables/"
source(paste0(gta26.path, "help files/GTA28 settings.R"))


g20.members <- country.names$un_code[country.names$is.g20]
g20.member.names <- country.names$name[country.names$is.g20]


gta_colour_palette()

includ.unpublished = T


chapters <- c("D","E","F","G","I","L","M","P","TARIFF","X")
# remove.ids = c(indian.2.3.exp.id)

for(cty in country.names$name[country.names$is.g20]){
  gta_trade_coverage(coverage.period = c(2009,2021),
                     gta.evaluation = c("Red","Amber"),
                     implementation.period = c("2008-11-01",cutoff.date),
                     exporters=cty,
                     keep.exporters = T,
                     implementer.role = c("importer","3rd country"),
                     mast.chapters = chapters,
                     keep.mast = T,
                     group.mast = F,
                     intervention.ids = firm.specific.outward.subsidies,
                     keep.interventions = F
                     # data.path = d.path,
                     # replica.path =r.path
                     )
  
  trade.coverage.estimates <- trade.coverage.estimates[,c(3,4,6:ncol(trade.coverage.estimates))]
  
  ## creating percentages
  for(i in 3:ncol(trade.coverage.estimates)){
    trade.coverage.estimates[,i]=sprintf("%.2f",round(trade.coverage.estimates[,i]*100,2))
  }
  
  # Adjusting names
  names(trade.coverage.estimates)[1:2]=c("UN MAST chapter", "Foreign discriminatory policy instrument")
  names(trade.coverage.estimates)=gsub("Trade coverage estimate for ","",names(trade.coverage.estimates))
  
  trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`Foreign discriminatory policy instrument`=="All included MAST chapters"]="All instruments"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="D"]="Contingent trade protection"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="E"]="Non-automatic licensing, quotas"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="F"]="Price control measures"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="G"]="Finance measures"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="I"]="Trade-related investment measures"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="L"]="Subsidies (excluding export subsidies)"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="M"]="Government procurement"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`UN MAST chapter`=="P"]="Export measures"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`Foreign discriminatory policy instrument`=="Tariff measures"]="Import tariff increases"
  # trade.coverage.estimates$`Foreign discriminatory policy instrument`[trade.coverage.estimates$`Foreign discriminatory policy instrument`=="Instrument unclear"]="Instrument unclassified"
  trade.coverage.estimates$`Foreign discriminatory policy instrument` <- trimws(stringr::str_remove(trade.coverage.estimates$`Foreign discriminatory policy instrument`, "[A-Z]{1}:"), "both")
  
  remove.ids=c("All included MAST chapters","X","TARIFF")
  trade.coverage.estimates$`UN MAST chapter`[trade.coverage.estimates$`UN MAST chapter` %in% remove.ids]=""
  
  
  write.xlsx(trade.coverage.estimates, file=paste0(gta26.path, out.path, cty,".xlsx"), row.names = F)
  #rm(trade.coverage.estimates)
  print(cty)
}

