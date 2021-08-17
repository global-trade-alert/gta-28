#GTA 28, Chapter: Global assessment on resort to subsidies
#Request SE: 
#For China, EU28, and the USA, please produce a list of subsidy interventions that our raw estimates suggest cover more than $10 billion in trade. 
#Please identify those subsidy interventions (JF: Mast chapters L and P, inward & outward subsidy)  where the amount of financial support involved was less than $100 million (JF: New_value field  in “data/database replica/gta_intervention.csv”; also extract unit field; contact PL or JF about those units.) . 
#Please identify those subsidy interventions that are trade finance (JF: Via intervention type of same name)  (not all P, just trade finance) or L outward (JF: Outward subsdiy) . 
#Please also identify those subsidy interventions that are firm specific (JF: Add implementation level). 
#With this information I can decide which subsidy interventions to exclude from any trade coverage calculations in this and subsequent chapters.

#Comments JF and SE
#JF: You’ll have to loop through the trade coverage function. Note that there are outtakes in that function (ask JF). There also was a list of agreed jumbos in an earlier report (ask PL).
#JF: Add outtakes as separate column based on line 191 of https://github.com/global-trade-alert/gtalibrary/blob/master/R/gta%20trade%20coverage.R
#JF: Add “is.jumbo” as separate column based on PL input

#Author: Silvan
#Date 16.8.2021

library(gtalibrary)
library(gtasql)
library(pool)
library(RMariaDB)
library(DBI)
library(tidyverse)

rm(list = ls())

#variables

#implementing countries
countries = gtalibrary::country.names
relevant.juristictions = countries[countries$is.eu == 1, "name"]
relevant.juristictions = c(relevant.juristictions, "United States of America", "China")
max.intervention.value = 100000000 #absolute
min.trade.share.value = 10000 #in Millions
#already checked cases which are to be excluded
load(file = "0 report production/GTA 25/prep pre report/jumbo.data.Rdata")

out=c(20387, 20389, 16408, 16817, 15248, 20098, 56907,
      18891,70350,16819,71578,58794,18254,13633,15366,19899,13512,14328,
      18602,14104,17285,18601,19351,19347,15100,18638,57474,14017,20375,
      57843,57619,62121,70692,72278,60042,13631,72137,18795,71645,13707,
      19425,70751,15747,58726,18897,18649,72800,72384,69601, 70466,
      71010,60343,68840,62147,71561, 82519, 81141, 80982, 78338,80442,80985,
      79456, 80739, 80552, 81549, 79111,81832,
      78439,80057, 78044, 78764, 81428, 79740, 81439, 78706, 81786, 78033,
      62120,70561,73612,73866,77800,79201,79440,80064,80547,81030,81059,81093,81785,84331, 73056,81550, 82986)


################################################################################
#1. get data -------------------------------------------------------------------


#1.1 get monetary values for interventions (code by JF)

gta_sql_kill_connections()
gta_setwd()

database <<- "gtamain"

gta_sql_pool_open(db.title=database,
                  db.host = gta_pwd(database)$host,
                  db.name = gta_pwd(database)$name,
                  db.user = gta_pwd(database)$user,
                  db.password = gta_pwd(database)$password,
                  table.prefix = "gta_")

gta.int.value=gta_sql_get_value("SELECT intervention_id, prior_level, new_level, giu.name
                                FROM gta_intervention_level gil
                                JOIN gta_intervention_unit giu
                                ON gil.intervention_unit_id = giu.id")

gta.int.value$prior.level=as.numeric(as.character(gta.int.value$prior.level))
gta.int.value$new.level=as.numeric(as.character(gta.int.value$new.level))

gta.int.value=unique(subset(gta.int.value, !(is.na(prior.level) & is.na(new.level))))

gta_sql_pool_close()
gta_sql_kill_connections()

rm(list=setdiff(ls(), c("relevant.juristictions","max.intervention.value", "min.trade.share.value", "jumbo.data", "out", "gta.int.value" )))
#1.2 load master 

load(file = "data/master_plus - 210816.RData")


#currently error in data slicer, so directly via master
# gta_data_slicer(
#   implementing.country = relevant.juristictions,
#   keep.implementer = T,
#   intervention.types = "trade finance",
#   implementation.level = "firm specific",
#   mast.chapters = c("L", "P"),
#   keep.mast = T,
# )



################################################################################
#2. filter data -------------------------------------------------------------------

master = master %>% filter(implementing.jurisdiction %in% relevant.juristictions)%>%
                      filter(mast.chapter %in% c("L", "P")) %>%
                        filter(intervention.type == "Trade finance" | affected.flow == "outward subsidy"  & mast.chapter=="L" ) #| eligible.firms == "firm-specific"
                        


#exclude interventions already check and determined to not be included in trade coverage
master = master[!master$intervention.id %in% jumbo.data$intervention.id, ] #jumbo file
master = master[!master$intervention.id %in% out, ] #excluded from trade coverage function

#merge with amounts
master = merge(master, gta.int.value, by ="intervention.id", all.x = T)



#get data without values
na.cases = master[is.na(master$name), ] #29 cases
master = master[!master$intervention.id %in% na.cases$intervention.id, ]

na.cases.post.2016 = na.cases[na.cases$date.announced > as.Date("2016-01-01"), ] #only use cases after 2016 since amounts were not reported before that date

nas = master[is.na(master$new.level), ]
master = master[!master$intervention.id %in% nas, ]
master = master[master$new.level < max.intervention.value, ]


#test for other anomalies in data and filter for wrong units
test2 = master[master$name == "percent", ] #11 cases, Error but leave in, treat like its a dollar amount
master = master[!(master$name == "percent" & master$new.level < 100), ] #get all "real percentage values away (such that are actually below 100, meaning actuall percentages)
master = master[!master$name == "USD/unit",] #filter out cases with other units 
master = master[!master$name == "USD/tonne",]



# openxlsx::write.xlsx(na.cases, "0 dev/gta-28-sh/code/Global-assessment/NA.cases.xlsx")
# openxlsx::write.xlsx(na.cases.post.2016, "0 dev/gta-28-sh/code/Global-assessment/NA.cases.post.2016.xlsx")

#reduce data to relevant columns


################################################################################
#3. add trade value data -------------------------------------------------------
affected.flow.na = master[is.na(master$affected.flow),]
master = master[!is.na(master$affected.flow),]
your.data = master[master$affected.flow != "outward subsidy",]
your.data = rbind(your.data, affected.flow.na)
only.outward.subsidy = master[master$affected.flow == "outward subsidy",]

#3.1 convert whats possible the quick way---------------------------------------
# this code is taken directly form JF
### NOTE: 
# 1) DO NOT USE THIS CODE unless Johannes has specifically requested that you do.
# 2) THIS SNIPPET ONLY WORKS FOR affected.flow!="outward subsidy". For export subsidies, please use the gta_trade_coverage function.


gta_sql_kill_connections()

database <<- "gtamain"

gta_sql_pool_open(db.title=database,
                  db.host = gta_pwd(database)$host,
                  db.name = gta_pwd(database)$name,
                  db.user = gta_pwd(database)$user,
                  db.password = gta_pwd(database)$password,
                  table.prefix = "gta_")


compute.coverage=T
correct.ij=T
trade.data.base.year="base"

coverage.outtakes=c(20387, 20389, 16408, 16817, 15248, 20098, 56907,
                    18891,70350,16819,71578,58794,18254,13633,15366,19899,13512,14328,
                    18602,14104,17285,18601,19351,19347,15100,18638,57474,14017,20375,
                    57843,57619,62121,70692,72278,60042,13631,72137,18795,71645,13707,
                    19425,70751,15747,58726,18897,18649,72800,72384,69601, 70466,
                    71010,60343,68840,62147,71561, 82519, 81141, 80982, 78338,80442,80985,
                    79456, 80739, 80552, 81549, 79111,81832,
                    78439,80057, 78044, 78764, 81428, 79740, 81439, 78706, 81786, 78033,
                    62120,70561,73612,73866,77800,79201,79440,80064,80547,81030,81059,81093,81785,84331, 73056,81550, 82986)



if(compute.coverage){
  
  your.interventions=unique(your.data$intervention.id)
  
  int.with.hs=gta_sql_get_value("SELECT DISTINCT intervention_id FROM gta_affected_tariff_line;")
  
  trade.coverage=gta_sql_get_value(paste0("SELECT DISTINCT gatl.intervention_id, tariff_line_code as hs6, gi.affected_flow_id, gj.un_code as i_un
                                                 FROM gta_affected_tariff_line gatl
                                                 JOIN gta_intervention gi
                                                 ON gatl.intervention_id = gi.id
                                                 JOIN gta_implementing_jurisdiction gij
                                                 ON gatl.intervention_id= gij.intervention_id
                                                 JOIN gta_jurisdiction gj
                                                 ON gij.jurisdiction_id = gj.id
                                                 WHERE gatl.intervention_id IN (",paste(your.interventions, collapse=","),");"))
  
  frozen.partners=gta_sql_get_value(paste0("SELECT DISTINCT gi.id as intervention_id, gj2.un_code as d_un, gj.un_code as a_un 
                                           FROM gta_intervention gi
                                           JOIN gta_distorted_market gdm
                                           ON gi.id= gdm.intervention_id
                                           JOIN gta_affected_jurisdiction gaj
                                           ON gi.id= gaj.intervention_id
                                           JOIN gta_jurisdiction gj
                                           ON gaj.jurisdiction_id = gj.id
                                           JOIN gta_jurisdiction gj2
                                           ON gdm.jurisdiction_id = gj2.id
                                           WHERE gi.id IN (",paste(your.interventions, collapse=","),")
                                           AND (gi.dm_freeze=1 OR gi.aj_freeze=1)
                                           AND gaj.type!='D'
                                           AND gdm.type!='D';"))
  
  gta.implementers=gta_sql_get_value(paste0("SELECT intervention_id, gj.un_code as i_un
                                             FROM gta_intervention gi
                                             JOIN gta_implementing_jurisdiction gij
                                             ON gi.id = gij.intervention_id
                                             JOIN gta_jurisdiction gj
                                             ON gij.jurisdiction_id = gj.id
                                             WHERE gi.id IN (",paste(your.interventions, collapse=","),");"))
  
  trade.coverage$hs6=as.numeric(trade.coverage$hs6)
  
  gta_trade_value_bilateral(trade.data = trade.data.base.year)
  
  if(trade.data.base.year=="base"){
    
    trade.base.bilateral$trade.value=trade.base.bilateral$trade.value/3
    
  }
  
  coverage.imports=merge(subset(trade.coverage, affected.flow.id==1),
                         trade.base.bilateral,
                         by=c("i.un","hs6"), all.x=T)
  
  setnames(trade.coverage, "i.un","t.un")
  coverage.exports=merge(subset(trade.coverage, affected.flow.id==2),
                         trade.base.bilateral,
                         by.x=c("t.un","hs6"),by.y = c("a.un","hs6"), all.x=T)
  setnames(coverage.exports, "i.un","a.un")
  setnames(coverage.exports, "t.un","i.un")
  setnames(trade.coverage, "t.un","i.un")
  
  trade.coverage.abs=rbind(coverage.exports,
                           coverage.imports)
  
  if(nrow(frozen.partners)>0){
    
    for(int in unique(frozen.partners$intervention.id)){
      
      trade.coverage.abs=rbind(subset(trade.coverage.abs, intervention.id!=int),
                               subset(trade.coverage.abs, intervention.id==int & a.un %in% frozen.partners$a.un[frozen.partners$intervention.id==int]))
    }
    
  }
  
  # removing implementers from the affected set [optional] 
  if(correct.ij){
    
    for(int in unique(trade.coverage.abs$intervention.id)){
      
      trade.coverage.abs=rbind(subset(trade.coverage.abs, intervention.id!=int),
                               subset(trade.coverage.abs, intervention.id==int & ! a.un %in% subset(gta.implementers, intervention.id==int)$i.un))
    }
    
    
  }
  
  
  trade.coverage.abs=unique(trade.coverage.abs)
  
  # Metric 1: The absolute value of global exports or imports of the implementing jurisdiction on the affected HS codes.
  your.data=merge(your.data,
                  aggregate(trade.value ~ intervention.id, trade.coverage.abs,sum),
                  by="intervention.id", all.x = T)
  your.data$trade.value[is.na(your.data$trade.value)]=0
  your.data$trade.value[!your.data$intervention.id %in% int.with.hs]=NA
  your.data$trade.value[your.data$intervention.id %in% coverage.outtakes]=NA
  your.data$trade.value=round(your.data$trade.value/1000000,2)
  setnames(your.data, "trade.value","trade.value.global")
  
  
} 

gta_sql_pool_close()
i = 7

#3.2 convert rest of code via trade coverage function---------------------------

only.outward.subsidy = cbind(only.outward.subsidy, trade.value.global = NA)

gta_trade_coverage(intervention.ids = only.outward.subsidy[7, "intervention.id"], 
                   keep.interventions = T, 
                   trade.statistic = "value")
trade.coverage.estimates$`Trade coverage estimate for 2019`[1] = "NA"

gta_trade_coverage(intervention.ids = only.outward.subsidy[1, "intervention.id"], 
                   keep.interventions = T, 
                   trade.statistic = "value")
trade.coverage.estimates$`Trade coverage estimate for 2019`[1] = "NA"
only.outward.subsidy[1, "trade.value.global"] = trade.coverage.estimates$`Trade coverage estimate for 2019`[1]

start = Sys.time()
for(i in 2:nrow(only.outward.subsidy)){
  
  gta_trade_coverage(intervention.ids = only.outward.subsidy[i, "intervention.id"], 
                     keep.interventions = T, 
                     trade.statistic = "value")
  if (trade.coverage.estimates$`Trade coverage estimate for 2019`[1] == only.outward.subsidy[i -1, "trade.value.global"]){
    trade.coverage.estimates$`Trade coverage estimate for 2019`[1] = "NA"
  }
  only.outward.subsidy[i, "trade.value.global"] = trade.coverage.estimates$`Trade coverage estimate for 2019`[1]
  
}
end = Sys.time()

only.outward.subsidy$trade.value.global = round(only.outward.subsidy$trade.value.global/1000000, 2) #format the same way as in 3.1

openxlsx::write.xlsx(only.outward.subsidy, "0 dev/gta-28-sh/code/Global-assessment/outwards.subsidy.cases.xlsx")

################################################################################
#4. clean up -------------------------------------------------------------------

data = your.data[!is.na(your.data$trade.value.global), ]
data = rbind(data, only.outward.subsidy)
data.na = your.data[is.na(your.data$trade.value.global), ]


data = data[data$trade.value.global > min.trade.share.value, ] 

