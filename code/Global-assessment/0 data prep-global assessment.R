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
threshold = 100000000
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


#1.2 load master 

load(file = "data/master_plus.RData")


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
                      filter(intervention.type == "Trade finance" | affected.flow == "outward subsidy") %>%
                        filter(mast.chapter %in% c("L", "P")) %>%
                          filter(eligible.firms == "firm-specific")


#exclude interventions already check and determined to not be included in trade coverage
master = master[!master$intervention.id %in% jumbo.data$intervention.id, ] #jumbo file
master = master[!master$intervention.id %in% out, ] #excluded from trade coverage function

#merge with amounts
master = merge(master, gta.int.value, by ="intervention.id", all.x = T)

#get data without values
test = master[is.na(master$name), ] #29 cases
master = master[!master$intervention.id %in% test$intervention.id, ]

#test for other anomalies in data
test2 = master[master$name == "percent", ] #11 cases, Error but leave in, treat like its a dollar amount

#filter out all cases which are below a certain amount
master = master[master$new.level < threshold, ]


#openxlsx::write.xlsx(test, "0 dev/gta-28-sh/code/Global-assessment/NA.cases.xlsx")

################################################################################
#3. add trade value data -------------------------------------------------------------------

