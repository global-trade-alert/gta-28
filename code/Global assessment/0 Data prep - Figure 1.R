####################################################################

# DATA PREP for chapter 4: Global assessment on resort to subsidies

####################################################################

library(gtalibrary)
library(tidyverse)
library(janitor)


rm(list = ls())

gta_setwd()


data.path = "0 dev/gta-28-ad/data/Global assessment/data.Rdata"
data.path2 = "0 dev/gta-28-ad/data/Global assessment/data master.Rdata"

########################## Figure 1: ############################### 
#Update: Two tables
# (1) L inward
# •	Number of implemented L inward policy changes #
# •	Percentage of implemented L inward policy changes that are amber or red (harmful to trading partners) #
# •	Percentage of firm-specific policy interventions #
# •	Percentage of horizontal subsidy interventions #
# •	Percentage of national or supranational interventions #
# •	Percentage of subsidy changes in the agricultural sector
# •	Percentage of subsidy changes in the manufacturing sector
# •	Percentage of subsidy changes in the service sector
# (2)  L+P outward subsidy
# •	Number of implemented L outward  and P policy changes
# •	Percentage of implemented L outward and P policy changes that are amber or red (harmful to trading partners)
# •	Percentage of firm-specific policy interventions
# •	Percentage of horizontal subsidy interventions
# •	Percentage of national or supranational interventions
# •	Percentage of subsidy changes in the agricultural sector
# •	Percentage of subsidy changes in the manufacturing sector
# •	Percentage of subsidy changes in the service sector

# Given the number of columns in this table please produce the table in landscape format.

eu.countries = gtalibrary::country.names$name[country.names$is.eu==T]


#First table########
#First three columns 
gta_data_slicer(implementing.country = c("China", "United States of America", eu.countries),
                keep.implementer = T,
                affected.flows = "inward",
                mast.chapters = "L",
                keep.mast = T)
master.l.inward = master.sliced #just for maps after this 
master.sliced$implementing.jurisdiction = as.character(master.sliced$implementing.jurisdiction)
master.sliced$implementing.jurisdiction[master.sliced$implementing.jurisdiction %in% eu.countries] <- "EU28"


figure.1 = aggregate(intervention.id ~ implementing.jurisdiction, master.sliced, function(x) length(unique(x)))
figure.1 = figure.1%>% adorn_totals("row")
names(figure.1)[2] = "l.inward.changes"


col.3 = master.sliced%>% 
  filter(gta.evaluation=="Red"|gta.evaluation=="Amber")
col.3 = aggregate(intervention.id ~ implementing.jurisdiction, col.3, function(x) length(unique(x)))
col.3 = col.3%>% adorn_totals("row")
names(col.3)[2] = "l.inward.harmful"
figure.1 = merge(figure.1,col.3, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, l.inward.harmful.perc = l.inward.harmful/l.inward.changes*100)
figure.1 = select(figure.1, -l.inward.harmful)

col.4 = master.sliced%>% 
  filter(eligible.firms=="firm-specific")
col.4 = aggregate(intervention.id ~ implementing.jurisdiction, col.4, function(x) length(unique(x)))
col.4 = col.4%>% adorn_totals("row")
names(col.4)[2] = "firm.specific"
figure.1 = merge(figure.1,col.4, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, firm.specific.perc = firm.specific/l.inward.changes*100)
figure.1 = select(figure.1, -firm.specific)

#adding horizontal measure
load("data/database replica/database replica - parts - base.Rdata")
master.sliced = merge(master.sliced, gta_intervention[c(1,9)], by.x = "intervention.id", by.y = "intervention_id")

col.5 = master.sliced%>% 
  filter(is_horizontal_measure==1)
col.5 = aggregate(intervention.id ~ implementing.jurisdiction, col.5, function(x) length(unique(x)))
col.5 = col.5%>% adorn_totals("row")
names(col.5)[2] = "horizontal"
figure.1 = merge(figure.1,col.5, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, horizontal.perc = horizontal/l.inward.changes*100)
figure.1 = select(figure.1, -horizontal)

col.6 = master.sliced%>% 
  filter(implementation.level=="national"|implementation.level=="supranational")
col.6 = aggregate(intervention.id ~ implementing.jurisdiction, col.6, function(x) length(unique(x)))
col.6 = col.6%>% adorn_totals("row")
names(col.6)[2] = "implementation.level"
figure.1 = merge(figure.1,col.6, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, implementation.level.perc = implementation.level/l.inward.changes*100)
figure.1 = select(figure.1, -implementation.level)

#By sector:
#agriculture
#manufacturing 
#services

master.sliced$agriculture[grepl("(^(0|2))", master.sliced$affected.sector)] = 1
master.sliced$agriculture[!grepl("(^(0|2))", master.sliced$affected.sector)] = 0

master.sliced$manufacturing[grepl("(^(1|3|4))", master.sliced$affected.sector)] = 1
master.sliced$manufacturing[!grepl("(^(1|3|4))", master.sliced$affected.sector)] = 0

master.sliced$services[grepl("(^(5|6|7|8|9))", master.sliced$affected.sector)] = 1
master.sliced$services[!grepl("(^(5|6|7|8|9))", master.sliced$affected.sector)] = 0

#adding the additional sector codes (if there are any)
#about 10 cases were checked since this is a new function I was using and all seems correct. 
master.sliced = master.sliced%>%
  mutate(additional.sectors = map_chr(str_extract_all(affected.sector, "(?<=, )(\\d)"), ~ str_c(.x, collapse=" ")))

master.sliced$agriculture[grepl("(0|2)", master.sliced$additional.sectors)] = 1
master.sliced$manufacturing[grepl("(1|3|4)", master.sliced$additional.sectors)] = 1
master.sliced$services[grepl("(5|6|7|8|9)", master.sliced$additional.sectors)] = 1


col.7 = master.sliced%>% 
  filter(agriculture==1)
col.7 = aggregate(intervention.id ~ implementing.jurisdiction, col.7, function(x) length(unique(x)))
col.7 = col.7%>% adorn_totals("row")
names(col.7)[2] = "agriculture"
figure.1 = merge(figure.1,col.7, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, agriculture.perc = agriculture/l.inward.changes*100)
figure.1 = select(figure.1, -agriculture)

col.8 = master.sliced%>% 
  filter(manufacturing==1)
col.8 = aggregate(intervention.id ~ implementing.jurisdiction, col.8, function(x) length(unique(x)))
col.8 = col.8%>% adorn_totals("row")
names(col.8)[2] = "manufacturing"
figure.1 = merge(figure.1,col.8, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, manufacturing.perc = manufacturing/l.inward.changes*100)
figure.1 = select(figure.1, -manufacturing)

col.9 = master.sliced%>% 
  filter(services==1)
col.9 = aggregate(intervention.id ~ implementing.jurisdiction, col.9, function(x) length(unique(x)))
col.9 = col.9%>% adorn_totals("row")
names(col.9)[2] = "services"
figure.1 = merge(figure.1,col.9, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, services.perc = services/l.inward.changes*100)
figure.1 = select(figure.1, -services)
figure.1 = figure.1[c(1,2,4,3),]





###########Second table########

gta_data_slicer(implementing.country = c("China", "United States of America", eu.countries),
                keep.implementer = T,
                affected.flows = "outward subsidy",
                mast.chapters = c("L","P"),
                keep.mast = T)
master.p.outward = master.sliced
master.sliced$implementing.jurisdiction = as.character(master.sliced$implementing.jurisdiction)
master.sliced$implementing.jurisdiction[master.sliced$implementing.jurisdiction %in% eu.countries] <- "EU28"

figure.2 = aggregate(intervention.id ~ implementing.jurisdiction, master.sliced, function(x) length(unique(x)))
figure.2 = figure.2%>% adorn_totals("row")
names(figure.2)[2] = "p.outward.changes"

col.3 = master.sliced%>% 
  filter(gta.evaluation=="Red"|gta.evaluation=="Amber")
col.3 = aggregate(intervention.id ~ implementing.jurisdiction, col.3, function(x) length(unique(x)))
col.3 = col.3%>% adorn_totals("row")
names(col.3)[2] = "p.outward.harmful"
figure.2 = merge(figure.2,col.3, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, p.outward.harmful.perc = p.outward.harmful/p.outward.changes*100)
figure.2 = select(figure.2, -p.outward.harmful)

col.4 = master.sliced%>% 
  filter(eligible.firms=="firm-specific")
col.4 = aggregate(intervention.id ~ implementing.jurisdiction, col.4, function(x) length(unique(x)))
col.4 = col.4%>% adorn_totals("row")
names(col.4)[2] = "firm.specific"
figure.2 = merge(figure.2,col.4, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, firm.specific.perc = firm.specific/p.outward.changes*100)
figure.2 = select(figure.2, -firm.specific)


#adding horizontal measure
master.sliced = merge(master.sliced, gta_intervention[c(1,9)], by.x = "intervention.id", by.y = "intervention_id")

col.5 = master.sliced%>% 
  filter(is_horizontal_measure==1)
col.5 = aggregate(intervention.id ~ implementing.jurisdiction, col.5, function(x) length(unique(x)))
col.5 = col.5%>% adorn_totals("row")
names(col.5)[2] = "horizontal"
figure.2 = merge(figure.2,col.5, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, horizontal.perc = horizontal/p.outward.changes*100)
figure.2 = select(figure.2, -horizontal)

col.6 = master.sliced%>% 
  filter(implementation.level=="national"|implementation.level=="supranational")
col.6 = aggregate(intervention.id ~ implementing.jurisdiction, col.6, function(x) length(unique(x)))
col.6 = col.6%>% adorn_totals("row")
names(col.6)[2] = "implementation.level"
figure.2 = merge(figure.2,col.6, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, implementation.level.perc = implementation.level/p.outward.changes*100)
figure.2 = select(figure.2, -implementation.level)

#By sector:
#agriculture
#manufacturing 
#services

master.sliced$agriculture[grepl("(^(0|2))", master.sliced$affected.sector)] = 1
master.sliced$agriculture[!grepl("(^(0|2))", master.sliced$affected.sector)] = 0

master.sliced$manufacturing[grepl("(^(1|3|4))", master.sliced$affected.sector)] = 1
master.sliced$manufacturing[!grepl("(^(1|3|4))", master.sliced$affected.sector)] = 0

master.sliced$services[grepl("(^(5|6|7|8|9))", master.sliced$affected.sector)] = 1
master.sliced$services[!grepl("(^(5|6|7|8|9))", master.sliced$affected.sector)] = 0

#adding the additional sector codes (if there are any)
master.sliced = master.sliced%>%
  mutate(additional.sectors = map_chr(str_extract_all(affected.sector, "(?<=, )(\\d)"), ~ str_c(.x, collapse=" ")))

master.sliced$agriculture[grepl("(0|2)", master.sliced$additional.sectors)] = 1
master.sliced$manufacturing[grepl("(1|3|4)", master.sliced$additional.sectors)] = 1
master.sliced$services[grepl("(5|6|7|8|9)", master.sliced$additional.sectors)] = 1

col.7 = master.sliced%>% 
  filter(agriculture==1)
col.7 = aggregate(intervention.id ~ implementing.jurisdiction, col.7, function(x) length(unique(x)))
col.7 = col.7%>% adorn_totals("row")
names(col.7)[2] = "agriculture"
figure.2 = merge(figure.2,col.7, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, agriculture.perc = agriculture/p.outward.changes*100)
figure.2 = select(figure.2, -agriculture)

col.8 = master.sliced%>% 
  filter(manufacturing==1)
col.8 = aggregate(intervention.id ~ implementing.jurisdiction, col.8, function(x) length(unique(x)))
col.8 = col.8%>% adorn_totals("row")
names(col.8)[2] = "manufacturing"
figure.2 = merge(figure.2,col.8, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, manufacturing.perc = manufacturing/p.outward.changes*100)
figure.2 = select(figure.2, -manufacturing)

col.9 = master.sliced%>% 
  filter(services==1)
col.9 = aggregate(intervention.id ~ implementing.jurisdiction, col.9, function(x) length(unique(x)))
col.9 = col.9%>% adorn_totals("row")
names(col.9)[2] = "services"
figure.2 = merge(figure.2,col.9, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, services.perc = services/p.outward.changes*100)
figure.2 = select(figure.2, -services)
figure.2 = figure.2[c(1,2,4,3),]



#########################################################################################

save(figure.1, figure.2, harmed.l.inward, harmed.p.outward, 
     #benefited.p.outward, 
     file = data.path)

save(master.l.inward, master.p.outward, file = data.path2)

