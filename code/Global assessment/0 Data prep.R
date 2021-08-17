####################################################################

# DATA PREP for chapter 4: Global assessment on resort to subsidies

####################################################################

library(gtalibrary)
library(tidyverse)

rm(list = ls())

gta_setwd()





########################## Figure 1: ############################### 

# Please prepare a table with separate rows for China, EU28, the United States, and the total for all three jurisdictions. The columns of the table should report:
# •	Number of implemented L inward policy changes #
# •	Percentage of implemented L inward policy changes that are amber or red (harmful to trading partners) #
# •	Number of implemented L outward  and P policy changes #
# •	Percentage of implemented L outward and P policy changes that are amber or red (harmful to trading partners) #
# •	Percentage of firm-specific policy interventions
# •	Percentage of horizontal subsidy interventions
# •	Percentage of subsidy changes in the agricultural sector
# •	Percentage of subsidy changes in the manufacturing sector
# •	Percentage of subsidy changes in the service sector  

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
master.sliced$implementing.jurisdiction = as.character(master.sliced$implementing.jurisdiction)
master.sliced$implementing.jurisdiction[master.sliced$implementing.jurisdiction %in% eu.countries] <- "EU28"


figure.1 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  summarise(l.inward.changes = n()
            #l.inward.harmful = length(filter(gta.evaluation=="Red"|gta.evaluation=="Amber"))/n()
            )
col.3 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(gta.evaluation=="Red"|gta.evaluation=="Amber")%>%
  summarize(l.inward.harmful = n())
figure.1 = merge(figure.1,col.3, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, l.inward.harmful.perc = l.inward.harmful/l.inward.changes*100)
figure.1 = select(figure.1, -l.inward.harmful)

col.4 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(eligible.firms=="firm-specific")%>%
  summarize(firm.specific = n())
figure.1 = merge(figure.1,col.4, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, firm.specific.perc = firm.specific/l.inward.changes*100)
figure.1 = select(figure.1, -firm.specific)

#adding horizontal measure
load("data/database replica/database replica - parts - base.Rdata")
master.sliced = merge(master.sliced, gta_intervention[c(1,9)], by.x = "intervention.id", by.y = "intervention_id")

col.5 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(is_horizontal_measure==1)%>%
  summarize(horizontal = n())
figure.1 = merge(figure.1,col.5, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, horizontal.perc = horizontal/l.inward.changes*100)
figure.1 = select(figure.1, -horizontal)

col.6 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(implementation.level=="national"|implementation.level=="supranational")%>%
  summarize(implementation.level = n())
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
master.sliced = master.sliced%>%
  mutate(additional.sectors = map_chr(str_extract_all(affected.sector, "(?<=, )(\\d)"), ~ str_c(.x, collapse=" ")))

master.sliced$agriculture[grepl("(0|2)", master.sliced$additional.sectors)] = 1
master.sliced$manufacturing[grepl("(1|3|4)", master.sliced$additional.sectors)] = 1
master.sliced$services[grepl("(5|6|7|8|9)", master.sliced$additional.sectors)] = 1

col.7 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(agriculture==1)%>%
  summarize(agriculture = n())
figure.1 = merge(figure.1,col.7, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, agriculture.perc = agriculture/l.inward.changes*100)
figure.1 = select(figure.1, -agriculture)

col.8 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(manufacturing==1)%>%
  summarize(manufacturing = n())
figure.1 = merge(figure.1,col.8, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, manufacturing.perc = manufacturing/l.inward.changes*100)
figure.1 = select(figure.1, -manufacturing)

col.9 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(services==1)%>%
  summarize(services = n())
figure.1 = merge(figure.1,col.9, by = "implementing.jurisdiction")
figure.1 = mutate(figure.1, services.perc = services/l.inward.changes*100)
figure.1 = select(figure.1, -services)





###########Second table########
gta_data_slicer(implementing.country = c("China", "United States of America", eu.countries),
                keep.implementer = T,
                affected.flows = "outward subsidy",
                mast.chapters = c("L","P"),
                keep.mast = T)
master.sliced$implementing.jurisdiction = as.character(master.sliced$implementing.jurisdiction)
master.sliced$implementing.jurisdiction[master.sliced$implementing.jurisdiction %in% eu.countries] <- "EU28"

figure.2 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  summarise(p.outward.changes = n())
col.3 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(gta.evaluation=="Red"|gta.evaluation=="Amber")%>%
  summarize(p.outward.harmful = n())
figure.2 = merge(figure.2,col.3, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, p.outward.harmful.perc = p.outward.harmful/p.outward.changes*100)
figure.2 = select(figure.2, -p.outward.harmful)


col.4 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(eligible.firms=="firm-specific")%>%
  summarize(firm.specific = n())
figure.2 = merge(figure.2,col.4, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, firm.specific.perc = firm.specific/p.outward.changes*100)
figure.2 = select(figure.2, -firm.specific)


#adding horizontal measure
master.sliced = merge(master.sliced, gta_intervention[c(1,9)], by.x = "intervention.id", by.y = "intervention_id")

col.5 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(is_horizontal_measure==1)%>%
  summarize(horizontal = n())
figure.2 = merge(figure.2,col.5, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, horizontal.perc = horizontal/p.outward.changes*100)
figure.2 = select(figure.2, -horizontal)

col.6 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(implementation.level=="national"|implementation.level=="supranational")%>%
  summarize(implementation.level = n())
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
  group_by(implementing.jurisdiction)%>% 
  filter(agriculture==1)%>%
  summarize(agriculture = n())
figure.2 = merge(figure.2,col.7, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, agriculture.perc = agriculture/p.outward.changes*100)
figure.2 = select(figure.2, -agriculture)

col.8 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(manufacturing==1)%>%
  summarize(manufacturing = n())
figure.2 = merge(figure.2,col.8, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, manufacturing.perc = manufacturing/p.outward.changes*100)
figure.2 = select(figure.2, -manufacturing)

col.9 = master.sliced%>% 
  group_by(implementing.jurisdiction)%>% 
  filter(services==1)%>%
  summarize(services = n())
figure.2 = merge(figure.2,col.9, by = "implementing.jurisdiction")
figure.2 = mutate(figure.2, services.perc = services/p.outward.changes*100)
figure.2 = select(figure.2, -services)


#########################################################################################

save(figure.1, figure.2, file = "0 dev/gta-28-ad/data/Global assessment/data.Rdata")


