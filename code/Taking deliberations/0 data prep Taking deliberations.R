# 1. 
# Go to the WTO website (https://www.wto.org/english/tratop_e/scm_e/scm_e.htm)  and download the statistics for the number of countervailing duty investigations launched each year since 2000. 
# Please produce a stacked bar chart showing the number of investigations initiated by China, EU28, and USA, and rest of the world. 
# Please compare this stacked bar chart with a comparable on produced for the years 2009 on using the GTA’s own data (Intervention type “anti-subsidy”; use announcement date for init year; ask Josse to confirm correspondence.). 
# Hopefully our coverage is better.
#Author: Silvan
#Date: 17.8.2021
#Supervisor Piotr

library(openxlsx)


rm(list = ls())


#parameters
path = "0 dev/gta-28-sh/code/taking deliberations/"
path.data = "0 dev/gta-28-sh/data/Taking deliberations/"
juristictions = c("European Union2", "United States", "China")
to.exclud = c("Botswana1", "Eswatini1", "Lesotho1", "Namibia1", "Kazakhstan3") #excluded since they would be double reports (e.g. Botswana is reported together with South Africa, but both Botswana and South Africa are listed seperately)
years = as.character(c(2000:2020))
################################################################################
#1. Get data -------------------------------------------------------------------

data1 = readxl::read_xls(paste0(path.data, "Kopie von CV_InitiationsByRepMem.xls"), skip = 1)



################################################################################
#2. prepare data ---------------------------------------------------------------

#1
#reduce to relevant data
names(data1) = c("reporting.member", 1995:2020, "total")
data1 = data.frame(data1[1:31,] %>% select(c(reporting.member, years))) %>%
                                      filter(!reporting.member %in% to.exclud)

#data1 = data.frame(lapply(data1, function(x) gsub("[*]", "", x)))
data1[, 2:ncol(data1)] = as.numeric(unlist(data1[, 2:ncol(data1)]))
names(data1) = c("reporting.member", 2000:2020)

#calculate rest of world numers
rest.of.the.world = colSums(subset(data1, !reporting.member %in% juristictions)[,2:ncol(data1) ], na.rm = T)
rest.of.the.world = c("rest.of.the.world", rest.of.the.world)
data1 = rbind(data1[data1$reporting.member %in% juristictions,], rest.of.the.world)

data1[data1$reporting.member == "European Union2", "reporting.member"] = "EU"


################################################################################
#3. save data ------------------------------------------------------------------


saveRDS(data1, paste0(path.data, "wto.countervailing.initiations.reported.RData"))

