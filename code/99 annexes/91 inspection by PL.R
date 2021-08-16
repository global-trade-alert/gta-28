library(gtalibrary)
library(lubridate)
library(stringr)
library(splitstackshape)
library(tidyverse)

rm(list = ls())

##Check ARG prot; CHN prot; EU (&UK) lib & prot; IND lib & prot; 

gtalibrary::gta_data_slicer(implementing.country = "Argentina", keep.implementer = T)

master.sliced=cSplit(master.sliced, which(colnames(master.sliced)=="affected.product"), direction="long", sep=", ")
master.sliced = merge(master.sliced,aggregate(intervention.id ~ affected.product, master.sliced, function(x) length(unique(x))), by="affected.product",all.x = T)
test = aggregate(affected.product ~ intervention.id.x + gta.evaluation + date.implemented, master.sliced, function(x) length(unique(x)))
test = merge(test, aggregate(affected.product ~ intervention.id.x, subset(master.sliced,intervention.id.y==1), function(x) length(unique(x))), by=c("intervention.id.x"),all.x = T)
test = test[order(-test$affected.product.x),]
names(test) = c("intervention.id","gta.evaluation","date.implemented","total.HS","contributed.HS")
test[1:5,]


gtalibrary::gta_data_slicer(implementing.country = "France", keep.implementer = T)

master.sliced=cSplit(master.sliced, which(colnames(master.sliced)=="affected.product"), direction="long", sep=", ")
master.sliced = merge(master.sliced,aggregate(intervention.id ~ affected.product, master.sliced, function(x) length(unique(x))), by="affected.product",all.x = T)
test = aggregate(affected.product ~ intervention.id.x + gta.evaluation + date.implemented, master.sliced, function(x) length(unique(x)))
test = merge(test, aggregate(affected.product ~ intervention.id.x, subset(master.sliced,intervention.id.y<2), function(x) length(unique(x))), by=c("intervention.id.x"),all.x = T)
test = test[order(-test$affected.product.x),]
names(test) = c("intervention.id","gta.evaluation","total.HS","contributed.HS")
test[1:5,]



gtalibrary::gta_data_slicer(implementing.country = "China", keep.implementer = T)

master.sliced=cSplit(master.sliced, which(colnames(master.sliced)=="affected.product"), direction="long", sep=", ")
master.sliced = merge(master.sliced,aggregate(intervention.id ~ affected.product, master.sliced, function(x) length(unique(x))), by="affected.product",all.x = T)
test = aggregate(affected.product ~ intervention.id.x + gta.evaluation + date.implemented, master.sliced, function(x) length(unique(x)))
test = merge(test, aggregate(affected.product ~ intervention.id.x, subset(master.sliced,intervention.id.y<2), function(x) length(unique(x))), by=c("intervention.id.x"),all.x = T)
test = test[order(-test$affected.product.x),]
names(test) = c("intervention.id","gta.evaluation","total.HS","contributed.HS")
test[1:5,]


gtalibrary::gta_data_slicer(implementing.country = "India", keep.implementer = T)

master.sliced=cSplit(master.sliced, which(colnames(master.sliced)=="affected.product"), direction="long", sep=", ")
master.sliced = merge(master.sliced,aggregate(intervention.id ~ affected.product, master.sliced, function(x) length(unique(x))), by="affected.product",all.x = T)
test = aggregate(affected.product ~ intervention.id.x + gta.evaluation + date.implemented, master.sliced, function(x) length(unique(x)))
test = merge(test, aggregate(affected.product ~ intervention.id.x, subset(master.sliced,intervention.id.y<1), function(x) length(unique(x))), by=c("intervention.id.x"),all.x = T)
test = test[order(-test$affected.product.x),]
names(test) = c("intervention.id","gta.evaluation","total.HS","contributed.HS")
test[1:5,]


