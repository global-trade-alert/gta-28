rm(list = ls())

### Request:
# I have no major changes in mind. However instead of highlighting 2017-2019 in the export exposure table, we should highlight only 2020.

library(splitstackshape)
library(openxlsx)
library(foreign)
library(ggplot2)
library(scales)
library(gtable)
library(grid)
library(extrafontdb)
library(extrafont)
library(Rttf2pt1)
library(rworldmap)
library(data.table)
library(gtalibrary)

# font_import()
# loadfonts(device="cairo_ps")
# loadfonts(device="win")

gta_setwd()
gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/99 annex - maps/"
out.path = "tables & figures/99 - Annex - maps/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

gta_colour_palette()

load("data/master_plus.Rdata")

op <- par(family = "HersheyGothicEnglish")
par(op)

#### FOR MAPS ONLY: change UN IDs for India, Italy and France

# master$a.un[master$a.un==251]<-250
# master$i.un[master$i.un==251]<-250
#
# master$a.un[master$a.un==381]<-380
# master$i.un[master$i.un==381]<-380
#
# master$a.un[master$a.un==699]<-356
# master$i.un[master$i.un==699]<-356
#
# master$a.un[master$a.un==729]<-728
# master$i.un[master$i.un==729]<-728


conversion <- gtalibrary::country.names[,c("un_code","name")]
setnames(conversion, old="un_code", new="i.un")

world.0 <- gtalibrary::world.geo

conversion$name=as.character(conversion$name)
conversion$name[conversion$name=="United Kingdom"]<-"the United Kingdom"
conversion$name[conversion$name=="United States of America"]<-"the United States"
conversion$name[conversion$name=="Republic of Korea"]<-"South Korea"
conversion$name[conversion$name=="Russian Federation"]<-"Russia"

# cty = "410"
#################### MAPS  ###########################
for(cty in g20.members){
  map.annex<-aggregate(intervention.id ~ a.un, data=subset(master, gta.evaluation!="Green" & currently.in.force=="Yes" & i.un==cty), function(x) length(unique(x)))
  
  world <- world.0
  
  data <- map.annex
  
  data[,c("UN","value")] <- data[,c("a.un","intervention.id")]
  data$UN <- gta_un_code_vector(data$UN)
  
  # merge data with map data
  world = merge(world, data[,c("UN","value")], by="UN", all.x=T)
  
  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  world$value[is.na(world$value) == T] <- 0
  
  marked.country <- gta_un_code_vector(cty)
  
  # CALCULATE THE GRADIENT BREAKS
  world$breaks[world$value == 0] <- "0"
  world$breaks[world$value >= 201] <- "4"
  world$breaks[world$value >= 101 & world$value <=200] <- "3"
  world$breaks[world$value >= 51 & world$value <=100] <- "2"
  world$breaks[world$value >= 1 & world$value <=50] <- "1"
  
  world.xlsx <- unique(subset(world, UN > 0)[,c("country","value")])
  write.xlsx(world.xlsx, file=paste0(gta26.path, out.path,"map_",unique(conversion$name[conversion$i.un==cty]),"_top_data.xlsx"),sheetName = "Results",row.names=F)
  
  map1 = ggplot() +
    geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = breaks), size = 0.15, color = "white") +
    geom_polygon(data=subset(world, UN == marked.country), aes(x=long, y=lat, group = group), fill=gta_colour$turquoise[4], size = 0.15, colour = "white") +
    geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
    coord_fixed() + # Important to fix world map proportions
    scale_x_continuous(limits=c(-13900000,17000000))+
    labs(x="", y="") +
    scale_fill_manual(values = c("0"="#dadada","1"=gta_colour$blue[4],"2"=gta_colour$blue[3],"3"=gta_colour$blue[2],"4"=gta_colour$blue[1]), position="bottom", labels=c("0","1 - 50","51 - 100","101 - 200","201 or more")) + # Set color gradient
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.55,0),
          legend.justification = c(0.5,0.3),
          legend.direction = "horizontal",
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
          legend.text.align = 0,
          legend.background = element_rect(fill="transparent"),
          plot.background = element_rect(fill="#FFFFFF"),
          plot.margin = unit(c(0.0,0.0,0.05,0.0), "npc"),
          
    ) +
    guides(fill=guide_legend(title=paste("Number of times harmed by a protectionist intervention \nimposed by ",unique(conversion$name[conversion$i.un==cty])," and currently in force", sep=""), label.position = "bottom",title.position = "top"),
           ymax=guide_legend(titel="size"))
  
  
  map1
  
  gta_plot_saver(plot=map1,
                 path=paste0(gta26.path, out.path),
                 name=paste0("map_",unique(conversion$name[conversion$i.un==cty]),"_top"),
                 pdf=T,
                 cairo_ps = T,
                 width = 21,
                 height = 12)
  
  
  map.annex<-aggregate(intervention.id ~ i.un, data=subset(master, gta.evaluation!="Green" & currently.in.force=="Yes" & a.un==cty), function(x) length(unique(x)))
  
  world <- world.0
  
  data <- map.annex
  
  data[,c("UN","value")] <- data[,c("i.un","intervention.id")]
  data$UN <- gta_un_code_vector(data$UN)
  
  # merge data with map data
  world = merge(world, data[,c("UN","value")], by="UN", all.x=T)
  
  ###### IMPORTANT, sort for X (id) again
  world <-  world[with(world, order(X)),]
  world$value[is.na(world$value) == T] <- 0
  
  marked.country <- gta_un_code_vector(cty)
  
  world.xlsx <- unique(subset(world, UN > 0)[,c("country","value")])
  write.xlsx(world.xlsx, file=paste0(gta26.path, out.path,"map_",unique(conversion$name[conversion$i.un==cty]),"_bottom_data.xlsx"),sheetName = "Results",row.names=F)
  
  
  # CALCULATE THE GRADIENT BREAKS
  world$breaks[world$value == 0] <- "0"
  world$breaks[world$value >= 201] <- "4"
  world$breaks[world$value >= 101 & world$value <=200] <- "3"
  world$breaks[world$value >= 51 & world$value <=100] <- "2"
  world$breaks[world$value >= 1 & world$value <=50] <- "1"
  
  map2 = ggplot() +
    geom_polygon(data= subset(world, country != "Antarctica"), aes(x = long, y = lat, group = group, fill = breaks), size = 0.15, color = "white") +
    geom_polygon(data=subset(world, UN == marked.country), aes(x=long, y=lat, group = group), fill=gta_colour$turquoise[4], size = 0.15, colour = "white") +
    geom_polygon(data=subset(world, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
    coord_fixed() + # Important to fix world map proportions
    labs(x="", y="") +
    scale_x_continuous(limits=c(-13900000,17000000))+
    scale_fill_manual(values = c("0"="#dadada","1"=gta_colour$blue[4],"2"=gta_colour$blue[3],"3"=gta_colour$blue[2],"4"=gta_colour$blue[1]), position="bottom", labels=c("0","1 - 50","51 - 100","101 - 200","201 or more")) + # Set color gradient
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank(),
          legend.position = c(0.55,0),
          legend.justification = c(0.5,0.3),
          legend.direction = "horizontal",
          plot.title = element_text(family = "", colour = "#333333", size = 11, hjust = 0.5, margin = margin(b=10)),
          legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5)),
          legend.text = element_text(family="", colour = "#333333", size = 11*0.8, angle = 0, hjust=0, vjust=0, margin = margin(r=10)),
          legend.text.align = 0,
          legend.background = element_rect(fill="transparent"),
          plot.background = element_rect(fill="#FFFFFF"),
          plot.margin = unit(c(0.0,0.0,0.05,0.0), "npc")
          
    ) +
    guides(fill=guide_legend(title=paste("Discriminatory interventions harming ",unique(conversion$name[conversion$i.un==cty])," \nwhich are currently in force", sep=""),  label.position = "bottom",title.position = "top"),
           ymax=guide_legend(titel="size"))
  
  
  map2
  
  gta_plot_saver(plot=map2,
                 path=paste0(gta26.path, out.path),
                 name=paste0("map_",unique(conversion$name[conversion$i.un==cty]),"_bottom"),
                 cairo_ps=T,
                 pdf=T,
                 width = 21,
                 height = 12)
  
  print(cty)
  
  
}




