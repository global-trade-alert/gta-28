####################################################################

# DATA PREP for chapter 4: Global assessment on resort to subsidies

####################################################################

library(gtalibrary)
library(tidyverse)
library(gt)

rm(list = ls())

gta_setwd()

load("0 dev/gta-28-ad/data/Global assessment/data.Rdata")

gtalibrary::gta_colour_palette()

out.path = "0 dev/gta-28-ad/results/Global assessment/"
#out.path.production = "0 report production/GTA 28/tables & figures/"


########################## Figure 1: ############################### 

# Given the number of columns in these tables please produce the tables in landscape format.

table.name = "Figure 1 - L inward table"


inward.table = figure.1%>% 
  gt()%>%
  tab_source_note(
    source_note = "Source: Global Trade Alert (2021)"
  ) %>%
  tab_style(style = cell_borders(sides = "all", color = "grey", weight = px(2)),
            locations = list(cells_body(columns = everything(), rows = everything()), 
                             cells_column_labels(columns = everything())))%>%
  cols_label(implementing.jurisdiction = md("**Implementing Jurisdiction**"), 
             l.inward.changes = md("**Number of implemented L inward changes**"),
             l.inward.harmful.perc = md("**Percentage of harmful L inward changes**"),
             firm.specific.perc = md("**Percentage of firm-specific changes**"),
             horizontal.perc = md("**Percentage of horizontal changes**"),
             implementation.level.perc = md("**Percentage of national or supranational changes**"),
             agriculture.perc = md("**Percentage of changes affecting agriculture**"),
             manufacturing.perc = md("**Percentage of changes affecting manufacturing**"),
             services.perc = md("**Percentage of changes affecting services**"))%>%
  tab_options(
    # hide_column_labels = TRUE,
    # summary_row.background.color = "#ACEACE80",
    # grand_summary_row.background.color = "#990000",
    row_group.background.color = gta_colour$panel.bg,
    # heading.background.color = "#EFFBFC",
    # column_labels.background.color = "#EFFBFC",
    # stub.background.color = "#EFFBFC",
    # table.font.color = "#323232",
    # table_body.hlines.color = "#989898",
    # table_body.border.top.color = "#989898",
    # heading.border.bottom.color = "#989898",
    # row_group.border.top.color = "#989898",
    # row_group.border.bottom.style = "none",
    # stub.border.style = "dashed",
    # stub.border.color = "#989898",
    # stub.border.width = "1px",
    # summary_row.border.color = "#989898",
    # table.width = "60%"
  )

inward.table

gtsave(inward.table, paste0(out.path, table.name,".png"))
#gtsave(inward.table, paste0(out.path.production, table.name,".pdf")) 

xlsx::write.xlsx(inward.table, file=paste0(out.path, table.name,".xlsx"))

#second table
table.name = "Figure 1 - L&P outward table"

outward.table = figure.2%>% 
  gt()%>%
  tab_source_note(
    source_note = "Source: Global Trade Alert (2021)"
  ) %>%
  tab_style(style = cell_borders(sides = "all", color = "grey", weight = px(2)),
            locations = list(cells_body(columns = everything(), rows = everything()), 
                             cells_column_labels(columns = everything())))%>%
  cols_label(implementing.jurisdiction = md("**Implementing Jurisdiction**"), 
             p.outward.changes = md("**Number of implemented L&P outward changes**"),
             p.outward.harmful.perc = md("**Percentage of harmful L&P outward changes**"),
             firm.specific.perc = md("**Percentage of firm-specific changes**"),
             horizontal.perc = md("**Percentage of horizontal changes**"),
             implementation.level.perc = md("**Percentage of national or supranational changes**"),
             agriculture.perc = md("**Percentage of changes affecting agriculture**"),
             manufacturing.perc = md("**Percentage of changes affecting manufacturing**"),
             services.perc = md("**Percentage of changes affecting services**"))%>%
  tab_options(
    # hide_column_labels = TRUE,
    # summary_row.background.color = "#ACEACE80",
    # grand_summary_row.background.color = "#990000",
    row_group.background.color = gta_colour$panel.bg,
    # heading.background.color = "#EFFBFC",
    # column_labels.background.color = "#EFFBFC",
    # stub.background.color = "#EFFBFC",
    # table.font.color = "#323232",
    # table_body.hlines.color = "#989898",
    # table_body.border.top.color = "#989898",
    # heading.border.bottom.color = "#989898",
    # row_group.border.top.color = "#989898",
    # row_group.border.bottom.style = "none",
    # stub.border.style = "dashed",
    # stub.border.color = "#989898",
    # stub.border.width = "1px",
    # summary_row.border.color = "#989898",
    # table.width = "60%"
  )

outward.table

gtsave(outward.table, paste0(out.path, table.name,".png"))
#gtsave(outward.table, paste0(out.path.production, table.name,".pdf")) 

xlsx::write.xlsx(outward.table, file=paste0(out.path, table.name,".xlsx"))


########################## Figure 2: ############################### 


map.name = "Figure 2 - Frequency interests harmed by L inward policies - 2019"

harmed.l.inward = merge(harmed.l.inward, select(country.names, name, un_code), by.x = "affected.jurisdiction", by.y = "name")
harmed.l.inward.data = gta_plot_map_df(data = harmed.l.inward, countries = "un_code", values = "count")


harmed.inward.plot <- ggplot() +
  geom_polygon(data = subset(harmed.l.inward.data, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
  geom_polygon(data = subset(harmed.l.inward.data, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=value), na.rm = T, size = 0.15, color = "white") +
  geom_polygon(data = subset(harmed.l.inward.data, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits = c(-13900000,17000000))+
  ggtitle("Frequency of interests harmed by L inward policies in 2019")+
  labs(x = "", y = "", caption = "Source: Global Trade Alert") +
  #scale_fill_manual(values = c("Slower Growth" = gta_colour$blue[4], "Faster Growth" = gta_colour$blue[2]),
  # position="bottom", labels=c("Faster improvement of labour", "Slower improvement of labour"), na.translate=F) +
  # scale_fill_gradientn(midpoint = mean(labour$difference), 
  #                      high = gta_colour$red[1], mid = gta_colour$amber[4], low = gta_colour$green[1], 
  #                      space = "Lab")+
  scale_fill_gradientn(name="Number of products affected",
                       na.value="#c6c6c6",
                       limits=c(0,max(harmed.l.inward.data$value, na.rm=T)),
                       colors = c(gta_colour$amber[4], gta_colour$amber[1], gta_colour$red[1]),
                       breaks=round(seq(0, max(harmed.l.inward.data$value, na.rm = T), max(harmed.l.inward.data$value, na.rm=T) / 4)),
                       guide=guide_colorbar(barwidth=13, label.hjust = 0.5, title.position = "top"),
                       labels=round(seq(0, max(harmed.l.inward.data$value, na.rm = T), max(harmed.l.inward.data$value, na.rm=T) / 4)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.5,0),
        legend.justification = c(0.5,0.4),
        legend.direction = "horizontal",
        plot.title = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 11, hjust = 0.5, margin = margin(t=6,b=6)),
        plot.subtitle = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 9, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5),hjust=0.5),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.75, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
        legend.text.align = 0.6,
        legend.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="#F9F9F9"),
        plot.caption = element_text(hjust = 0.5, vjust = 6, margin = margin(t=60, b=5),size=8, color="#777777",lineheight = 1))

harmed.inward.plot
gta_plot_saver(plot = harmed.inward.plot,
               path = out.path,
               name = map.name,
               png = T,
               width = 27.4,
               height = 17.2)
xlsx::write.xlsx(harmed.l.inward, file=paste0(out.path, map.name, ".xlsx"))



########################## Figure 3: ############################### 


map.name = "Figure 3 - Frequency interests harmed by L&P outward policies - 2019"

harmed.p.outward = merge(harmed.p.outward, select(country.names, name, un_code), by.x = "affected.jurisdiction", by.y = "name")
harmed.p.outward.data = gta_plot_map_df(data = harmed.p.outward, countries = "un_code", values = "count")


harmed.outward.plot <- ggplot() +
  geom_polygon(data = subset(harmed.p.outward.data, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
  geom_polygon(data = subset(harmed.p.outward.data, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=value), na.rm = T, size = 0.15, color = "white") +
  geom_polygon(data = subset(harmed.p.outward.data, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
  coord_fixed() + # Important to fix world map proportions
  scale_x_continuous(limits = c(-13900000,17000000))+
  ggtitle("Frequency of interests harmed by L inward policies in 2019")+
  labs(x = "", y = "", caption = "Source: Global Trade Alert") +
  #scale_fill_manual(values = c("Slower Growth" = gta_colour$blue[4], "Faster Growth" = gta_colour$blue[2]),
  # position="bottom", labels=c("Faster improvement of labour", "Slower improvement of labour"), na.translate=F) +
  # scale_fill_gradientn(midpoint = mean(labour$difference), 
  #                      high = gta_colour$red[1], mid = gta_colour$amber[4], low = gta_colour$green[1], 
  #                      space = "Lab")+
  scale_fill_gradientn(name="Number of products affected",
                       na.value="#c6c6c6",
                       limits=c(0,max(harmed.p.outward.data$value, na.rm=T)),
                       colors = c(gta_colour$amber[4], gta_colour$amber[1], gta_colour$red[1]),
                       breaks=round(seq(0, max(harmed.p.outward.data$value, na.rm = T), max(harmed.p.outward.data$value, na.rm=T) / 4)),
                       guide=guide_colorbar(barwidth=13, label.hjust = 0.5, title.position = "top"),
                       labels=round(seq(0, max(harmed.p.outward.data$value, na.rm = T), max(harmed.p.outward.data$value, na.rm=T) / 4)))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.5,0),
        legend.justification = c(0.5,0.4),
        legend.direction = "horizontal",
        plot.title = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 11, hjust = 0.5, margin = margin(t=6,b=6)),
        plot.subtitle = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 9, hjust = 0.5, margin = margin(b=10)),
        legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5),hjust=0.5),
        legend.text = element_text(family="", colour = "#333333", size = 11*0.75, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
        legend.text.align = 0.6,
        legend.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="#F9F9F9"),
        plot.caption = element_text(hjust = 0.5, vjust = 6, margin = margin(t=60, b=5),size=8, color="#777777",lineheight = 1))

harmed.outward.plot
gta_plot_saver(plot = harmed.outward.plot,
               path = out.path,
               name = map.name,
               png = T,
               width = 27.4,
               height = 17.2)
xlsx::write.xlsx(harmed.p.outward, file=paste0(out.path, map.name, ".xlsx"))



########################## Figure 4: ############################### 


# map.name = "Figure 4 - Frequency interests benefiting from L&P outward policies - 2019"
# 
# benefited.p.outward = merge(benefited.p.outward, select(country.names, name, un_code), by.x = "affected.jurisdiction", by.y = "name")
# benefited.p.outward.data = gta_plot_map_df(data = benefited.p.outward, countries = "un_code", values = "count")
# 
# 
# benefitted.outward.plot <- ggplot() +
#   geom_polygon(data = subset(benefited.p.outward.data, country != "Antarctica"), aes(x = long, y = lat, group = group), fill="#dadada", size = 0.15, color = "white") +
#   geom_polygon(data = subset(benefited.p.outward.data, country != "Antarctica"), aes(x = long, y = lat, group = group, fill=value), na.rm = T, size = 0.15, color = "white") +
#   geom_polygon(data = subset(benefited.p.outward.data, country == "Greenland"), aes(x=long, y=lat, group = group), fill="#dadada", size = 0.15, colour = "white") +
#   coord_fixed() + # Important to fix world map proportions
#   scale_x_continuous(limits = c(-13900000,17000000))+
#   ggtitle("Frequency of interests harmed by L inward policies in 2019")+
#   labs(x = "", y = "", caption = "Source: Global Trade Alert") +
#   #scale_fill_manual(values = c("Slower Growth" = gta_colour$blue[4], "Faster Growth" = gta_colour$blue[2]),
#   # position="bottom", labels=c("Faster improvement of labour", "Slower improvement of labour"), na.translate=F) +
#   # scale_fill_gradientn(midpoint = mean(labour$difference), 
#   #                      high = gta_colour$red[1], mid = gta_colour$amber[4], low = gta_colour$green[1], 
#   #                      space = "Lab")+
#   scale_fill_gradientn(name="Number of products affected",
#                        na.value="#c6c6c6",
#                        limits=c(0,max(benefited.p.outward.data$value, na.rm=T)),
#                        colors = c(gta_colour$amber[4], gta_colour$amber[1], gta_colour$red[1]),
#                        breaks=round(seq(0, max(benefited.p.outward.data$value, na.rm = T), max(benefited.p.outward.data$value, na.rm=T) / 4)),
#                        guide=guide_colorbar(barwidth=13, label.hjust = 0.5, title.position = "top"),
#                        labels=round(seq(0, max(benefited.p.outward.data$value, na.rm = T), max(benefited.p.outward.data$value, na.rm=T) / 4)))+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         panel.grid = element_blank(),
#         panel.background = element_blank(),
#         legend.position = c(0.5,0),
#         legend.justification = c(0.5,0.4),
#         legend.direction = "horizontal",
#         plot.title = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 11, hjust = 0.5, margin = margin(t=6,b=6)),
#         plot.subtitle = element_text(family = "Open Sans", face = "bold", colour = "#333333", size = 9, hjust = 0.5, margin = margin(b=10)),
#         legend.title = element_text(vjust= 0.3, family="", colour = "#333333", size = 11*0.8, margin = margin(r=10,b=5),hjust=0.5),
#         legend.text = element_text(family="", colour = "#333333", size = 11*0.75, angle = 0, hjust=0, vjust=1, margin = margin(r=10)),
#         legend.text.align = 0.6,
#         legend.background = element_rect(fill="transparent"),
#         plot.background = element_rect(fill="#F9F9F9"),
#         plot.caption = element_text(hjust = 0.5, vjust = 6, margin = margin(t=60, b=5),size=8, color="#777777",lineheight = 1))
# 
# benefitted.outward.plot
# gta_plot_saver(plot = benefitted.outward.plot,
#                path = out.path,
#                name = map.name,
#                png = T,
#                width = 27.4,
#                height = 17.2)
# xlsx::write.xlsx(benefited.p.outward, file=paste0(out.path, map.name, ".xlsx"))


