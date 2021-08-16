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
library(lubridate)
library(gtalibrary)

# loadfonts(device="postscript")
# loadfonts(device = "win")
# windowsFonts(my_font=windowsFont("Open Sans"))

gta_setwd()
gta26.path = "0 report production/GTA 26/"
data.path = "data/99 annex - bar charts/"
out.path = "tables & figures/99 - Annex - bar charts/"
source(paste0(gta26.path, "help files/GTA 26 cutoff and definitions.R"))

load("data/master_plus.Rdata")

gta_colour_palette()

## bar chart data: cumulative number of implemented interventions for each G20 member
master$protect=as.numeric(master$gta.evaluation!="Green")
master$year=year(master$date.implemented)
bars.yr=aggregate(intervention.id ~ i.un + year + protect, subset(master, i.un %in% g20.members & is.na(date.implemented)==F) , function(x) length(unique(x)))

bars=expand.grid(unique(bars.yr$i.un),c(2009:2020), c(0,1))
names(bars)=c("i.un","year", "protect")
bars$measures=apply(bars, 1, function(x) sum(subset(bars.yr, i.un==x[1] & year<=x[2] & protect==x[3])$intervention.id))


save(bars, file=paste0(gta26.path, data.path, "Data for bar charts.Rdata"))
write.xlsx(bars, file=paste0(gta26.path, out.path, "Data for bar charts.xlsx"))


annex.bar.charts <- function(member=NULL) {
  
  ## protectionist
  
  (
    p1 <- ggplot(data=subset(bars, i.un==g20.members[member] & protect==1), aes(y=measures, x=year))+
      geom_bar(stat="identity", fill=gta_colour$red[1])+
      geom_label(aes(label=measures), colour="#555555", position=position_dodge(width=0.9), vjust=-0.5)+
      scale_x_continuous(breaks=2009:2020)+
      scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==g20.members[member] & protect==1)$measures)/100+.5)*100)))+
      labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year (or YTD)\n",
           fill="")+
      gta_theme(background.color = "#FFFFFF")    
  )
  
  (
    p2 <- ggplot(data=subset(bars, i.un==g20.members[member] & protect==1), aes(y=measures, x=year))+
      geom_bar(stat="identity", fill=gta_colour$red[1])+
      geom_label(aes(label=measures), colour="#555555", position=position_dodge(width=0.9), vjust=-0.5)+
      scale_x_continuous(breaks=2009:2020)+
      scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==g20.members[member] & protect==1)$measures)/100+.5)*100)))+
      labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
           fill="")+
      gta_theme(background.color = "#FFFFFF")
    )
  
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  # draw it
  png(paste0(gta26.path, out.path, g20.member.names[member],"_bottom_protectionist.png"), width=800, height=800/1.8, res=76, type="cairo")
  grid.draw(g)
  dev.off()
  
  cairo_ps(paste0(gta26.path, out.path, g20.member.names[member],"_bottom_protectionist.eps"), bg = "white", width=11, height=11/1.8, family="Open Sans")
  grid.draw(g)
  dev.off()
  
  cairo_pdf(paste0(gta26.path, out.path, g20.member.names[member],"_bottom_protectionist.pdf"), bg = "white", width=11, height=11/1.8, family="Open Sans")
  grid.draw(g)
  dev.off()
  
  ## liberalising
  
  (
    p1 <- ggplot(data=subset(bars, i.un==g20.members[member] & protect==0), aes(y=measures, x=year))+
      geom_bar(stat="identity", fill=gta_colour$green[1])+
      geom_label(aes(label=measures), colour="#555555", position=position_dodge(width=0.9), vjust=-0.5)+
      scale_x_continuous(breaks=2009:2020)+
      scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==g20.members[member] & protect==1)$measures)/100+.5)*100)))+
      labs(x="\nYear", y="Number of interventions implemented\nfrom November 2008 until the end of the given year (or YTD)\n",
           fill="")+
      gta_theme(background.color = "#FFFFFF")
  )
  
  (
    p2 <- ggplot(data=subset(bars, i.un==g20.members[member] & protect==0), aes(y=measures, x=year))+
      geom_label(aes(label=measures), colour="#555555", position=position_dodge(width=0.9), vjust=-0.5)+
      geom_bar(stat="identity", fill=gta_colour$green[1])+
      scale_x_continuous(breaks=2009:2020)+
      scale_y_continuous(limits=c(0,as.numeric(round(max(subset(bars, i.un==g20.members[member] & protect==1)$measures)/100+.5)*100)))+
      labs(x="Year", y="Number of interventions implemented\nfrom November 2008 until the end of the given year",
           fill="")+
      gta_theme(background.color = "#FFFFFF")
  )
  
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  # draw it
  png(paste0(gta26.path, out.path, g20.member.names[member],"_bottom_liberalising.png"), width=800, height=800/1.8, res=76)
  grid.draw(g)
  dev.off()
  
  cairo_ps(paste0(gta26.path, out.path, g20.member.names[member],"_bottom_liberalising.eps"), bg = "white", width=11, height=11/1.8, family="Open Sans")
  grid.draw(g)
  dev.off()
  
  cairo_pdf(paste0(gta26.path, out.path, g20.member.names[member],"_bottom_liberalising.pdf"), bg = "white", width=11, height=11/1.8, family="Open Sans")
  grid.draw(g)
  dev.off()

}  


# DRAW FIGURES

for (m in 1:length(g20.members)) {
  annex.bar.charts(m)
}
  
