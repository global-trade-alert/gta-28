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



