library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)
library(lattice)
library(effects)
library(boot)
library(labelled)
#library(questionr) #pour quoi faire ?
library(tidyr)
library(stringr)
#attention conflit entre le "select" de dplyr et celui de raster

#https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html

library(networkD3)
library(htmlwidgets)

# Total -------------------------------------------------------------------

# Make a connection data frame and checking in/out flows
links <- read.csv("IDF_all.csv", sep=";")
check <- full_join( #checking in/out in all nodes
  links %>% group_by(source) %>% summarise(in_flow = sum(value)) %>% rename(node = source),
  out_flow <- links %>% group_by(target) %>% summarise(out_flow = sum(value)) %>% rename(node = target), 
  by = "node") %>%
  mutate(abs_diff = out_flow-in_flow, rel_diff = (out_flow-in_flow)/in_flow)
check %>% filter(is.na(in_flow) == T) %>% select(out_flow) %>% sum() #total out
check %>% filter(is.na(out_flow) == T) %>% select(in_flow) %>% sum() #total in
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
# nodes colors
nodes$group <- as.factor(c("nodes_group"))
my_color <- 
'd3.scaleOrdinal() 
.domain(["Egouts", "Autres filières", "Rivière", "STEU", "Air", "Boues", "Incinérateurs", "Agriculture", "Sous-sol", "nodes_group"]) 
.range(["#616a6b", "#a569bd" , "#3498db", "#424949", "#1b3aff", "#6e2c00", "#f5b041", "#a04000", "#800000", "black"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color,
                   nodeWidth=10, fontSize=25, nodePadding=100,
                   width = 1200, sinksRight = T, margin = 0,
                   LinkGroup="flow_group", NodeGroup="group")
# save the widget
saveWidget(p, file="IDF_all.html")


# Deconnected -------------------------------------------------------------

# Make a connection data frame
links <- read.csv("IDF_disconnected.csv", sep=";")
check <- full_join( #checking in/out in all nodes
  links %>% group_by(source) %>% summarise(in_flow = sum(value)) %>% rename(node = source),
  out_flow <- links %>% group_by(target) %>% summarise(out_flow = sum(value)) %>% rename(node = target), 
  by = "node") %>%
  mutate(abs_diff = out_flow-in_flow, rel_diff = (out_flow-in_flow)/in_flow)
check %>% filter(is.na(in_flow) == T) %>% select(out_flow) %>% sum() #total out
check %>% filter(is.na(out_flow) == T) %>% select(in_flow) %>% sum() #total in

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)
# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
# nodes colors
nodes$group <- as.factor(c("nodes_group"))
my_color <- 
  'd3.scaleOrdinal() 
.domain(["ANC", "Toilettes embarquées ", "Toilettes chimiques", "Toilettes sèches", "Couches enfants", "Couches adultes ", "STEU", "Boues", "Incinérateurs", "Air", "Rivière", "Agriculture", "Sous-Sol", "nodes_group"]) 
.range(["#641e16", "#f7dc6f" , "#f7dc6f", "#f7dc6f", "#808000", "#808000", "#424949", "#6e2c00", "#f5b041", "#1b3aff", "#3498db", "#a04000", "#800000", "black"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color,
                   nodeWidth=10, fontSize=25, nodePadding=50,
                   height = 600, width = 1200, sinksRight = T, margin = 5,
                   LinkGroup="flow_group", NodeGroup="group")
p
# save the widget

saveWidget(p, file="IDF_disconnected.html")


