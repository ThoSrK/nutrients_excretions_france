library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)
library(lattice)
library(effects)
library(boot)
library(labelled)
library(questionr)
library(tidyr)
library(stringr)
library(sf) #for spherical geometry operations


#pas ouf en fait les données des ODD
test_fra <- readRDS("indicateur_territoire/ODD_FRA.rds")
test_REG <- readRDS("indicateur_territoire/ODD_REG.rds")
metadonnees_ODD <- read.csv("indicateur_territoire/metadonnees_indicateurs_territoriaux_developpement_durable.csv")

#densité de pop
test <- read.csv("insee_densite_pop/densite_pop_commune.csv", sep = ";")
test <- read.csv("insee_densite_pop/densite_pop_dep.csv", sep = ";")
test <- read.csv("insee_densite_pop/densite_pop_reg.csv", sep = ";")



# MAPS --------------------------------------------------------------------
#cf https://rstudio-pubs-static.s3.amazonaws.com/543650_d382de059cea47cebecf1198eb551f7a.html
#données administratives france : https://gadm.org/download_country_v3.html

path <- "data/maps/"
fra_0 <- readRDS(paste(path, "gadm36_FRA_0_sf.rds", sep =""))#france enitère (country)
fra_1 <- readRDS(paste(path, "gadm36_FRA_1_sf.rds", sep =""))#régions (region)
fra_2 <- readRDS(paste(path, "gadm36_FRA_2_sf.rds", sep =""))#départements (departments)
fra_3 <- readRDS(paste(path, "gadm36_FRA_3_sf.rds", sep =""))#arrondissement (districts)
fra_4 <- readRDS(paste(path, "gadm36_FRA_4_sf.rds", sep =""))#cantons (cantons)
fra_5 <- readRDS(paste(path, "gadm36_FRA_5_sf.rds", sep ="")) %>% rename(NAME = NAME_5)#commune simple (commune)
fra_5_bis <- st_read("data/georef-france-commune/georef-france-commune-millesime.shp") #il va falloir enlever dom tom

st_crs(fra_0) #projection “long/lat” (système non projeté, mieux d'utiliser un système projeté)
list(fra_0, fra_1, fra_2, fra_3, fra_4, fra_5, fra_5_bis) %>% # Mofifier le système de coordonnées vers "WGS 84 / Pseudo-Mercator"
  purrr::map(.f = st_transform, crs = 3857) %>%
  setNames(nm = c("fra_0", "fra_1", "fra_2", "fra_3", "fra_4", "fra_5", "fra_5_bis"))
list2env(envir = .GlobalEnv)

#plots france maps
plot(st_geometry(fra_0)) 
plot(st_geometry(fra_1)) 
plot(st_geometry(fra_2)) 
plot(st_geometry(fra_3)) 
plot(st_geometry(fra_4)) 
plot(st_geometry(fra_5)) 
#fra_5_bis a l'air un peu trop précis et compliqué, met du temps à charger
#plot(st_geometry(fra_5_bis))

#creating de maps at different scales
MAP_REG <- fra_1 %>% dplyr::select(RG_NAME= NAME_1, RG = GID_1, geometry) #il faudra adapter les nums de région
MAP_DEP <- fra_2 %>% dplyr::select(DEP_NAME= NAME_2, DEP = CC_2, geometry)
MAP_COM <- fra_5 %>% dplyr::select(COM_NAME= NAME, COM = GID_5, geometry)
#MAP_COM_bis <- fra_5_bis %>% dplyr::select(COM_NAME= com_name, CODE_GEO = com_code, geometry)

