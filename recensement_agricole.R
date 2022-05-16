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
#attention conflit entre le "select" de dplyr et celui de raster


# RA crops (recensement agricole 1970-2010) -------------------------------
path <- "data/Agreste/recensement_agricole/"
file_RA_crops <- read.csv(paste(path, "crops/FDS_G_1013_2010.txt", sep=""), sep = ";")
#possibilité de charger depuis 1970 mais prends du temps
# file_RA_crops <- bind_rows(
#   read.csv("recensement_agricole/crops/FDS_G_1013_2010.txt", sep = ";"),
#   read.csv("recensement_agricole/crops/FDS_G_1013_2000.txt", sep = ";"),
#   read.csv("recensement_agricole/crops/FDS_G_1013_1988.txt", sep = ";"),
#   read.csv("recensement_agricole/crops/FDS_G_1013_1979.txt", sep = ";"),
#   read.csv("recensement_agricole/crops/FDS_G_1013_1970.txt", sep = ";")
# )

RA_crops <- file_RA_crops %>% 
  dplyr::select(
    Year = ANNREF,
    FRDOM, RG = REGION, DEP, COM,
    FARM_SIZE = G_1013_LIB_DIM1, 
    CROP_TYPE = G_1013_LIB_DIM2,
    UNITE = G_1013_LIB_DIM4,
    VALUE = VALEUR)
RA_crops <- RA_crops %>% 
  filter(
    FARM_SIZE == "Ensemble des exploitations (hors pacages collectifs)", #pas de filtre sur la taille des exploitations
    FRDOM == "METRO") %>% #garder que métropole
  dplyr::select(-FARM_SIZE, -FRDOM)
#possibilité d'affiner les catégories de CROPS
RA_crops <- RA_crops%>% mutate(#pour rendre lisible par R
  UNITE = str_replace_all(UNITE, " ", "_"), #enlever espaces, remplacer par _
  UNITE = gsub("hectares", "ha", UNITE)) #enlever parenthèses
RA_crops <-  RA_crops %>%
  spread(UNITE, VALUE)

RA_crops_FR <- RA_crops %>% filter(RG == "............") %>% dplyr::select(-RG, -DEP, -COM)
RA_crops_RG <- RA_crops %>% filter(RG != "............", DEP == "............") %>% dplyr::select(-DEP, -COM)
RA_crops_DEP <- RA_crops %>% filter(RG != "............", DEP != "............", COM == "............") %>% dplyr::select(-COM)
RA_crops_COM <- RA_crops %>% filter(RG != "............", DEP != "............", COM != "............")


#RA livestock recensement agricole (only 2010 for communes) ----------------------------------------------------
file_RA_livestock <- read.csv(paste(path, "livestock/by_commune/FDS_G_2141_2010.csv", sep=""), sep = ";")
RA_livestock <- file_RA_livestock %>% 
  dplyr::select(
    Year = ANNREF, #il n'y a que l'année 2010 en fait
    FRDOM, RG = REGION, DEP, COM,
    FARM_SIZE = G_2141_LIB_DIM1, 
    LIVESTOCK_TYPE = G_2141_LIB_DIM2,
    UNITE = G_2141_LIB_DIM3,
    VALUE = VALEUR)
RA_livestock <- RA_livestock %>% 
  filter(
    FARM_SIZE == "Ensemble des exploitations (hors pacages collectifs)", #pas de filtre sur la taille des exploitations
    FRDOM == "METRO", #garder que métropole
    LIVESTOCK_TYPE %in% c(
      "Total Bovins", "Vaches laitières", "Vaches allaitantes",
      "Total Equidés", 
      "Total Caprins", "Chèvres",
      "Total Ovins", "Brebis laitières", "Brebis nourrices",
      "Total Porcins", "Truies reproductrices de 50 kg ou plus",
      "Volailles"),
    UNITE == "Cheptel correspondant (têtes)", #je ne veux pas en UGB
    COM != "............") %>%
  #possibilité d'affiner les catégories de livestock
  mutate(
    LIVESTOCK_TYPE = case_when(#obligé de supprimer les espaces pour les futurs titres de colonnes
      LIVESTOCK_TYPE == "Total Bovins" ~ "Total_Bovins",
      LIVESTOCK_TYPE == "Total Equidés" ~ "Total_Equidés",
      LIVESTOCK_TYPE == "Total Caprins" ~ "Total_Caprins",
      LIVESTOCK_TYPE == "Total Ovins" ~ "Total_Ovins",
      LIVESTOCK_TYPE == "Total Porcins" ~ "Total_Porcins",
      LIVESTOCK_TYPE == "Volailles" ~ "Volailles")) %>%
  dplyr::select(-Year, -FRDOM, -FARM_SIZE, -UNITE)

#vérifier qu'en groupant par région et dep on obtient même résultat qu'eux au global
test <- RA_livestock %>% spread(LIVESTOCK_TYPE, VALUE, fill = NA)
#departmental
test <- RA_livestock %>% group_by(RG, DEP, LIVESTOCK_TYPE) %>% summarise(VALUE = sum(VALUE, na.rm = T))
#regional
test <- RA_livestock %>% group_by(RG, LIVESTOCK_TYPE) %>% summarise(VALUE = sum(VALUE, na.rm = T))

#Effectifs en UGBTA : possibilité d'avoir nb de têtes ou autre UGB
RA_livestock_UGB<- RA_livestock %>% filter(UNITE == "Unité Gros Bétail Tous aliments  (UGBTA)")
#échelle région (il faudra probablement faire tomber le "NR" pour les liaisons)
RA_livestock_UGB_RG <- RA_livestock_UGB %>% 
  filter(RG != "............", DEP == "............", COM == "............") %>% 
  spread(LIVESTOCK_TYPE, VALUE) %>%
  dplyr::select(-DEP, -COM, -FARM_SIZE, -UNITE)
#départementale
RA_livestock_UGB_DEP <- RA_livestock_UGB %>% 
  filter(DEP != "............", COM == "............") %>% 
  spread(LIVESTOCK_TYPE, VALUE) %>%
  dplyr::select(-RG, -COM, -FARM_SIZE, -UNITE)
#échelle communale
RA_livestock_UGB_COM <- RA_livestock_UGB %>% 
  filter(COM != "............") %>% 
  spread(LIVESTOCK_TYPE, VALUE) %>%
  dplyr::select(-RG, -DEP, -FARM_SIZE, -UNITE)


#UGB by DEP
MAP_DEP_UGB <- full_join(MAP_DEP, RA_livestock_UGB_DEP, by="DEP")
plot_grid(
  ggplot(MAP_DEP_UGB) + 
    geom_sf(aes(fill = Total_Bovins/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
    ggtitle("Répartition des bovins en France (milliers UGB)") + 
    scale_fill_gradientn(colours = heat.colors(2)) +
    theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
  ggplot(MAP_DEP_UGB) + 
    geom_sf(aes(fill = Total_Porcins/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
    ggtitle("Répartition des Porcins en France (milliers UGB)") +
    scale_fill_gradientn(colours = heat.colors(2)) +
    theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
  ggplot(MAP_DEP_UGB) + 
    geom_sf(aes(fill = Volailles/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
    ggtitle("Répartition des volailles en France (milliers UGB)") + 
    scale_fill_gradientn(colours = heat.colors(2)) +
    theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
  ggplot(MAP_DEP_UGB) + 
    geom_sf(aes(fill = (Total_Bovins + Total_Porcins + Volailles)/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
    ggtitle("Total livetstock UGB (milliers UGB)") + 
    scale_fill_gradientn(colours = heat.colors(2)) +
    theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
)


