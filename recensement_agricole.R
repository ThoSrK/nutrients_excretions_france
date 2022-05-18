library(tidyr)
library(dplyr)
library(ggplot2) #for ggplot graphs
library(cowplot) #for plot_grid()
#library(tidyverse) #to read excel ?
#library(lattice) #pour graphiques temporels, je n'utilise pas
#library(effects) #for linear models I guess
#library(boot) #bootstrap
#library(labelled) #pas utilisés je crois
#library(questionr) #pas utilisé je crois
library(stringr) #to manipulate strings
library(sf) #for spherical geometry operations
#attention conflit entre le "select" de dplyr et celui de raster


#setting working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- "data/Agreste/recensement_agricole/"
# RA crops (recensement agricole 1970-2010) -------------------------------

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


#RA livestock communes ----------------------------------------------------
file_RA_livestock <- read.csv(paste(path, "livestock/by_commune/FDS_G_2141_2010.csv", sep=""), sep = ";")
RA_livestock <- file_RA_livestock %>% 
  dplyr::select(
    Year = ANNREF, #il n'y a que l'année 2010 en fait
    FRDOM, INSEE_REG = REGION, INSEE_DEP = DEP, INSEE_COM = COM,
    FARM_SIZE = G_2141_LIB_DIM1, 
    LIVESTOCK_TYPE = G_2141_LIB_DIM2,
    UNITE = G_2141_LIB_DIM3,
    VALUE = VALEUR)
RA_livestock <- RA_livestock %>% 
  # dplyr::filter(
  #   FARM_SIZE == "Ensemble des exploitations (hors pacages collectifs)", #pas de filtre sur la taille des exploitations
  #   FRDOM == "METRO", #garder que métropole
  #   LIVESTOCK_TYPE %in% c(
  #     "Total Bovins", "Vaches laitières", "Vaches allaitantes",
  #     "Total Equidés", 
  #     "Total Caprins", "Chèvres",
  #     "Total Ovins", "Brebis laitières", "Brebis nourrices",
  #     "Total Porcins", "Truies reproductrices de 50 kg ou plus",
  #     "Volailles"),
  #   UNITE == "Cheptel correspondant (têtes)", #je ne veux pas en UGB
  #   INSEE_COM != "............") %>%
  dplyr::filter(
    FARM_SIZE == "Ensemble des exploitations (hors pacages collectifs)", #pas de filtre sur la taille des exploitations
    FRDOM == "METRO", #garder que métropole
    LIVESTOCK_TYPE %in% c(
      "Total Bovins",
      "Total Equidés", 
      "Total Caprins", 
      "Total Ovins", 
      "Total Porcins",
      "Volailles"),
    UNITE %in% c("Unité Gros Bétail Tous aliments  (UGBTA)",
                 "Unité Gros Bétail Alimentation grossière (UGBAG)",
                 "Cheptel correspondant (têtes)"), #je veux que garder effectifs et UGB
    INSEE_COM != "............") %>% #enelver les subtotaux DEP, REG, FR
  #possibilité d'affiner les catégories de livestock
  # mutate(
  #   LIVESTOCK_TYPE = case_when(#obligé de supprimer les espaces pour les futurs titres de colonnes
  #     LIVESTOCK_TYPE == "Total Bovins" ~ "Total_Bovins",
  #     LIVESTOCK_TYPE == "Total Equidés" ~ "Total_Equidés",
  #     LIVESTOCK_TYPE == "Total Caprins" ~ "Total_Caprins",
  #     LIVESTOCK_TYPE == "Total Ovins" ~ "Total_Ovins",
  #     LIVESTOCK_TYPE == "Total Porcins" ~ "Total_Porcins",
  #     LIVESTOCK_TYPE == "Volailles" ~ "Volailles")) %>%
  dplyr::select(-Year, -FRDOM, -FARM_SIZE) %>%
  mutate(INSEE_REG = substring(INSEE_REG, 3)) %>% #removes the "NR" in region indice
  mutate(VALUE = case_when( #replacing NAs with 0 to be able to sum
    is.na(VALUE) == T ~ 0, T ~ VALUE)) %>%
  mutate(UNITE = case_when(
    UNITE == "Cheptel correspondant (têtes)" ~ "heads", 
    UNITE == "Unité Gros Bétail Alimentation grossière (UGBAG)" ~ "UGBAG", 
    UNITE == "Unité Gros Bétail Tous aliments  (UGBTA)" ~ "UGBTA")) %>%
  spread(UNITE, VALUE) %>%
  replace(is.na(.), 0) #replace NAs with 0 to be able to sum

#checking non matching communes with map file
temp <- anti_join( #in RA_livestock but not in map : 1624, due to new communes merged ?
  RA_livestock %>% select(INSEE_COM) %>% distinct(),
  fra_comm %>% select(INSEE_COM),
  by = "INSEE_COM")
temp <- anti_join( #in map but not in RA_livestock : 3915, due to no livestock in here ?
  fra_comm %>% select(INSEE_COM),
  RA_livestock %>% select(INSEE_COM) %>% distinct(),
  by = "INSEE_COM")

#commune map
temp <- RA_livestock %>% group_by(INSEE_COM) %>% summarise(
  heads = sum(heads), UGBTA = sum(UGBTA), UGBAG = sum(UGBAG)) %>%
  left_join(fra_comm, ., by = "INSEE_COM") %>%
  replace(is.na(.), 0)#replacing Nas with 0 for mapping
g <- temp %>%
  ggplot() + geom_sf(data = fra_reg, aes(), color="black", size=.2) +
  geom_sf(aes(fill = UGBTA), color = NA) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  scale_fill_gradientn(colours = c("white", "orange","red")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
ggsave("test.pdf", g, w=10, h=8)
temp <- RA_livestock %>% group_by(INSEE_COM, LIVESTOCK_TYPE) %>% summarise(
  heads = sum(heads), UGBTA = sum(UGBTA), UGBAG = sum(UGBAG),) %>%
  left_join(fra_comm, ., by = "INSEE_COM")
g <- temp %>%
  ggplot() + geom_sf(data = fra_reg, aes(), color="black", size=.2) +
  geom_sf(aes(fill = UGBTA), color = NA) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  scale_fill_gradientn(colours = c(NA, "orange","red")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) +
  facet_wrap(vars(LIVESTOCK_TYPE))
ggsave("test.pdf", g, w=10, h=8)
#dep map
#by species
g <-  RA_livestock %>% group_by(INSEE_DEP, LIVESTOCK_TYPE) %>% summarise(
  heads = sum(heads), UGBTA = sum(UGBTA), UGBAG = sum(UGBAG),) %>%
  left_join(fra_dep, ., by = "INSEE_DEP") %>%
  ggplot() + geom_sf(aes(fill = UGBTA), color = "black", size=.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  scale_fill_gradientn(colours = c("white", "orange","red")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))+
  facet_wrap(vars(LIVESTOCK_TYPE))
ggsave("test.pdf", g, w=10, h=8)
#all
g <-  RA_livestock %>% group_by(INSEE_DEP) %>% summarise(
  heads = sum(heads), UGBTA = sum(UGBTA), UGBAG = sum(UGBAG),) %>%
  left_join(fra_dep, ., by = "INSEE_DEP") %>%
  ggplot() + geom_sf(aes(fill = UGBTA), color = "black", size =.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  scale_fill_gradientn(colours = c("white", "orange","red")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
ggsave("test.pdf", g, w=10, h=8)
#reg map
#by species
g <- RA_livestock %>% group_by(INSEE_REG, LIVESTOCK_TYPE) %>% summarise(
  heads = sum(heads), UGBTA = sum(UGBTA), UGBAG = sum(UGBAG),) %>%
  left_join(fra_reg, ., by = "INSEE_REG") %>% 
  ggplot() +geom_sf(aes(fill = UGBTA), color = NA) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  scale_fill_binned(type="viridis") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) +
  facet_wrap(vars(LIVESTOCK_TYPE))
ggsave("test.pdf", g, w=10, h=8)
#all
g <- RA_livestock %>% group_by(INSEE_REG) %>% summarise(
  heads = sum(heads), UGBTA = sum(UGBTA), UGBAG = sum(UGBAG),) %>%
  left_join(fra_reg, ., by = "INSEE_REG") %>% 
  ggplot() +geom_sf(aes(fill = UGBTA), color = NA) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  scale_fill_binned(type="viridis") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
ggsave("test.pdf", g, w=10, h=8)

# RA livestock canton -----------------------------------------------------
file_RA_livestock <- read.csv(paste(path, "livestock/by_canton/FDS_RA_3053_2010.csv", sep=""), sep = ";")






# Check UGB total matching subtotals --------------------------------------
#je pense que la différence est due aux villes NA
#est-ce que par canton donne le même problème ?

#reference file to check
check <- file_RA_livestock %>% 
  dplyr::select(
    Year = ANNREF, #il n'y a que l'année 2010 en fait
    FRDOM, INSEE_REG = REGION, INSEE_DEP = DEP, INSEE_COM = COM,
    FARM_SIZE = G_2141_LIB_DIM1, 
    LIVESTOCK_TYPE = G_2141_LIB_DIM2,
    UNITE = G_2141_LIB_DIM3,
    VALUE = VALEUR) %>%
  dplyr::filter(
    FARM_SIZE == "Ensemble des exploitations (hors pacages collectifs)", #pas de filtre sur la taille des exploitations
    FRDOM == "METRO", #garder que métropole
    LIVESTOCK_TYPE %in% c(
      "Total Bovins",
      "Total Equidés", 
      "Total Caprins", 
      "Total Ovins", 
      "Total Porcins",
      "Volailles")) %>%
  dplyr::select(-Year, -FRDOM, -FARM_SIZE) %>%
  filter(
    UNITE %in% c("Unité Gros Bétail Tous aliments  (UGBTA)",
                 "Unité Gros Bétail Alimentation grossière (UGBAG)",
                 "Cheptel correspondant (têtes)")) %>%
  mutate(UNITE = case_when(
    UNITE == "Cheptel correspondant (têtes)" ~ "heads", 
    UNITE == "Unité Gros Bétail Alimentation grossière (UGBAG)" ~ "UGBAG", 
    UNITE == "Unité Gros Bétail Tous aliments  (UGBTA)" ~ "UGBTA")) %>%
  spread(UNITE, VALUE)

#france (from commune) : à l'échelle france différence de 20-40% UGB total
temp1 <- check %>% filter(INSEE_REG == "............") %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
temp2 <- check %>% filter(INSEE_COM != "............") %>%
  group_by(LIVESTOCK_TYPE) %>% 
  summarise(heads = sum(heads, na.rm=T), UGBAG = sum(UGBAG, na.rm=T), UGBTA = sum(UGBTA, na.rm=T)) %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
test <- full_join(temp1, temp2, by=c("LIVESTOCK_TYPE", "UNITE"))
ggplot(test) + geom_point(aes(x = LIVESTOCK_TYPE, y=VALUE.x/VALUE.y)) +
  ylab("vrai total / total calculé") + geom_hline(yintercept =  1) +
  facet_wrap(vars(UNITE)) + ylim(0,NA)
#all species together
test <- test %>% group_by(UNITE) %>% 
  summarise(VALUE.x = sum(VALUE.x, na.rm = T), VALUE.y = sum(VALUE.y, na.rm = T)) %>%
  mutate(ratio = VALUE.x/VALUE.y)
ggplot(test) + geom_point(aes(x = VALUE.x, y=VALUE.y)) +
  ylab("vrai total / total calculé") + geom_abline(slope=1, intercept=0) +
  facet_wrap(vars(UNITE), scales="free")

#REG (from commune)
temp1 <- check %>% filter(INSEE_REG != "............", INSEE_DEP == "............") %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
temp2 <- check %>% filter(INSEE_COM != "............") %>%
  group_by(INSEE_REG, LIVESTOCK_TYPE) %>% 
  summarise(heads = sum(heads, na.rm=T), UGBAG = sum(UGBAG, na.rm=T), UGBTA = sum(UGBTA, na.rm=T)) %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
test <- full_join(temp1, temp2, by=c("INSEE_REG","LIVESTOCK_TYPE", "UNITE")) %>%
  mutate(ratio = VALUE.x/VALUE.y)
ggplot(test) + geom_point(aes(x = VALUE.x, y=VALUE.y)) +
  ylab("vrai total / total calculé") + geom_abline(slope=1, intercept=0) +
  facet_wrap(UNITE ~ LIVESTOCK_TYPE, scales="free")
#all species together
test <- test %>% group_by(INSEE_REG, UNITE) %>% 
  summarise(VALUE.x = sum(VALUE.x, na.rm = T), VALUE.y = sum(VALUE.y, na.rm = T)) %>%
  mutate(ratio = VALUE.x/VALUE.y)
ggplot(test) + geom_point(aes(x = VALUE.x, y=VALUE.y)) +
  ylab("vrai total / total calculé") + geom_abline(slope=1, intercept=0) +
  facet_wrap(vars(UNITE), scales="free")

#DEP (from commune)
temp1 <- check %>% filter(INSEE_DEP != "............", INSEE_COM == "............") %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
temp2 <- check %>% filter(INSEE_COM != "............") %>%
  group_by(INSEE_DEP, LIVESTOCK_TYPE)%>% 
  summarise(heads = sum(heads, na.rm=T), UGBAG = sum(UGBAG, na.rm=T), UGBTA = sum(UGBTA, na.rm=T)) %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
test <- full_join(temp1, temp2, by=c("INSEE_DEP","LIVESTOCK_TYPE", "UNITE"))
ggplot(test) + geom_point(aes(x = VALUE.x, y=VALUE.y)) +
  ylab("vrai total / total calculé") + geom_abline(slope=1, intercept=0) +
  facet_wrap(UNITE ~ LIVESTOCK_TYPE, scales="free")
#all species together
test <- test %>% group_by(INSEE_DEP, UNITE) %>% 
  summarise(VALUE.x = sum(VALUE.x, na.rm = T), VALUE.y = sum(VALUE.y, na.rm = T)) %>%
  mutate(ratio = VALUE.x/VALUE.y)
ggplot(test) + geom_point(aes(x = VALUE.x, y=VALUE.y)) +
  ylab("vrai total / total calculé") + geom_abline(slope=1, intercept=0) +
  facet_wrap(vars(UNITE), scales="free")


#france (from REG) : perfect
temp1 <- check %>% filter(INSEE_REG == "............") %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
temp2 <- check %>% filter(INSEE_REG != "............", INSEE_DEP == "............") %>%
  group_by(LIVESTOCK_TYPE) %>% 
  summarise(heads = sum(heads, na.rm=T), UGBAG = sum(UGBAG, na.rm=T), UGBTA = sum(UGBTA, na.rm=T)) %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
test <- full_join(temp1, temp2, by=c("LIVESTOCK_TYPE", "UNITE")) %>%
  mutate(ratio = VALUE.x/VALUE.y)

#france (from DEP) : almost perfect
temp1 <- check %>% filter(INSEE_REG == "............") %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
temp2 <- check %>% filter(INSEE_DEP != "............", INSEE_COM == "............") %>%
  group_by(LIVESTOCK_TYPE) %>% 
  summarise(heads = sum(heads, na.rm=T), UGBAG = sum(UGBAG, na.rm=T), UGBTA = sum(UGBTA, na.rm=T)) %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
test <- full_join(temp1, temp2, by=c("LIVESTOCK_TYPE", "UNITE")) %>%
  mutate(ratio = VALUE.x/VALUE.y)

#REF (from DEP) : almost perfect exept 94 and 11 (corse et ile de france)
temp1 <- check %>% filter(INSEE_REG != "............", INSEE_DEP == "............") %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
temp2 <- check %>% filter(INSEE_DEP != "............", INSEE_COM == "............") %>%
  group_by(INSEE_REG, LIVESTOCK_TYPE) %>% 
  summarise(heads = sum(heads, na.rm=T), UGBAG = sum(UGBAG, na.rm=T), UGBTA = sum(UGBTA, na.rm=T)) %>%
  gather(key = UNITE, value = VALUE, heads:UGBTA, na.rm = T)
test <- full_join(temp1, temp2, by=c("INSEE_REG", "LIVESTOCK_TYPE", "UNITE")) %>%
  mutate(ratio = VALUE.x/VALUE.y)









