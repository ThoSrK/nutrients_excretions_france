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


#SAA crops Statistique Agricole Annuelle -------------------------------------------
#c'est à l'échelle départementale
file_SAA <- read.csv("Statistique_agricole_commune/SAANR_2020.csv", sep=";") #pour 2020

# test <- read.csv("Statistique_agricole_commune/SAANR_2019.csv", sep=";")
# test <- read.csv("Statistique_agricole_commune/SAANR_2018.csv", sep=";")
# file_SAA <- bind_rows(
#   read.csv("Statistique_agricole_commune/SAANR_2020.csv", sep=";"),
#   read.csv("Statistique_agricole_commune/SAANR_2019.csv", sep=";"),
#   read.csv("Statistique_agricole_commune/SAANR_2018.csv", sep=";"))
SAA <- file_SAA %>%
  dplyr::select(
    Département, Year = Année.de.référence,
    CAT_1 = Cultures.développées.1, CAT_2 = Cultures.développées.2, CAT_3 = Cultures.développées.3, CAT_4 = Cultures.développées.4,
    YIELD = Rendement, AREA = Superficie.développée, PRODUCTION= Production..volume.) %>%
  mutate(
    YIELD = as.numeric(YIELD), AREA = as.numeric(AREA), 
    PRODUCTION = as.numeric(PRODUCTION), Year = as.numeric(Year))
SAA <- SAA %>% #enlever dom tom plus pratique
  filter(Département != "971 - Guadeloupe" &
           Département != "972 - Martinique" &
           Département != "973 - Guyane" &
           Département != "974 - La Réunion" &
           Département != "976 - Mayotte" &
           Département != "") %>% #enlève stats agrégées nationales
  mutate(DEP = str_sub(Département, start = 1, end = 2)) #ne garde que les num de départements

SAA_CAT_1 <- SAA %>% filter(CAT_1 !="", CAT_2 =="") %>%
  dplyr::select( -CAT_2, -CAT_3, -CAT_4) #now useless
SAA_CAT_2 <- SAA %>% filter(CAT_2 !="", CAT_3 =="") %>%
  dplyr::select(-CAT_3, -CAT_4) #now useless
SAA_CAT_3 <- SAA %>% filter(CAT_3 !="", CAT_4 =="") %>% #a priori on prendre cette échelle mais en discriminant blé dur et blé tendre 
  dplyr::select(-CAT_4) #now useless


# Livestock nb load  -----------------------------------------------------------
path <- "data/Agreste/statistique_agricole_commune/nb_animals/"

#loading ruminants and pigs
file_SAA_nb_cattle_pig <- list.files( #read and merge csv of all years
  path = paste(path, "nb_livestock/", sep = ""),
  pattern = "*csv", full.names = T) %>% lapply(read.csv, sep = ";") %>% bind_rows
file_SAA_nb_cattle_pig <- file_SAA_nb_cattle_pig %>% #rename coloumns to merge with other animals
  rename(DIM1 = SAANR_6_DIM1, MOD_DIM1 = SAANR_6_MOD_DIM1, LIB_DIM1 = SAANR_6_LIB_DIM1,
         DIM3 = SAANR_6_DIM3, MOD_DIM3 = SAANR_6_MOD_DIM3, LIB_DIM3 = SAANR_6_LIB_DIM3,
         DIM4 = SAANR_6_DIM4, MOD_DIM4 = SAANR_6_MOD_DIM4, LIB_DIM4 = SAANR_6_LIB_DIM4) %>%
  filter(LIB_DIM3 != "dont brebis-mères laitières" ) %>%  #enlève sous-catégorie inutile
  filter(#filtre de toutes les catégories qui sont des sous-totaux
    LIB_DIM3 != "Toutes vaches" & # = vaches laitières + vaches nourrices
      LIB_DIM3 != "Total bovins de plus de 2 ans" & # = les nomes en "plus de 2 ans"
      LIB_DIM3 != "Total autres bovins de 1 à 2 ans" & # = noms en "de 1 à 2 ansé
      LIB_DIM3 != "Total bovins de moins de 1 an" & # = veaux + noms en "moins de 1 an"
      LIB_DIM3 != "Ensemble espèce bovine" &
      LIB_DIM3 != "Ensemble espèce porcine" &
      LIB_DIM3 != "Ensemble espèce caprine" &
      LIB_DIM3 != "Ensemble espèce ovine")

#loading horses and mules
file_SAA_nb_horses <- list.files( #read and merge csv of all years
  path = paste(path, "nb_horses/", sep = ""),
  pattern = "*csv", full.names = T) %>% lapply(read.csv, sep = ";") %>% bind_rows
#quand REGION != de "" et "..." (donc == "NR.."), DEP == "..." : RAS
#quand REGION == "...", DEP == les NR et "..."
#quand REGION == "", DEP == "..."
#donc ce qu'on veut c'est remplacer REGION par DEP quand REGION == "..." ou ""
file_SAA_nb_horses <- file_SAA_nb_horses %>%
  rename(DIM1 = SAANR_7_DIM1, MOD_DIM1 = SAANR_7_MOD_DIM1, LIB_DIM1 = SAANR_7_LIB_DIM1,
         DIM3 = SAANR_7_DIM3, MOD_DIM3 = SAANR_7_MOD_DIM3, LIB_DIM3 = SAANR_7_LIB_DIM3,
         DIM4 = SAANR_7_DIM4, MOD_DIM4 = SAANR_7_MOD_DIM4, LIB_DIM4 = SAANR_7_LIB_DIM4) %>%
  filter(
    LIB_DIM4 == "Effectif total (tête)", #ne pas faire la distinction si dans ou hors des exploitations
    LIB_DIM3 != "Chevaux lourds" & #enlève 2 sous-catégorie inutile
      LIB_DIM3 != "Chevaux de selle, sport, loisirs et course" &
      LIB_DIM3 != "Ensemble équidés") %>% #enlève sous-total
  mutate(REGION = case_when(
    REGION %in% c("...","") ~ DEP, 
    TRUE ~ REGION)) %>% #mise en place des bonnes valeurs REGION
  mutate(DEP = "...") #on enlève valeurs incorrectes de DEP

#loading poultry and rabbits
file_SAA_nb_poultry_rabbit <- list.files( #read and merge csv of all years
  path = paste(path, "nb_poultry_rabbit/", sep = ""),
  pattern = "*csv", full.names = T) %>% lapply(read.csv, sep = ";") %>% bind_rows
#quand REGION != de "" et "..." (donc == "NR.."), DEP == "..." : RAS
#quand REGION == "...", DEP == "..." : RAS
#quand REGION == "", DEP == "..." et les "NR." : à changer
#donc ce qu'on veut c'est remplacer REGION par DEP quand REGION == ""
file_SAA_nb_poultry_rabbit <- file_SAA_nb_poultry_rabbit %>%
  rename(DIM1 = SAANR_8_DIM1, MOD_DIM1 = SAANR_8_MOD_DIM1, LIB_DIM1 = SAANR_8_LIB_DIM1,
         DIM3 = SAANR_8_DIM3, MOD_DIM3 = SAANR_8_MOD_DIM3, LIB_DIM3 = SAANR_8_LIB_DIM3,
         DIM4 = SAANR_8_DIM4, MOD_DIM4 = SAANR_8_MOD_DIM4, LIB_DIM4 = SAANR_8_LIB_DIM4) %>%
  mutate(VALEUR = VALEUR*1000) %>%#car effectif reporté par 1000 têtes
  mutate(REGION = case_when(
    REGION == "" ~ DEP, 
    TRUE ~ REGION)) %>%#on remplace (jusqu'en 2015 ils ont mis REGION dans DEP et laissé REGION vide)
  mutate(DEP = "...") %>% #on enlève valeurs incorrectes de DEP
  filter(LIB_DIM3 != "Ensemble gallus") #on enlève sous-total = poules, poulettes et poulets


# Livestock nb excr -------------------------------------------------------

#merging all animals
file_SAA_livestock_nb <- bind_rows(
  file_SAA_nb_cattle_pig, 
  file_SAA_nb_horses, #attention pas de départements
  file_SAA_nb_poultry_rabbit) #attention pas de départements 
SAA_livestock_nb <- file_SAA_livestock_nb %>% 
  dplyr::select(Year = ANNREF, FRDOM, RG = REGION, DEP, 
                Species_name = LIB_DIM1, 
                Livestock_name = LIB_DIM3, 
                HEADS = VALEUR) %>%
  filter(FRDOM == "METRO") %>% #select only metropole (inclut la corse)
  dplyr::select(-FRDOM)

#attention pour le fichier bovin pour l'année 2000 ils ont mis les DEP 1, 2, 3... et pas 01, 02, ...
#pas urgent pour l'instant
# test <- SAA_livestock_nb %>% filter(
#   DEP %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

#adding N and C excretions and LU units
excretion_coeff <- read.csv("data/excretion_coeff.csv", sep = ";")
excretion_coeff <- excretion_coeff %>% select(
  Livestock_name = SAA_denomination_FR,
  kgN_per_head, LU_head, C_N_ratio_excr)
SAA_livestock_nb <- full_join(by = "Livestock_name", SAA_livestock_nb, excretion_coeff)
SAA_livestock_nb <-  SAA_livestock_nb %>% 
  mutate(LU = LU_head*HEADS, tN_excr = kgN_per_head*HEADS/10^3, tC_excr = tN_excr*C_N_ratio_excr)

#adding simple names categories
SAA_livestock_nb <- SAA_livestock_nb %>%
  mutate(simple_name = case_when(
    Species_name %in% c("Espèce caprine", "Espèce ovine") ~ "Ovins_Caprins", 
    Species_name == "Espèce porcine" ~ "Porcins", 
    Species_name == "Volailles et lapins" ~ "Volaille", 
    Species_name == "Espèce équine" ~ "Chevaux", 
    Livestock_name == "Vaches laitières" ~ "Vaches_laitières", 
    Livestock_name == "Vaches nourrices" ~ "Vaches_nourrices", 
    TRUE ~ "Males_et_bovins_moins_2_ans"))

#A FAIRE = ajouter les volatilisation

#ajout des excrétions à la MAP départementale
MAP_DEP <- fra_2 %>% dplyr::select(DEP_NAME= NAME_2, DEP = CC_2, geometry)
MAP_DEP <- SAA_livestock_nb %>% 
  filter(Year == 2018, DEP != "...") %>%
  group_by(DEP, simple_name) %>% summarise(tN_excr = sum(tN_excr)) %>%
  full_join(by = "DEP", MAP_DEP, .)



# Livestock nb graphs -----------------------------------------------------

test <- read.csv("data/insee_cartes/demographie/structure_pop/pop_municipale_2019_et_age/pop_municpale_2019_commune.csv", sep = ";")

#graph of evolution in france for 20 years
#il faudra vérifier que total frace correspond à RG == "..." et au groupement des DEP
plot_grid(nrow = 2, # not same y axis
  #bovines environ 1.5 Mt
  SAA_livestock_nb %>% filter(RG == "...", Species_name == "Espèce bovine") %>% 
    group_by(Year, simple_name) %>% summarise(tN_excr = sum(tN_excr)) %>%
    ggplot() + geom_area(aes(Year, tN_excr/10^3, fill = simple_name)) + ylab("ktN"),
  #porcins environ 0.3 MtN
  SAA_livestock_nb %>% filter(RG == "...", Species_name == "Espèce porcine") %>% 
    ggplot() + geom_area(aes(Year, tN_excr/10^3, fill = Livestock_name)) + ylab("ktN"),
  #Volailles et lapins : problème sur les années probablement dû à sélection RG == "..." qui foutait bordel
  #environ jusqu'à 2018
  #environ 0.3 MtN
  SAA_livestock_nb %>% filter(RG == "...", Species_name == "Volailles et lapins") %>%
    mutate(name_simple = case_when(
      Livestock_name == "Poules pondeuses d'oeufs à couver" ~ "Poules", 
      Livestock_name == "Poules pondeuses d'oeufs de consommation" ~ "Poules",
      Livestock_name == "Poulettes" ~ "Poules",
      Livestock_name == "Poulets de chair (y compris coqs et coquelets)" ~ "Poulets",
      T ~ "Autre volaille")) %>% 
    group_by(Year, name_simple) %>% summarise(tN = sum(tN)) %>%
    ggplot() + geom_area(aes(Year, tN_excr/10^3, fill = name_simple)) + ylab("ktN"),
  #caprins : 0.02 MtN
  SAA_livestock_nb %>% filter(RG == "...", Species_name == "Espèce caprine") %>% 
    ggplot() + geom_area(aes(Year, tN_excr/10^3, fill = Livestock_name)) + ylab("ktN"),
  #ovins : 0.08 MtN
  SAA_livestock_nb %>% filter(RG == "...", Species_name == "Espèce ovine") %>% 
    ggplot() + geom_area(aes(Year, tN_excr/10^3, fill = Livestock_name)) + ylab("ktN"),
  #equins : 0.05 MtN
  #même problème que sur la volaille 
  SAA_livestock_nb %>% filter(RG == "...", Species_name == "Espèce équine") %>% 
    ggplot() + geom_area(aes(Year, tN_excr/10^3, fill = Livestock_name)) + ylab("ktN")
  )

#évolution temporelle ensemble par type d'animaux
SAA_livestock_nb %>% filter(RG == "...") %>% 
  group_by(Year, simple_name) %>% summarise(tN_excr = sum(tN_excr)) %>%
  ggplot() + geom_area(aes(Year, tN_excr/10^3, fill = simple_name)) + ylab("ktN excreted")

#excrétions spatiales par types d'animaux
ggplot(MAP_DEP) + labs(fill = "ktN") + 
  geom_sf(aes(fill = tN_excr/10^3), color = "black", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Excretions animales") +  scale_fill_gradientn(colours = c("white", "orange","red")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) +
  facet_wrap(vars(simple_name))
#au DEP 75 species name = NA (ainsi que les valeurs numériques)
#voir comment régler ça

#excretions spatiales totales
MAP_DEP %>% group_by(DEP) %>%
  summarise(tN_excr = sum(tN_excr)) %>%
  ggplot() + labs(fill = "ktN") + 
  geom_sf(aes(fill = tN_excr/10^3), color = "black", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Excretions animales totales") +  scale_fill_gradientn(colours = c("white", "orange","red")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) 

#par la définition adoptée de LU (=85kgN), carte identiqueà kgN




# SAA Livestock production ----------------------------------------------------------------
file_SAA_production_meat_cattle_pig <- bind_rows(
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2000.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2001.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2002.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2003.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2004.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2005.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2006.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2007.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2008.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2009.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2010.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2011.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2012.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2013.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2014.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2015.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2016.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2017.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2018.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2019.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat/FDS_SAANR_9_2020.csv", sep=";")) 
file_SAA_production_meat_cattle_pig <- file_SAA_production_meat_cattle_pig %>%
  rename(DIM1 = SAANR_9_DIM1, MOD_DIM1 = SAANR_9_MOD_DIM1, LIB_DIM1 = SAANR_9_LIB_DIM1,
         DIM3 = SAANR_9_DIM3, MOD_DIM3 = SAANR_9_MOD_DIM3, LIB_DIM3 = SAANR_9_LIB_DIM3,
         DIM4 = SAANR_9_DIM4, MOD_DIM4 = SAANR_9_MOD_DIM4, LIB_DIM4 = SAANR_9_LIB_DIM4)
#spread ne marche pas ici, comprendre pourquoi en voyant les lignes qui sont identiques
file_SAA_production_meat_cattle_pig <- file_SAA_production_meat_cattle_pig %>%
  spread(VALEUR, LIB_DIM4)

# test <- file_SAA_production_meat_cattle_pig %>% 
#   filter(DEP != "...",
#          LIB_DIM3 == "Ensemble bovins",
#          LIB_DIM4 == "Poids moyen (kg net/tête)",
#          ANNREF == "2018") 
# MAP_DEP_meat <- full_join(MAP_DEP, test, by="DEP")
# ggplot(MAP_DEP_meat) + 
#   geom_sf(aes(fill = VALEUR), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
#   ggtitle("Bovins kg/tête poids moyen") + 
#   scale_fill_gradientn(colours = heat.colors(2)) +
#   theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
# test <- file_SAA_production_meat_cattle_pig %>% 
#   filter(DEP != "...",
#          LIB_DIM3 == "Ensemble bovins",
#          LIB_DIM4 == "Poids produit (tonne équivalent carcasse)",
#          ANNREF == "2018") 
# MAP_DEP_meat <- full_join(MAP_DEP, test, by="DEP")
# ggplot(MAP_DEP_meat) + 
#   geom_sf(aes(fill = VALEUR), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
#   ggtitle("Production totale bovin viande (tonne eq carcasse)") + 
#   scale_fill_gradientn(colours = heat.colors(2)) +
#   theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))

file_SAA_production_meat_poultry_rabbit <- bind_rows(#pas de DEP mais apparemment pas de probleme
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2000.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2001.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2002.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2003.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2004.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2005.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2006.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2007.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2008.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2009.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2010.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2011.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2012.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2013.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2014.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2015.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2016.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2017.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2018.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2019.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/meat_poultry_lapine/FDS_SAANR_10_2020.csv", sep=";"))
file_SAA_production_meat_poultry_rabbit <- file_SAA_production_meat_poultry_rabbit %>%
  rename(DIM1 = SAANR_10_DIM1, MOD_DIM1 = SAANR_10_MOD_DIM1, LIB_DIM1 = SAANR_10_LIB_DIM1,
         DIM3 = SAANR_10_DIM3, MOD_DIM3 = SAANR_10_MOD_DIM3, LIB_DIM3 = SAANR_10_LIB_DIM3,
         DIM4 = SAANR_10_DIM4, MOD_DIM4 = SAANR_10_MOD_DIM4, LIB_DIM4 = SAANR_10_LIB_DIM4) %>%
  mutate(VALEUR = case_when(
    LIB_DIM4 == "Production totale (1000 têtes)" ~ VALEUR*1000, #on ne met pas en 1000 têtes
    LIB_DIM4 == "Poids moyen (kg net/1000 têtes)" ~ VALEUR/1000, #on ne met pas en 1000 têtes
    TRUE ~ VALEUR)) %>%
  mutate(LIB_DIM4 = case_when(
    LIB_DIM4 == "Production totale (1000 têtes)" ~ "Production totale (tête)",
    LIB_DIM4 == "Poids moyen (kg net/1000 têtes)" ~ "Poids moyen (kg net/tête)",
    TRUE ~ LIB_DIM4))
#spread marche bien ici


file_SAA_production_meat <- bind_rows(
  file_SAA_production_meat_cattle_pig,
  file_SAA_production_meat_poultry_rabbit) 

SAA_production_meat <- file_SAA_production_meat %>% 
  dplyr::select(Year = ANNREF,
                FRDOM,
                RG = REGION, 
                DEP, 
                Species_name = LIB_DIM1, 
                Livestock_name = LIB_DIM3, 
                Mesure = LIB_DIM4,
                VALEUR) %>%
  filter(FRDOM == "METRO") %>% #select only metropole (inclut la corse)
  dplyr::select(-FRDOM)
#il faut spread dim 4
#spread ne marche pas car double valeurs qq part
file_SAA_production_meat <- file_SAA_production_meat %>%
  spread(VALEUR, LIB_DIM4)

file_SAA_production_milk <- bind_rows(
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2000.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2001.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2002.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2003.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2004.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2005.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2006.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2007.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2008.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2009.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2010.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2011.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2012.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2013.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2014.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2015.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2016.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2017.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2018.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2019.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/milk/FDS_SAANR_12_2020.csv", sep=";")) 
file_SAA_production_milk <- file_SAA_production_milk %>%
  rename(DIM1 = SAANR_12_DIM1, MOD_DIM1 = SAANR_12_MOD_DIM1, LIB_DIM1 = SAANR_12_LIB_DIM1,
         DIM3 = SAANR_12_DIM3, MOD_DIM3 = SAANR_12_MOD_DIM3, LIB_DIM3 = SAANR_12_LIB_DIM3,
         DIM4 = SAANR_12_DIM4, MOD_DIM4 = SAANR_12_MOD_DIM4, LIB_DIM4 = SAANR_12_LIB_DIM4) 

file_SAA_production_eggs <- bind_rows(#problème sur les régions / départements encore
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2000.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2001.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2002.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2003.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2004.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2005.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2006.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2007.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2008.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2009.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2010.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2011.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2012.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2013.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2014.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2015.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2016.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2017.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2018.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2019.csv", sep=";"),
  read.csv("Agreste/statistique_agricole_commune/production/eggs/FDS_SAANR_13_2020.csv", sep=";")) 
file_SAA_production_eggs <- file_SAA_production_eggs %>%
  rename(DIM1 = SAANR_13_DIM1, MOD_DIM1 = SAANR_13_MOD_DIM1, LIB_DIM1 = SAANR_13_LIB_DIM1,
         DIM3 = SAANR_13_DIM3, MOD_DIM3 = SAANR_13_MOD_DIM3, LIB_DIM3 = SAANR_13_LIB_DIM3,
         DIM4 = SAANR_13_DIM4, MOD_DIM4 = SAANR_13_MOD_DIM4, LIB_DIM4 = SAANR_13_LIB_DIM4) %>%
  mutate(REGION = case_when(REGION == "" ~ DEP, TRUE ~ REGION)) %>%#ils ont mis REGION dans DEP et laissé REGION vide : on remplace
  mutate(DEP = "...") #Départements pas rapportés, on enlève leurs erreurs où ils avaient mis région à la place


#SAANR_14 : apiculture

SAA_milk <- file_SAA_milk %>% 
  dplyr::select(Year = ANNREF,
                FRDOM,
                RG = REGION, 
                DEP, 
                Species_milk = SAANR_12_LIB_DIM3, 
                PRODUCTION = SAANR_12_LIB_DIM4, 
                VALEUR) %>%
  filter(FRDOM == "METRO") %>% #select only metropole (inclut la corse)
  dplyr::select(-FRDOM) 
SAA_milk <-  SAA_milk %>% mutate(#pour rendre lisible par R
  PRODUCTION = str_replace_all(PRODUCTION, " ", "_"), #enlever espaces, remplacer par _
  PRODUCTION = gsub("[()]", "", PRODUCTION), #enlever parenthèses
  PRODUCTION = gsub("'", "", PRODUCTION), # enlever '
  PRODUCTION = str_replace_all(PRODUCTION,"/", "_par_")) #remplacer / par _par_

SAA_milk <-  SAA_milk %>%
  spread(PRODUCTION, VALEUR) 
SAA_milk <-  SAA_milk %>% #pour réordonner les colonnes
  dplyr::select(Year, RG, DEP, Species_milk,
                Taux_butyreux_g_par_l, Matière_protéique_g_par_l,
                Livraisons_à_lindustrie_hl, Fabrication_de_produits_fermiers_hl, 
                dont_livraisons_à_lindustrie_de_produits_fermiers_hl, Vente_directe_et_autoconsommation_de_lait_entier_hl,
                Production_finale_hl) 
#Production_finale_hl = Livraisons_à_lindustrie_hl + Fabrication_de_produits_fermiers_hl + Vente_directe_et_autoconsommation_de_lait_entier_hl
SAA_milk_FR <- SAA_milk %>% filter(RG == "...") %>% dplyr::select(-RG, -DEP)
SAA_milk_RG <- SAA_milk %>% filter(RG != "...", DEP == "...") %>% dplyr::select(-DEP)
SAA_milk_DEP <- SAA_milk %>% filter(RG != "...", DEP != "...")

#graphs de production
plot_grid(# il faudra faire un graph d'aires superoposée de la somme des 3 termes de production finale pour voir si 1 écrase les autres
  ggplot(SAA_milk_FR %>% filter(Species_milk =="Lait de vache")) +
    geom_line(aes(Year, Taux_butyreux_g_par_l)) + ylim(0, NA),
  ggplot(SAA_milk_FR %>% filter(Species_milk =="Lait de vache")) +
    geom_line(aes(Year, Matière_protéique_g_par_l)) + ylim(0, NA),
  ggplot(SAA_milk_FR %>% filter(Species_milk =="Lait de vache")) +
    geom_line(aes(Year, Production_finale_hl)) + ylim(0, NA),
  ggplot(SAA_milk_FR %>% filter(Species_milk =="Lait de brebis")) +
    geom_line(aes(Year, Production_finale_hl)) + ylim(0, NA),
  ggplot(SAA_milk_FR %>% filter(Species_milk =="Lait de chèvre")) +
    geom_line(aes(Year, Production_finale_hl)) + ylim(0, NA)
)
#maps
MAP_DEP_Milk <- full_join(MAP_DEP, SAA_milk_DEP %>% filter(Year == 2020), by="DEP")
ggplot(MAP_DEP_Milk) + 
  geom_sf(aes(fill = Production_finale_hl/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Production de lait (milliers hl)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))



# OLD MAP -----------------------------------------------------------------
#Importation du package
# cf https://dimension.usherbrooke.ca/pages/32
library(raster)
#Découpage en département
FranceFormes <- getData(name="GADM", country="FRA", level=2)
plot(FranceFormes, main="Carte de la France, départements")
idx <- match(FranceFormes$CC_2, Department$Department_nb)
concordance <- Department[idx, "Rendement"]
FranceFormes$Rendements <- concordance
couleurs <- colorRampPalette(c('white', 'red'))
spplot(FranceFormes, 
       "Rendements",
       col.Department_nb=couleurs(30))

year = as.factor(str_sub(year, start = -4)) #transforme year


#Importation du package
# cf https://dimension.usherbrooke.ca/pages/32
library(raster)
#Découpage en département
FranceFormes <- getData(name="GADM", country="FRA", level=2)
plot(FranceFormes, main="Carte de la France, départements")
ggplot()+ geom_raster(FranceFormes, aes(x=x, y=y)) + coord_quickmap()

idx <- match(FranceFormes$CC_2, test_2020$DEP)
concordance <- test_2020[idx, "Espèce bovine"]
FranceFormes$Livestock_nb <- concordance

couleurs <- colorRampPalette(c('white', 'red'))
spplot(FranceFormes, 
       "Livestock_nb",
       col.Department_nb=couleurs(30),
       main=list(label="Nombre de bovins",cex=.8))

mutate(
  Rendement = as.numeric(Rendement),
  Superficie.développée = as.numeric(Superficie.développée),
  Production..volume. = as.numeric(Production..volume.))



