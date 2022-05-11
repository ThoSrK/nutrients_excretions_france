library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(cowplot)
library(fishmethods) #problems with dplyr ?  or boot ?
library(tidyverse)
library(kgc)
library(rcompanion) #to be able to load boot
library(FSA)
library(lmerTest)
library(lattice)
#library(multcomp) #makes some problem with select in dplyr
library(effects)
library(boot)



#INSEE Population --------------------------------------------------------------

file_population <- read.csv("data/Pop/base-pop-historiques-1876-2019.csv", sep = ";")
Population <- file_population %>%
  dplyr::select(
    RG = Région,
    DEP = Département,#vérifier la douille des arrondissements de Paris
    COM = Code.géographique,
    COM_NAME = Libellé.géographique,
    POPULATION = Population.en.2019) %>%
  #enlève Corse et Outre Mer pour simplifier
  filter(DEP != "971" & DEP != "972" & DEP != "973" & DEP != "974") %>% 
  mutate(DEP = case_when(#afin de se conformer au format des autres fichiers
    DEP == "1" ~ "01", DEP == "2" ~ "02", DEP == "3" ~ "03", DEP == "4" ~ "04", DEP == "5" ~ "05",
    DEP == "6" ~ "06", DEP == "7" ~ "07", DEP == "8" ~ "08", DEP == "9" ~ "09", TRUE ~ DEP))

Population_COM <- Population %>% dplyr::select(COM, POPULATION) #il faudra rajouter un 0 initial comme pour les DEP...
Population_DEP <- Population %>% group_by(DEP) %>% summarise(POPULATION = sum(POPULATION, na.rm = T))
Population_RG <- Population %>% group_by(RG) %>% summarise(POPULATION = sum(POPULATION, na.rm = T))

MAP_DEP_POPULATION <- full_join(MAP_DEP, Population_DEP, by="DEP")
ggplot(MAP_DEP_POPULATION) + 
  geom_sf(aes(fill = POPULATION/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Population (milliers hab)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))

# #fusionner les arrondissements de Paris en 1 seule ville
# Paris <- Population %>% 
#   filter(Département ==75) %>% group_by(year)
# ggplot(Paris, aes(as.numeric(as.character(year)), population)) + geom_line() + facet_wrap(vars(Libellé.géographique))
# Paris <- Paris %>%
#   mutate(population = sum(population), Libellé.géographique = "Paris") %>%
#   filter(Code.géographique == 75101)
# ggplot(Paris, aes(as.numeric(as.character(year)), population, population)) + geom_line() + ylim(0,3000000)
# #fusionner Paris avec le reste (après avoir enlevé les arrondissements)
# Population <- rbind(Population %>% filter(Département != 75), Paris) %>%
#   arrange(desc(population)) %>%
#   group_by(year) %>% mutate(
#     cumulative = cumsum(population),
#     rank = rank(-population, ties.method = "first"),
#     pop_tot = sum(population, na.rm = T),
#     rank_tot = max(rank),
#     max_pop = max(population))  






# a trier -----------------------------------------------------------------





file <- read_excel("Pop/base-pop-historiques-1876-2019.xlsx", 
                   sheet = "pop_1876_2019", range = "A5:AK35000", na="NA")

#cf carte et téléchargement https://www.assainissement.developpement-durable.gouv.fr/PortailAC/data#!
file <- read.csv("db_export_2020.csv", sep = "|")

Boues <- file %>% select(
  
  #coordonnées
  Latitude.du.STEU..WGS84.,
  Longitude.du.STEU..WGS84.,
  Latitude.du.rejet..WGS84.,
  Longitude.du.rejet..WGS84.,
  
  #Capacité et flux
  Taille.agglomération..EH.,
  Maximum.de.la.somme.des.pollutions.entrantes..EH.,
  Somme.des.capacités.nominales..EH.,
  Type.de.réseau.majoritaire,
  Pourcentage.rejet.direct.temps.sec,
  Capacité.nominale.en.EH,
  Capacité.nominale.en.Kg.de.DBO5,
  Percentile95.calculé.en.m3.j,
  Charge.maximale.entrante..EH.,
  Débit.entrant.en.m3.j,
  
  #Type traitement
  Filière.eau.principale,
  Filière.boues.principale,
  Niveau.traitement.existant...biologique,
  Niveau.traitement.existant...azote,
  Niveau.traitement.existant...phosphore,
  Niveau.traitement.existant...désinfection,
  #conformité perf DBO, N, P pas encore pris
  
  #boues produites
  Prod.boues.sans.réactif..tMS.an.,
  Quantité.réactifs.utilisés..t.an.,
  Quantité.épandage.agricole..tMS.an.,
  Quantité.incinérée..tMS.an.,
  Quantité.Compostage..produit...tMS.an.,
  Quantité.mise.en.décharge..tMS.an.,
  Quantité.en.valorisation.industrielle..tMS.an.,
  Qté.envoyée.sur.autre.STEU..tMS.an.,
  #compotage, déchet, transit, centre méthanisation,  séchage...
  
  #sensibilité
  Sensibilité.azote, #aussi la date
  Sensibilité.phosphore #aussi la date
  )

Paris <- file %>% filter(Capacité.nominale.en.EH>3000000)




library(maps)
france <- map_data( "france")
#saving the map of the sites and climates
ggplot() +
  geom_polygon(data = france, 
               aes(x= long, y = lat, group = group), 
               fill= NA, colour = "black", size = .3) +
  geom_point(data = Boues%>% filter(Taille.agglomération..EH.>10000),
             aes(y= Latitude.du.rejet..WGS84., 
                 x = Longitude.du.rejet..WGS84., 
                 size = Prod.boues.sans.réactif..tMS.an.)) +
  coord_fixed(1.3) + theme_classic() + 
  ylim(41, 51.5) + xlim(-5.3, 10) +
  theme(legend.position = "bottom")



library(ggplot2)
library(sf)
#> Linking to GEOS 3.9.0, GDAL 3.2.2, PROJ 7.2.1
library(magrittr)
library(spData)
#> To access larger datasets in this package, install the spDataLarge
#> package with: `install.packages('spDataLarge',
#> repos='https://nowosad.github.io/drat/', type='source')`
data("world")
world[140,] %>% st_geometry() %>% ggplot() + geom_sf() + theme_minimal()


sum(Boues$Taille.agglomération..EH.)
sum(Boues$Somme.des.capacités.nominales..EH.)
sum(Boues$Capacité.nominale.en.EH)





production_boues <- sum(Boues$Prod.boues.sans.réactif..tMS.an.)

réactifs <- sum(Boues$Quantité.réactifs.utilisés..t.an.)

épandage <- sum(Boues$Quantité.épandage.agricole..tMS.an.)
épandage/production_boues

incinere <- sum(Boues$Quantité.incinérée..tMS.an.)
incinere/production_boues

compostage <- sum(Boues$Quantité.Compostage..produit...tMS.an.)
compostage/production_boues

decharge <- sum(Boues$Quantité.mise.en.décharge..tMS.an.)
decharge/production_boues

val_industrie <- sum(Boues$Quantité.en.valorisation.industrielle..tMS.an.)
val_industrie/production_boues

autre_STEU <- sum(Boues$Qté.envoyée.sur.autre.STEU..tMS.an.)
autre_STEU/production_boues

(compostage + épandage + incinere + decharge + val_industrie)/production_boues







