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

#setting working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#loading France maps ------------------------------------------------------------
path <- "data/maps/admin_express/"
fra_reg <- sf::st_read(paste(path, "region/simplified/REGION.shp", sep=""))
fra_dep <- sf::st_read(paste(path, "departement/simplified/DEPARTEMENT.shp", sep=""))
fra_comm <- sf::st_read(paste(path, "commune/simplified/COMMUNE.shp", sep=""))
list(fra_reg, fra_dep, fra_comm) %>% # Mofifier le système de coordonnées vers "WGS 84 / Pseudo-Mercator"
  purrr::map(.f = st_transform, crs = 3857) %>%
  setNames(nm = c("fra_reg", "fra_dep", "fra_comm"))
#reomving overseas
fra_reg <- fra_reg %>% filter(INSEE_REG != "01" & INSEE_REG != "02" & INSEE_REG != "03" & 
                        INSEE_REG != "04" & INSEE_REG != "06") #enlever outre mer
fra_dep <- fra_dep %>% filter(INSEE_REG != "01" & INSEE_REG != "02" & INSEE_REG != "03" & 
                                INSEE_REG != "04" & INSEE_REG != "06") #enlever outre mer
fra_comm <- fra_comm %>% filter(INSEE_REG != "01" & INSEE_REG != "02" & INSEE_REG != "03" & 
                                INSEE_REG != "04" & INSEE_REG != "06") #enlever outre mer

#region
ggplot(fra_reg) + 
  geom_sf(aes(), color = "black", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) 
#departement
ggplot(fra_dep) + 
  geom_sf(aes(), color = "black", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) 
#commune
ggplot(fra_comm) + 
  geom_sf(aes(), color = "black", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))


#pop density ---------------------------------------------------------------------
#densité de pop commune
path <- "data/insee_cartes/demographie/structure_pop/densite_pop_2018/"
pop_density <- read.csv(paste(path, "densite_pop_2018_commune.csv", sep=""), sep=";") %>%
  dplyr::rename(densite = densite_pop, INSEE_COM = Code) %>%
  mutate(densite = as.numeric(densite))
pop_density <- pop_density %>%
  mutate( #adding 0 before INSEE_COM to have same format as in map
    INSEE_COM = case_when(
        nchar(INSEE_COM) == 4 ~ paste("0", INSEE_COM, sep=""),
        T ~ INSEE_COM),
    kgN_per_ha = densite*4.5/100)
#density map
g <- full_join(fra_comm, pop_density, by = "INSEE_COM") %>%
  ggplot() + labs(fill = "density") +
  geom_sf(aes(fill = densite), color = NA) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Densité") +  
  scale_fill_binned(type="viridis", trans="log",
                    breaks = c(30, 100, 300, 1000, 3000)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) 
ggsave("test.pdf", g, w=10, h=8)
#power scale representation
temp <- pop_density %>% arrange(desc(densite)) %>% mutate(rank = row_number())
n <- nrow(temp)
ggplot(temp) + geom_point(aes(rank/n*100, densite)) + scale_y_log10() + scale_x_log10()
ggplot(temp) + geom_point(aes(rank/n*100, densite)) + scale_y_log10()

#density pop department
path <- "data/insee_cartes/demographie/structure_pop/densite_pop_2018/"
pop_density <- read.csv(paste(path, "densite_pop_2018_dep.csv", sep=""), sep=";") %>%
  dplyr::rename(densite = densite_pop, INSEE_DEP = Code) %>%
  mutate(densite = as.numeric(densite))
pop_density <- pop_density %>%
  mutate( #adding 0 before INSEE_DEP to have same format as in map
    INSEE_DEP = case_when(
      nchar(INSEE_DEP) == 1 ~ paste("0", INSEE_DEP, sep=""),
      T ~ INSEE_DEP),
    kgN_per_ha = densite*4.5/100)
#density map
g <- full_join(fra_dep, pop_density, by = "INSEE_DEP") %>%
  ggplot() + labs(fill = "density") +
  geom_sf(aes(fill = densite), color = NA) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Densité") +  
  scale_fill_binned(type="viridis", trans="log") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) 
ggsave("test.pdf", g, w=10, h=8)

#density pop region
path <- "data/insee_cartes/demographie/structure_pop/densite_pop_2018/"
pop_density <- read.csv(paste(path, "densite_pop_2018_reg.csv", sep=""), sep=";") %>%
  dplyr::rename(densite = densite_pop, INSEE_REG = Code) %>%
  mutate(densite = as.numeric(densite))
pop_density <- pop_density %>%
  mutate( #adding 0 before INSEE_REG to have same format as in map
    INSEE_REG = as.character(INSEE_REG),
    INSEE_REG = case_when(
      nchar(INSEE_REG) == 1 ~ paste("0", INSEE_REG, sep=""),
      T ~ INSEE_REG),
    kgN_per_ha = densite*4.5/100)
#density map
g <- full_join(fra_reg, pop_density, by = "INSEE_REG") %>%
  ggplot() + labs(fill = "density") +
  geom_sf(aes(fill = densite), color = NA) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Densité") +  
  scale_fill_binned(type="viridis", trans="log") +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) 
ggsave("test.pdf", g, w=10, h=8)


# old MAPS --------------------------------------------------------------------
#cf https://rstudio-pubs-static.s3.amazonaws.com/543650_d382de059cea47cebecf1198eb551f7a.html
#données administratives france : https://gadm.org/download_country_v3.html

# path <- "data/maps/"
# fra_0 <- readRDS(paste(path, "gadm36_FRA_0_sf.rds", sep =""))#france enitère (country)
# fra_1 <- readRDS(paste(path, "gadm36_FRA_1_sf.rds", sep =""))#régions (region)
# fra_2 <- readRDS(paste(path, "gadm36_FRA_2_sf.rds", sep =""))#départements (departments)
# fra_3 <- readRDS(paste(path, "gadm36_FRA_3_sf.rds", sep =""))#arrondissement (districts)
# fra_4 <- readRDS(paste(path, "gadm36_FRA_4_sf.rds", sep =""))#cantons (cantons)
# fra_5 <- readRDS(paste(path, "gadm36_FRA_5_sf.rds", sep ="")) %>% rename(NAME = NAME_5)#commune simple (commune)
# fra_5_bis <- sf::st_read("data/maps/georef-france-commune-millesime.shp") #il va falloir enlever dom tom

# sf::st_crs(fra_0) #projection “long/lat” (système non projeté, mieux d'utiliser un système projeté)
# list(fra_0, fra_1, fra_2, fra_3, fra_4, fra_5, fra_5_bis) %>% # Mofifier le système de coordonnées vers "WGS 84 / Pseudo-Mercator"
#   purrr::map(.f = st_transform, crs = 3857) %>%
#   setNames(nm = c("fra_0", "fra_1", "fra_2", "fra_3", "fra_4", "fra_5", "fra_5_bis"))


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

