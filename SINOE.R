#rappel : DMA inclut OMA 
#+ déchets d'entreprise et admin collectés
#+ déchets occasionnels en déchetterie
library(lubridate)
#OK tonnages DMA et déchetteries ----------------------------------------------------------------
file_dma <- read.csv('ADEME_data/sinoe/tonnage/tonnage-dma-par-type-de-dechet.csv')
dma <- file_dma %>% select(
  Year = ANNEE, RG = C_REGION, DEP = C_DEPT, 
  DECHET = L_TYP_REG_DECHET, TON = TONNAGE_DMA_T) 

#!!!! OMR à mettre coeff ~0.3 pour garder que putrescible !!!!
#dma
ggsave(
  "dechets/dma.pdf",  height = 7, width = 8,
  plot = 
    plot_grid(
      #graph évolution en france depuis 2009 des 3 types
      dma %>% mutate(Year = ymd(Year, truncated = 2L)) %>%
        group_by(Year, DECHET) %>%
        summarise(TON = sum(TON)) %>%
        ggplot() + geom_line(aes(Year, TON/10^6, color = DECHET)) +
        ylab("Mtons") + ylim(0, NA) + ggtitle("DMA en France par déchet") + labs(color = "") + 
        theme_classic() + scale_x_date(),
      plot_grid(
        #OMR
        full_join(by = "DEP", MAP_DEP, dma %>% 
                    filter(DECHET == "Ordures ménagères résiduelles", Year == 2019)) %>%
          ggplot() + labs(fill = "ktons") + 
          geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + 
          coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
          ggtitle("OMR (2019)") +  scale_fill_gradientn(colours = c("white", "orange","red")) +
          theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
        #Dechets verts et biodéchets
        full_join(by = "DEP", MAP_DEP, dma %>% 
                    filter(DECHET == "Déchets verts et biodéchets", Year == 2019)) %>%
          ggplot() + labs(fill = "ktons") + 
          geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + 
          coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
          ggtitle("Déchets verts et biodéchets (2019)") +  scale_fill_gradientn(colours = c("white", "orange","red")) +
          theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
      ),
      nrow = 2))

#déchetterie
file_dechetterie <- read.csv('ADEME_data/sinoe/tonnage/tonnage-decheterie-par-type-dechet-par-dept.csv')
dechetterie <- file_dechetterie %>% select(
  Year = ANNEE, RG = C_REGION, DEP = C_DEPT, 
  DECHET = L_TYP_REG_DECHET, TON = TONNAGE_DECH)
ggsave(
  "dechets/dechetterie.pdf", height = 7, width = 8,
  plot = plot_grid(
    #évolution déchets verts en déchetterie depuis 2009
    dechetterie %>% mutate(Year = ymd(Year, truncated = 2L)) %>%
      group_by(Year, DECHET) %>%
      summarise(TON = sum(TON)) %>%
      ggplot() + geom_line(aes(Year, TON/10^6, color = DECHET)) +
      ylab("Mtons") + ylim(0, NA) + ggtitle("Déchets en déchetterie en France par type de déchet") +
      theme_classic() + labs(color ="") ,
    #map tous types de déchets en déchetterie en 2017
    full_join(by = "DEP", MAP_DEP, dechetterie %>% 
                filter(DECHET == "Déchets verts", Year == 2017)) %>%
      ggplot() + labs(fill ="ktons") + 
      geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + 
      coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
      ggtitle("Déchets verts en déchetterie (2017)") + 
      scale_fill_gradientn(colours = c("white", "orange","red")) +
      theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
    nrow = 2)
  )

#density (on departments) of OMR and déchets verts et biodéchets
ggsave(
  "dechets/density_dma_OMR_dechets_verts_et_biodechets.pdf",
  plot = 
    dma %>% filter(DECHET %in% c("Ordures ménagères résiduelles", "Déchets verts et biodéchets")) %>%
    ggplot()+ geom_density(aes(TON/10^3, colour = as.factor(Year))) +
    facet_wrap(vars(DECHET), scales = "free"),
  height = 4, width = 8
)


#OK type collecte DMA -------------------------------------------------------
file_dma_type_collecte <- read.csv('ADEME_data/sinoe/destination_repartition/repartition-dma-collectes-par-type-de-collecte.csv')
type_collecte <- file_dma_type_collecte %>% select(
  Year = ANNEE, RG = C_REGION, DEP = C_DEPT, 
  TYP_COLLECTE, 
  TONNAGE_DMA_T_HG, TONNAGE_DMA_T)

#DMA par type collecte Hors Gravats
ggsave("dechets/dma_type_collecte_hors_gravat.pdf", height = 7, width = 8,
       plot = 
         plot_grid(
           #carte 3 types par département en 2017
           full_join(MAP_DEP, type_collecte %>% 
                       filter(Year == 2017), by = "DEP") %>%
             ggplot() + geom_sf(aes(fill = TONNAGE_DMA_T_HG/10^6), color = "black", size = 0.2) + 
             coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
             ggtitle("DMA hors gravats par filière (2017)") + labs(fill = "Mtons") +
             scale_fill_gradientn(colours = c("white", "orange","red")) +
             theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) +
             facet_wrap(vars(TYP_COLLECTE)),
           #graph évolution en france depuis 2009 des 3 types
           type_collecte %>% mutate(Year = ymd(Year, truncated = 2L)) %>%
             group_by(Year, TYP_COLLECTE) %>%
             summarise(TONNAGE_DMA_T_HG = sum(TONNAGE_DMA_T_HG),
                       TONNAGE_DMA_T = sum(TONNAGE_DMA_T)) %>%
             ggplot() + geom_line(aes(Year, TONNAGE_DMA_T_HG/10^6, color = TYP_COLLECTE)) +
             ylab("Mtons") + ylim(0, NA) + ggtitle("DMA en France hors gravats par type de collecte") +
             theme_classic() + labs(colour = ""),
           nrow = 2
         )
       )

#DMA par type collecte Avec Gravats
ggsave("dechets/dma_type_collecte_avec_gravat.pdf", height = 7, width = 8,
       plot = 
         plot_grid(
           #carte 3 types par département en 2017
           full_join(MAP_DEP, type_collecte %>% 
                       filter(Year == 2017), by = "DEP") %>%
             ggplot() + geom_sf(aes(fill = TONNAGE_DMA_T/10^6), color = "black", size = 0.2) + 
             coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
             ggtitle("DMA avec gravats par filière (2017)") + labs(fill = "Mtons") +
             scale_fill_gradientn(colours = c("white", "orange","red")) +
             theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) +
             facet_wrap(vars(TYP_COLLECTE)),
           #graph évolution en france depuis 2009 des 3 types
           type_collecte %>% mutate(Year = ymd(Year, truncated = 2L)) %>%
             group_by(Year, TYP_COLLECTE) %>%
             summarise(TONNAGE_DMA_T_HG = sum(TONNAGE_DMA_T_HG),
                       TONNAGE_DMA_T = sum(TONNAGE_DMA_T)) %>%
             ggplot() + geom_line(aes(Year, TONNAGE_DMA_T/10^6, color = TYP_COLLECTE)) +
             ylab("Mtons") + ylim(0, NA) + ggtitle("DMA en France avec gravats par type de collecte") +
             theme_classic() + labs(colour = ""),
           nrow = 2
         )
       )

#denisty (on departments) of dma by type de collecte
ggsave(
  "dechets/density_dma_collecte.pdf",
  plot = 
    type_collecte %>% 
    ggplot()+ geom_density(aes(TONNAGE_DMA_T_HG/10^3, colour = as.factor(Year))) +
    facet_wrap(vars(TYP_COLLECTE), scales = "free", ncol = 2),
  height = 6, width = 8
)


#OK chiffres clés -----------------------------------------------------------

#oma chiffres clés
#temp <- read.csv('ADEME_data/sinoe/chiffres_cles/oma-2009-2017.csv')
#donne just OMA total (même pas par type de déchet) par cap et par dep : déjà ci-dessous 

#dma chiffres clés
#temp <- read.csv('ADEME_data/sinoe/chiffres_cles/dma-hors-gravats-2009-2017.csv')
#temp <- read.csv('ADEME_data/sinoe/chiffres_cles/dma-avec-gravats-2009-2017.csv')
#donne just DMA total (même pas par type de déchet) par cap et par dep : déjà ci-dessus 
#mais tout de même le faire  /cap car pas encore fait

#déchèteries
#temp <- read.csv('ADEME_data/sinoe/chiffres_cles/decheterie-hors-gravats-2009-2017.csv')
#temp <- read.csv('ADEME_data/sinoe/chiffres_cles/decheterie-avec-gravats-2009-2017.csv')
#données sur couverture pop et villes, nb de déchetterie, poids moyen par apport
#intéressant mais pas mon sujet

#OK performance collecte OMA -----------------------------------------------------
#performance-collecte-omr-2017.csv (chiffres clés) inutile, car carte déjà générée ci-dessous 
file_oma_collecte <- read.csv('ADEME_data/sinoe/performance_collecte/performances-collecte-oma-par-type-dechet-par-dept.csv')
oma_collecte <- file_oma_collecte %>% select(
  Year = ANNEE, RG = C_REGION, DEP = C_DEPT, 
  DECHET = L_TYP_REG_DECHET, 
  TON = TONNAGE_OMA_T, POP = POPULATION, kg_per_cap = RATIO_OMA)

ggsave(
  "dechets/oma.pdf", width = 10, height = 7, 
  plot = 
    plot_grid(
      plot_grid(
        #all OMA
        oma_collecte %>% group_by(Year, DECHET) %>%
          summarise(TON = sum(TON),POP = sum(POP)) %>%
          ggplot() + geom_line(aes(Year, TON/10^6, colour = DECHET)) +
          ylab("Mtons") + ggtitle("OMA par déchet en France") +
          labs(colour = ""),
        #OMR
        full_join(MAP_DEP, by = "DEP", oma_collecte %>% 
                    filter(DECHET == "Ordures ménagères résiduelles", Year == 2017)) %>%
          ggplot() + ggtitle("OMR (2017)") + labs(fill = "ktons") + 
          geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) +
          coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
          scale_fill_gradientn(colours = c("white", "orange","red")) +
          theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
        #Déchets produits alimentaires
        full_join(MAP_DEP, by = "DEP", oma_collecte %>% 
                    filter(DECHET == "Déchets de produits alimentaires", Year == 2017)) %>%
          ggplot() + ggtitle("Déchets de produits alimentaires (2017)") + labs(fill = "ktons") + 
          geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) +
          coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
          scale_fill_gradientn(colours = c("white", "orange","red")) +
          theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
        ncol = 1
      ),
      plot_grid(
        #all OMA per cap
        oma_collecte %>% group_by(Year, DECHET) %>%
          summarise(kg_per_cap = weighted.mean(kg_per_cap, POP), POP = sum(POP)) %>%
          ggplot() + geom_line(aes(Year, kg_per_cap, colour = DECHET)) +
          ylab("kg/cap") + ggtitle("OMA par pers par déchet en France") +
          labs(colour = ""),
        #OMR per cap
        full_join(MAP_DEP, by = "DEP", oma_collecte %>% 
                    filter(DECHET == "Ordures ménagères résiduelles", Year == 2017)) %>%
          ggplot() + ggtitle("OMR per cap (2017)") + labs(fill = "kg/cap") + 
          geom_sf(aes(fill = kg_per_cap), color = "black", size = 0.2) + 
          coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
          scale_fill_gradientn(colours = c("white", "orange","red")) +
          theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
        #OMR per cap
        full_join(MAP_DEP, by = "DEP", oma_collecte %>% 
                    filter(DECHET == "Déchets de produits alimentaires", Year == 2017)) %>%
          ggplot() + ggtitle("Déchets de produits alimentaires per cap (2017)") + labs(fill = "kg/cap") + 
          geom_sf(aes(fill = kg_per_cap), color = "black", size = 0.2) + 
          coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
          scale_fill_gradientn(colours = c("white", "orange","red")) +
          theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)),
        ncol = 1
      ),
      ncol = 2
    )
  )


#density (on departments) of dma by type de collecte
ggsave(
  "dechets/density_OMA_OMR_dechets_alimentaires.pdf",
  plot = 
    oma_collecte %>% 
    filter(DECHET %in% c("Ordures ménagères résiduelles", "Déchets de produits alimentaires")) %>%
    ggplot()+ geom_density(aes(kg_per_cap, colour = as.factor(Year))) +
    facet_wrap(vars(DECHET), scales = "free"),
  height = 4, width = 8
)

#possibilité de faire emballages journaux magazines

#just population pour voir
full_join(MAP_DEP, by = "DEP", oma_collecte %>% 
            filter(DECHET == "Ordures ménagères résiduelles")) %>%
ggplot() + 
  geom_sf(aes(fill = POP/1000), color = "black", size = 0.2) + 
  coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Population (2017)") + labs(fill = "milliers") +
  scale_fill_gradientn(colours = c("white", "yellow","red")) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))



# destination et répartition ----------------------------------------------
file_oma_destination <- read.csv('ADEME_data/sinoe/destination_repartition/destination-oma-par-type-de-traitement.csv')
oma_destination <- file_oma_destination %>% select(
  Year = ANNEE, RG = C_REGION, DEP = C_DEPT, 
  DECHET = L_TYP_REG_DECHET, TRAITEMENT = L_TYP_REG_SERVICE,
  TON = TONNAGE_OMA_T) %>%
  filter(Year == 2017)
#types de traitement possibles : 
#Valo matière : tous
#Valo orga : que 4 : biodechets, OMR, encombrants, collectes séparées hors gravats
#incinération avec E : manque 2 :  verre et collecte séparée hors gravats 
#incinération sans E : que 2 :  OMR et déchets dangereux
#stockage : tous
#non précisé : manque 1 : collecte séparée hors gravats

#Valorisation orga tous
test <- destination_oma %>% filter(TRAITEMENT == "Valorisation organique")
MAP <- full_join(MAP_DEP, test, by = "DEP")
ggplot(MAP) + 
  geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Valorisation organique : biodéchets (ktons)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) +
  facet_wrap(vars(DECHET))
#valorisation orga : biodéchets
full_join(MAP_DEP, test %>% filter(DECHET == "Biodéchets"), by = "DEP") %>%
ggplot() + 
  geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Valorisation organique : biodéchets (ktons)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
#valorisation orga : omr
full_join(MAP_DEP, test %>% filter(DECHET == "Ordures ménagères résiduelles"), by = "DEP") %>%
ggplot() + 
  geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Valorisation organique : OMR (ktons)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
#valorisation orga : encombrants
full_join(MAP_DEP, test %>% filter(DECHET == "Encombrants"), by = "DEP") %>%
ggplot() + 
  geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Valorisation organique : Encombrants (ktons)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))
#valorisation orga : collecte séparée hors gravats
full_join(MAP_DEP, test %>% filter(DECHET == "Collectes séparées hors gravats"), by = "DEP") %>%
ggplot() + 
  geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Valorisation organique : Collectes séparées hors gravats (ktons)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16))

#gros test
MAP <- full_join(MAP_DEP, destination_oma, by = "DEP")
ggplot(MAP) + 
  geom_sf(aes(fill = TON/1000), color = "black", size = 0.2) + coord_sf(datum = NA, expand = FALSE) +  # enlever l'affichage des coordonnés et de la grille
  ggtitle("Valorisation organique : biodéchets (ktons)") + 
  scale_fill_gradientn(colours = heat.colors(2)) +
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 16)) +
  facet_grid(DECHET ~ TRAITEMENT)




file_destination_dma <- read.csv('ADEME_data/sinoe/destination_repartition/destination-dma-par-type-de-traitement.csv')
file_destination_dechets <- read.csv('ADEME_data/sinoe/destination_repartition/destination-dechets-collectes-en-decheterie-par-type-de-traitement.csv')

#comparaison des catégories de déchets : différents 
unique(file_destination_oma$L_TYP_REG_DECHET)
unique(file_destination_dma$L_TYP_REG_DECHET)
unique(file_destination_dechets$L_TYP_REG_DECHET)
#comparaison des catégories de traitement : les mêmes
unique(file_destination_oma$L_TYP_REG_SERVICE)
unique(file_destination_dma$L_TYP_REG_SERVICE)
unique(file_destination_dechets$L_TYP_REG_SERVICE)





