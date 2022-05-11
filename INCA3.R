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
library(openxlsx)
library(labelled)
library(questionr)
library(tidyr)
library(stringr)


# Package INCA3
remotes::install_github("thinkr-open/inca3", build_vignettes = FALSE)
library(inca3)


# Toute BDD ---------------------------------------------------------------
#très très complet, 250k obs. Permet d'avoir la composition nutritionnelle des aliments
head(conso_compo_alim)
BDD <- conso_compo_alim %>%
  select(POPULATION, NOIND, NUM_LIGNE,
         gpe_INCA3, aliment_code_INCA3, aliment_libelle_INCA3, aliment_code_FX, aliment_libelle_FX, 
         qte_conso, qte_conso_pond,
         aet, aesa, proteines, glucides, lipides, phosphore, potassium, eau)

table_nutritionnelle <- BDD %>% select(
  gpe_INCA3, aet, aesa, proteines, glucides, lipides, phosphore, potassium, eau)
#attention pas une valeur nutritionnelle unique pour chacun des 44 groupes INCAS, cf ci-dessous

ggplot(table_nutritionnelle %>% filter(gpe_INCA3=="1")) + geom_histogram(aes(x = phosphore))+
  xlab("mgP pour 100g") + ylab("pain et panification sèche raffiné")
ggplot(table_nutritionnelle %>% filter(gpe_INCA3=="7")) + geom_histogram(aes(x = phosphore))+
  xlab("mgP pour 100g") + ylab("Lait")

#test que la consommation de volaille
test <- BDD %>% filter(gpe_INCA3 == "16") 
# si on ne fait que ça on perd les individus qui mangent 0 volaille (<3000 obs au lieu de 4000)
#il faut donc créer des 0 pour ces individus

# Conso nutriments --------------------------------------------------------
#très bien, conso par nutriments, il faut juste croiser avec les ID des individus pour avoir leur age
head(apports_nut_alim)
apport_nutriments <- apports_nut_alim %>%
  select(NOIND,
         nutriment1, nutriment2, nutriment3, nutriment4, nutriment10, nutriment33, nutriment34,
         contrib3, contrib4, contrib10) %>%
  rename(c(
    AET=nutriment1, AESA=nutriment2,
    protéines=nutriment3, glucides=nutriment4, lipides=nutriment10,
    phosphore=nutriment33, potassium=nutriment34,
    contribu_proteines_a_AESA=contrib3, contribu_glucides_a_AESA=contrib4, contribu_lipides_a_AESA=contrib10))


# Conso aliments ----------------------------------------------------------
#très bien, conso par aliments
#j'ai commencé à faire de grandes catégories, on verra si je poursuis
head(conso_gpe_inca3)
aliments_consommés <- conso_gpe_inca3

mutate(
  céréales = conso_gpe1+conso_gpe2+conso_gpe3+conso_gpe4+conso_gpe5,#pain, céréales, riz
  produits_laitiers=conso_gpe7+conso_gpe8+conso_gpe9, #lait, yaourt fromages blancs, fromages
  oeufs=conso_gpe14,
  boeuf_porc=conso_gpe15+conso_gpe17+conso_gpe20,
  volaille=conso_gpe16,
  légumes=,
  légumineuses=,
  pomme_de_terre=,
  fruits=)


# Individus ----------------------------------------------
head(description_indiv)
individus <- description_indiv %>% 
  select(NOIND, sex_PS, tage_PS, taille, poids, imc) %>% 
  mutate(age_classe = case_when(
      tage_PS == 1~"<1", tage_PS == 2~"1-3", tage_PS == 3~"4-6", tage_PS == 4~"7-10", tage_PS == 5~"11-14",
      tage_PS == 6~"15-17", tage_PS == 7~"18-44", tage_PS == 8~"45-64", tage_PS == 9~"65-79")) %>%
  mutate(age_classe_large = case_when(
    tage_PS %in% c(1,2,3,4)~"enfant (<10 ans)", tage_PS %in% c(5,6)~"adolescent (11-18 ans)", tage_PS %in% c(7,8,9)~"adulte (>18 ans)")) %>%
  mutate(sex = case_when(sex_PS == 1~"H", sex_PS == 2~"F"))
individus$age_classe <- 
  factor(individus$age_classe, levels = c("<1", "1-3", "4-6", "7-10", "11-14", "15-17", "18-44", "45-64", "65-79"))
individus$age_classe <- 
  factor(individus$age_classe_large, levels = c("enfant (<10 ans)", "adolescent (11-18 ans)", "adulte (>18 ans)"))


# Fusion ------------------------------------------------------------------
join <- inner_join(apport_nutriments, individus, by = "NOIND")
join <- inner_join(join, aliments_consommés, by = "NOIND")



# Graph nutriments --------------------------------------------------------
plot_grid(
  #phosphore
  ggplot(join %>% filter() , aes(x = as.factor(sex), y = phosphore/1000)) +
    geom_violin() +
    geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA) +
    stat_summary(fun = median, geom="crossbar", width = .05, color = "white") +
    stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red") +
    xlab("classe d'âge et sexe") + ylab("phosphore (g/j)") + theme_classic() + 
    facet_grid(.~age_classe_large, space="free_x", scales="free_x", switch="x") +
    theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm")),
  #azote
  ggplot(join %>% filter() , aes(x = as.factor(sex), y = protéines/6.25)) +
    geom_violin() +
    geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA) +
    stat_summary(fun = median, geom="crossbar", width = .05, color = "white") +
    stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red") +
    xlab("classe d'âge et sexe") + ylab("azote (g/j)") + theme_classic() + 
    facet_grid(.~age_classe_large, space="free_x", scales="free_x", switch="x") +
    theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm")),
  #potassium
  ggplot(join %>% filter() , aes(x = as.factor(sex), y = potassium/1000)) +
    geom_violin() +
    geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA) +
    stat_summary(fun = median, geom="crossbar", width = .05, color = "white") +
    stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red") +
    xlab("classe d'âge et sexe") + ylab("potassium (g/j)") + theme_classic() + 
    facet_grid(.~age_classe_large, space="free_x", scales="free_x", switch="x") +
    theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm")),
  #énergie
  ggplot(join %>% filter() , aes(x = as.factor(sex), y = AET)) +
    geom_violin() +
    geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA) +
    stat_summary(fun = median, geom="crossbar", width = .05, color = "white") +
    stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red") +
    xlab("classe d'âge et sexe") + ylab("énergie totale (kcal/j)") + theme_classic() + 
    facet_grid(.~age_classe_large, space="free_x", scales="free_x", switch="x") +
    theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm")),
  ncol = 2
)

plot_grid(
  ggplot(join) + geom_density(aes(AET)),
  ggplot(join) + geom_density(aes(protéines)),
  ggplot(join) + geom_density(aes(glucides)),
  ggplot(join) + geom_density(aes(lipides)),
  ggplot(join) + geom_density(aes(phosphore)),
  ggplot(join) + geom_density(aes(potassium))
)


# Graphs aliments ---------------------------------------------------------
#lait
ggplot(join , aes(x = as.factor(sex), y = conso_gpe7)) +
  geom_violin(width = .4, scale = "width", position = position_nudge(x = -.3, y = 0)) +
  geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA, position = position_nudge(x = -.3, y = 0)) +
  geom_jitter(shape = 20, size = .8, width = .05) +
  stat_summary(fun = median, geom="crossbar", width = .05, color = "white", position = position_nudge(x = -.3, y = 0)) +
  stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red", position = position_nudge(x = -.3, y = 0)) +
  xlab("classe d'âge (années) et sexe") + ylab("lait (g/j") + theme_classic() + 
  facet_grid(.~age_classe, space="free_x", scales="free_x", switch="x") +
  theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm"))
#fromage
ggplot(join , aes(x = as.factor(sex), y = conso_gpe9)) +
  geom_violin(width = .4, scale = "width", position = position_nudge(x = -.3, y = 0)) +
  geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA, position = position_nudge(x = -.3, y = 0)) +
  geom_jitter(shape = 20, size = .8, width = .05) +
  stat_summary(fun = median, geom="crossbar", width = .05, color = "white", position = position_nudge(x = -.3, y = 0)) +
  stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red", position = position_nudge(x = -.3, y = 0)) +
  xlab("classe d'âge et sexe") + ylab("fromage (g/j)") + theme_classic() + 
  facet_grid(.~age_classe, space="free_x", scales="free_x", switch="x") +
  theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm"))
#oeufs
ggplot(join , aes(x = as.factor(sex), y = conso_gpe14)) +
  geom_violin(width = .4, scale = "width", position = position_nudge(x = -.3, y = 0)) +
  geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA, position = position_nudge(x = -.3, y = 0)) +
  geom_jitter(shape = 20, size = .8, width = .05) +
  stat_summary(fun = median, geom="crossbar", width = .05, color = "white", position = position_nudge(x = -.3, y = 0)) +
  stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red", position = position_nudge(x = -.3, y = 0)) +
  xlab("classe d'âge et sexe") + ylab("oeufs (g/j)") + theme_classic() + 
  facet_grid(.~age_classe, space="free_x", scales="free_x", switch="x") +
  theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm"))
#volaille
ggplot(join , aes(x = as.factor(sex), y = conso_gpe15)) +
  geom_violin(width = .4, scale = "width", position = position_nudge(x = -.3, y = 0)) +
  geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA, position = position_nudge(x = -.3, y = 0)) +
  geom_jitter(shape = 20, size = .8, width = .05) +
  stat_summary(fun = median, geom="crossbar", width = .05, color = "white", position = position_nudge(x = -.3, y = 0)) +
  stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red", position = position_nudge(x = -.3, y = 0)) +
  xlab("classe d'âge et sexe") + ylab("volaille (g/j)") + theme_classic() + 
  facet_grid(.~age_classe, space="free_x", scales="free_x", switch="x") +
  theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm"))



# Taille, poids, IMC ------------------------------------------------------
plot_grid(
  #taille
  ggplot(individus , aes(x = as.factor(sex), y = taille)) +
    geom_violin(width = .4, scale = "width", position = position_nudge(x = -.3, y = 0)) +
    geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA, position = position_nudge(x = -.3, y = 0)) +
    geom_jitter(shape = 20, size = .8, width = .05) +
    stat_summary(fun = median, geom="crossbar", width = .05, color = "white", position = position_nudge(x = -.3, y = 0)) +
    stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red", position = position_nudge(x = -.3, y = 0)) +
    xlab("classe d'âge (années) et sexe") + ylab("taille (cm)") + theme_classic() + 
    facet_grid(.~age_classe, space="free_x", scales="free_x", switch="x") +
    theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm")),
  #poids
  ggplot(individus , aes(x = as.factor(sex), y = poids)) +
    geom_violin(width = .4, scale = "width", position = position_nudge(x = -.3, y = 0)) +
    geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA, position = position_nudge(x = -.3, y = 0)) +
    geom_jitter(shape = 20, size = .8, width = .05) +
    stat_summary(fun = median, geom="crossbar", width = .05, color = "white", position = position_nudge(x = -.3, y = 0)) +
    stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red", position = position_nudge(x = -.3, y = 0)) +
    xlab("classe d'âge (années) et sexe") + ylab("poids (kg)") + theme_classic() + 
    facet_grid(.~age_classe, space="free_x", scales="free_x", switch="x") +
    theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm")),
  #imc
  ggplot(individus , aes(x = as.factor(sex), y = imc)) +
    geom_violin(width = .4, scale = "width", position = position_nudge(x = -.3, y = 0)) +
    geom_boxplot(fill = "black", color = "black", width = .05, outlier.shape = NA, position = position_nudge(x = -.3, y = 0)) +
    geom_jitter(shape = 20, size = .8, width = .05) +
    stat_summary(fun = median, geom="crossbar", width = .05, color = "white", position = position_nudge(x = -.3, y = 0)) +
    stat_summary(fun = mean, geom="point", shape = 23, size = 2, fill = "red", position = position_nudge(x = -.3, y = 0)) +
    xlab("classe d'âge (années) et sexe") + ylab("imc (kg/m2)") + theme_classic() + 
    facet_grid(.~age_classe, space="free_x", scales="free_x", switch="x") +
    theme(strip.placement = "outside", strip.background = element_rect(fill=NA, colour="grey50"), panel.spacing.x=unit(0,"cm")),
  ncol = 1
)
# plutôt que de tous les représenter ensemble plutot faire adultes / ados / enfants
# aussi enlever les points je pense





















