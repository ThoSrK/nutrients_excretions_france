library(ggplot2)
#library(plyr)
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

#A FAIRE : VOIR SI LES AUTRES FACONS DE CALCULER LES MOYENNES DE RENDEMENTS CHANGENT BEAUCOUP DE CHOSES

# Load and clean data -----------------------------------------------------
steu <- read.csv("data/STEU/Adour_Garonne/donnees_rejets_collectivites_2020/listeObj.csv", sep = ";")
rejets <- read.csv("data/STEU/Adour_Garonne/donnees_rejets_collectivites_2020/listeDataIndicateurs.csv", sep = ";")
rejets <-  rejets %>% filter(code_rj !="0564520V0011") %>% # la sation 0564520V0011 est en double !!!! avec des valeurs différentes
  mutate(valeur = rowMeans(.[7:18], na.rm(T))) %>% #on fait la moyenne annuelle
  #attention ici je prend la moyenne des rendements, chaque mois a le même poids. Pas forcément pertinent ?
  #on peut aussi faire sum(in)/sum(out)
  #ou weighted.meand(in/out, weight = in) donne plus de poids aux jours à fort débit
  #donc à essayer pour voir ce que ça change
  select(-valeur_01, -valeur_02, -valeur_03, -valeur_04, -valeur_05, -valeur_06, #on enlève valeurs mensuelles
         -valeur_07, -valeur_08, -valeur_09, -valeur_10, -valeur_11, -valeur_12)  %>%
  mutate(valeur = case_when(
    is.na(valeur_y) == F ~ valeur_y, T~valeur)) %>% #on met les valeurs de boue avec les autres
  select(-valeur_y, -type_rj) %>% #colonnes inutiles
  mutate_all(~ifelse(is.nan(.), NA, .)) #on remplace les NAN issus de la moyenne par des na
all_data <- full_join(rejets, steu, by = "code_rj")

#phosphore
data_PT <- all_data %>% filter(parametre == "PT") %>% #4456 obs
  spread(indicateur, valeur) %>% 
  mutate(rendement = 1-POSOR/POENT) #calcul du rendement
n1 <- nrow(data_PT)
data_PT <- data_PT %>% #4489 obs qui ne sont pas na
  filter(is.na(rendement) == F)
n2 <- nrow(data_PT)
data_PT <- data_PT %>% #4140 obs qui n'ont pas un rendement incohérent
  filter(rendement >=0 & rendement<=1)
n3 <- nrow(data_PT)
(1-n3/n2)*100 # pourcentage de valeurs incohérentes
(1-n2/n1)*100 # pourcentage de valeurs vides
#je filtre direct ici les rendements mais si on veut des chiffres juste sur entrée ou sortie il faut filtrer plus tard
#d'un autre coté permet d'éliminer les entrées et sorties aux valeurs incohérentes ?

#azote
data_NGL <- all_data %>% filter(parametre == "NGL") %>% #4456 obs
  spread(indicateur, valeur) %>% 
  mutate(rendement = 1-POSOR/POENT) #calcul du rendement
n1 <- nrow(data_NGL)
data_NGL <- data_NGL %>% #4157 obs qui ne sont pas na
  filter(is.na(rendement) == F)
n2 <- nrow(data_NGL)
data_NGL <- data_NGL %>% #3952 obs qui n'ont pas un rendement incohérent
  filter(rendement >=0 & rendement<=1)
n3 <- nrow(data_NGL)
(1-n3/n2)*100 # pourcentage de valeurs incohérentes
(1-n2/n1)*100 # pourcentage de valeurs vides
#beaucoup de valeurs de rendement = 1 ou 0 (fait passer exclusion pour incohérence de 35% à 5%)
#checker ces valeurs 0 et 1 (pareil pour P)

#Boues
data_BOUES <- all_data %>% filter(indicateur == "BOUE") %>% 
  spread(parametre, valeur) 




# P Graphs and results ----------------------------------------------------------------
#compare "pollution entrante" et "pollution déversée par système collecte jour moyen"
ggplot(data_PT) + geom_point(aes(PORDO, POENT)) + scale_x_log10() + scale_y_log10() + geom_abline(slope=1, intercept=0)
#pollution entrante vs rendement : motif bizarre pour faibles flux
ggplot(data_PT) + geom_point(aes(POENT, rendement)) + ylim(0,1) + scale_x_log10() 
#distribution des rendements sur toutes les stations
g <- ggplot(data_PT) + 
  geom_density(aes(rendement, color = "no weight")) +   xlim(0,1) +
  geom_density(aes(rendement, weight = POENT/sum(POENT), color = "P in weight"))  +
  geom_vline(aes(xintercept = mean(rendement), color = "no weight"), linetype = "dashed") +
  geom_vline(aes(xintercept = weighted.mean(rendement, POENT), color = "P in weight"), linetype = "dashed")
ggsave("graphs/Adour_Garonne/P_yield.pdf", g, w=6, h=4)
mean(data_PT$rendement, na.rm = T) #rendement moyen non pondéré
weighted.mean(data_PT$rendement, data_PT$POENT, nar.rm = T) #rendement moyen pondéré par P entrant
#moyenne de 45% vs 70%
#distribution rendement par type de traitement, non pondéré par la charge entrante
g <- ggplot(data_PT) + 
  geom_density(aes(rendement, color = "no weight")) +   xlim(0,1) +
  geom_density(aes(rendement, weight = POENT/sum(POENT), color = "P in weight"))  +
  facet_wrap(vars(traitement_principal), scales = "free")
ggsave("graphs/Adour_Garonne/P_yield_treatment.pdf", g, w=12, h=8)
total_P_in <-  sum(data_PT$POENT)
total_obs <- nrow(data_PT)
temp <- data_PT %>% group_by(traitement_principal) %>% summarise(
  mean_no_weight = mean(rendement)*100,
  mean_P_in_weight = weighted.mean(rendement, POENT)*100,
  nb_observation = n(), 
  percent_obs = n()/total_obs*100,
  P_in = sum(POENT),
  percent_of_total_P = sum(POENT)/total_P_in*100) %>%
  arrange(P_in)

#environ 5-10 points de pourcentage de différence entre weighted et non weighted mean
#sauf Secondaire bio (Ntk) qui représente 80% des obs et 50% de la charge entrante, là passe de 40% à 60%



# N Graphs and results ----------------------------------------------------------------
#compare "pollution entrante" et "pollution déversée par système collecte jour moyen"
ggplot(data_NGL) + geom_point(aes(PORDO, POENT)) + scale_x_log10() + scale_y_log10() + geom_abline(slope=1, intercept=0)
#pollution entrante vs rendement : contrairement à P pas de motif bizarre
ggplot(data_NGL) + geom_point(aes(POENT, rendement)) + ylim(0,1) + scale_x_log10() 
#distribution des rendements sur toutes les stations
g <- ggplot(data_NGL) + 
  geom_density(aes(rendement, color = "no weight")) +   xlim(0,1) +
  geom_density(aes(rendement, weight = POENT/sum(POENT), color = "N in weight"))  +
  geom_vline(aes(xintercept = mean(rendement), color = "no weight"), linetype = "dashed") +
  geom_vline(aes(xintercept = weighted.mean(rendement, POENT), color = "N in weight"), linetype = "dashed")
ggsave("graphs/Adour_Garonne/N_yield.pdf", g, w=6, h=4)
mean(data_NGL$rendement, na.rm = T) #rendement moyen non pondéré
weighted.mean(data_NGL$rendement, data_NGL$POENT, nar.rm = T) #rendement moyen pondéré par N entrant
#moyenne passe de 40 à 60%  (mais quasi identique à 62% si on exclut les rendements 0 et 1)
#distribution rendement par type de traitement, non pondéré par la charge entrante
g <- ggplot(data_NGL) + 
  geom_density(aes(rendement, color = "no weight")) +   xlim(0,1) +
  geom_density(aes(rendement, weight = POENT/sum(POENT), color = "N in weight"))  +
  facet_wrap(vars(traitement_principal), scales = "free")
ggsave("graphs/Adour_Garonne/N_yield_treatment.pdf", g, w=12, h=8)
total_N_in <-  sum(data_NGL$POENT)
total_obs <- nrow(data_NGL)
temp <- data_NGL %>% group_by(traitement_principal) %>% summarise(
  mean_no_weight = mean(rendement)*100,
  mean_N_in_weight = weighted.mean(rendement, POENT)*100,
  nb_observation = n(), 
  percent_obs = n()/total_obs*100,
  N_in = sum(POENT),
  percent_of_total_N = sum(POENT)/total_N_in*100) %>%
  arrange(N_in)
#


# P in Boues graphs and results -------------------------------------------

#teneur en P des boues
test <- data_BOUES %>% select(PROD, code_rj, traitement_principal)
test2 <- data_PT %>% select(POSOR, POENT, code_rj)
test3 <- full_join(test2, test, by = "code_rj")
test3 <- test3 %>% mutate(txP = (POENT-POSOR)*365/PROD*100) %>% filter(is.na(txP) == F) #1065 obs
test3 <- test3 %>% filter(txP <=100)#1057 obs
#test3 <- test3 %>% filter(txP <=10)#1009 obs
#5% de valeurs ayant une teneur supérieure à 10%, juste 8 valeurs incohérentes >100, semble correct
ggplot(test3) + scale_x_log10() + 
  geom_density(aes(x = txP, color = "no weight")) + 
  geom_density(aes(x = txP, weight = PROD/sum(PROD), color = "Sludge weight")) 
mean(test3$txP, na.rm = T) #teneur moyenne en P non pondérée 3.7%
weighted.mean(test3$txP, test3$PROD, nar.rm = T) # pondéré par production de boue 1.5%
#distribution teneur en P par technologie
ggplot(test3) + scale_x_log10() + 
  geom_density(aes(x = txP, color = "no weight")) + 
  geom_density(aes(x = txP, weight = PROD/sum(PROD), color = "Sludge weight")) +
  facet_wrap(vars(traitement_principal), scales = "free_y" )

total_sludge_in <-  sum(test3$sludge)
total_obs <- nrow(test3)
test3 %>% group_by(traitement_principal) %>% summarise(
  mean_no_weight = mean(txP),
  mean_sludge_in_weight = weighted.mean(txP, PROD),
  nb_observation = n(), 
  percent_obs = n()/total_obs*100,
  sludge_in = sum(PROD),
  percent_of_total_sludge = sum(PROD)/total_sludge_in*100) %>%
  arrange(sludge_in)



