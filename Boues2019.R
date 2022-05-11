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



# Europe data -------------------------------------------------------------
#zipf plots pas très satisfaisants: surtout dépendants des délimitations administratices

#simple NUTS area : only about 1600 obs, correspond en gros aux départements
Europe <- read.csv("domesticWasteEmissionsAdmin_v10.csv") %>%
  #NUTS3_ID : Identifier of Eurostat Nuts level 3 administrative areas https://ec.europa.eu/eurostat/fr/web/nuts/background
  # (en fait correspond aux départements)
  #pour carte france cf https://ec.europa.eu/eurostat/documents/345175/7451602/2021-NUTS-3-map-FR.pdf
  arrange(desc(PE_TOT)) %>%
  mutate(
    cumulative = cumsum(PE_TOT),
    rank = rank(-PE_TOT, ties.method = "first"),
    pop_tot = sum(PE_TOT, na.rm = T),
    rank_tot = max(rank),
    max_pop = max(PE_TOT))  
ggplot(Europe, aes(x = rank, y = cumulative/pop_tot*100)) +
  geom_step() + xlab("rank of administrative area") + ylab("% cumulative population") +
  scale_x_log10() + scale_y_log10()
ggplot(Europe, aes(x = rank/rank_tot*100, y = PE_TOT)) +
  geom_point() + xlab("") + ylab("") +
  scale_x_log10(limits = c(NA, 95)) + scale_y_log10(limits = c(10000, NA))
ggplot(Europe, aes(x = rank, y = PE_TOT)) +
  geom_point() + xlab("rank of administrative area") + ylab("PE") +
  scale_x_log10() + scale_y_log10(limits = c(1, NA))

#more complex catchment area, about 1.3M obs
#WSO1_ID Catchment Characterisation Model (CCM) 
#https://data.europa.eu/data/datasets/fe1878e8-7541-4c66-8453-afdae7469221?locale=en
Europe2 <- read.csv("domesticWasteEmissions_v10.csv") %>%
  arrange(desc(PE_TOT)) %>%
  mutate(
    cumulative = cumsum(PE_TOT),
    rank = rank(-PE_TOT, ties.method = "first"),
    pop_tot = sum(PE_TOT, na.rm = T),
    rank_tot = max(rank),
    max_pop = max(PE_TOT))
#zipf plot for all europe
ggsave(plot = 
         ggplot(Europe2, aes(x = rank, y = PE_TOT)) +
         geom_point() + xlab("rank of CCM area") + ylab("PE") +
         scale_x_log10() + scale_y_log10(),
       "Europe.png", width = 10, height = 8
)
#zipf plot by country
Europe2 <-  Europe2 %>%
  mutate(country = str_sub(NUTS3_ID, end = 2)) %>%
  group_by(country) %>%
  arrange(desc(PE_TOT)) %>%
  mutate(
    cumulative = cumsum(PE_TOT),
    rank = rank(-PE_TOT, ties.method = "first"),
    pop_tot = sum(PE_TOT, na.rm = T),
    rank_tot = max(rank),
    max_pop = max(PE_TOT))
ggsave(plot = 
         ggplot(Europe2, aes(x = rank, y = PE_TOT)) +
         geom_point() + xlab("rank of CCM area") + ylab("PE") +
         scale_x_log10() + scale_y_log10() + facet_wrap(vars(country)),
       "Europe_by country.png", width = 10, height = 8
)

#france (pas échelle département, échelle fine, 90k obs)
France <- Europe2 %>%
  filter(grepl('^FR', NUTS3_ID)) %>%
  arrange(desc(PE_TOT)) %>%
  mutate(
    cumulative = cumsum(PE_TOT),
    rank = rank(-PE_TOT, ties.method = "first"),
    pop_tot = sum(PE_TOT, na.rm = T),
    rank_tot = max(rank),
    max_pop = max(PE_TOT)) 
#kinf of zipf plots, faire le tri
ggplot(France, aes(x = rank, y = cumulative/pop_tot*100)) +
  geom_step() + xlab("rank of CCM area") + ylab("% cumulative population") +
  scale_x_log10() + scale_y_log10()
ggplot(France, aes(x = rank, y = cumulative)) +
  geom_step() + xlab("rank of CCM area") + ylab("cumulative population") +
  scale_x_log10()
ggplot(France, aes(x = rank, y = cumulative)) +
  geom_step() + xlab("rank of CCM area") + ylab("cumulative population") +
  scale_y_log10()
ggplot(France, aes(x = rank/rank_tot*100, y = PE_TOT)) +
  geom_point() + xlab("") + ylab("") +
  scale_x_log10() + scale_y_log10()
ggsave(plot = 
  ggplot(France, aes(x = rank, y = PE_TOT)) +
    geom_point() + xlab("rank of CCM area") + ylab("PE") +
    scale_x_log10() + scale_y_log10(),
  "France.png", width = 10, height = 8
)

#répatition 1aire, 2aire, 3aire et autres
ggplot(France %>% select(N_SD, N_IAS, N_0, N_1, N_2, N_3, N_3P) %>%
         gather(value = N_load, key = N_treatment) %>%
         mutate(Ntot = sum(N_load))) + 
  geom_col(aes(x = N_treatment, y = N_load/Ntot))
#pas sûr qu'il faille garder N_SD

ggplot(France %>% select(P_SD, P_IAS, P_0, P_1, P_2, P_3, P_3P) %>%
         gather(value = P_load, key = P_treatment) %>%
         mutate(Ptot = sum(P_load))) + 
  geom_col(aes(x = P_treatment, y = P_load/Ptot))
#pas sûr qu'il faille garder P_SD




# Population --------------------------------------------------------------
Population <- read.csv("Pop/base-pop-historiques-1876-2019.csv", sep = ";") %>%
  select(-c(Population.en.2018, Population.en.2017, Population.en.2016, Population.en.2015, Population.en.2014, Population.en.2013,
            Population.en.2011, Population.en.2010, Population.en.2009, Population.en.2008, Population.en.2007
            )) %>%
  gather(value = population, key = year, 5:26) %>%
  #enlève Corse et Outre Mer pour simplifier
  filter(Département != "2A" & Département != "2B" & 
           Département != "971" & Département != "972" & Département != "973" & Département != "974") %>%
  mutate(
    year = as.factor(str_sub(year, start = -4)), #transforme year
    Région = as.numeric(Région),
    Département = as.numeric(Département),
    population = as.numeric(population, na.rm = T))
#fusionner les arrondissements de Paris en 1 seule ville
Paris <- Population %>% 
  filter(Département ==75) %>% group_by(year)
ggplot(Paris, aes(as.numeric(as.character(year)), population)) + geom_line() + facet_wrap(vars(Libellé.géographique))
Paris <- Paris %>%
  mutate(population = sum(population), Libellé.géographique = "Paris") %>%
  filter(Code.géographique == 75101)
ggplot(Paris, aes(as.numeric(as.character(year)), population, population)) + geom_line() + ylim(0,3000000)
#fusionner Paris avec le reste (après avoir enlevé les arrondissements)
Population <- rbind(Population %>% filter(Département != 75), Paris) %>%
  arrange(desc(population)) %>%
  group_by(year) %>% mutate(
    cumulative = cumsum(population),
    rank = rank(-population, ties.method = "first"),
    pop_tot = sum(population, na.rm = T),
    rank_tot = max(rank),
    max_pop = max(population))  

ggsave(
  plot = 
    plot_grid(
      ggplot(Population, aes(x = rank, y = cumulative/pop_tot*100, color = year)) +
        geom_step() + xlab("rank of city") + ylab("% cumulative population")+ theme(legend.position = "none"),
      ggplot(Population, aes(x = rank, y = population, color = year)) +
        geom_point() + xlab("rank of city") + ylab("city population") +
        scale_x_log10() + scale_y_log10(),
      ggplot(Population, aes(x = rank, y = cumulative, color = year)) +
        geom_step() + xlab("rank of city") + ylab("cumulative population")+ theme(legend.position = "none"),
      ggplot(Population, aes(x = rank/rank_tot*100, y = population/max_pop, color = year)) +
        geom_point() + xlab("% rank of city") + ylab("% of max city population") +
        scale_x_log10() + scale_y_log10(),
      ncol = 2, rel_widths = c(2/5, 3/5)
      ),
  "pop_vs_city.png", width = 10, height = 8)


# BDERU preparation -------------------------------------------------------------------
#cf carte et téléchargement https://www.assainissement.developpement-durable.gouv.fr/PortailAC/data#!
f2020 <- read.csv("BDERU/db_export_2020.csv", sep = "|") %>% mutate(year = 2020)
f2019 <- read.csv("BDERU/db_export_2019.csv", sep = "|") %>% mutate(year = 2019)
f2018 <- read.csv("BDERU/db_export_2018.csv", sep = "|") %>% mutate(year = 2018)
f2017 <- read.csv("BDERU/db_export_2017.csv", sep = "|") %>% mutate(year = 2017)
f2016 <- read.csv("BDERU/db_export_2016.csv", sep = "|") %>% mutate(year = 2016)
f2015 <- read.csv("BDERU/db_export_2015.csv", sep = "|") %>% mutate(year = 2015)
f2014 <- read.csv("BDERU/db_export_2014.csv", sep = "|") %>% mutate(year = 2014)
file <- bind_rows(f2020, f2019, f2018, f2017, f2016, f2015, f2014)
Boues <- file %>% select(
  year,
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
  Niveau.traitement.existant...désinfection,#conformité perf DBO, N, P pas encore pris
  #boues produites
  Prod.boues.sans.réactif..tMS.an.,
  Quantité.réactifs.utilisés..t.an.,
  Quantité.épandage.agricole..tMS.an.,
  Quantité.incinérée..tMS.an.,
  Quantité.Compostage..produit...tMS.an.,
  Quantité.mise.en.décharge..tMS.an.,
  Quantité.en.valorisation.industrielle..tMS.an.,
  Qté.envoyée.sur.autre.STEU..tMS.an., #compotage, déchet, transit, centre méthanisation,  séchage...
  #sensibilité
  Sensibilité.azote, #aussi la date
  Sensibilité.phosphore #aussi la date
)

# threshold_sludge <- 0
# zero_sludge <- file %>% filter(Prod.boues.sans.réactif..tMS.an. <= threshold_sludge)
# positive_sludge <- file %>% filter(Prod.boues.sans.réactif..tMS.an. > threshold_sludge)

Boues %>% filter(is.na(Prod.boues.sans.réactif..tMS.an.) == T) #check no NA values for sludge : OK

# STEU vs sludge ----------------------------------------------------------
#nb of STEU vs sludge
test <- Boues %>% arrange(desc(Prod.boues.sans.réactif..tMS.an.)) %>%
  group_by(year) %>% mutate( 
  cumulative_sludge = cumsum(Prod.boues.sans.réactif..tMS.an.),
  rank_STEU = rank(-Prod.boues.sans.réactif..tMS.an., ties.method = "first"),
  sludge_tot = sum(Prod.boues.sans.réactif..tMS.an., na.rm = T),
  rank_tot_STEU = max(rank_STEU)) 
ggsave(plot = 
         plot_grid(
#nb STEU vs cumulative Sludge, absolute
ggplot(test, aes(x = rank_STEU, y = cumulative_sludge/1000000, color = as.factor(year))) +
  geom_step() + xlab("nb of STEU") + ylab("cumulative sludge produced \n(Mt of dry sludge)") +
  scale_x_log10()+ theme(legend.position = "none"),
#nb STEU vs Sludge, relative : 1% of STEU make 1/2 to 2/3 of total sludge
ggplot(test, aes(x = rank_STEU/rank_tot_STEU*100, y = cumulative_sludge/sludge_tot*100, color = as.factor(year))) +
  geom_step() + xlab("% of STEU") + ylab("% cumulative sludge produced") +
  scale_x_log10()+ theme(legend.position = "none"),
#Zipf nb STEU vs Sludge, absolute
ggplot(test, aes(x = rank_STEU, y = Prod.boues.sans.réactif..tMS.an., color = as.factor(year))) +
  geom_point() + xlab("nb of STEU") + ylab("sludge produced \n(tonnes MS))") +
  scale_x_log10() + scale_y_log10(),
#nb STEU vs cumulative Sludge, absolute
ggplot(test, aes(x = rank_STEU, y = cumulative_sludge/1000000, color = as.factor(year))) +
  geom_step() + xlab("nb of STEU") + ylab("cumulative sludge produced \n(Mt of dry sludge)") +
  theme(legend.position = "none"),
#nb STEU vs Sludge, relative : 1% of STEU make 1/2 to 2/3 of total sludge
ggplot(test, aes(x = rank_STEU/rank_tot_STEU*100, y = cumulative_sludge/sludge_tot*100, color = as.factor(year))) +
  geom_step() + xlab("% of STEU") + ylab("% cumulative sludge produced") +
  theme(legend.position = "none"),
ncol = 3, rel_widths = c(1/4, 1/4, 1/2)
         ), "nb_STEU_vs_sludge.png", height = 6, width = 12
)

# STEU vs EH --------------------------------------------------------------
#nb STEU vs capacité nominale EH
test <- Boues %>% arrange(desc(Capacité.nominale.en.EH)) %>%
  group_by(year) %>% mutate( 
    cumulative_EH = cumsum(Capacité.nominale.en.EH),
    rank_STEU = rank(-Capacité.nominale.en.EH, ties.method = "first"),
    EH_tot = sum(Capacité.nominale.en.EH, na.rm = T),
    rank_tot_STEU = max(rank_STEU)) 
ggsave(plot = 
plot_grid(
#nb of STEU vs Cumulative nominal capacity EH, absolute, log
ggplot(test, aes(x = rank_STEU, y = cumulative_EH/1000000, color = as.factor(year))) +
  geom_step() +   xlab("nb of STEU") + ylab("cumulative nominal capacity \n(millions EH)") +
  scale_x_log10()+ theme(legend.position = "none"),
#nb of STEU vs Cumulative nominal capacity EH, relative, log : 1% of STEU make 50% of total EH capacity
ggplot(test, aes(x = rank_STEU/rank_tot_STEU*100, y = cumulative_EH/EH_tot*100, color = as.factor(year))) +
  geom_step() + xlab("% of STEU") + ylab("% of cumulative nominal EH capacity") +
  scale_x_log10()+ theme(legend.position = "none"),
#Zipf nb of STEU vs capacité nominale
ggplot(test, aes(x = rank_STEU, y = Capacité.nominale.en.EH/1000, color = as.factor(year))) +
  geom_point() + xlab("nb of STEU") + ylab("capacité nominale de la STEU \n(milliers EH)") +
  scale_x_log10() + scale_y_log10(),
#nb of STEU vs Cumulative nominal capacity EH, absolute, log
ggplot(test, aes(x = rank_STEU, y = cumulative_EH/1000000, color = as.factor(year))) +
  geom_step() +   xlab("nb of STEU") + ylab("cumulative nominal capacity \n(millions EH)")+ theme(legend.position = "none"),
#nb of STEU vs Cumulative nominal capacity EH, relative, log : 1% of STEU make 50% of total EH capacity
ggplot(test, aes(x = rank_STEU/rank_tot_STEU*100, y = cumulative_EH/EH_tot*100, color = as.factor(year))) +
  geom_step() + xlab("% of STEU") + ylab("% of cumulative nominal EH capacity")+ theme(legend.position = "none"),
ncol = 3, rel_widths = c(1/4, 1/4, 1/2)
), "nb_STEU_vs_EH.png", height = 6, width = 12
)

# EH vs sludge ------------------------------------------------------------
#EH vs sludge
test <- Boues %>% arrange(desc(Capacité.nominale.en.EH)) %>%
  group_by(year) %>% mutate( 
    cumulative_EH = cumsum(Capacité.nominale.en.EH),
    cumulative_sludge = cumsum(Prod.boues.sans.réactif..tMS.an.),
    EH_tot = sum(Capacité.nominale.en.EH, na.rm = T),
    sludge_tot = sum(Prod.boues.sans.réactif..tMS.an., na.rm = T))
#EH vs sludge : 2014 et 2015 threshold bizarre de déclaration sludge
ggsave(plot = 
ggplot(test, aes(x = Capacité.nominale.en.EH, y = Prod.boues.sans.réactif..tMS.an.)) +
  geom_point() + xlab("Nominal capacity EH") + ylab("Sludge produced (tonnes MS)") + scale_x_log10() + scale_y_log10() +
  facet_wrap(vars(year)),
"EH_vs_sludge.png", height = 6, width = 8
)

#cumulative EH vs cumulative sludge
test <- Boues %>% arrange(desc(Prod.boues.sans.réactif..tMS.an.)) %>%
  group_by(year) %>% mutate( 
    cumulative_EH = cumsum(Capacité.nominale.en.EH),
    cumulative_sludge = cumsum(Prod.boues.sans.réactif..tMS.an.),
    EH_tot = sum(Capacité.nominale.en.EH, na.rm = T),
    sludge_tot = sum(Prod.boues.sans.réactif..tMS.an., na.rm = T))
#absolute
ggplot(test, aes(x = cumulative_EH, y = cumulative_sludge/1000000, color = as.factor(year))) +
  geom_step() + xlab("cumulative EH") + ylab("cumulative sludge produced (Mt of dry sludge)") 
#relative
ggplot(test, aes(x = cumulative_sludge/sludge_tot*100, y = cumulative_EH/EH_tot*100, color = as.factor(year))) +
  geom_step() + ylab("% ot total EH nominal capacity") + xlab("% of total sludge produced")


#% of STEU reporting 0 sludge
Boues %>% count(year, Prod.boues.sans.réactif..tMS.an.==0) %>% group_by(year) %>%
  mutate(proportion = prop.table(n)) %>% filter(`Prod.boues.sans.réactif..tMS.an. == 0` == TRUE)


n <- nrow(Boues)
n_zero_sudge <- nrow(zero_sludge)
n_zero_sudge/n #63% of STEU do not report sludge quantity
EH_zero_sludge <- sum(zero_sludge$Capacité.nominale.en.EH)
EH_zero_sludge/EH #but they represent only 8% of the total EH
describe(Boues)

sum(Boues$Taille.agglomération..EH.)
sum(Boues$Somme.des.capacités.nominales..EH.)
EH <- sum(Boues$Capacité.nominale.en.EH)






# Merge BDERU INSEE -------------------------------------------------------
Population <- read.csv("Pop/base-pop-historiques-1876-2019.csv", sep = ";") %>%
  select(c(Code.géographique, Région, Département, Libellé.géographique,
           Population.en.2018, Population.en.2017, Population.en.2016, Population.en.2015, Population.en.2014
           )
         ) %>%
  gather(value = population, key = year, 5:9) %>%
  #enlève Corse et Outre Mer pour simplifier
  filter(Département != "2A" & Département != "2B" & 
           Département != "971" & Département != "972" & Département != "973" & Département != "974") %>%
  mutate(
    year = as.factor(str_sub(year, start = -4)), #transforme year
    Région = as.numeric(Région),
    Département = as.numeric(Département),
    population = as.numeric(population, na.rm = T))
#fusionner les arrondissements de Paris en 1 seule ville
Paris <- Population %>% 
  filter(Département ==75) %>% group_by(year) %>%
  mutate(population = sum(population), Libellé.géographique = "Paris") %>%
  filter(Code.géographique == 75101)
#fusionner Paris avec le reste (après avoir enlevé les arrondissements)
Population <- rbind(Population %>% filter(Département != 75), Paris) %>%
arrange(desc(population)) %>%
  group_by(year) %>% mutate(
    cumulative = cumsum(population),
    rank = rank(-population, ties.method = "first"),
    pop_tot = sum(population, na.rm = T),
    rank_tot = max(rank),
    max_pop = max(population))

Population <- Population %>% filter(year==2018)


test <- Boues %>% arrange(desc(Capacité.nominale.en.EH)) %>%
  filter(year==2018) %>% mutate( 
    cumulative_EH = cumsum(Capacité.nominale.en.EH),
    rank_STEU = rank(-Capacité.nominale.en.EH, ties.method = "first"),
    EH_tot = sum(Capacité.nominale.en.EH, na.rm = T),
    rank_tot_STEU = max(rank_STEU)) 

ggplot(test, aes(x = rank_STEU, y = cumulative_EH/1000000)) +
  geom_step() +   
  geom_step(data = Population, aes(x = rank, y = cumulative/1000000)) +
  xlab("rank (city or STEU") + ylab("cumulative EH or population)") +
  scale_x_log10()

ggplot(test) +
  geom_step(aes(x = rank_STEU/rank_tot_STEU, y = cumulative_EH/EH_tot, color = "STEU")) +   
  geom_step(data = Population, aes(x = rank/rank_tot, y = cumulative/pop_tot, color = "Cities")) +
  xlab("% rank of city or STEU") + ylab("cumulative EH or population") +
  scale_x_log10() + scale_y_log10()

ggplot(test) +
  geom_step(aes(x = rank_STEU/rank_tot_STEU, y = Capacité.nominale.en.EH, color = "STEU")) +   
  geom_step(data = Population, aes(x = rank/rank_tot, y = population, color = "Cities")) +
  xlab("% rank of city or STEU") + ylab("EH or population") +
  scale_x_log10() + scale_y_log10()

ggplot(test) +
  geom_step(aes(x = rank_STEU, y = Capacité.nominale.en.EH, color = "STEU")) +   
  geom_step(data = Population, aes(x = rank, y = population, color = "Cities")) +
  xlab("rank of city or STEU") + ylab("EH or population") +
  scale_x_log10() + scale_y_log10()


# Graphs Zipf density -----------------------------------------------------

#% rang vs % cumulative capacity
ggplot(Boues %>% arrange(desc(Capacité.nominale.en.EH)), 
                         aes(x = rank(-Capacité.nominale.en.EH)/n*100, 
                             y = cumsum(Capacité.nominale.en.EH)/EH*100)) +
  geom_step()
ggplot(Boues %>% arrange(desc(Capacité.nominale.en.EH)), 
       aes(x = rank(-Capacité.nominale.en.EH)/n*100, 
           y = cumsum(Capacité.nominale.en.EH)/EH*100)) +
  geom_point() + scale_x_log10() + scale_y_log10(limits = c(1,100))
#faire la même chose avec taille des villes INSEE !

pop <- sum(Population$Population.en.2019)
n_pop <- nrow(Population %>% select(Population.en.2019))

ggplot(Population %>% arrange(population) %>% group_by(year),
       aes(group = year)) +
  geom_step(aes(x = rank(-population)/n_pop,
                y = cumsum(population)/pop))


#density of EH nominal capacity 
plot_grid(
  ggplot(zero_sludge) + geom_density(aes(x = Capacité.nominale.en.EH)) + scale_x_log10(),
  ggplot(positive_sludge) + geom_density(aes(x = Capacité.nominale.en.EH)) + scale_x_log10(),
  ggplot(Boues) + geom_density(aes(x = Capacité.nominale.en.EH)) + scale_x_log10()
) #rajouter les NA ?
max(zero_sludge$Capacité.nominale.en.EH)

#density of EH town size
plot_grid(
  ggplot(zero_sludge) + geom_density(aes(x = Taille.agglomération..EH.)) + scale_x_log10(),
  ggplot(positive_sludge) + geom_density(aes(x = Taille.agglomération..EH.)) + scale_x_log10(),
  ggplot(Boues) + geom_density(aes(x = Taille.agglomération..EH.)) + scale_x_log10()
)

#density of sludge production
ggplot(zero_sludge) + geom_density(aes(x = Prod.boues.sans.réactif..tMS.an.)) + scale_x_log10()
ggplot(Boues) + geom_density(aes(x = Prod.boues.sans.réactif..tMS.an.)) + scale_x_log10()
ggplot(positive_sludge) + geom_density(aes(x = Prod.boues.sans.réactif..tMS.an.)) + scale_x_log10()

#zipf nominal capacity
ggplot(zero_sludge) + geom_point(aes(y = Capacité.nominale.en.EH, x = rank(-Capacité.nominale.en.EH))) + 
  scale_y_log10() + scale_x_log10() + xlab("rang")
ggplot(positive_sludge) + geom_point(aes(y = Capacité.nominale.en.EH, x = rank(-Capacité.nominale.en.EH))) + 
  scale_y_log10() + scale_x_log10() + xlab("rang")
ggplot(Boues) + geom_point(aes(y = Capacité.nominale.en.EH, x = rank(-Capacité.nominale.en.EH))) + 
  scale_y_log10() + scale_x_log10() + xlab("rang")

#zipf Eh city size
ggplot(zero_sludge) + geom_point(aes(y = Taille.agglomération..EH., x = rank(-Taille.agglomération..EH.))) + 
  scale_y_log10() + scale_x_log10()
ggplot(positive_sludge) + geom_point(aes(y = Taille.agglomération..EH., x = rank(-Taille.agglomération..EH.))) + 
  scale_y_log10() + scale_x_log10()
ggplot(Boues) + geom_point(aes(y = Taille.agglomération..EH., x = rank(-Taille.agglomération..EH.))) + 
  scale_y_log10() + scale_x_log10()

#zipf sum nominal capacities
ggplot(zero_sludge) + geom_point(aes(y = Somme.des.capacités.nominales..EH., x = rank(-Somme.des.capacités.nominales..EH.))) + 
  scale_y_log10() + scale_x_log10()
ggplot(positive_sludge) + geom_point(aes(y = Somme.des.capacités.nominales..EH., x = rank(-Somme.des.capacités.nominales..EH.))) + 
  scale_y_log10() + scale_x_log10()
ggplot(Boues) + geom_point(aes(y = Somme.des.capacités.nominales..EH., x = rank(-Somme.des.capacités.nominales..EH.))) + 
  scale_y_log10() + scale_x_log10()

#EH vs boues
plot_grid(
  ggplot(Boues) + geom_point(aes(x = Taille.agglomération..EH., y = Prod.boues.sans.réactif..tMS.an.)) + 
    scale_y_log10() + scale_x_log10(),
  ggplot(Boues) + geom_point(aes(x = Somme.des.capacités.nominales..EH., y = Prod.boues.sans.réactif..tMS.an.)) + 
    scale_y_log10() + scale_x_log10(),
  ggplot(Boues) + geom_point(aes(x = Capacité.nominale.en.EH, y = Prod.boues.sans.réactif..tMS.an.)) + 
    scale_y_log10() + scale_x_log10()
)
#ICI SEMBLE BIEN INDIQUER QU'IL FAUT PRENDRE CAPACITE NOMINALE !!!!
plot_grid(
  ggplot(Boues) + geom_point(aes(x = Taille.agglomération..EH., y = Prod.boues.sans.réactif..tMS.an.)),
  ggplot(Boues) + geom_point(aes(x = Somme.des.capacités.nominales..EH., y = Prod.boues.sans.réactif..tMS.an.)),
  ggplot(Boues) + geom_point(aes(x = Capacité.nominale.en.EH, y = Prod.boues.sans.réactif..tMS.an.))
)

#plot comparaison differents EH
ggplot(Boues) + geom_point(aes(y = Somme.des.capacités.nominales..EH., x = Capacité.nominale.en.EH)) + 
  scale_y_log10() + scale_x_log10()
ggplot(Boues) + geom_point(aes(y = Somme.des.capacités.nominales..EH., x = Taille.agglomération..EH.)) + 
  scale_y_log10() + scale_x_log10()
ggplot(Boues) + geom_point(aes(y = Capacité.nominale.en.EH, x = Taille.agglomération..EH.)) + 
  scale_y_log10() + scale_x_log10()







# Graphs Boues------------------------------------------------------------------

#NB DE STEU
#filières eau et boue
plot_grid(
ggplot(data = Boues) + geom_bar(aes(x = Filière.eau.principale, weight = 1/n*100)) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)),
ggplot(data = Boues) + geom_bar(aes(x = Filière.boues.principale, weight = 1/n*100)) + 
  theme(axis.text.x = element_text(angle = 75, hjust = 1)),
nrow = 2
)
#traitement
plot_grid(
ggplot(Boues, aes(x = "", y=1/n*100 , fill = Niveau.traitement.existant...biologique)) + 
  geom_bar(stat="identity", width=1),
ggplot(Boues, aes(x = "", y=1/n*100 , fill = Niveau.traitement.existant...azote)) + 
  geom_bar(stat="identity", width=1),
ggplot(Boues, aes(x = "", y=1/n*100 , fill = Niveau.traitement.existant...phosphore)) + 
  geom_bar(stat="identity", width=1),
ggplot(Boues, aes(x = "", y=1/n*100 , fill = Niveau.traitement.existant...désinfection)) + 
  geom_bar(stat="identity", width=1),
nrow = 2
)
#sensibilité
plot_grid(
  ggplot(Boues, aes(x = "", y=1/n*100 , fill = Sensibilité.azote)) + 
    geom_bar(stat="identity", width=1),
  ggplot(Boues, aes(x = "", y=1/n*100 , fill = Sensibilité.phosphore)) + 
    geom_bar(stat="identity", width=1),
  nrow = 1
)

#NB D'EG
#filières eau et boue
plot_grid(
  ggplot(data = Boues) + geom_bar(aes(x = Filière.eau.principale, weight = 1/n*100)) + 
    theme(axis.text.x = element_text(angle = 75, hjust = 1)),
  ggplot(data = Boues) + geom_bar(aes(x = Filière.boues.principale, weight = 1/n*100)) + 
    theme(axis.text.x = element_text(angle = 75, hjust = 1)),
  nrow = 2
)
#traitement
plot_grid(
  ggplot(Boues, aes(x = "", y=Capacité.nominale.en.EH, fill = Niveau.traitement.existant...biologique)) + 
    geom_bar(stat="identity", width=1),
  ggplot(Boues, aes(x = "", y=Capacité.nominale.en.EH , fill = Niveau.traitement.existant...azote)) + 
    geom_bar(stat="identity", width=1),
  ggplot(Boues, aes(x = "", y=Capacité.nominale.en.EH , fill = Niveau.traitement.existant...phosphore)) + 
    geom_bar(stat="identity", width=1),
  ggplot(Boues, aes(x = "", y=Capacité.nominale.en.EH , fill = Niveau.traitement.existant...désinfection)) + 
    geom_bar(stat="identity", width=1),
  nrow = 2
)
#sensibilité
plot_grid(
  ggplot(Boues, aes(x = "", y=Capacité.nominale.en.EH , fill = Sensibilité.azote)) + 
    geom_bar(stat="identity", width=1),
  ggplot(Boues, aes(x = "", y=Capacité.nominale.en.EH , fill = Sensibilité.phosphore)) + 
    geom_bar(stat="identity", width=1),
  nrow = 1
)





#graphes boues
ggplot(data = Boues) + geom_density(aes(x = Prod.boues.sans.réactif..tMS.an.))






# Graphs zero sludge ------------------------------------------------------



#NB DE STEU
#filières eau et boue
plot_grid(
  ggplot(data = zero_sludge) + geom_bar(aes(x = Filière.eau.principale, weight = 1/n_zero_sudge*100)) + 
    theme(axis.text.x = element_text(angle = 75, hjust = 1)),
  ggplot(data = zero_sludge) + geom_bar(aes(x = Filière.boues.principale, weight = 1/n_zero_sudge*100)) + 
    theme(axis.text.x = element_text(angle = 75, hjust = 1)),
  nrow = 2
)
#traitement : most of unreported sludge do not treat N nor P
plot_grid(
  ggplot(zero_sludge, aes(x = "", y=1/n_zero_sudge*100 , fill = Niveau.traitement.existant...biologique)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=1/n_zero_sudge*100 , fill = Niveau.traitement.existant...azote)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=1/n_zero_sudge*100 , fill = Niveau.traitement.existant...phosphore)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=1/n_zero_sudge*100 , fill = Niveau.traitement.existant...désinfection)) + 
    geom_bar(stat="identity", width=1),
  nrow = 2
)
#sensibilité
plot_grid(
  ggplot(zero_sludge, aes(x = "", y=1/n_zero_sudge*100 , fill = Sensibilité.azote)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=1/n_zero_sudge*100 , fill = Sensibilité.phosphore)) + 
    geom_bar(stat="identity", width=1),
  nrow = 1
)

#NB D'EG
#filières eau et boue
plot_grid(
  ggplot(data = zero_sludge) + geom_bar(aes(x = Filière.eau.principale, weight = 1/n_zero_sudge*100)) + 
    theme(axis.text.x = element_text(angle = 75, hjust = 1)),
  ggplot(data = zero_sludge) + geom_bar(aes(x = Filière.boues.principale, weight = 1/n_zero_sudge*100)) + 
    theme(axis.text.x = element_text(angle = 75, hjust = 1)),
  nrow = 2
)
#traitement
plot_grid(
  ggplot(zero_sludge, aes(x = "", y=Capacité.nominale.en.EH, fill = Niveau.traitement.existant...biologique)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=Capacité.nominale.en.EH , fill = Niveau.traitement.existant...azote)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=Capacité.nominale.en.EH , fill = Niveau.traitement.existant...phosphore)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=Capacité.nominale.en.EH , fill = Niveau.traitement.existant...désinfection)) + 
    geom_bar(stat="identity", width=1),
  nrow = 2
)
#sensibilité
plot_grid(
  ggplot(zero_sludge, aes(x = "", y=Capacité.nominale.en.EH , fill = Sensibilité.azote)) + 
    geom_bar(stat="identity", width=1),
  ggplot(zero_sludge, aes(x = "", y=Capacité.nominale.en.EH , fill = Sensibilité.phosphore)) + 
    geom_bar(stat="identity", width=1),
  nrow = 1
)





#graphes boues
ggplot(data = Boues) + geom_density(aes(x = Prod.boues.sans.réactif..tMS.an.))



sum(Boues$Taille.agglomération..EH.)
sum(Boues$Somme.des.capacités.nominales..EH.)
sum(Boues$Capacité.nominale.en.EH)

# next --------------------------------------------------------------------







write.xlsx(x = data.frame(table(Boues$Sensibilité.azote)), file = "test.xlsx", sheetName = "SensiN")
a <- loadWorkbook("test.xlsx")
addWorksheet(a, sheetName = paste0("Boues",length(names(a))+1))
saveWorkbook(a,"test.xlsx",overwrite = TRUE)
write.xlsx(x = data.frame(table(Boues$Quantité.épandage.agricole..tMS.an.)), file = "test.xlsx", sheetName = "Boues")

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





# Carte -------------------------------------------------------------------
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


#tutorials raster and conversion
#https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
#https://www.r-bloggers.com/2014/05/converting-shapefiles-to-rasters-in-r/








