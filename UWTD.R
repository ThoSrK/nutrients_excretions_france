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

Agglomerations <- read.csv("UWTD/Agglomerations.csv")
library(maps)
france <- map_data( "france")
#saving the map of the sites and climates
ggplot() +
  geom_polygon(data = france, 
               aes(x= long, y = lat, group = group), 
               fill= NA, colour = "black", size = .3) +
  geom_point(data = Agglomerations,
             aes(y= as.numeric(aggLatitude), 
                 x = as.numeric(aggLongitude)), size=.01) +
  coord_fixed(1.3) + theme_classic() + 
  ylim(35, 70) + xlim(-10, 37) +
  theme(legend.position = "bottom")


Art17_FLAAgglomeration <- read.csv("UWTD/Art17_FLAAgglomeration.csv")
Art17_FLAUWWTP <- read.csv("UWTD/Art17_FLAUWWTP.csv")


# Removal efficiency ------------------------------------------------------
#preparing data
UWWTPS_emission_load <- read.csv("UWTD/UWWTPS_emission_load.csv")
UWWTPS_emission_load <- read.csv("UWTD/UWWTPS_emission_load.csv") %>%
  mutate(uwwNIncoming = as.numeric(uwwNIncoming),
         uwwNDischarge = as.numeric(uwwNDischarge),
         uwwPIncoming = as.numeric(uwwPIncoming),
         uwwPDischarge = as.numeric(uwwPDischarge),
         uwwBODIncoming = as.numeric(uwwBODIncoming),
         uwwBODDischarge = as.numeric(uwwBODDischarge),
         uwwCODIncoming = as.numeric(uwwCODIncoming),
         uwwCODDischarge = as.numeric(uwwCODDischarge)
         )

plot_grid(
  
  #N
  ggplot(data = UWWTPS_emission_load) + 
    geom_point(aes(x = uwwNIncoming, y =1-uwwNDischarge/uwwNIncoming)) + 
    ylim(c(0,1)) + scale_x_log10() + xlab("N in (t/y)") + ylab("% N removed"),
  ggplot(data = UWWTPS_emission_load) + 
    geom_point(aes(x = uwwNDischarge, y =1-uwwNDischarge/uwwNIncoming)) + 
    ylim(c(0,1)) + scale_x_log10() + xlab("N out (t/y)") + ylab("% N removed"),
  ggplot(data = UWWTPS_emission_load %>% #density
           filter(uwwNDischarge/uwwNIncoming <=1)) +
    geom_density(aes(x = 1-uwwNDischarge/uwwNIncoming)) + xlab("N removal efficiency"),
  
  #P
  ggplot(data = UWWTPS_emission_load) + 
    geom_point(aes(x = uwwPIncoming, y =1-uwwPDischarge/uwwPIncoming)) + 
    ylim(c(0,1)) + scale_x_log10() + xlab("P in (t/y)") + ylab("% P removed"),
  ggplot(data = UWWTPS_emission_load) + 
    geom_point(aes(x = uwwPDischarge, y =1-uwwPDischarge/uwwPIncoming)) + 
    ylim(c(0,1)) + scale_x_log10() + xlab("P out (t/y)") + ylab("% P removed"),
  ggplot(data = UWWTPS_emission_load %>% #density
           filter(uwwPDischarge/uwwPIncoming <=1)) +
    geom_density(aes(x = 1-uwwPDischarge/uwwPIncoming)) + xlab("P removal efficiency"),
  
  #BOD
  ggplot(data = UWWTPS_emission_load %>% #density
           filter(uwwBODDischarge/uwwBODIncoming <=1)) +
    geom_density(aes(x = 1-uwwBODDischarge/uwwBODIncoming)) + xlab("BOD removal efficiency"),
  
  ncol = 3
)


plot_grid(
  
  ggplot(data = UWWTPS_emission_load ) + 
    geom_point(aes(x = rank(-uwwPIncoming, ties.method = "first"), y = uwwPIncoming)) + 
    scale_x_log10() +  scale_y_log10() + xlab("rank") + ylab("P in"),
  ggplot(data = UWWTPS_emission_load ) + 
    geom_point(aes(x = rank(-uwwPDischarge, ties.method = "first"), y = uwwPDischarge)) + 
    scale_x_log10() +  scale_y_log10() + xlab("rank") + ylab("P out"),
  ggplot(data = UWWTPS_emission_load ) + 
    geom_point(aes(x = rank(-(1-uwwPDischarge/uwwPIncoming), ties.method = "first"), y = 1-uwwPDischarge/uwwPIncoming)) + 
    scale_x_log10() +  scale_y_log10() + xlab("rank") + ylab("% P removed"),
  
  ggplot(data = UWWTPS_emission_load ) + 
    geom_point(aes(x = rank(-uwwNIncoming, ties.method = "first"), y = uwwNIncoming)) + 
    scale_x_log10() +  scale_y_log10() + xlab("rank") + ylab("N in"),
  ggplot(data = UWWTPS_emission_load ) + 
    geom_point(aes(x = rank(-uwwNDischarge, ties.method = "first"), y = uwwNDischarge)) + 
    scale_x_log10() +  scale_y_log10() + xlab("rank") + ylab("N out"),
  
  ncol = 2
)

# vérifier le % de résultats aberrants avec efficacité >1
# P removed sorted
ggplot(data = UWWTPS_emission_load %>% 
         filter(uwwPDischarge/uwwPIncoming <=1) %>%
         filter(uwwPDischarge !="" )) + 
  geom_point(aes(x = rank(-(1-uwwPDischarge/uwwPIncoming), ties.method = "first"), y = 1-uwwPDischarge/uwwPIncoming)) + 
  xlab("rank") + ylab("% P removed")
# N removed sorted
ggplot(data = UWWTPS_emission_load %>% 
         filter(uwwNDischarge/uwwNIncoming <=1) %>%
         filter(uwwNDischarge !="" )) + 
  geom_point(aes(x = rank(-(1-uwwNDischarge/uwwNIncoming), ties.method = "first"), y = 1-uwwNDischarge/uwwNIncoming)) + 
  xlab("rank") + ylab("% N removed")
# % N removed vs % P removed 
ggplot(data = UWWTPS_emission_load %>% 
         filter(uwwNDischarge/uwwNIncoming <=1) %>%
         filter(uwwPDischarge/uwwPIncoming <=1) %>%
         filter(uwwNDischarge !="" ) %>%
         filter(uwwPDischarge !="" )) + 
  geom_point(aes(x = 1-uwwNDischarge/uwwNIncoming, y = 1-uwwPDischarge/uwwPIncoming)) + 
  xlab("% N removed") + ylab("% P removed")







#détail sur N reporté
nrow(UWWTPS_emission_load) #nb rows in database
nrow(UWWTPS_emission_load %>% filter(uwwNDischarge != "")) #nb N out
nrow(UWWTPS_emission_load %>% filter(uwwNIncoming != "")) #nb N in 
nrow(UWWTPS_emission_load %>% filter(uwwNDischarge/uwwNIncoming >1)) #nb incohérents out/in > 1
nrow(UWWTPS_emission_load %>% filter(uwwNDischarge/uwwNIncoming >1)) / 
  nrow(UWWTPS_emission_load %>% filter(uwwNIncoming != "")) *100 # % incohérents
#détail sur P reporté
nrow(UWWTPS_emission_load) #nb rows in database
nrow(UWWTPS_emission_load %>% filter(uwwPDischarge != "")) #nb P out
nrow(UWWTPS_emission_load %>% filter(uwwPIncoming != "")) #nb P in 
nrow(UWWTPS_emission_load %>% filter(uwwPDischarge/uwwPIncoming >1)) #nb incohérents out/in > 1
nrow(UWWTPS_emission_load %>% filter(uwwPDischarge/uwwPIncoming >1)) / 
  nrow(UWWTPS_emission_load %>% filter(uwwPIncoming != "")) *100 # % incohérents
#détail sur BOD reporté
nrow(UWWTPS_emission_load) #nb rows in database
nrow(UWWTPS_emission_load %>% filter(uwwBODDischarge != "")) #nb P out
nrow(UWWTPS_emission_load %>% filter(uwwBODIncoming != "")) #nb P in 
nrow(UWWTPS_emission_load %>% filter(uwwBODDischarge/uwwBODIncoming >1)) #nb incohérents out/in > 1
nrow(UWWTPS_emission_load %>% filter(uwwBODDischarge/uwwBODIncoming >1)) / 
  nrow(UWWTPS_emission_load %>% filter(uwwBODIncoming != "")) *100 # % incohérents







