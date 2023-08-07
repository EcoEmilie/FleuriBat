# Titre : VerifPrairieAnnee
# But : Vérifier la différence de prairie entre les années 
# Auteur : Emilie
# Date : 09/05/2023

rm(list=ls())
# Library  ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(ggplot2)
library(units)
library(lme4)

# Chargement données  -----------------------------------------------------

Folderpath = paste("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees")
FolderCarto = "Carto"
FolderGraph = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/3.Sorties")

#2021

prairie_2021 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_RPG_2021.gpkg"))%>% 
  st_transform(2154) %>% 
  filter(CODE_CULTU == c("PPH","PRL","PTR"))%>% 
  mutate(CODE_CULTU = as.factor(CODE_CULTU))

Area_prairie_2021 = prairie_2021 %>% 
  mutate(area = st_area(geom))

area_prairie_2021 =  sum(Area_prairie_2021$area)

#2020
prairie_2020 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_RPG_2020.gpkg"))%>% 
  st_transform(2154) %>% 
  filter(CODE_CULTU == c("PPH","PRL","PTR"))%>% 
  mutate(CODE_CULTU = as.factor(CODE_CULTU))

Area_prairie_2020 = prairie_2020 %>% 
  mutate(area = st_area(geom))

area_prairie_2020 =  sum(Area_prairie_2020$area)
  
#2019
prairie_2019 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_RPG_2019.gpkg"))%>% 
  st_transform(2154) %>% 
  filter(CODE_CULTU == c("PPH","PRL","PTR")) %>% 
  mutate(CODE_CULTU = as.factor(CODE_CULTU))

Area_prairie_2019 = prairie_2019 %>% 
  mutate(area = st_area(geom))

area_prairie_2019 =  sum(Area_prairie_2019$area)

# Graphique ---------------------------------------------------------------

#Surace des prairie permanente/rotation longue 
prairie_total = bind_rows("2019" = Area_prairie_2019,
                          "2020" = Area_prairie_2020,
                          "2021" = Area_prairie_2021,
                          .id = "Annee")

GraphCultuAnnee_Prarie = ggplot(prairie_total)+
  aes(y = area, x = CODE_CULTU, fill = Annee)+ 
  geom_boxplot()

#Surface des prairies
GraphSurfAnnee_Prairie = ggplot(prairie_total)+
  aes(y = area, x = Annee, fill = Annee)+
  geom_boxplot()


# Test --------------------------------------------------------------------


# Sauvegarde --------------------------------------------------------------

ggsave(file.path(FolderGraph,"GraphCultuAnnee_Prarie.png"), plot = GraphCultuAnnee_Prarie)
ggsave(file.path(FolderGraph,"GraphSurfAnnee_Prairie.png"), plot = GraphSurfAnnee_Prairie)
