# Titre : VerifPrairieAnnee
# But : Verifier la diférence de prairie entre les années 
# Auteur : Emilie
# Date : 09/05/2023

rm(list=ls())
# Library  ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(ggplot2)
library(units)


# Chargement données  -----------------------------------------------------

Folderpath = paste("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees")
FolderCarto = "Carto"

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

#Surface des prairies
hist(Area_prairie_2019$area)
hist(Area_prairie_2020$area)
hist(Area_prairie_2021$area)

#Surace des prairie permanente/rotation longue 
prairie_total = bind_rows("2019" = Area_prairie_2019,
                          "2020" = Area_prairie_2020,
                          "2021" = Area_prairie_2021,
                          .id = "Annee")

ggplot(prairie_total)+
  aes(y = area, x = CODE_CULTU, fill = Annee)+ 
  geom_boxplot()


# Test --------------------------------------------------------------------

mod = 
