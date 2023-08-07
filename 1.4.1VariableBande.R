# Titre : VariableBande
# But : Chargement des données sur les bandes et calcule de l'aire + mise en forme de la variable commune 
# Auteur : Emilie
# Date : 11.05.2023

rm(list=ls())


# Library -----------------------------------------------------------------

library(sf)
library(tidyverse)


# Données -----------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderSource = "1.Donnees_sources"
FolderCarto = "Cartographie"
FolderBande = "Bandes_fleuries"
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties" 

data_bande = st_read(dsn = file.path(FolderDonnees,FolderSource,FolderCarto,FolderBande, "bandes_fleuries_modif.shp")) %>% 
  st_transform(2154) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  rename(Commune = Village) %>% 
  mutate(Commune = str_to_upper(Commune)) %>% 
  mutate(Commune = str_replace(Commune,"-","_"))

saveRDS(data_bande,file.path(FolderDonnees,FolderInter,"data_bande.rds"))
hist(sqrt(data_bande$area))
