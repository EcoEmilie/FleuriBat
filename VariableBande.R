# Titre : VariableBande
# But : Modèle GLM sur l'effet de sbandes sur les chauves souris 
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
  mutate(area = st_area(geometry),
         perimeter = st_perimeter(geometry)) %>% 
  st_cast("POINT") %>% 
  st_cast("MULTILINESTRING")

tmap_mode("view")
lol = tm_shape(data_bande)+
  tm_dots(col="red")#fonction pour afficher les points 

lol
tmap_mode("plot")


