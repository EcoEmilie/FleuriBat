# Titre : ShpDivEau
# But : Script pour compliler les données des élements aquatiques en shp 
# Auteur : Emilie
# Date : 09/05/2023


# Library  ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)


# Chargement data ---------------------------------------------------------

Folderpath = paste("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees")
FolderCarto = "Carto"



## Plan d'eau  -------------------------------------------------------------

data_plan_eau = st_read(dsn = file.path(Folderpath,FolderCarto,"data_plan_eau_total.gpkg")) %>% 
  st_transform(2154)

## Cours d'eau  -------------------------------------------------------------

data_cours_eau = st_read(dsn = file.path(Folderpath,FolderCarto,"data_cours_eau_total.gpkg")) %>% 
  st_transform(2154)

## Éléments aquatiques  -------------------------------------------

#Shapefile 
data_eau = bind_rows("Cours_eau" = data_cours_eau,
                         "Plan_eau" = data_plan_eau,
                         .id = "Nature") %>% 
  dplyr::select(Nature,geom)


# Ecriture ----------------------------------------------------------------

#GPKG
st_write(data_eau, dsn = file.path(Folderpath, FolderCarto, "data_eau.gpkg"),driver = "GPKG", append = FALSE)

#RDS
saveRDS(data_eau, file = file.path(Folderpath, FolderCarto, "data_eau.rds"))


