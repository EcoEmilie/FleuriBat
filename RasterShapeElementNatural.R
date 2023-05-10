# Titre : RasterShpDivElementNaturel
# But : Script pour compliler les données des élements naturel en shp et en raster
# Auteur : Emilie
# Date : 27/04/2023

rm(list=ls())
# Library  ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(purrr)


# Function ----------------------------------------------------------------

source("~/Documents sur ordi/GitHub/FleuriBat/FunctionRasterize.R")

# Chargement data ---------------------------------------------------------

Folderpath = paste("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees")
FolderCarto = "Carto"
FolderRaster = "RASTERS"
FolderSite = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")
FolderRDS = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")


# Site --------------------------------------------------------------------
data_site = readRDS(file = file.path(FolderSite,"data_site.rds")) %>% 
  mutate(X = as.numeric(X),
         Y = as.numeric(Y)) %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% #au départ en WGS 84
  st_transform(2154) 

## Cours d'eau  -------------------------------------------------------------

data_cours_eau = st_read(dsn = file.path(Folderpath,FolderCarto,"data_cours_eau_total.gpkg")) %>% 
  st_transform(2154) %>% 
  dplyr :: select(geom)%>% 
  st_buffer(dist = 1)

## Plan d'eau  -------------------------------------------------------------

data_plan_eau = st_read(dsn = file.path(Folderpath,FolderCarto,"data_plan_eau_total.gpkg")) %>% 
  st_transform(2154)%>% 
  dplyr :: select(geom)

## Forêt  -------------------------------------------------------------

data_foret = st_read(dsn = file.path(Folderpath,FolderCarto,"data_occupation_foret_total.gpkg")) %>% 
  st_transform(2154)

#Feuillu
data_foret_feuillu = data_foret %>%
  filter(str_detect(nature,"feuillus"))%>% 
  dplyr :: select(geom)


#Conifère
data_foret_conifere = data_foret %>%
  filter(str_detect(nature,"conifères"))%>% 
  dplyr :: select(geom)


## Ripisylve  -------------------------------------------------------------

# data_cours_eau = st_read(dsn = file.path(Folderpath,FolderCarto,"data_cours_eau_total.gpkg")) %>% 
#   st_transform(2154)
# 
# data_ripisylve = data_cours_eau %>% 
#   st_buffer(dist = 2)

## Haie  -------------------------------------------------------------

data_haie = st_read(dsn = file.path(Folderpath,FolderCarto,"data_occupation_haie_total.gpkg")) %>% 
  st_transform(2154) %>% 
  dplyr :: select(geom) %>% 
  st_buffer(dist = 1)

## Prairie temporaire ----------------------------------------------------

prairie_2021 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_RPG_2021.gpkg"))%>% 
  st_transform(2154) 

prairie_temp = prairie_2021%>% 
  filter(CODE_CULTU == "PTR")%>% 
  dplyr :: select(geom)

prarie_perm = prairie_2021 %>% 
  filter(CODE_CULTU == c("PPH","PRL"))%>% 
  dplyr :: select(geom)

## Élément semi-naturel/naturel  -------------------------------------------


#Shapefile 
data_naturel = bind_rows("Feuillu" = data_foret_feuillu,
                         "Conifere" = data_foret_conifere,
                         "Haie" = data_haie,
                         #"Ripisylve" = data_ripisylve,
                         "prairie_temp" = prairie_temp,
                         "prairie_perm" = prarie_perm,
                         "Plan_eau" = data_plan_eau,
                         "Cours_eau" = data_cours_eau,
                         .id = "Nature") 

data_naturel1 =   st_intersection(data_naturel, st_buffer(data_site, dist = 4000)) %>% 
  mutate(Nature = as.factor(Nature))
  

# Ecriture ----------------------------------------------------------------

#GPKG
st_write(data_naturel1, dsn = file.path(Folderpath, FolderCarto, "data_naturel.gpkg"),driver = "GPKG", append = FALSE)

#RDS
saveRDS(data_naturel1, file = file.path(FolderRDS, "data_naturel.rds"))

# Raster 
shp = file.path(Folderpath,FolderCarto,"data_naturel.gpkg")
R = RasterizeFunction(shp, 5, Nature)

writeRaster(R,file = file.path(Folderpath, FolderCarto, FolderRaster, "data_naturel.tif"),overwrite=TRUE)
#RDS
saveRDS(R, file = file.path(FolderRDS, "data_naturel_raster.rds"))
