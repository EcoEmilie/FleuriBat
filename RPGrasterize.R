# Titre : RPGRasterize 
# But : Resterisation des carte RPG
# Auteur : Emilie PEN
# Date : 15/05/2023

rm(list=ls())
# Library  ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(rgdal)


# Chemin ------------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/1.Donnees_sources/Cartographie/RPG")
  
FolderFinal = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")


# RPG 2019 ----------------------------------------------------------------

x = file.path(FolderDonnees, "donnees_RPG_2019.gpkg")

ZONE <- readOGR(x)
ZONE$CODE_CULTU = as.factor(ZONE$CODE_CULTU)
Zproj<-"+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
ZRASTER <- raster(ext=extent( ZONE),res=10,crs=Zproj, vals=NA)
R <- rasterize(ZONE, ZRASTER, field=ZONE$CODE_CULTU)

writeRaster(R,file.path(FolderFinal, "donnees_RPG_2019.tif"),overwrite=TRUE)
saveRDS(R,file.path(FolderFinal, "donnees_RPG_2019_raster.rds"))

# RPG 2020 ----------------------------------------------------------------

x = file.path(FolderDonnees, "donnees_RPG_2020.gpkg")

ZONE <- readOGR(x)
ZONE$CODE_CULTU = as.factor(ZONE$CODE_CULTU)
Zproj<-"+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
ZRASTER <- raster(ext=extent( ZONE),res=10,crs=Zproj, vals=NA)
R <- rasterize(ZONE, ZRASTER, field=ZONE$CODE_CULTU)

writeRaster(R,file.path(FolderFinal, "donnees_RPG_2020.tif"),overwrite=TRUE)
saveRDS(R,file.path(FolderFinal, "donnees_RPG_2020_raster.rds"))

# RPG 2021 ----------------------------------------------------------------

x = file.path(FolderDonnees, "donnees_RPG_2021.gpkg")

ZONE <- readOGR(x)
ZONE$CODE_CULTU = as.factor(ZONE$CODE_CULTU)
Zproj<-"+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
ZRASTER <- raster(ext=extent( ZONE),res=10,crs=Zproj, vals=NA)
R <- rasterize(ZONE, ZRASTER, field=ZONE$CODE_CULTU)

writeRaster(R,file.path(FolderFinal, "donnees_RPG_2021.tif"),overwrite=TRUE)
saveRDS(R,file.path(FolderFinal, "donnees_RPG_2021_raster.rds"))


