# Titre : FunctionRasterize  
# But : transformer en les raster en shapefile
# Auteur : Emilie PEN
# Date : 27/04/2023

library(raster)
library(rgdal)
# library(sf)
# library(landscapemetrics)
# library(landscapetools)
# library(tidyverse)
# library(usethis) 


RasterizeFunction = function(Shp, res, field){
  ZONE <- readOGR(Shp)
  ZRASTER <- raster(ext=extent( ZONE ),res=res,crs=Zproj, vals=NA) 
  R <- rasterize(ZONE, ZRASTER, field=ZONE$field)
}

RasterizeFunction(data_naturel, 10, Nature)


# setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto") # repertoire de travail
# dir.create("./RASTERS") # creer un dossier pour stocker les rasters finaux
# 
# # definir la projection (ici un exemple pour Lamber93) -> plus d'info et source ici : https://reseau-resste.mathnum.inrae.fr/reseau-resste/sites/default/files/2019-09/Atelier2016/Projection_Saby.pdf
# Zproj<-"+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"
# 
# # Recuperer un shp avec la zone d-etude élargie (par exemple, des buffers de 10km autour de tous les points étudiés)
# ZONE <- readOGR("data_occupation_haie_buffer.gpkg")
# #ZONE <- projection( Zproj )
# ZRASTER <- raster(ext=extent( ZONE ),res=10,crs=Zproj, vals=NA) # cree un raster sur la même etendue que la zone d-etude de resolution 20m
# #plot( ZRASTER )
# #plot( ZONE ,add=TRUE)
# 
# # Chargement des variables (exemple ici avec une variable bati)
# #raster_haie <- readOGR("data_occupation_haie_total.gpkg")
# #raster_haie$CODE_GROUP = as.factor(RPG_2019$CODE_GROUP)
# #projection(BATI) <- projection(Zproj)
# #raster_haie$F <- 1 # F est un code pour identifier l'occupation du sol (si qu'un seul type d'occupation du sol dans le shp)
# ZONE$F <- 1
# R <- rasterize(ZONE, ZRASTER, field=ZONE$F)
# writeRaster(R,paste("./RASTERS/","raster_haie",".tif",sep=""),overwrite=TRUE)
# 
# show_landscape(R, discrete = TRUE)




