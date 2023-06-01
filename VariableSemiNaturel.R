# Titre : VariableSemiNaturel
# But : Création nombre de classe d'élement semi naturel et pourcentage d'occupation 
# Auteur : Emilie
# Date : 27/04/2023

rm(list=ls())


# Library -----------------------------------------------------------------

#Ranger les données 
library(dplyr)
library(tidyr)
library(lubridate)#manipuler les dates et les heures
library(stringr)#manipuler les characteres 


#_Cartographie 
library(sf)#opération en SIG
library(tmap)#faire des cartes
library(leaflet)#afficher les cartes de manière interactive 
library(tmaptools)#outils pour les couleurs
library(mapview)#afficher les données sur une carte
library(terra)#gestion de raster similaire au package `raster`
library(landscapemetrics)#calcule de variable paysagère
library(landscapetools)
library(lwgeom) #Calcul des perimetres 


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_naturel = readRDS(file.path(FolderDonnees, FolderInter,"data_naturel.rds"))%>% #au départ en WGS 84
  st_transform(2154) %>% 
  dplyr::select(Nature, geom) %>% 
  distinct()

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds"))%>% 
  #slice_sample(n = 10) %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% #au départ en WGS 84
  st_transform(2154) 


# Varirables --------------------------------------------------------------

data_nat = data.frame() 
buffer = c(100,500,1000,2000)

for (i in 1:nrow(data_site)){
  names_year = data_site[i,c("carre_year_pass","year","Modalite_protocole", "geometry")]

  for (j in 1:length(buffer)){
    
    ## !!! Calcul du buffer + de son aire----
    dist_buffer = buffer[j]
    b = st_buffer(names_year, dist = buffer[j]) 
    b_v = vect(b)
    buffer_area = st_area(b)
    
    ##Surface naturel----
    a = st_intersection(data_naturel , b)%>% 
      mutate(area = (st_area(geom)* 100)/buffer_area) %>% 
      distinct() 
      
    area_naturel = c(ifelse(nrow(a) == 0, 0, sum(a$area)))
    
    ##Nombre de classe ----
    c = a %>% 
      group_by(Nature) %>% 
      tally()
    
    nb_naturel = c(ifelse(nrow(c) == 0, 0, nrow(c)))
    
    ##Collage vecteur 
    vecteur_var = cbind(b,
                        dist_buffer, 
                        buffer_area,
                        area_naturel, 
                        nb_naturel)
    
    ##Collage data
    data_nat = rbind(data_nat,vecteur_var)
  }
  print(i)
}


saveRDS(data_nat,file.path(FolderDonnees,FolderInter,"data_varpaysage.rds"))






