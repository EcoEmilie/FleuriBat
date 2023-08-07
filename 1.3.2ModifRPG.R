# Titre : ModifRPG 
# But : Charger les RPG selon les années et les régions, les fusionner et faire une buffer de 4000m autours des sites pour alléger 
# Auteur : Emilie PEN
# Date : 10/04/2023

rm(list=ls())

# Library  ----------------------------------------------------------------

library(sf)
library(tidyverse)


# Chemin  -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

FolderCarto = paste("1.Donnees_sources/Cartographie/RPG")
RPG2021 = "RPG_2021"
RPG2020 = "RPG_2020"
RPG2019 = "RPG_2019"
IDF = "IDF"
CVL = "Centre_Val_de_Loire"
N = "Normandie"

FolderFinal = paste("2.Donnees_intermediaire")
FolderSite = paste("2.Donnees_intermediaire")

#Site
data_site_sf = readRDS(file.path(FolderDonnees, FolderInter,"data_site.rds")) %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% #au départ en WGS 84
  st_transform(2154) 

# Chargement des données 2021 ---------------------------------------------

data_RPG_2021_IDF = st_read(file.path(FolderDonnees,FolderCarto,RPG2021,"IDF/IDF_ajustee.shp")) %>% 
  st_transform(2154)

data_RPG_2021_CVL = st_read(file.path(FolderDonnees,FolderCarto,RPG2021,"Centre_Val_de_Loire/CVL_ajustee.shp")) %>% 
  st_transform(2154)

data_RPG_2021_N= st_read(file.path(FolderDonnees,FolderCarto,RPG2021,"Normandie/N_ajustee.shp")) %>% 
  st_transform(2154)
# 
# data_RPG_1 = st_difference(data_RPG_2021_IDF,data_RPG_2021_CVL)#IDF est découpé selon CVL
# data_RPG_2 = st_as_sf(rbind(data_RPG_2021_CVL,data_RPG_2021_IDF)) #Fusion d'IDF avec CVL
# 
# data_RPG_3 = st_difference(data_RPG_2, data_RPG_2021_N)#decoupage selon N

data_RPG_2021 = st_as_sf(rbind(data_RPG_2021_CVL,data_RPG_2021_IDF, data_RPG_2021_N))#fusion total


data_site_sf_2021 =  data_site_sf %>% 
  filter(year==2021) %>% 
  select(geometry)

data_RPG_2021_B = st_intersection(data_RPG_2021, st_buffer(data_site_sf_2021, dist = 4000)) 

st_write(data_RPG_2021_B,file.path(FolderDonnees,FolderInter,"donnees_RPG_2021.gpkg"), driver = "GPKG", append = FALSE)
saveRDS(data_RPG_2021_B,file.path(FolderDonnees,FolderInter, "donnees_RPG_2021.rds"))

# Chargement données RPG 2020 --------------------------------------------------

data_RPG_2020_IDF = st_read(file.path(FolderDonnees,FolderCarto,RPG2020,"Ile_de_France/PARCELLES_GRAPHIQUES.shp")) %>% 
  st_transform(2154)

data_RPG_2020_CVL = st_read(file.path(FolderDonnees,FolderCarto,RPG2020,"Centre_Val_de_Loire/PARCELLES_GRAPHIQUES.shp")) %>% 
  st_transform(2154)

data_RPG_2020_N= st_read(file.path(FolderDonnees,FolderCarto,RPG2020,"Normandie/PARCELLES_GRAPHIQUES.shp")) %>% 
  st_transform(2154)

data_RPG_2020 = st_as_sf(rbind(data_RPG_2020_CVL,data_RPG_2020_IDF, data_RPG_2020_N))

data_site_sf_2020 = data_site_sf %>% 
  filter(year==2020) %>% 
  select(geometry)

data_RPG_2020_B = st_intersection(data_RPG_2020, st_buffer(data_site_sf_2020, dist = 4000)) 

st_write(data_RPG_2020_B,file.path(FolderDonnees,FolderInter,"donnees_RPG_2020.gpkg"), driver = "GPKG", append = FALSE)
saveRDS(data_RPG_2020_B,file.path(FolderDonnees,FolderInter,"donnees_RPG_2020.rds"))

# Chargement données RPG 2019 --------------------------------------------------

data_RPG_2019_IDF = st_read(file.path(FolderDonnees,FolderCarto,RPG2019,"IDF/IDF_ajustee.shp")) %>% 
  st_transform(2154)

data_RPG_2019_CVL = st_read(file.path(FolderDonnees,FolderCarto,RPG2019,"Centre_Val_de_Loire/CVL_ajustee.shp")) %>% 
  st_transform(2154)

data_RPG_2019_N= st_read(file.path(FolderDonnees,FolderCarto,RPG2019,"Normandie/N_ajustee.shp")) %>% 
  st_transform(2154)

data_RPG_2019 = st_as_sf(rbind(data_RPG_2019_CVL,data_RPG_2019_IDF, data_RPG_2019_N))

data_site_sf_2019 = data_site_sf %>% 
  filter(year==2019) %>% 
  select(geometry)

data_RPG_2019_B = st_intersection(data_RPG_2019, st_buffer(data_site_sf_2019, dist = 4000)) 
st_write(data_RPG_2019_B,file.path(FolderDonnees,FolderInter,"donnees_RPG_2019.gpkg"), driver = "GPKG", append = FALSE)
saveRDS(data_RPG_2019_B,file.path(FolderDonnees,FolderInter,"donnees_RPG_2019.rds"))
