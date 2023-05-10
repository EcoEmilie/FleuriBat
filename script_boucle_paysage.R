rm(list=ls())

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


# Chargement des données --------------------------------------------------


## Dossier  ----------------------------------------------------------------

Folderpath = paste("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees")
FolderChiro = "Chiropteres"
FolderCarto = "Carto"
FolderRDS = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")

## Données sites -----------------------------------------------------------

data_site= readRDS(file = file.path(FolderRDS, "data_site.rds")) %>% #A changer avec données site
  mutate(Commune = str_to_upper(Commune), year = as.factor(year)) %>% 
  mutate(Commune = str_replace_all(Commune,"_"," ")) %>% 
  unite(Mod_pass, Modalite_protocole, Num_passag, sep = "_", remove = FALSE)%>% 
  #slice_sample(n = 10) %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% #au départ en WGS 84
  st_transform(2154) 

## Carte -------------------------------------------------------------------

### Zone urbaine  -------------------------------------------------------------

data_habitation = st_read(dsn = file.path(Folderpath,FolderCarto,"data_occupation_bati_total.gpkg")) %>% 
  st_transform(2154)

dist_habitation = c() 

### Forêt  -------------------------------------------------------------

data_foret = st_read(dsn = file.path(Folderpath,FolderCarto,"data_occupation_foret_total.gpkg")) %>% 
  st_transform(2154)
dist_foret = c() 

# #Feuillu
# data_foret_feuillu = data_foret %>% 
#   filter(str_detect(nature,"feuillus"))
# 
# #Conifère
# data_foret_conifere = data_foret %>% 
#   filter(str_detect(nature,"conifères"))

### Éléments aquatiques  -------------------------------------------------------------

data_eau = readRDS(file.path(Folderpath,FolderCarto,"data_eau.rds"))

dist_eau =c()#Création d'un vecteur pour accueillir la variable 

# ### Ripisylve  -------------------------------------------------------------
# 
# data_ripisylve = data_cours_eau %>% 
#   st_buffer(dist = 2)
# 
# dist_ripisylve = c()
# area_ripi = c()

### Haie  -------------------------------------------------------------

data_haie = st_read(dsn = file.path(Folderpath,FolderCarto,"data_occupation_haie_total.gpkg"))

data_lisiere = bind_rows("foret" = st_cast(data_foret, "MULTILINESTRING"),
                         "haie" = st_cast(data_haie,"MULTILINESTRING" ),
                         .id = "Data") 

# raster_haie = rast("raster_haie.tif")

density_haie= c()
lisiere_density= c()

### Route -------------------------------------------------------------

data_route = st_read(dsn = file.path(Folderpath,FolderCarto,"data_route_total.gpkg"))

### Bande -------------------------------------------------------------

data_bande = st_read(dsn = file.path(Folderpath,FolderCarto,"bandes_fleuries_modif.shp")) 
st_crs(data_bande)= 2154


### RPG ----------------------------------------------------

#2019 
RPG_2019 = readRDS(file.path(FolderRDS,"donnees_RPG_2019.rds")) %>% 
  st_transform(2154)

RPG_cultu_2019 = RPG_2019 %>% 
  filter(!CODE_CULTU == c("11","17","18", "28"))

#2020 
RPG_2020 = readRDS(file.path(FolderRDS,"donnees_RPG_2020.rds"))%>% 
  st_transform(2154)

RPG_cultu_2020 = RPG_2020%>% 
  filter(!CODE_CULTU == c("11","17","18", "28"))

#2021
RPG_2021 = readRDS(file.path(FolderRDS,"donnees_RPG_2021.rds"))%>% 
  st_transform(2154)

RPG_cultu_2021 = RPG_2021%>% 
  filter(!CODE_CULTU == c("11","17","18", "28"))

nb_parcelle= c()

##Diversité raster
#2019
Div_2019 = rast( file.path(Folderpath,FolderCarto,"RASTERS/RPG_2019_DIV.tif"))

#2020 
Div_2020 = rast(file.path(Folderpath,FolderCarto,"RASTERS/RPG_2020_DIV.tif"))

#2021 
Div_2021 = rast(file.path(Folderpath,FolderCarto,"RASTERS/RPG_2021_DIV.tif"))

i = data_frame()

### Prairie ----
#2019 
prairie_2019 = RPG_2019 %>% 
  filter(CODE_CULTU == c("PPH","PRL","PTR"))

#2020 
prairie_2020 = RPG_2020 %>% 
  filter(CODE_CULTU == c("PPH","PRL","PTR"))

#2021 
prairie_2021 = RPG_2021 %>% 
  filter(CODE_CULTU == c("PPH","PRL","PTR"))

#prairie permanente
#2019 
praiperm_2019 = RPG_2019 %>% 
  filter(CODE_CULTU == c("PPH","PRL"))

#2020 
praiperm_2020 = RPG_2020 %>% 
  filter(CODE_CULTU == c("PPH","PRL"))

#2021 
praiperm_2021 = RPG_2021 %>% 
  filter(CODE_CULTU == c("PPH","PRL"))

#prairie temporaire
#2019 
praitemp_2019 = RPG_2019 %>% 
  filter(CODE_CULTU == c("PTR"))

#2020 
praitemp_2020 = RPG_2020 %>% 
  filter(CODE_CULTU == c("PTR"))

#2021 
praitemp_2021 = RPG_2021 %>% 
  filter(CODE_CULTU == c("PTR"))




### RPG BIO -----------------------------------------------------------------

#2019
BIO_2019 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_BIO_2019.gpkg"))%>% 
  st_transform(2154)

#2020
BIO_2020 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_BIO_2020.gpkg"))%>% 
  st_transform(2154)

#2021 
BIO_2021 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_BIO_2021.gpkg"))%>% 
  st_transform(2154)

### Élément semi-naturel/naturel  -------------------------------------------

data_naturel = rast(file.path(Folderpath,FolderCarto,"RASTERS/data_naturel.tif"))


# Calcule des variables paysagères  ---------------------------------------

#Départ
data_pay = data.frame() 
buffer = c(100,500,1000,2000)




#selection de la ligne 
for (i in 1:nrow(data_site)){
  names_year = data_site[i,c("Carre_Point_vigiechiro","year","Modalite_protocole", "geometry")]
  
  ##Distance aux zones urbaines ----
  dist_habitation = st_distance(names_year, data_habitation, by_element = FALSE) %>% 
    min()
  
  ##Distance à la forêt ----
  dist_foret = st_distance(names_year, data_foret, by_element = FALSE) %>% 
    min()
  
  ##Distance à l'eau ----
  dist_eau = st_distance(names_year, data_eau, by_element = FALSE) %>% 
    min()
  
  ##Distance à la haie ----
  dist_haie = st_distance(names_year, data_haie, by_element = FALSE) %>% 
    min()
  
  ##Surface bande ----
  a = if(names_year$Modalite_protocole == "bande"){
    st_intersection(data_bande,st_buffer(names_year,dist = 100))
  }else{data.frame()}
  bande_area = c(ifelse(nrow(a) == 0, 0,st_area(a$geom)))
  
  for (j in 1:length(buffer)){
    
    ## !!! Calcul du buffer + de son aire----
    dist_buffer = buffer[j]
    b = st_buffer(names_year, dist = buffer[j]) 
    b_v = vect(b)
    buffer_area = st_area(b)
    
    ##Surace de forêt total----
    a = st_intersection(data_foret , b)%>% 
      mutate(area = (st_area(geom)* 100)/buffer_area) %>% 
      distinct() 
    area_foret = c(ifelse(nrow(a) == 0, 0, sum(a$area)))
    
    ##Surface zone urbaine ----
    a = st_intersection(data_habitation , b) %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct() 
    area_habitation = c(ifelse(nrow(a) == 0, 0, sum(a$area)))
    
    ##Surface de BIO----
    
    a = if(b$year == 2019){st_intersection(BIO_2019 , b)}else if(b$year==2020){st_intersection(BIO_2020 , b)} else{st_intersection(BIO_2021 , b)}
    a = a  %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area) %>% 
      distinct()
      
    area_BIO = c(ifelse(nrow(a) == 0, 0,sum(a$area)))

    ##Diversité de culture ----
  
    a = if(b$year == 2019){mask(crop(Div_2019, b_v),b_v)}else if(b$year==2020){mask(crop(Div_2020, b_v),b_v)} else{mask(crop(Div_2021, b_v),b_v)} 
    Shannon_cultu = lsm_l_shdi(a)
    
    ## Surface/moyenne/perimetre agricole ----
    
    a = if(b$year == 2019){st_intersection(RPG_2019 , b)}else if(b$year==2020){st_intersection(RPG_2020 , b)} else{st_intersection(RPG_2021 , b)}
    a = a  %>% 
      mutate(area = st_area(geometry), perimeter = st_perimeter(geometry))%>% 
      distinct() %>% 
      add_tally(name = "n")
    
    moy_area_agri = c(ifelse(nrow(a) == 0, 0, mean(a$area * 10000)))
    perimeter_agri = c(ifelse(nrow(a) == 0, 0, mean(a$perimeter)))
    nb_parcelle =  c(ifelse(nrow(a) == 0, 0, a$n[1]))
    
    ## Surface culture ----
    a = if(b$year == 2019){st_intersection(RPG_cultu_2019 , b)}else if(b$year==2020){st_intersection(RPG_cultu_2020 , b)} else{st_intersection(RPG_cultu_2021 , b)}
    a = a  %>% 
      mutate(area = st_area(geometry))%>% 
      distinct() 
    
    area_agri = c(ifelse(nrow(a) == 0, 0, (sum(a$area) * 100)/buffer_area))
    
    ## Surface de prairie total ----
    
    a = if(b$year == 2019){st_intersection(prairie_2019 , b)}else if(b$year==2020){st_intersection(prairie_2020 , b)} else{st_intersection(prairie_2021 , b)}
    a = a  %>% 
      mutate(area = (st_area(geometry)* 100)/buffer_area)%>% 
      distinct()
      area_prairie = c(ifelse(nrow(a) == 0, 0, sum(a$area)))
    
    ## Surface de prairie permanente ----
    
    a = if(b$year == 2019){st_intersection(praiperm_2019 , b)}else if(b$year==2020){st_intersection(praiperm_2020 , b)} else{st_intersection(praiperm_2021 , b)}
    a = a  %>% 
      mutate(area = (st_area(geometry)* 100)/buffer_area)%>% 
      distinct()
      area_praiperm = c(ifelse(nrow(a) == 0, 0, sum(a$area)))
    
    ## Surface de prairie temporaire ----
    
    a = if(b$year == 2019){st_intersection(praitemp_2019 , b)}else if(b$year==2020){st_intersection(praitemp_2020 , b)} else{st_intersection(praitemp_2021 , b)}
    a = a  %>% 
      mutate(area = (st_area(geometry)* 100)/buffer_area)%>% 
      distinct()
    area_praitemp = c(ifelse(nrow(a) == 0, 0, sum(a$area)))
    
                      
    ## Densité de haie ----
    a = st_intersection(data_haie, b) %>%
      mutate(longueur = st_length(geom))
    haie_density = c(ifelse(nrow(a) == 0, 0,(sum(a$longueur)/buffer_area) * 10000))
    
    ##Densité de route----
    a = st_intersection(data_route, b) %>%
      mutate(longueur = st_length(geom))
    route_density = c(ifelse(nrow(a) == 0, 0,(sum(a$longueur)/buffer_area) * 10000))
    
    ##Diversité d'élément semi-naturel/naturel ----
    a = mask(crop(data_naturel, b_v),b_v)
    Shannon_naturel= lsm_l_shdi(a)


    ##Densité de lisière ----
    a = st_intersection(data_lisiere, b) %>%
      mutate(longueur = st_length(geom))
    lisiere_density = c(ifelse(nrow(a) == 0, 0,(sum(a$longueur)/buffer_area) * 10000))
    

    
    ## !!! collage dans le vecteur ----
    vecteur_var = cbind(b,
                        dist_buffer, 
                        buffer_area,
                        dist_eau, 
                        dist_habitation, 
                        dist_foret, 
                        dist_eau,
                        dist_haie,
                        bande_area,
                        area_foret,
                        area_habitation,
                        area_agri,
                        area_BIO,
                        area_prairie,
                        area_praiperm,
                        area_praitemp,
                        moy_area_agri,
                        perimeter_agri,
                        haie_density,
                        route_density,
                        lisiere_density,
                        nb_parcelle,
                        Shannon_naturel = Shannon_naturel$value,
                        Shannon_cultu = Shannon_cultu$value)
    
    ## !!! collage ligne par ligne----
    data_pay = rbind(data_pay,vecteur_var)
    
    
  }
print(i)
}


