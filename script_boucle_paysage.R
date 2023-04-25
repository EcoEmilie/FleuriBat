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

#Graphique 
library(ggthemes)#palette de couleur pour ggplot 
library(paletteer)#palette de couleur 
library(ggplot2)#faire des graphiques 
library(units)
library(effects)#mettre les effets des varaibles dans un vecteur ?
library(broom)#transformer les résultats d'un modèle en tableau rangé 
library(ggeffects)#représentation des modèles 

# Chargement des données --------------------------------------------------

## Données sites -----------------------------------------------------------
setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Chiropteres")

data_site= read.csv("donnees_site_Nuit.csv", header = TRUE, sep = ",", dec = ".") %>% #A changer avec données site
  mutate(Commune = str_to_upper(Commune), year = as.factor(year)) %>% 
  mutate(Commune = str_replace_all(Commune,"_"," ")) %>% 
  unite(Mod_pass, Modalite_protocole, Num_passag, sep = "_", remove = FALSE)%>% 
  slice_sample(n = 10) %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% #au départ en WGS 84
  st_transform(2154) 

## Carte -------------------------------------------------------------------

### Plan d'eau  -------------------------------------------------------------

setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto")

data_plan_eau = st_read("data_plan_eau_total.gpkg") %>% 
  st_transform(2154)


dist_eau =c()#Création d'un vecteur pour accueillir la variable 

### Zone urbaine  -------------------------------------------------------------

setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto")

data_habitation = st_read("data_occupation_bati_total.gpkg") %>% 
  st_transform(2154)

dist_habitation = c() 

### Forêt  -------------------------------------------------------------

setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto")

data_foret = st_read("data_occupation_foret_total.gpkg") %>% 
  st_transform(2154)
dist_foret = c() 

#Feuillu
data_foret_feuillu = data_foret %>% 
  filter(str_detect(nature,"feuillus"))

#Conifère
data_foret_conifere = data_foret %>% 
  filter(str_detect(nature,"conifères"))

### Cours d'eau  -------------------------------------------------------------

setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto")

data_cours_eau = st_read("data_cours_eau_total.gpkg") %>% 
  st_transform(2154)

dist_cours_eau = c()

### Ripisylve  -------------------------------------------------------------

data_ripisylve = data_cours_eau %>% 
  st_buffer(dist = 10)

dist_ripisylve = c()
area_ripi = c()

### Haie  -------------------------------------------------------------

data_haie = st_read("data_occupation_haie_total.gpkg")

setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto/RASTERS")
raster_haie = rast("raster_haie.tif")


### Bande -------------------------------------------------------------





### RPG ----------------------------------------------------
setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto")

#2019 
RPG_2019 = st_read("donnees_RPG_2019.gpkg") %>% 
  st_transform(2154)

#2020 
RPG_2020 = st_read("donnees_RPG_2020.gpkg")%>% 
  st_transform(2154)

#2021
RPG_2021 = st_read("donnees_RPG_2021.gpkg")%>% 
  st_transform(2154)

#Diversité raster
#2019
Div_2019 = rast("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto/RASTERS/RPG_2019_DIV.tif")

#2020 
Div_2020 = rast("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto/RASTERS/RPG_2020_DIV.tif")

#2021 
Div_2021 = rast("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto/RASTERS/RPG_2021_DIV.tif")

i = data_frame()

#Prairie 


### RPG BIO -----------------------------------------------------------------
setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Carto")

#2019
BIO_2019 = st_read("donnees_BIO_2019.gpkg")%>% 
  st_transform(2154)

#2020
BIO_2020 = st_read("donnees_BIO_2020.gpkg")%>% 
  st_transform(2154)

#2021 
BIO_2021 = st_read("donnees_BIO_2021.gpkg")%>% 
  st_transform(2154)

# Calcule des variables paysagères  ---------------------------------------

#Départ
data_pay = data.frame() 
buffer = c(100,500,1000,1500)




#selection de la ligne 
for (i in 1:nrow(data_site)){
  names_year = data_site[i,c("Carre_Point_vigiechiro","year","geometry")]
  
  ##Distance à la mare ----
  dist_eau = st_distance(names_year, data_plan_eau, by_element = FALSE) %>% 
    min()
  
  ##Distance aux zones urbaines ----
  dist_habitation = st_distance(names_year, data_habitation, by_element = FALSE) %>% 
    min()
  
  ##Distance à la forêt ----
  dist_foret = st_distance(names_year, data_foret, by_element = FALSE) %>% 
    min()
  
  ##Distance au cours d'eau ----
  dist_cours_eau = st_distance(names_year, data_cours_eau, by_element = FALSE) %>% 
    min()
  
  ##Distance ripisylve ----
  dist_ripisylve = st_distance(names_year, data_ripisylve, by_element = FALSE) %>% 
    min()
  
  ##Distance à la haie ----
  dist_haie = st_distance(names_year, data_haie, by_element = FALSE) %>% 
    min()
  
  for (j in 1:length(buffer)){
    
    ## !!! Calcul du buffer + de son aire----
    dist_buffer = buffer[j]
    b = st_buffer(names_year, dist = buffer[j]) 
    b_v = vect(b)
    buffer_area = st_area(b)
    
    ##Surface de la bande----
    
    ##Surface de ripisylve----
    c = st_intersection(data_ripisylve, b)  %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct()
    area_ripi = c(ifelse(nrow(c) == 0, 0, sum(c$area)))  
    
    ##Surace de forêt total----
    d = st_intersection(data_foret , b)%>% 
      mutate(area = (st_area(geom)* 100)/buffer_area) %>% 
      distinct() 
    area_foret = c(ifelse(nrow(d) == 0, 0, sum(d$area)))
    
    ##Surface de forêt feuillu ----
    e = st_intersection(data_foret_feuillu , b) %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct() 
    area_feuillu = c(ifelse(nrow(e) == 0, 0, sum(e$area)))
    
    ##Surface de forêt conifere ----
    f = st_intersection(data_foret_conifere , b)%>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct() 
    area_conifere = c(ifelse(nrow(f) == 0, 0, sum(f$area)))
    
    ##Surface zone urbaine ----
    g = st_intersection(data_habitation , b) %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct() 
    area_habitation = c(ifelse(nrow(g) == 0, 0, sum(g$area)))
    
    ##Surface de BIO----
    
    j = if(b$year == 2019){st_intersection(BIO_2019 , b)}else if(b$year==2020){st_intersection(BIO_2020 , b)} else{st_intersection(BIO_2021 , b)}
    j = j  %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area) %>% 
      distinct()
      
    area_BIO = c(ifelse(nrow(j) == 0, 0,sum(j$area)))

    ##Diversité de culture ----
  
    k = if(b$year == 2019){mask(crop(Div_2019, b_v),b_v)}else if(b$year==2020){mask(crop(Div_2020, b_v),b_v)} else{mask(crop(Div_2021, b_v),b_v)} 
    Shannon_cultu = lsm_l_shdi(k)
    
    ## Surface agricole ----
    
    l = if(b$year == 2019){st_intersection(RPG_2019 , b)}else if(b$year==2020){st_intersection(RPG_2020 , b)} else{st_intersection(RPG_2021 , b)}
    l = l  %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct()
    area_agri = c(ifelse(nrow(l) == 0, 0, sum(l$area)))
    
    ## Surface de prairie permanente
    
    
    ##Densité de haie ----
    # k = mask(crop(raster_haie, b_v),b_v)
    # dens_haie = lsm_c_ed(k, FALSE, 8)#pas sur des directions 
      
    ##Diversité d'élément semi-naturel/naturel ----
    
    ##Diversité d'occupation du sol ----
    
    ##Densité de bordure ----
    
    ##Taille moyenne des parcelles ----
    
    ##Densité de route----
    
    ##Luminosité ----

    
    ## !!! collage dans le vecteur ----
    vecteur_var = cbind(b,
                        dist_buffer, 
                        buffer_area,
                        dist_eau, 
                        dist_habitation, 
                        dist_foret, 
                        dist_cours_eau, 
                        dist_ripisylve,
                        dist_haie,
                        area_ripi,
                        area_foret,
                        area_feuillu,
                        area_conifere,
                        area_habitation,
                        area_agri,
                        area_BIO,
                        Shannon_cultu = Shannon_cultu$value)
    
    ## !!! collage ligne par ligne----
    data_pay = rbind(data_pay,vecteur_var)
    
    
  }
print(i)
}

tmap_mode("view")
buffer = tm_shape(b)+
  tm_polygons(col = "red")

ripisylve = tm_shape(c)+
  tm_polygons(col = "green")

cours_deau = tm_shape(st_intersection(data_cours_eau,b))+
  tm_lines(col = "blue")

habitation = tm_shape(g)+ 
  tm_polygons(col = "red")

point = #buffer  + 
  #cours_deau+ 
  habitation + 
  tm_shape(l)+
  tm_polygons(col ="ID_PARCEL")

point
tmap_mode("plot")

lool = l %>% 
  mutate(nul = (sum(st_area(g))* 100)/buffer_area)
