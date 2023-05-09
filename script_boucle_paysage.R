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

## Données sites -----------------------------------------------------------

data_site= read.csv(file = file.path(Folderpath, FolderChiro, "donnees_site_Nuit.csv"), header = TRUE, sep = ",", dec = ".") %>% #A changer avec données site
  mutate(Commune = str_to_upper(Commune), year = as.factor(year)) %>% 
  mutate(Commune = str_replace_all(Commune,"_"," ")) %>% 
  unite(Mod_pass, Modalite_protocole, Num_passag, sep = "_", remove = FALSE)%>% 
  slice_sample(n = 10) %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% #au départ en WGS 84
  st_transform(2154) 

## Carte -------------------------------------------------------------------

### Plan d'eau  -------------------------------------------------------------


data_plan_eau = st_read(dsn = file.path(Folderpath,FolderCarto,"data_plan_eau_total.gpkg")) %>% 
  st_transform(2154)


dist_eau =c()#Création d'un vecteur pour accueillir la variable 

### Zone urbaine  -------------------------------------------------------------


data_habitation = st_read(dsn = file.path(Folderpath,FolderCarto,"data_occupation_bati_total.gpkg")) %>% 
  st_transform(2154)

dist_habitation = c() 

### Forêt  -------------------------------------------------------------


data_foret = st_read(dsn = file.path(Folderpath,FolderCarto,"data_occupation_foret_total.gpkg")) %>% 
  st_transform(2154)
dist_foret = c() 

#Feuillu
data_foret_feuillu = data_foret %>% 
  filter(str_detect(nature,"feuillus"))

#Conifère
data_foret_conifere = data_foret %>% 
  filter(str_detect(nature,"conifères"))

### Cours d'eau  -------------------------------------------------------------


data_cours_eau = st_read(dsn = file.path(Folderpath,FolderCarto,"data_cours_eau_total.gpkg")) %>% 
  st_transform(2154)

dist_cours_eau = c()

### Ripisylve  -------------------------------------------------------------

data_ripisylve = data_cours_eau %>% 
  st_buffer(dist = 2)

dist_ripisylve = c()
area_ripi = c()

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
RPG_2019 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_RPG_2019.gpkg")) %>% 
  st_transform(2154)

#2020 
RPG_2020 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_RPG_2020.gpkg"))%>% 
  st_transform(2154)

#2021
RPG_2021 = st_read(dsn = file.path(Folderpath,FolderCarto,"donnees_RPG_2021.gpkg"))%>% 
  st_transform(2154)

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
  
  ##Distance à la haie ----
  dist_haie = st_distance(names_year, data_haie, by_element = FALSE) %>% 
    min()
  
  ##Surface bande ----
  v = if(names_year$Modalite_protocole == "bande"){
    st_intersection(data_bande,st_buffer(names_year,dist = 2000))
  }else{data.frame}
  bande_area = c(ifelse(nrow(v) == 0, 0,st_area(v$geom)))
  
  for (j in 1:length(buffer)){
    
    ## !!! Calcul du buffer + de son aire----
    dist_buffer = buffer[j]
    b = st_buffer(names_year, dist = buffer[j]) 
    b_v = vect(b)
    buffer_area = st_area(b)
    
    ##Surace de forêt total----
    d = st_intersection(data_foret , b)%>% 
      mutate(area = (st_area(geom)* 100)/buffer_area) %>% 
      distinct() 
    area_foret = c(ifelse(nrow(d) == 0, 0, sum(d$area)))
    
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
    
    ## Surface/moyenne/perimetre agricole ----
    
    l = if(b$year == 2019){st_intersection(RPG_2019 , b)}else if(b$year==2020){st_intersection(RPG_2020 , b)} else{st_intersection(RPG_2021 , b)}
    l = l  %>% 
      mutate(area = st_area(geom), perimeter = st_perimeter(geom), pourcentage = (area * 100)/buffer_area)%>% 
      distinct() %>% 
      add_tally(name = "n")
    
    area_agri = c(ifelse(nrow(l) == 0, 0, (sum(l$area) * 100)/buffer_area))
    moy_area_agri = c(ifelse(nrow(l) == 0, 0, mean(l$area)))
    perimeter_agri = c(ifelse(nrow(l) == 0, 0, mean(l$perimeter)))
    nb_parcelle =  c(ifelse(nrow(l) == 0, 0, l$n[1]))
    
    ## Surface de prairie total ----
    
    m = if(b$year == 2019){st_intersection(prairie_2019 , b)}else if(b$year==2020){st_intersection(prairie_2020 , b)} else{st_intersection(prairie_2021 , b)}
    m = m  %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct()
    area_prairie = c(ifelse(nrow(m) == 0, 0, sum(m$area)))
    
    ## Surface de prairie permanente ----
    
    n = if(b$year == 2019){st_intersection(praiperm_2019 , b)}else if(b$year==2020){st_intersection(praiperm_2020 , b)} else{st_intersection(praiperm_2021 , b)}
    n = n  %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct()
    area_praiperm = c(ifelse(nrow(n) == 0, 0, sum(m$area)))
    
    ## Surface de prairie temporaire ----
    
    o = if(b$year == 2019){st_intersection(praitemp_2019 , b)}else if(b$year==2020){st_intersection(praitemp_2020 , b)} else{st_intersection(praitemp_2021 , b)}
    o = o  %>% 
      mutate(area = (st_area(geom)* 100)/buffer_area)%>% 
      distinct()
    area_praitemp = c(ifelse(nrow(o) == 0, 0, sum(m$area)))
    
                      
    ## Densité de haie ----
    q = st_intersection(data_haie, b) %>%
      mutate(longueur = st_length(geom))
    haie_density = c(ifelse(nrow(q) == 0, 0,(sum(q$longueur)/buffer_area) * 10000))
    
    ##Densité de route----
    r = st_intersection(data_route, b) %>%
      mutate(longueur = st_length(geom))
    route_density = c(ifelse(nrow(r) == 0, 0,(sum(r$longueur)/buffer_area) * 10000))
    
    ##Diversité d'élément semi-naturel/naturel ----
    s = mask(crop(data_naturel, b_v),b_v)
    Shannon_naturel= lsm_l_shdi(s)


    ##Densité de lisière ----
    u = st_intersection(data_lisiere, b) %>%
      mutate(longueur = st_length(geom))
    lisiere_density = c(ifelse(nrow(u) == 0, 0,(sum(u$longueur)/buffer_area) * 10000))
    

    
    ## !!! collage dans le vecteur ----
    vecteur_var = cbind(b,
                        dist_buffer, 
                        buffer_area,
                        dist_eau, 
                        dist_habitation, 
                        dist_foret, 
                        dist_cours_eau,
                        dist_haie,
                        bande_area,
                        area_ripi,
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



