#Ranger les données 
library(tidyverse)
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
library(landscapemetrics)#cartographie des variables 
library(landscapetools) 
library(terra)#manipuler des raster
library(fasterize)#transformer des vecteurs en raster
library(raster)#transformer des vecteurs en raster
#library(gdalUtils)#transformer des vecteurs en raster

#Graphique 
library(ggthemes)#palette de couleur pour ggplot 
library(paletteer)#palette de couleur 
library(ggplot2)#faire des graphiques 
library(units)
library(effects)#mettre les effets des varaibles dans un vecteur ?
library(broom)#transformer les résultats d'un modèle en tableau rangé 
library(ggeffects)#représentation des modèles 
library(gridExtra)#mettre plusieurs graphs cote à cote 

#Statistique 
library(emmeans) # Pour les tests de comparaisons de moyennes (ajustées)
library(car)#Anova
library(lme4) # Pour l'ajustement de modèle mixte
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot