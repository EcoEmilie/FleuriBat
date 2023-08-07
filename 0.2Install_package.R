#Ranger les données 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")#manipuler les dates et les heures
install.packages("stringr")#manipuler les characteres 

#_Cartographie 
install.packages("sf")#opération en SIG
install.packages("tmap")#faire des cartes
install.packages("leaflet")#afficher les cartes de manière interactive 
install.packages("tmaptools")#outils pour les couleurs
install.packages("mapview")#afficher les données sur une carte
install.packages("landscapemetrics")#cartographie des variables 
install.packages("landscapetools") 
install.packages("terra")#manipuler des raster
install.packages("fasterize")#transformer des vecteurs en raster
install.packages("raster")#transformer des vecteurs en raster
#install.packages("gdalUtils")#transformer des vecteurs en raster

#Graphique 
install.packages("ggthemes")#palette de couleur pour ggplot 
install.packages("paletteer")#palette de couleur 
install.packages("ggplot2")#faire des graphiques 
install.packages("units")
install.packages("effects")#mettre les effets des varaibles dans un vecteur ?
install.packages("broom")#transformer les résultats d'un modèle en tableau rangé 
install.packages("ggeffects")#représentation des modèles 
install.packages("gridExtra")#mettre plusieurs graphs cote à cote 

#Statistique 
install.packages("emmeans") # Pour les tests de comparaisons de moyennes (ajustées)
install.packages("car")#Anova
install.packages("lme4") # Pour l'ajustement de modèle mixte
install.packages("lmerTest") # Pour tests d'effets aléatoires dans le modèle mixte
install.packages("DHARMa")#QQ plot