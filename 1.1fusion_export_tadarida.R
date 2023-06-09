# Titre : 1.1fusion_export_tadarida 
# But : Fusionner les données export des années 2019, 2020, 2021 de Tadarida 
#         - On regroupe les données de chaque fichier de de chaque année dans un seul ficher  
#         - 
# Auteur : Emilie
# Date : 21/04/2023

rm(list=ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)#manipulation de texte
library(readr)#pour le fichier bizarre

# Data path  --------------------------------------------------------------
folderpath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/1.Donnees_sources/Chiroptères/Tadarida")
finalpath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")
AnneeExport_2019="export_2019"
AnneeExport_2020 = "export_2020"
AnneeExport_2021 = "export_2021"
filepath_2019=file.path(folderpath,AnneeExport_2019)
filepath_2020=file.path(folderpath,AnneeExport_2020)
filepath_2021=file.path(folderpath,AnneeExport_2021)

# Chargement de export 2019 -----------------------------------------------

listP <- unique(list.files(path=filepath_2019))#lister le nom des fichers dans le dossier 

tabz<-NULL #Créarion d'une data pour acceuillir les données 

for(p in listP) {
  print(p)
  a<-read.csv(file.path(filepath_2019,p), sep = ";", header = TRUE)
  tabz<-bind_rows(tabz,a,.id = "source")
}


## Modification ------------------------------------------------------------

export2019<-tabz #changer le nom 
head(export2019) #regarde les 6 première lignes du tableau pour vérifier si ça été bien importer   

a2019 = export2019 %>%
  separate(col="nom.du.fichier",into=c("carre","year","Num_passag","point","date"),sep="-", remove = FALSE)%>% 
  mutate(Num_passag = str_replace(Num_passag,"Pass1", "PASS1")) %>% 
  mutate(Num_passag = str_replace(Num_passag,"Pass2", "PASS2"))

# Chargement de export 2020 -----------------------------------------------

# Sys.setlocale("LC_ALL", "C")

listP <- unique(list.files(path=filepath_2020))

taby<-NULL

for(p in listP) {
  print(p)
  a<-read.csv(file.path(filepath_2020,p),sep=";", fileEncoding="latin1", header = TRUE)
  taby<-bind_rows(taby,a,.id = "source")
}


## Modification ------------------------------------------------------------

export2020<-taby
a2020 = export2020 %>%
  separate(col="nom.du.fichier",into=c("carre","year","Num_passag","point","date"),sep="-", remove = FALSE) %>%  
  mutate(year = str_replace(year,"2021", "2020")) %>% 
  mutate(Num_passag = str_replace(Num_passag,"Pass1", "PASS1")) %>% 
  mutate(Num_passag = str_replace(Num_passag,"Pass2", "PASS2"))

# Chargement de export 2021 -----------------------------------------------

listP <- unique(list.files(path=filepath_2021))

tabx<-NULL

for(p in listP) {
  print(p)
  a<-read.csv(file.path(filepath_2021,p),sep=";",header = TRUE)
  tabx<-bind_rows(tabx,a,.id = "source")
}


## Modification ------------------------------------------------------------

export2021<-tabx
a2021 = export2021 %>%
  separate(col="nom.du.fichier",into=c("carre","year","Num_passag","point","date"),sep="-", remove= FALSE)%>% 
  mutate(Num_passag = str_replace(Num_passag,"Pass1", "PASS1")) %>% 
  mutate(Num_passag = str_replace(Num_passag,"Pass2", "PASS2"))



# Fusion  -----------------------------------------------------------------

export19_20_21 = bind_rows(a2019,a2020,a2021,.id = "source") %>% 
  unite(carre, point, sep="_", col = "Carre_Point_vigiechiro")%>% 
  unite(participation, Carre_Point_vigiechiro, year, Num_passag, sep = "_", remove = FALSE) %>%  
  filter(!Carre_Point_vigiechiro %in% c("Car280117_Z1","Car280389_Z2", "Car280449_Z2","Car780267_Z1","Car780286_Z1"))%>%
  separate(col="date",into=c("date","heure","autre"),sep="_") %>% 
  mutate(date=ymd(date)) 


# Ecriture ----------------------------------------------------------------
saveRDS(export19_20_21, file = file.path(finalpath, "export_fusion.rds"))

