# Titre : VariableDuréeCumulée 
# But : Calcule de la variable durée cumulée : 
        #La durée cumulée est la temps total d'enregistrement d'une espéce à un site. 
# Auteur : Emilie
# Date : 22/05/2023

rm(list=ls())


# Library -----------------------------------------------------------------

library(tidyverse)


# Chargement données ------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) 

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds")) 

data_total = left_join(data_chiro, data_site)


# Calcul variable --------------------------------------------------------

data_dureecum = data_total %>% 
  mutate(duree = temps_fin - temps_debut, .after = temps_fin) %>% 
  dplyr::select(carre_year_pass, year, Commune,Modalite_protocole, Num_passag, duree) %>% 
  group_by(carre_year_pass) %>% 
  mutate(Duree_tot = sum(duree)) %>% 
  dplyr::select(-duree) %>% 
  distinct() 

saveRDS(data_dureecum, file.path(FolderDonnees,FolderInter,"data_dureecum.rds"))

