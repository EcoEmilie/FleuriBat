# Titre : 1.3clean_data_chiro
# But : Filtrer les resultats de Tadarida, on ne veut garder que les donnees chiropteres + remettre les dates correctements +
# Auteur : Emilie
# Date : 21/04/2023

rm(list=ls())
# Library -----------------------------------------------------------------

library(tidyverse)
library(lubridate)


# Chargement donnees ------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_brute = readRDS(file = file.path(Folderpath,"2.Donnees_intermediaire","export_fusion.rds"))

list_esp = read.csv(file = file.path(Folderpath,"1.Donnees_sources","Chiroptères","Informations","SpeciesList.csv"), sep = ";") %>% 
  filter(Group == "bat")

# Filtrage ----------------------------------------------------------------
data_chiro = data_brute %>% 
  unite(carre_year_pass,Carre_Point_vigiechiro,year,Num_passag, sep = "_", remove = FALSE) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(year = factor(year)) %>% 
  filter(tadarida_taxon %in% list_esp$Esp) %>% #on garde que les CS
  mutate(tadarida_taxon= factor(tadarida_taxon)) 


## Fichier RDS -------------------------------------------------------------
saveRDS(data_chiro, file = file.path(FolderDonnees,FolderInter, "data_chiro.rds"))
