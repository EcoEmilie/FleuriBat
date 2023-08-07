# Titre : ModifDataChiro
# But : Modifification des sites d'enregistrmeent 
# Auteur : Emilie
# Date : 24.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)

# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) %>% 
  mutate(Commune = str_to_upper(Commune))%>% 
  mutate(Commune = str_replace(Commune,"MEZIERES_BROUE_1","GERMAINVILLE_OUEST"),
         Commune = str_replace(Commune,"MEZIERES_BROUE_2","GERMAINVILLE_EST" ),
         Commune = str_replace(Commune,"MEZIERE-EN-DROUAIS", "MEZIERE")) %>% 
  filter(!Commune == "SERVILLE_2") 

data_lol = data_chiro %>% 
  filter(year == "2020" & Commune == "GERMAINVILLE") %>% 
  mutate(Commune = str_replace(Commune,"GERMAINVILLE", "GERMAINVILLE_OUEST"))

data_lool = data_chiro %>% 
  filter(year == "2021" & Commune == "GERMAINVILLE") %>% 
  mutate(Commune = str_replace(Commune,"GERMAINVILLE", "GERMAINVILLE_EST"))

data_loool = data_chiro %>% 
  filter(carre_year_pass == "Car780534_Z1_2020_PASS1") %>% 
  mutate(Commune = str_replace(Commune,"SONCHAMP", "SONCHAMP_1"))

data_looool = data_chiro %>% 
  filter(carre_year_pass == "Car780542_Z1_2020_PASS1") %>% 
  mutate(Commune = str_replace(Commune,"SONCHAMP", "SONCHAMP_2"))

data_loooool = data_chiro %>% 
  filter(carre_year_pass == "Car780542_Z1_2020_PASS1") %>% 
  mutate(Commune = str_replace(Commune,"SONCHAMP", "SONCHAMP_2"))

data_chiro_modif = bind_rows(data_chiro, data_lol, data_lool, data_loool, data_looool) %>% 
  filter(!Commune =="GERMAINVILLE") %>% 
  filter(!Commune == "SONCHAMP") %>% 
  distinct()

saveRDS(data_chiro_modif, file.path(FolderDonnees, FolderInter, "data_filtree_seuil08_modif.rds"))
