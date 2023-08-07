# Titre : VariableRichesseSpe
# But : Calcule de la richesse spécifique 
# Auteur : Emilie
# Date : 23.05.2023

rm(list=ls())
# Library -----------------------------------------------------------------

library(tidyverse)

# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) %>% 
  dplyr::select(!Commune)

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds")) 

data_agri = readRDS(file.path(FolderDonnees,FolderInter, "SDCChiroall.rds")) %>% 
  rename(carre_year_pass = carre_year.1)

data_total = left_join(data_chiro, data_site) %>% 
  left_join(data_agri)

data_richesse = data_total %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  group_by(carre_year_pass, tadarida_taxon) %>% 
  add_tally(name = "Sum_contact_spe") %>% 
  ungroup() %>% 
  dplyr :: select(carre_year_pass, Modalite_protocole,Num_passag, year, Commune, SDC, tadarida_taxon, Sum_contact_spe) %>% 
  distinct() %>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = 'Richesse_spe') %>% 
  mutate(Num_passag = as.factor(Num_passag),
         Richesse_spe = as.numeric(Richesse_spe),
         Sum_contact_spe = as.numeric(Sum_contact_spe)) %>% 
  dplyr :: select(carre_year_pass, Modalite_protocole,Num_passag, year, Commune, SDC,Richesse_spe ) %>% 
  distinct()

mean(data_richesse$Richesse_spe)

saveRDS(data_richesse, file.path(FolderDonnees,FolderInter, "data_RichesseSpe.rds"))
