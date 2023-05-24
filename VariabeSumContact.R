# Titre : VariableSumContact
# But : Calcule du nombre de contact  
# Auteur : Emilie
# Date : 23.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)

# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds"))

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) %>% 
  select(!Commune)

data_agri = readRDS(file.path(FolderDonnees,FolderInter, "SDCChiroall.rds")) %>% 
  rename(carre_year_pass = carre_year.1)

data_total = left_join( data_site,data_chiro) %>% 
  left_join(data_agri)

data_contact = data_total %>% 
  filter(!Modalite_protocole == "exclos")%>% 
  dplyr :: select(carre_year_pass, Modalite_protocole,Num_passag, year, Commune, SDC) %>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = "sum_contact") %>% 
  distinct() %>% 
  mutate(Commune = str_replace(Commune, " ", "_"))

saveRDS(data_contact, file.path(FolderDonnees,FolderInter, "data_sumcontact.rds"))
