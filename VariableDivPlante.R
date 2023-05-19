# Titre : VariableDivPlante
# But : Calcule de la diversité des plantes dans les bandes 
# Auteur : Emilie
# Date : 12.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(stringr)


# Donnée ------------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderSources = "1.Donnees_sources"
Bande = "Bande"
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"  

Releve_bota = read.csv2(file.path(FolderDonnees,FolderSources, Bande, "Releve_bota_bandes.csv"),  sep = ";", dec = ",") 

Releve_bota_modif = Releve_bota %>% 
  mutate(recouvrement = str_replace(recouvrement,"e", "0,1"),
         recouvrement = str_replace(recouvrement,",", ".")) %>% 
  mutate(recouvrement = as.numeric(recouvrement),
         bande = as.factor(bande),
         annee = as.factor(annee),
         seme = as.factor(seme))

Releve_NA = Releve_bota_modif %>% 
  filter(is.na(recouvrement))

Richesse_spe = Releve_bota_modif %>% 
  group_by(annee,bande) %>% 
  tally()

Semee_spont = Releve_bota_modif %>% 
  group_by(annee,bande, seme) %>% 
  tally()

Bande_Shanon = Releve_bota_modif %>% 
  filter(!is.na(recouvrement)) %>% 
  group_by(annee,bande) %>% 
  mutate(Indi_Shannon = - sum((recouvrement/sum(recouvrement))*log(recouvrement/sum(recouvrement)))) %>% 
  group_by(annee,bande, seme) %>% 
  mutate(Indi_seme_Shannon = - sum((recouvrement/sum(recouvrement))*log(recouvrement/sum(recouvrement)))) %>%
  ungroup() %>% 
  select(bande,annee, seme, Indi_Shannon, Indi_seme_Shannon) %>% 
  distinct() %>% 
  mutate(bande = str_to_upper(bande)) %>% 
  arrange(by_groupe = bande) %>% 
  filter(!annee == "2022") %>% 
  mutate(bande = str_replace(bande," ","_")) %>% 
  mutate(bande = str_replace(bande,"È","E")) %>% 
  mutate(bande = str_replace(bande,"É","E"))


# Ecriture ----------------------------------------------------------------

saveRDS(Releve_bota_modif, file.path(FolderDonnees, FolderInter, "Releve_bota.rds"))
saveRDS(Richesse_spe, file.path(FolderDonnees, FolderInter, "Richesse_spe_bande.rds"))
saveRDS(Bande_Shanon, file.path(FolderDonnees, FolderInter, "Div_Plante_Shannon.rds"))
