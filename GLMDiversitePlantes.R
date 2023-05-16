# Titre : GLMDiversitePlante
# But : Modèle GLM sur l'effet de la diversité de plante
# Auteur : Emilie
# Date : 11.05.2023


rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot
library(glmmTMB)#pour faire des glmmTMB : negative binomiale
library(performance)#pour calculer le VIF
library(car)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) %>% 
  mutate(Commune = str_to_upper(Commune))

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds")) %>% 
  filter(Modalite_protocole == "bande")

data_contact = left_join(data_site, data_chiro) %>% 
  select(carre_year_pass, Modalite_protocole, year, Num_passag, Commune) %>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = "sum_contact") %>% 
  distinct() 

data_div = readRDS(file.path(FolderDonnees,FolderInter, "Div_Plante_Shannon.rds")) %>% 
  rename(Commune = bande,
         year = annee) %>% 
  mutate(Commune = str_to_upper(Commune))

data_mod = left_join(data_contact, data_div) %>% 
  filter(!is.na(Indi_Shannon))
