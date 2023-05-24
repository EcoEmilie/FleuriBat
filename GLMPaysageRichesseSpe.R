# Titre : GLMPaysageRichesseSpe  
# But : Modèle avec les variables paysages et la richesse spécifique  
# Auteur : Emilie PEN
# Date : 19/05/2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(units)
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

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) 

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds")) 

data_pay = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) 

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

data_pay = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  st_drop_geometry()

data_pay_100 = data_pay %>% 
  filter(dist_buffer  == "100") %>% 
  dplyr ::select(carre_year_pass, area_praitemp, dist_eau)

data_pay_500 = data_pay %>% 
  filter(dist_buffer  == "500") %>% 
  dplyr ::select(carre_year_pass, dist_foret, nb_parcelle) %>% 
  rename(nb_parcelle_500 = nb_parcelle)

data_pay_1000 = data_pay %>% 
  filter(dist_buffer == "1000") %>% 
  dplyr ::select(carre_year_pass, perimeter_agri, nb_parcelle) %>% 
  rename(nb_parcelle_1000 = nb_parcelle)

data_pay_2000 = data_pay %>% 
  filter(dist_buffer == "2000") %>% 
  dplyr ::select(carre_year_pass, area_praiperm, Shannon_cultu) 


data_paysage = left_join(data_pay_100, data_pay_500) %>% 
  left_join(data_pay_1000) %>% 
  left_join(data_pay_2000)

data_mod = left_join(data_richesse, data_paysage)


# Modèle ------------------------------------------------------------------


