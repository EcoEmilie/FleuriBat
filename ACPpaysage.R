# Titre : ACP paysage 
# But : Graphique + ACP avec les variables paysages 
# Auteur : Emilie
# Date : 12.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(sf)
library(FactoMineR) # Pour effectuer l'ACP
library(factoextra) # Pour visualiser les résultats de l'ACP

# Donnée ------------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"  

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) 

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds")) 

data_pay = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) 

data_total = left_join(data_chiro, data_site)

data_contact = data_total %>% 
  filter(!Modalite_protocole == "exclos")%>% 
  dplyr :: select(carre_year_pass, Modalite_protocole,Num_passag, year, Commune) %>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = "sum_contact") %>% 
  distinct()

data_paysage = inner_join(data_contact, data_pay) %>% 
  distinct() %>% 
  mutate(dist_buffer = as.factor(dist_buffer))
  

# 100 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_paysage_100 = data_paysage %>% 
  st_drop_geometry() %>% 
  filter(dist_buffer == "100") %>% 
  select(!Shannon_naturel) 

data_paysage_100_corr = data_paysage_100%>%  
  select_if(is.numeric) %>% 
  select(!carre_year_pass) %>% 
  cor()

cor_paysage = corrplot(data_paysage_100_cor, method = "ellipse", order = "hclust")
ggsave(file.path(FolderDonnees,FolderSortie,"corr_paysage_100.png"),device = "png" )

## ACP ---------------------------------------------------------------------

resultat_acp <- data_paysage %>% 
  st_drop_geometry() %>%
  select(!Shannon_naturel) %>% 
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp$eig

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp)

#CP1 x CP2
fviz_pca_var(resultat_acp,
             axes = c(1, 2))

#CP1 x CP3
fviz_pca_var(resultat_acp,
             axes = c(1, 3),
             repel = TRUE)

#CP