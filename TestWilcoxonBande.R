# Titre : TestWilcoxBande 
# But : Test de Wilcoxon pour voir l'effet des bandes sur les CS 
#       On compare la moyenne des deux échatillons : Bande et Témoins 
# Auteur : Emilie
# Date : 10.05.2023

rm(list=ls())
# Library  ----------------------------------------------------------------
library(tidyverse)

# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds"))

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds"))

data_total = left_join(data_chiro, data_site) %>% 
  filter(!Modalite_protocole == "exclos")%>% 
  group_by(carre_year_pass,Modalite_protocole) %>% 
  tally()

data_bande = data_total %>% 
  filter(Modalite_protocole == "bande") 

data_temoin = data_total %>% 
  filter(Modalite_protocole == "temoin") 

# Test Wilcoxon  ----------------------------------------------------------

TestWilcox = wilcox.test(data_bande$n, data_temoin$n)

TestWilcox

# Graphique  --------------------------------------------------------------

GraphTraitement = ggplot(data_total)+
  aes(x = Modalite_protocole,y = n, fill = Modalite_protocole )+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le traitement bande/temoins")

# Ecriture ----------------------------------------------------------------
saveRDS(TestWilcox, file.path(FolderDonnees,FolderSortie, "TestWilcoxonTraitement.rds"))
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitement.png"), device = "png")
