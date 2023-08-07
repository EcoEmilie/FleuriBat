# Titre : ChargDonneesFiltrees08Camille  
# But : Chargement des données de Camille filtré à 0.8
# Auteur : Emilie
# Date : 10.05.2023


# Library -----------------------------------------------------------------

library(tidyverse)

# Chemin ------------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderSource = "1.Donnees_sources"
FolderChiro = "Chiroptères"
FolderFiltrees = "Data_Chiro_Filtrees"

FolderInter= "2.Donnees_intermediaire"

# Chargement --------------------------------------------------------------

Data_filtre08 = read.csv(file.path(FolderDonnees,FolderSource, FolderChiro,FolderFiltrees, "data_chiro_filtre_seuil08.csv"),
                         sep = ";", dec = ",") %>%
  mutate(year = as.factor(year),
         Modalite_protocole = as.factor(Modalite_protocole))
  

Data_filtre05 = read.csv(file.path(FolderDonnees,FolderSource, FolderChiro,FolderFiltrees, "data_chiro_filtre_seuil05.csv"),
                         sep = ";", dec = ",")%>%
  mutate(year = as.factor(year),
         Modalite_protocole = as.factor(Modalite_protocole))


# Ecriture ----------------------------------------------------------------

saveRDS(Data_filtre08, file.path(FolderDonnees, FolderInter,"data_filtree_seuil08.rds"))
saveRDS(Data_filtre08, file.path(FolderDonnees, FolderInter,"data_filtree_seuil05.rds"))
        