# Titre : 2.1VarNombreContact
# But : Création de la variable nombre de contact
# Auteur : Emilie
# Date : 26/04/2023

rm(list=ls())
# Library ----------------------------------------------------------------

library(tidyverse)


# Chargement data ---------------------------------------------------------

Folderpath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")
FinalPath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")
data_chiro_nuit = readRDS(file = file.path(Folderpath,"data_chiro_nuit.rds"))





