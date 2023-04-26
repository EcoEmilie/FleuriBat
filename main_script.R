# Titre : main_script 
# But : le script principale contenant tous les scripts intermédiaires 
# Auteur : Emilie
# Date : 21/04/2023

rm(list=ls())
# Directory ---------------------------------------------------------------
setwd("~/Documents sur ordi/Github/FleuriBat")
# Packages ----------------------------------------------------------------
source("0.Install_package.R")

# Construction des données ------------------------------------------------
source("1.1fusion_export_tadarida.R")
source("1.2modif_site.R")
source("1.3clean_data_chiro.R")
source("1.4variable_nuit.R")

## Chiroptères -------------------------------------------------------------


### Nettoyage  --------------------------------------------------------------


### Variables Activité ----------------------------------------------------


## Paysages ----------------------------------------------------------------


### Variables paysageres ----------------------------------------------------












