# Titre : main_script 
# But : le script principale contenant tous les scripts intermédiaires 
# Auteur : Emilie
# Date : 21/04/2023

rm(list=ls())
# 0.1 Directory ---------------------------------------------------------------
setwd("~/Documents sur ordi/Github/FleuriBat")

# 0.2 Packages ----------------------------------------------------------------
source("0.Install_package.R")

# 0.3 Fonctions  ---------------------------------------------------------------
source("FunctionRasterize.R")
source("FunctionDensity.R")


# 1 Construction des données ------------------------------------------------

## 1.1 Base Chiroptères -----------------------------------------------------
source("1.1fusion_export_tadarida.R")
source("1.2modif_site.R")
source("1.3clean_data_chiro.R")
source("1.4variable_nuit.R")


## 1.2 Variables Activité ----------------------------------------------------


## 1.3 Variables Paysagères --------------------------------------------------
source("VerifPrarie.R")
source("ModifRPG.R")
source("ShapeElementAqua.R")
source("script_boucle_paysage.R")


# 2 Analyses ----------------------------------------------------------------














