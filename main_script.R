# Titre : main_script 
# But : le script principale contenant tous les scripts intermédiaires 
# Auteur : Emilie
# Date : 21/04/2023

rm(list=ls())

# 0.Mise en place -----------------------------------------------------------

## 0.1 Directory ---------------------------------------------------------------
setwd("~/Documents sur ordi/Github/FleuriBat")

## 0.2 Packages ----------------------------------------------------------------
source("0.Install_package.R")

## 0.3 Fonctions  ---------------------------------------------------------------
source("FunctionRasterize.R")
source("FunctionDensity.R")


# 1.Construction des données ------------------------------------------------

## 1.1 Base Chiroptères -----------------------------------------------------
source("1.1fusion_export_tadarida.R")
source("1.2modif_site.R")
source("1.3clean_data_chiro.R")
source("1.4variable_nuit.R")
#Filtrage des données par Camille 
source("ChargDonneesFiltrees08Camille.R")


## 1.2 Variables Activité ----------------------------------------------------


## 1.3 Variables Paysagères --------------------------------------------------
source("VerifPrarie.R")
source("ModifRPG.R")
source("ShapeElementAqua.R")
source("script_boucle_paysage.R")

## 1.4 Variables Bandes ------------------------------------------------------


# 2 Analyses ----------------------------------------------------------------


## 2.1 Effet des bandes  ----------------------------------------------------

source("2.1.1TestWilcoxonBande.R")
source("2.1.2GLMTraitementSumContact.R")
source("2.1.3GLMTraitementRichesseSpe.R")

## 2.2 Test des caractéristiques de la bande --------------------------------

source("2.2.1GLMDiversitePlantesSumContact.R")
source("2.2.2LMDiversitePlantesRichesseSpe.R")

## 2.3 ACP + Modéle  -----------------------------------------------------------------

source("2.3.1ACP_GLM_100m.R")
source("2.3.2ACP_GLM_500m.R")
source("2.3.3ACP_GLM_1000m.R")
source("2.3.4ACP_GLM_2000m.R")

## 2.4 Modèle par final  --------------------------------------------------

source("2.4.1GLMPaysageFinalSumContact.R")
source("2.4.2GLMPaysageFinalRichesseSpe.R")







