# Titre : main_script 
# But : le script principale contenant tous les scripts  
# Auteur : Emilie
# Date : 21/04/2023

rm(list=ls())

# 0.Mise en place -----------------------------------------------------------

## 0.1 Directory ---------------------------------------------------------------
setwd("~/Documents sur ordi/Github/FleuriBat")

## 0.2 Packages ----------------------------------------------------------------
source("0.2Install_package.R")#Installer tous les packages pour run les scripts

## 0.3 Fonctions  ---------------------------------------------------------------
source("0.3.1FunctionRasterize.R")#fonction pour rasteriser les cartes 
source("0.3.2FunctionDensity.R")#la densité ??? 


# 1.Construction des données ------------------------------------------------

## 1.1 Base Chiroptères -----------------------------------------------------
source("1.1.1fusion_export_tadarida.R")#exporter les fichiers qui proviennent de tadarida et les fusionner 
source("1.1.2modif_site.R")#modification des noms des sites 
#source("1.1.2EspecesVerifieesParSites.R")#script de Camille que j'arrive pas à faire marcher 
source("1.1.3clean_data_chiro.R")#filtrage des données pour avoir que les chiro + mise en forme des noms de variables
source("1.1.4variable_nuit.R")#création de la variable "NUIT"
#Filtrage des données par Camille 
source("1.1.5ChargDonneesFiltrees08Camille.R")#chargement des données filtrées par camille 
source("1.1.6ModifDataChiro08.R")#Modification des sites 

## 1.2 Variables Chiro ----------------------------------------------------
source("1.2.1VariableSumContact.R")#création des variables nombre de contact 
source("1.2.2VariableRichesseSpe.R")#création de la variable richesse spécifique 
source("1.2.3VariableDureeCummulee.R")#création de la variable durée cummulée mais pas fini 

## 1.3 Variables Paysagères --------------------------------------------------
source("1.3.1VerifPrarie.R")
source("1.3.2ModifRPG.R")
source("1.3.3ShapeElementAqua.R")
source("1.3.4RPGraterize.R")
source("1.3.5RasterShapeElementNatural.R")
source("1.3.6VariableSemiNaturel.R")
source("1.3.7script_boucle_paysage.R")

## 1.4 Variables Bandes ------------------------------------------------------
source("1.4.1VariableBande.R")
source("1.4.1VariableDivPlante.R")

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


# 3. Figure résultats -----------------------------------------------------

source("3.1TableauSummaryChiro.R")
source("3.2TableauPaysage.R")





