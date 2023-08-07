# Titre : GLMTraitementRichesseSpe
# But : Modèle GLM sur l'effet des bandes sur la richesse spé des chauves souris 
# Auteur : Emilie
# Date : 19.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot
library(glmmTMB)#pour faire des glmmTMB : negative binomiale
library(performance)#pour calculer le VIF
library(car)
library(MASS)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_richesse = readRDS(file.path(FolderDonnees,FolderInter, "data_RichesseSpe.rds"))


# Graphique ---------------------------------------------------------------

#Traitement
GraphTraitement= ggplot(data_richesse)+
  aes(x = Modalite_protocole ,y = Richesse_spe, fill = Modalite_protocole )+
  geom_boxplot()+
  labs(x = "Modalite_protocole", 
       y = "Richesse_spe/Site/Annee/Passage",
       title = "Richesse spécifique selon le traitement")
GraphTraitement
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementRichesse.png"), device = "png")

#Annees
GraphAnnee= ggplot(data_richesse)+
  aes(x = year ,y = Richesse_spe, fill = year )+
  geom_boxplot()+
  labs(x = "Annees", 
       y = "Richesse_spe/Site/Annee/Passage",
       title = "Richesse spécifique selon l'année")
GraphAnnee
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotAnneeRichesse.png"), device = "png")

#Passage
GraphNum_passag= ggplot(data_richesse)+
  aes(x = Num_passag ,y = Richesse_spe, fill = Num_passag )+
  geom_boxplot()+
  labs(x = "Passage", 
       y = "Richesse_spe/Site/Annee/Passage",
       title = "Richesse spécifique selon le passage dans l'année ")
GraphNum_passag
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotNum_passagRichesse.png"), device = "png")

#Annees x traitement
GraphRichesseAnnee = ggplot(data_richesse)+
  aes(x = Modalite_protocole,y = Richesse_spe, fill = year )+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Richesse_spe/Site/Annee/Passage",
       title = "Richesse spécifique selon le traitement bande/temoins et l'année")
GraphRichesseAnnee
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementAnneeRichesse.png"), device = "png")

#Passage x traitement
GraphRichessePassag = ggplot(data_richesse)+
  aes(x = Modalite_protocole,y = Richesse_spe, fill = Num_passag )+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Richesse_spe/Site/Annee/Passage",
       title = "Richesse spécifique selon le traitement bande/temoins et le passage")
GraphRichessePassag
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementPassagRichesse.png"), device = "png")

#Commune x traitement
GraphRichesseCommune = ggplot(data_richesse)+
  aes(x = Modalite_protocole,y = Richesse_spe, fill = Commune)+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Richesse_spe/Site/Annee/Passage",
       title = "Richesse spécifique selon le traitement bande/temoins et l'exploitation")
GraphRichesseCommune
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementCommuneRichesse.png"), device = "png")

hist(data_richesse$Richesse_spe)

# Modèle  -----------------------------------------------------------------


#Lmer 

Mod = lmer(Richesse_spe ~  Num_passag + Modalite_protocole + (1| year/Commune), 
            data = data_richesse)

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

png(file.path(FolderDonnees,FolderSortie,"DHARMATraitementRichesseSpe.png"),
    width=1200, height=700)
plot(simulationOutput) 
dev.off()

#Dispersion des résidus 
testDispersion(simulationOutput)
hist(residuals(Mod))

#shapiro test
shap<-shapiro.test(residuals(Mod))
shap

#R2 
r2(Mod)

#VIF
check_collinearity(Mod) 




