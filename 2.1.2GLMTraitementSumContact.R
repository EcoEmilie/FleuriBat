# Titre : GLMTraitement
# But : Modèle GLM sur l'effet de sbandes sur les chauves souris 
# Auteur : Emilie
# Date : 10.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot
library(glmmTMB)#pour faire des glmmTMB : negative binomiale
library(performance)#pour calculer le VIF
library(car)#ANOVA 
library(MuMIn)#R2 pour modéle mixte


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_contact = readRDS(file.path(FolderDonnees,FolderInter, "data_sumcontact.rds"))

# Graphique ---------------------------------------------------------------

#Annees
GraphTraitement= ggplot(data_contact)+
  aes(x = Modalite_protocole ,y = sum_contact, fill = year )+
  geom_boxplot()+
  labs(x = "Annees", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon l'année")
GraphTraitement
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotAnnee.png"), device = "png")

#Passage
GraphNum_passag= ggplot(data_contact)+
  aes(x = Num_passag ,y = sum_contact, fill = Num_passag )+
  geom_boxplot()+
  labs(x = "Passage", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le passage dans l'année ")
GraphNum_passag
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotNum_passag.png"), device = "png")

#Annees x traitement
GraphTraitementAnnee = ggplot(data_contact)+
  aes(x = Modalite_protocole,y = sum_contact, fill = year )+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le traitement bande/temoins et l'année")
GraphTraitementAnnee
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementAnnee.png"), device = "png")

#Passage x traitement
GraphTraitementPassag = ggplot(data_contact)+
  aes(x = Modalite_protocole,y = sum_contact, fill = Num_passag )+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le traitement bande/temoins et le passage")
GraphTraitementPassag
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementPassag.png"), device = "png")

#Commune x traitement
GraphTraitementCommune = ggplot(data_contact)+
  aes(x = Modalite_protocole,y = sum_contact, fill = Commune)+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le traitement bande/temoins et l'exploitation")
GraphTraitementCommune
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementCommune.png"), device = "png")

#BIO
GraphBIO = ggplot(data_paysage)+
  aes(x = area_BIO, y = sum_contact , color = dist_buffer)+
  geom_point()+
  labs(x = "Pourcentage de BIO", 
       y = "Nombre de contact",
       title = "Nombre de contact en fonction le pourcentage de BIO")
GraphBIO

hist(log(data_paysage$sum_contact))
hist(data_paysage$dist_foret)
hist(data_paysage$area_BIO)

hist(data_contact$sum_contact)

# Modèle  -----------------------------------------------------------------

#### GLM mixte ####
Mod = glmmTMB(sum_contact ~ Num_passag + Modalite_protocole + (1| year/Commune ), 
         data = data_contact,
         family = poisson(link = "log"))

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

png(file.path(FolderDonnees,FolderSortie,"DHARMATraitementSumContact.png"),
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
