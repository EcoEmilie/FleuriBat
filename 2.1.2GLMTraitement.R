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
library(car)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds"))

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds"))

data_total = left_join(data_chiro, data_site)

data_contact = data_total %>% 
  filter(!Modalite_protocole == "exclos")%>% 
  group_by(carre_year_pass,Modalite_protocole,Num_passag, year, Commune) %>% 
  tally()


# Graphique ---------------------------------------------------------------

#Annees
GraphTraitement= ggplot(data_contact)+
  aes(x = year ,y = n, fill = year )+
  geom_boxplot()+
  labs(x = "Annees", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon l'année")
GraphTraitement
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotAnnee.png"), device = "png")

#Passage
GraphNum_passag= ggplot(data_contact)+
  aes(x = Num_passag ,y = n, fill = Num_passag )+
  geom_boxplot()+
  labs(x = "Passage", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le passage dans l'année ")
GraphNum_passag
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotNum_passag.png"), device = "png")

#Annees x traitement
GraphTraitementAnnee = ggplot(data_contact)+
  aes(x = Modalite_protocole,y = n, fill = year )+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le traitement bande/temoins et l'année")
GraphTraitementAnnee
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementAnnee.png"), device = "png")

#Passage x traitement
GraphTraitementPassag = ggplot(data_contact)+
  aes(x = Modalite_protocole,y = n, fill = Num_passag )+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le traitement bande/temoins et le passage")
GraphTraitementPassag
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementPassag.png"), device = "png")

#Commune x traitement
GraphTraitementCommune = ggplot(data_contact)+
  aes(x = Modalite_protocole,y = n, fill = Commune)+
  geom_boxplot()+
  labs(x = "Modalite de Traitement", 
       y = "Nombre de contact/Site/Annee/Passage",
       title = "Nombre de contact selon le traitement bande/temoins et l'exploitation")
GraphTraitementCommune
ggsave(file.path(FolderDonnees,FolderSortie, "BoxplotTraitementCommune.png"), device = "png")


# Modèle  -----------------------------------------------------------------

#Corrélation 

#GLM sans effet mixte 
Mod_simple = glm(n~year + Num_passag + Modalite_protocole,
                 data = data_contact)

#GLM mixte
Mod_commune = glmmTMB(n ~ year + Num_passag + Modalite_protocole + (1| Commune), 
         data = data_contact,
         family = poisson(link = "log"))

summary(Mod_commune)
Anova(Mod_commune)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod_commune)
plot(simulationOutput) 

#Dispersion des résidus 
testDispersion(simulationOutput)
hist(residuals(Mod_commune))

sim <- simulateResiduals(Mod_commune, n=99)
testDispersion(sim)

#shapiro test
hist(residuals(Mod_commune))
shap<-shapiro.test(residuals(Mod_commune))
shap

#R2 
r2(Mod_commune)

R2<-1-(Mod_commune$deviance/Mod_commune$null.deviance)
R2

#VIF
check_collinearity(Mod_commune) 
car::vif(Mod_commune)
