# Titre : VariableDuréeCumulée 
# But : GLM de la durée cummulée de contact avec les PIPPIP
# Auteur : Emilie
# Date : 22/05/2023

rm(list=ls())


# Library -----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot
library(glmmTMB)#pour faire des glmmTMB : negative binomiale
library(performance)#pour calculer le VIF
library(car)


# Chargement données ------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_dureecum = readRDS(file.path(FolderDonnees,FolderInter, "data_dureecum.rds"))

data_dureecum_pippip = data_dureecum %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  filter(tadarida_taxon == "Pippip")

# Graphique ---------------------------------------------------------------

ggplot(data_dureecum_pippip)+
  aes(x = log(Duree_tot))+
  geom_histogram(colour="black", fill="gray")+
  labs( x = "log de la Durée cumulée (s) ", y = "Fréquences", title = "Distribution de la durée cumulée des Pippip")

ggplot(data_dureecum_pippip)+
  aes(y = log (Duree_tot), x = Modalite_protocole, fill = Modalite_protocole)+
  geom_boxplot()+
  labs( x = "Traitement", y = "log de la Durée cumulée", title = "Durée cummulée des pippip selon le traitement ")

# Modèle  -----------------------------------------------------------------

#GLM négative binomiale 
Mod = glmer.nb(Duree_tot ~ year + Num_passag + Modalite_protocole + (1| Commune), 
              data = data_dureecum_pippip)

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

#Dispersion des résidus 
testDispersion(simulationOutput)
hist(residuals(Mod))

sim <- simulateResiduals(Mod, n=99)
testDispersion(sim)

#shapiro test
hist(residuals(Mod))
shap<-shapiro.test(residuals(Mod))
shap

#R2 
r2(Mod)

#VIF
check_collinearity(Mod) 

