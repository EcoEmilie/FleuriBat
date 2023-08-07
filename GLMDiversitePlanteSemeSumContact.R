# Titre : GLMDiversitePLanteSemeSumContact
# But : Modèle avec la diversité de plantes seme dans la bande sur le nombre de contact 
# Auteur : Emilie PEN
# Date : 24/05/2023

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


data_div = readRDS(file.path(FolderDonnees,FolderInter, "Div_Plante_Shannon.rds")) 

data_contact = readRDS(file.path(FolderDonnees,FolderInter, "data_sumcontact.rds")) %>% 
  filter(Modalite_protocole == "bande")

data_total= left_join(data_contact, data_div) %>% 
  filter(!is.na(Indi_Shannon)) %>% 
  distinct()

data_Shannon = data_total %>% 
  select(!c(Indi_Shannon)) %>% 
  mutate(seme = as.factor(seme)) %>% 
  distinct()


# Graphique ---------------------------------------------------------------

ggplot(data_Shannon)+
  aes(x = Indi_seme_Shannon, y = sum_contact, color = seme)+ 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Diversité végétale (Indice de Shannon)",
       y = "Nombre de contact",
       title = "Le nombre de contact selon la diversité végétale")

ggsave(file.path(FolderDonnees,FolderSortie, "RegressionDivPlanteSemeSumContact.png"), device = "png")

hist(data_Shannon$Indi_Shannon)

# Modèle  -----------------------------------------------------------------

Mod = glmmTMB(sum_contact ~  Num_passag + seme + Indi_seme_Shannon + (1|year/Commune), 
              data = data_Shannon,
              family = poisson(link = "log"))

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

png(file.path(FolderDonnees,FolderSortie,"DHARMADivPlanteSemeSumContact.png"),
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
