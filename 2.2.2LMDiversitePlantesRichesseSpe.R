# Titre : GLMDiversitePlanteRichesseSpe
# But : Modèle GLM sur l'effet de la diversité de plante sur la richesse spécifique 
# Auteur : Emilie
# Date : 24.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot
library(glmmTMB)#pour faire des glmmTMB : négative binomiale
library(performance)#pour calculer le VIF
library(car)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"


data_div = readRDS(file.path(FolderDonnees,FolderInter, "Div_Plante_Shannon.rds")) 

data_richesse = readRDS(file.path(FolderDonnees,FolderInter, "data_RichesseSpe.rds"))

data_bande = readRDS(file.path(FolderDonnees,FolderInter,"data_bande.rds"))%>% 
  mutate(Longueur = area/Largeur)

data_total= left_join(data_richesse, data_div) %>%
  left_join(data_bande) %>% 
  filter(!is.na(Indi_Shannon)) %>% 
  filter(Modalite_protocole == "bande") %>% 
  distinct()

data_Shannon = data_total  %>% 
  distinct()

# Graphique ---------------------------------------------------------------

ggplot(data_Shannon)+
  aes(x = Indi_Shannon, y = Richesse_spe)+ 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Diversité végétale (Indice de Shannon)",
       y = "Richesse spécifique",
       title = "Le richesse spécifique selon la diversité végétale")

ggsave(file.path(FolderDonnees,FolderSortie, "RegressionDivPlanteRichesseSpe.png"), device = "png")

hist(data_Shannon$Indi_Shannon)


# Modèle ------------------------------------------------------------------


#Lmer 

Mod = lmer(Richesse_spe ~  Num_passag + Indi_Shannon + Longueur + (1| year), 
           data = data_Shannon)

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

png(file.path(FolderDonnees,FolderSortie,"DHARMADivPlanteRichesseSpe.png"),
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


