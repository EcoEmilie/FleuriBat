# Titre : GLMDiversitePlante
# But : Modèle GLM sur l'effet de la diversité de plante
# Auteur : Emilie
# Date : 11.05.2023


rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot
library(glmmTMB)#pour faire des glmmTMB : negative binomiale
library(performance)#pour calculer le VIF
library(car)
library(units)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"


data_div = readRDS(file.path(FolderDonnees,FolderInter, "Div_Plante_Shannon.rds")) %>% 
  distinct() 


data_contact = readRDS(file.path(FolderDonnees,FolderInter, "data_sumcontact.rds")) %>% 
  filter(Modalite_protocole == "bande") 
  
                   
 data_div$Commune == data_contact$Commune

data_bande = readRDS(file.path(FolderDonnees,FolderInter,"data_bande.rds")) %>% 
  mutate(Longueur = area/Largeur)
  
data_total= left_join(data_contact, data_div) %>% 
  left_join(data_bande) %>% 
  filter(!is.na(Indi_Shannon)) %>% 
  distinct()

data_Shannon = data_total %>% 
  filter(!is.na(area)) %>% 
  distinct() %>% 
  dplyr :: select(carre_year_pass, Commune, year, Num_passag, SDC, sum_contact, Indi_Shannon, Longueur)# %>% 
  # group_by(Commune) %>% 
  # tally()


# Graphique ---------------------------------------------------------------

ggplot(data_Shannon)+
  aes(x = Indi_Shannon, y = sum_contact)+ 
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x = "Diversité végétale (Indice de Shannon)",
       y = "Nombre de contact",
       title = "Le nombre de contact selon la diversité végétale")

ggsave(file.path(FolderDonnees,FolderSortie, "RegressionDivPlanteSumContact.png"), device = "png")

hist(data_Shannon$Indi_Shannon)


# Modèle  -----------------------------------------------------------------


Mod = glmmTMB(sum_contact ~  Num_passag + Indi_Shannon + Longueur + (1| year), 
              data = data_Shannon,
              family = poisson(link = "log"))

Mod = glmmTMB(sum_contact ~  Num_passag + Indi_Shannon + Longueur + (1| year), 
              data = data_Shannon,
              family = nbinom1(link = "log"))

summary(Mod)
Anova(Mod)


# ggplot(data=data_Shannon, aes(x = sqarea, y = exp(predict(Mod))))+ 
#   geom_point()+
#   geom_smooth(method="lm",se=T,span=2)+
#   labs(x = "Diversité végétale (Indice de Shannon)",
#        y = "Nombre de contact",
#        title = "Le nombre de contact selon la diversité végétale")
# 
# predict(Mod)


# gma1<-ggplot(data_Shannon, aes(x=data_Shannon$squarea, y=exp(predict(Mod))))+ #again, excluding intercept because estimates so much larger
#   geom_smooth(method="lm",se=T,span=2)+
#   # scale_x_continuous(trans='log2')+
#   geom_point(size=1)+theme_classic(base_size = 10)+ ylab("nb de contacts (log)")+xlab("Surface des bandes (racine carr?e")
# gma1

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

png(file.path(FolderDonnees,FolderSortie,"DHARMADivPlanteSumContact.png"),
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
