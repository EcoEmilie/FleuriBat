# Titre : Modèle finale  
# But : GLM avec les variables paysages séléctionnées 
# Auteur : Emilie
# Date : 25.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(units)
library(lme4)
library(sf)
library(lmerTest) # Pour tests d'effets aléatoires dans le modèle mixte
library(DHARMa)#QQ plot
library(glmmTMB)#pour faire des glmmTMB : negative binomiale
library(performance)#pour calculer le VIF
library(car)
library(MuMIn)
library(corrplot)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_contact = readRDS(file.path(FolderDonnees, FolderInter, "data_sumcontact.rds")) 

data_paysage = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  dplyr :: select(!dist_eau.1) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  dplyr :: select(carre_year_pass, dist_buffer,dist_foret, nb_parcelle, route_density, perimeter_agri, area_BIO, area_prairie)

data_100 = data_paysage %>% 
  filter(dist_buffer == "100") %>% 
  dplyr::select(carre_year_pass, moy_area_agri, Shannon_cultu, area_BIO ) %>% 
  rename( moy_area_agri_100 = moy_area_agri,
          area_BIO_100 = area_BIO)

data_500 = data_paysage %>% 
  filter(dist_buffer == "500") %>% 
  dplyr::select(carre_year_pass,nb_naturel, moy_area_agri, route_density) %>% 
  rename( nb_naturel_500 = nb_naturel,
          moy_area_agri_500 = moy_area_agri)

data_1000 = data_paysage %>% 
  filter(dist_buffer == "1000") %>% 
  dplyr::select(carre_year_pass,perimeter_agri, nb_parcelle, area_BIO) %>% 
  rename( nb_parcelle_1000 = nb_parcelle,
          area_BIO_1000 = area_BIO)

data_2000 = data_paysage %>% 
  filter(dist_buffer == "2000") %>% 
  dplyr::select(carre_year_pass,area_prairie)

data_mod = left_join(data_contact, data_500) %>% 
  left_join(data_1000) %>% 
  left_join(data_2000) 

# Modèle  -----------------------------------------------------------------


#####GLM poisson : effet simple #####
Mod = glmmTMB(sum_contact ~ Num_passag + dist_foret + nb_parcelle_500 + route_density +perimeter_agri + nb_parcelle_1000 +
                area_BIO + area_prairie + Modalite_protocole + SDC + (1| year/Commune), 
              data = data_mod,
              family = poisson(link = "log"))#probléme d'homogénéité de la variance 

Mod = glmer.nb(sum_contact ~ Num_passag + dist_foret + nb_parcelle_500 + route_density +perimeter_agri + nb_parcelle_1000 +
                 area_BIO + area_prairie + Modalite_protocole + SDC + (1| year/Commune), 
               data = data_mod)

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

png(file.path(FolderDonnees,FolderSortie,"DHARMAPaysage100mSumContact.png"),
    width=1200, height=700)
plot(simulationOutput) 
dev.off()

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
r.squaredGLMM(Mod)

#VIF
check_collinearity(Mod) 










