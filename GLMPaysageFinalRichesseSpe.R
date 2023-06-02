# Titre : Modèle finale  paysage SumContact
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

data_richesse = readRDS(file.path(FolderDonnees, FolderInter, "data_RichesseSpe.rds")) 

data_paysage = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, scale)


data_500 = data_paysage %>% 
  filter(dist_buffer == "500") %>% 
  dplyr::select(carre_year_pass, route_density) %>% 
  rename( route_density_500 = route_density)

data_2000 = data_paysage %>% 
  filter(dist_buffer == "2000") %>% 
  dplyr::select(carre_year_pass, area_agri, Shannon_cultu) %>% 
  rename(area_agri_2000 = area_agri)

data_mod = left_join(data_richesse, data_500)%>% 
  left_join(data_2000) 

# Modèle  -----------------------------------------------------------------


#####GLM mixte : NOmbre de contact #####
Mod = lmer(Richesse_spe ~ Num_passag + route_density_500  + area_agri_2000  + Modalite_protocole + SDC + (1| year/Commune), 
              data = data_mod)

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) 

png(file.path(FolderDonnees,FolderSortie,"DHARMAPaysageFinaleRichesseSpe.png"),
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
