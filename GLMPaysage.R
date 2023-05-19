# Titre : GLMpaysage  
# But : Modèle avec les variables paysages 
# Auteur : Emilie PEN
# Date : 19/05/2023

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

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds"))  %>% 
  mutate(Commune = str_to_upper(Commune))

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds"))

data_total = left_join(data_chiro, data_site) 

data_contact = data_total %>% 
  filter(!Modalite_protocole == "exclos")%>% 
  dplyr :: select(carre_year_pass,year, Modalite_protocole,Num_passag,  Commune) %>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = "sum_contact") %>% 
  distinct()

data_pay = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  st_drop_geometry()

data_pay_100 = data_pay %>% 
  filter(dist_buffer  == "100") %>% 
  select(carre_year_pass, area_praitemp, dist_eau)

data_pay_500 = data_pay %>% 
  filter(dist_buffer  == "500") %>% 
  select(carre_year_pass, dist_foret, nb_parcelle) %>% 
  rename(nb_parcelle_500 = nb_parcelle)

data_pay_1000 = data_pay %>% 
  filter(dist_buffer == "1000") %>% 
  select(carre_year_pass, perimeter_agri, nb_parcelle) %>% 
  rename(nb_parcelle_1000 = nb_parcelle)

data_pay_2000 = data_pay %>% 
  filter(dist_buffer == "2000") %>% 
  select(carre_year_pass, area_praiperm, Shannon_cultu) 


data_paysage = left_join(data_pay_100, data_pay_500) %>% 
  left_join(data_pay_1000) %>% 
  left_join(data_pay_2000)

data_mod = inner_join(data_contact, data_paysage) %>% 
  distinct()

# Modèle  -----------------------------------------------------------------

#Corrélation 

#GLM mixte
Mod_dep = glmmTMB(sum_contact ~ year + Num_passag + Modalite_protocole + area_praitemp + dist_eau + 
                    dist_foret + nb_parcelle_500 + perimeter_agri + nb_parcelle_1000 + area_praiperm + Shannon_cultu +
                    (1| Commune), 
                      data = data_mod,
                      family = poisson(link = "log"))

summary(Mod_dep)
Anova(Mod_dep)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod_dep)
plot(simulationOutput) 

#Dispersion des résidus 
testDispersion(simulationOutput)
hist(residuals(Mod_dep))

sim <- simulateResiduals(Mod_dep, n=99)
testDispersion(sim)

#shapiro test
hist(residuals(Mod_dep))
shap<-shapiro.test(residuals(Mod_dep))
shap

#R2 
r2(Mod_dep)

#VIF
check_collinearity(Mod_dep) 
