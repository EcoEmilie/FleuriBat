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
  
data_pay = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) 

data_total = left_join(data_chiro, data_site)

data_contact = data_total %>% 
  filter(!Modalite_protocole == "exclos")%>% 
  dplyr :: select(carre_year_pass, Modalite_protocole,Num_passag, year, Commune) %>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = "sum_contact") %>% 
  distinct()

data_paysage = inner_join(data_contact, data_pay) %>% 
  distinct() %>% 
  mutate(dist_buffer = as.factor(dist_buffer))

# Graphique ---------------------------------------------------------------

#Annees
GraphTraitement= ggplot(data_contact)+
  aes(x = year ,y = sum_contact, fill = year )+
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

# Modèle  -----------------------------------------------------------------

#Corrélation 

#GLM sans effet mixte 
Mod_simple = glm(sum_contact~year + Num_passag + Modalite_protocole,
                 data = data_paysage)

#GLM mixte
Mod_commune = glmmTMB(sum_contact ~ year + Num_passag + Modalite_protocole + (1| Commune), 
         data = data_paysage,
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

#VIF
check_collinearity(Mod_commune) 

#GLM mixte : year, NUm_passag, modalite_protocole, commune + BIO

Mod_BIO = glmmTMB(sum_contact ~ year + Num_passag + Modalite_protocole + area_BIO +  (1| Commune), 
                      data = data_paysage,
                      family = poisson(link = "log"))

summary(Mod_BIO )
Anova(Mod_BIO )

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod_BIO)
plot(simulationOutput) 

#Dispersion des résidus 
testDispersion(simulationOutput)
hist(residuals(Mod_BIO))

sim <- simulateResiduals(Mod_BIO, n=99)
testDispersion(sim)

#shapiro test
hist(residuals(Mod_BIO))
shap<-shapiro.test(residuals(Mod_BIO))
shap

#R2 
r2(Mod_BIO)

#VIF
check_collinearity(Mod_BIO) 

#GLM mixte : year, NUm_passag, modalite_protocole, commune + dist_foret

Mod_foret = glmmTMB(sum_contact ~ year + Num_passag + Modalite_protocole + dist_foret +  (1| Commune), 
                  data = data_paysage,
                  family = poisson(link = "log"))

summary(Mod_foret )
Anova(Mod_foret )

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod_foret)
plot(simulationOutput) 

#Dispersion des résidus 
testDispersion(simulationOutput)
hist(residuals(Mod_foret))

sim <- simulateResiduals(Mod_foret, n=99)
testDispersion(sim)

#shapiro test
hist(residuals(Mod_foret))
shap<-shapiro.test(residuals(Mod_foret))
shap

#R2 
r2(Mod_foret)

#VIF
check_collinearity(Mod_foret) 
