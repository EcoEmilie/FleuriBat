# Titre : GLMpaysage  
# But : Modèle avec les variables paysages 
# Auteur : Emilie PEN
# Date : 19/05/2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(units)
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
  dplyr ::select(carre_year_pass, area_praitemp, dist_eau)

data_pay_500 = data_pay %>% 
  filter(dist_buffer  == "500") %>% 
  dplyr ::select(carre_year_pass, dist_foret, nb_parcelle) %>% 
  rename(nb_parcelle_500 = nb_parcelle)

data_pay_1000 = data_pay %>% 
  filter(dist_buffer == "1000") %>% 
  dplyr ::select(carre_year_pass, perimeter_agri, nb_parcelle) %>% 
  rename(nb_parcelle_1000 = nb_parcelle)

data_pay_2000 = data_pay %>% 
  filter(dist_buffer == "2000") %>% 
  dplyr ::select(carre_year_pass, area_praiperm, Shannon_cultu) 


data_paysage = left_join(data_pay_100, data_pay_500) %>% 
  left_join(data_pay_1000) %>% 
  left_join(data_pay_2000)

data_mod = inner_join(data_contact, data_paysage) %>% 
  distinct()

# Graphique ---------------------------------------------------------------

# GraphSumContact = function(data, x, y, remp, name_x){
#   ggplot(data)+
#     aes(x = x ,y = y, color = remp )+
#     geom_point()+
#     geom_smooth(method = "loess", se = FALSE)+
#     labs(x = name_x, 
#          y = "Nombre de contact",
#          title = title = paste("Distribution", name))
#   ggsave(file.path(FolderDonnees,FolderSortie, paste(name, ".png")), device = "png")
# }
# 
# GraphAreaprintempSum = GraphSumContact(data_mod, data_mod$area_praitemp, data_mod$sum_contact, data_mod$Modalite_protocole, "Surface de Prairie temporaire")
# GraphAreaprintempSum
# 
# GrapphDistEauSum = GraphSumContact(data_mod, data_mod$dist_eau, data_mod$sum_contact, data_mod$Modalite_protocole, "Distance a leau")
# GrapphDistEauSum
# 
# GraphDistForetSum = GraphSumContact(data_mod, data_mod$dist_foret, data_mod$sum_contact, data_mod$Modalite_protocole, "Distance à la foret")
# GraphDistForetSum
# 
# GraphNbParcelle500Sum = GraphSumContact(data_mod, data_mod$nb_parcelle_500, data_mod$sum_contact, data_mod$Modalite_protocole,
#                                         "Nombre de parcelle dans 500m")
# GraphNbParcelle500Sum
# 
# data_var = data_mod %>%
#   dplyr::select(!carre_year_pass) %>% 
#   dplyr::select_if(is.numeric) 
#   
# for (i in 1:ncol(data_mod)){
#   var = data_mod[,i]
#   name=names[i]
#   fill = randomColor()
#   GraphSumContact(data_mod, var,  data_mod$sum_contact , fill, name)
# }


# Modèle  -----------------------------------------------------------------

#Corrélation 

#####GLM mixte + Commune #####
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

#####GLM mixte négative binomiale - Commune #####
Mod_nb = glm.nb(sum_contact ~ year + Num_passag + Modalite_protocole + area_praitemp + dist_eau + 
                    dist_foret + nb_parcelle_500 + perimeter_agri + nb_parcelle_1000 + area_praiperm + Shannon_cultu, 
                  data = data_mod)

summary(Mod_nb)
Anova(Mod_nb)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod_nb)
plot(simulationOutput) 

#Dispersion des résidus 
testDispersion(simulationOutput)
hist(residuals(Mod_nb))

sim <- simulateResiduals(Mod_nb, n=99)
testDispersion(sim)

#shapiro test
hist(residuals(Mod_nb))
shap<-shapiro.test(residuals(Mod_nb))
shap

#R2 
r2(Mod_nb)

#VIF
check_collinearity(Mod_nb) 
