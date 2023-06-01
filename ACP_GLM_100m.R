# Titre : GLM paysage 100m 
# But : GLM + ACP avec les variables paysages 
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
library(FactoMineR)
library(factoextra)
library(GLMMadaptive)



# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_contact = readRDS(file.path(FolderDonnees, FolderInter, "data_sumcontact.rds")) 

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

data_paysage = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  st_drop_geometry() %>% 
  as.data.frame()

data_naturel = readRDS(file.path(FolderDonnees,FolderInter, "data_varpaysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer))%>% 
  st_drop_geometry() %>% 
  as.data.frame()


data_paysage_100 = left_join(data_paysage, data_naturel)%>% 
  filter(dist_buffer == "100") %>% 
  distinct() %>% 
  dplyr::select(!Shannon_naturel)%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass") %>% 
  filter(!is.na(nb_naturel))

summary(data_paysage_100)
#buffer_area,area_foret,area_habitation,area_prairie,area_praiperm,area_praitemp,route_density

data_acp = data_paysage_100 %>% 
  dplyr::select(!c(buffer_area,area_foret,area_habitation,area_prairie,area_praiperm,area_praitemp,haie_density,route_density))
  


# 100 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_acp %>% 
  mutate_if(is.numeric, scale) %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot( method="color", col=col(200),  
            type="upper",order="hclust", 
            addCoef.col = "black", # Ajout du coefficient de corrélation
            tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
            # Combiner avec le niveau de significativité
            diag=FALSE 
  )

ggsave(filename = file.path(FolderDonnees,FolderSortie,"corr_paysage_100.png"), device = "png")


## ACP ---------------------------------------------------------------------

resultat_acp_100 <- data_acp %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_100$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_100$eig# On prend 3 composantes principales 

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_100$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_100)

#CP1 x CP2
fviz_pca_var(resultat_acp_100,
             axes = c(1, 2),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_100_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_100,
             axes = c(1, 3),
             repel = TRUE)

ggsave(file.path(FolderDonnees,FolderSortie,"ACP_100_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_100,
             axes = c(2, 3),
             repel = TRUE)

ggsave(file.path(FolderDonnees,FolderSortie,"ACP_100_CP2_3.png"), device = "png")

#Représentation avec le traitement
fviz_pca_biplot(resultat_acp_100,
                label = "var",
                axes = c(1,2),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_100_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_100,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_100_CP1_3.png"), device = "png")


fviz_pca_biplot(resultat_acp_100,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_100_CP2_3.png"), device = "png")


# Contribution 
resultat_acp_100_A <- dimdesc(resultat_acp_100, axes = c(1,2), proba = 0.05)
resultat_acp_100_B <- dimdesc(resultat_acp_100, axes = c(1,3), proba = 0.05)
resultat_acp_100_C <- dimdesc(resultat_acp_100, axes = c(1,4), proba = 0.05)

# Description de la dimension 1
resultat_acp_100_A$Dim.1 # perimeter_agri,moy_area_agri,nb_parcelle -> agricole 

# Description de la dimension 2
resultat_acp_100_A$Dim.2 # dist_foret, dist_haie, Shannon_cultu -> Hetérogénéité 

# Description de la dimension 3
resultat_acp_100_B$Dim.3 #area_agri , dist_eau,   area_naturel -> distance au zone urbaine 




# Données -----------------------------------------------------------------
  
data_acp = data_paysage_100%>% 
  dplyr :: select(perimeter_agri,moy_area_agri,dist_foret,dist_habitation,dist_eau,area_BIO,Shannon_cultu) %>% 
  rownames_to_column(var = "carre_year_pass") %>% 
  mutate(dist_foret = as.numeric(dist_foret),
         dist_habitation = as.numeric(dist_habitation))

data_mod = left_join(data_contact, data_acp) %>% 
  column_to_rownames(var = "carre_year_pass") %>% 
  drop_na() %>% 
  mutate(Commune = as.factor(Commune))# %>% 
  #mutate_if(is.numeric, scale) 



# Modèle  -----------------------------------------------------------------

hist(data_mod$perimeter_agri)
hist(data_mod$dist_foret)
hist(data_mod$dist_habitation)
hist(data_mod$sum_contact)

#####GLM poisson : effet simple #####
Mod = glmmTMB(sum_contact ~ Num_passag + moy_area_agri + Shannon_cultu  + area_BIO + (1| year/Commune), 
              data = data_mod,
              family = poisson(link = "log"))

Mod = glmmTMB(sum_contact ~ Num_passag + moy_area_agri + Shannon_cultu  + area_BIO + (1| year/Commune), 
                  data = data_mod,
                  family = nbinom1(link = "log"))#probléme d'homogénéité de la variance 

Mod = glmer.nb(log(sum_contact) ~ Num_passag + moy_area_agri + Shannon_cultu + Shannon_cultu + (1| year/Commune),
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


#####GLM poisson : interaction  #####
Mod = glmer(sum_contact ~ Num_passag + Modalite_protocole* moy_area_agri + Modalite_protocole*dist_foret + Modalite_protocole*dist_habitation +
                (1| year/Commune), 
              data = data_mod,
              family = poisson(link = "log"))




summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) #homogéinisation de la variance

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




