# Titre : GLM paysage 100m 
# But : GLM + ACP avec les variables paysages 
# Auteur : Emilie
# Date : 25.05.2023

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
library(MuMIn)
library(FactoMineR)
library(factoextra)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_contact = readRDS(file.path(FolderDonnees, FolderInter, "data_sumcontact.rds")) 
data_richesse = readRDS(file.path(FolderDonnees, FolderInter, "data_RichesseSpe.rds")) 

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

data_paysage = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos")  %>% 
  st_drop_geometry()

data_naturel = readRDS(file.path(FolderDonnees,FolderInter, "data_varpaysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer))%>% 
  st_drop_geometry() %>% 
  as.data.frame()

data_paysage_1000 = left_join(data_paysage,data_naturel) %>% 
  filter(dist_buffer == "1000") %>% 
  distinct()%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass")  %>%
  select_if(is.numeric) %>% 
  filter(!is.na(nb_naturel))

summary(data_paysage_1000) 
#area_habitation,area_praitemp

data_paysage_1000 = data_paysage_1000 %>% 
  dplyr::select(!c(area_habitation,buffer_area))

# 1000 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_paysage_1000_corr = data_paysage_1000 %>% 
  select_if(is.numeric) %>% 
  cor()%>% 
  corrplot( method="color", col=col(200),  
            type="upper", #order="hclust", 
            addCoef.col = "black", # Ajout du coefficient de corrélation
            tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
            # Combiner avec le niveau de significativité
            diag=FALSE 
  )

ggsave(file.path(FolderDonnees,FolderSortie,"corr_paysage_1000.png"),device = "png" )

## ACP ---------------------------------------------------------------------

resultat_acp_1000 <- data_paysage_1000 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_1000$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_1000$eig#3 composantes

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_1000$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_1000)

#CP1 x CP2
fviz_pca_var(resultat_acp_1000,
             axes = c(1, 2),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_1000_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_1000,
             axes = c(1, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_1000_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_1000,
             axes = c(2, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_1000_CP2_3.png"), device = "png")

#Représentation avec le traitement

fviz_pca_biplot(resultat_acp_1000,
                label = "var",
                axes = c(1,2),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_1000_CP1_2.png"), device = "png", width=4, height=7)

fviz_pca_biplot(resultat_acp_1000,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_1000_CP1_3.png"), device = "png", width=4, height=7)


fviz_pca_biplot(resultat_acp_1000,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_1000_CP2_3.png"), device = "png", width=4, height=7)

# Contribution 
resultat_acp_1000_A <- dimdesc(resultat_acp_1000, axes = c(1,2), proba = 0.05)
resultat_acp_1000_B <- dimdesc(resultat_acp_1000, axes = c(1,3), proba = 0.05)


# Description de la dimension 1
resultat_acp_1000_A$Dim.1 # perimeter_agri,area_agri,moy_area_agri,nb_naturel   

# Description de la dimension 2
resultat_acp_1000_A$Dim.2 # area_BIO,area_praiperm,area_prairie

# Description de la dimension 3
resultat_acp_1000_B$Dim.3 #nb_parcelle, moy_area_agri, area_BIO

hist(data_paysage_1000$area_BIO)
# Données -----------------------------------------------------------------

data_paysage_1000 = data_paysage_1000 %>% 
  mutate_if(is.numeric, scale) %>% 
  rownames_to_column(var = "carre_year_pass")

data_mod = left_join(data_contact, data_paysage_1000)

data_mod_spe = left_join(data_richesse, data_paysage_1000) %>% 
  column_to_rownames(var = "carre_year_pass") %>% 
  drop_na() %>% 
  mutate(Commune = as.factor(Commune)) 


# Modèle  -----------------------------------------------------------------

#Corrélation 

#####GLM mixte + Commune #####
Mod = glmmTMB(sum_contact ~ Num_passag + Modalite_protocole + SDC + perimeter_agri + nb_parcelle +area_BIO + 
                (1| year/Commune), 
              data = data_mod,
              family = nbinom1(link = "log"))

# Mod1 = glmmTMB(sum_contact ~ Num_passag + moy_area_agri + area_agri  + area_BIO  +
#                 (1| year/Commune), 
#               data = data_mod,
#               family = poisson(link = "log"))#DHARMA non 
# 
# Mod1 = glmer.nb(sum_contact ~ Num_passag + perimeter_agri + Shannon_cultu  + area_BIO + 
#                  (1| year/Commune), 
#                data = data_mod)

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) # ok

png(file.path(FolderDonnees,FolderSortie,"DHARMAPaysage1000mSumContact.png"),
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

#####GLM mixte: Richesse spécifique  #####
Mod = lmer(Richesse_spe ~ Num_passag + Modalite_protocole + SDC + perimeter_agri + nb_parcelle +area_BIO + 
                (1| year/Commune), 
              data = data_mod_spe)

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput) #homogénéisation des variances 

png(file.path(FolderDonnees,FolderSortie,"DHARMAPaysage1000mRichesseSpe.png"),
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
r2(Mod)

#VIF
check_collinearity(Mod) 






