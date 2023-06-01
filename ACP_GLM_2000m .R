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
library(MuMIn)#R2
library(FactoMineR)#ACP
library(factoextra)#ACP



# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_contact = readRDS(file.path(FolderDonnees, FolderInter, "data_sumcontact.rds")) 

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

data_paysage = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  dplyr::select(!dist_eau.1) %>% 
  st_drop_geometry()

data_naturel = readRDS(file.path(FolderDonnees,FolderInter, "data_varpaysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer))%>% 
  st_drop_geometry() %>% 
  as.data.frame()

data_paysage_2000 = left_join(data_paysage,data_naturel) %>% 
  filter(dist_buffer == "2000") %>% 
  distinct() %>% 
  dplyr::select(!Shannon_naturel)%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass")  %>%
  select_if(is.numeric) %>% 
  filter(!is.na(nb_naturel))

summary(data_paysage_2000) 
#area_habitation,area_praiperm,area_praitemp

# 2000 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_paysage_2000 = data_paysage %>% 
  filter(dist_buffer == "2000") %>% 
  distinct() %>% 
  dplyr::select(!Shannon_naturel)%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass")

data_paysage_2000_corr = data_paysage_2000 %>% 
  select_if(is.numeric) %>% 
  cor()%>% 
  corrplot( method="color", col=col(200),  
            type="upper", order="hclust", 
            addCoef.col = "black", # Ajout du coefficient de corrélation
            tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
            # Combiner avec le niveau de significativité
            diag=FALSE 
  )
ggsave(file.path(FolderDonnees,FolderSortie,"corr_paysage_2000.png"),device = "png" )

## ACP ---------------------------------------------------------------------

resultat_acp_2000 <- data_paysage_2000 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_2000$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_2000$eig#3 composantes

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_2000$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_2000)

#CP1 x CP2
fviz_pca_var(resultat_acp_2000,
             axes = c(1, 2),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_2000_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_2000,
             axes = c(1, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_2000_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_2000,
             axes = c(2, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_2000_CP2_3.png"), device = "png")

#Représentation avec le traitement
fviz_pca_biplot(resultat_acp_2000,
                label = "var",
                axes = c(1,2),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_2000_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_2000,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_2000_CP1_3.png"), device = "png")


fviz_pca_biplot(resultat_acp_2000,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_2000_CP2_3.png"), device = "png")

# Contribution 
resultat_acp_2000_A <- dimdesc(resultat_acp_2000, axes = c(1,2), proba = 0.05)
resultat_acp_2000_B <- dimdesc(resultat_acp_2000, axes = c(1,3), proba = 0.05)


# Description de la dimension 1
resultat_acp_2000_A$Dim.1 # area_prairie  

# Description de la dimension 2
resultat_acp_2000_A$Dim.2 # nb_parcelle, moy_area_agri

# Description de la dimension 3
resultat_acp_2000_B$Dim.3 #area_BIO


# Données -----------------------------------------------------------------

data_paysage_2000 = data_paysage_2000 %>% 
  rownames_to_column(var = "carre_year_pass")

data_mod = left_join(data_contact, data_paysage_2000)

summary(data_mod)

# Modèle  -----------------------------------------------------------------

#Corrélation 



#####GLM mixte + Commune #####

Mod = glmmTMB(sum_contact ~ Num_passag + area_prairie + nb_parcelle + area_BIO +
                (1| year/Commune),
              data = data_mod,
              family = poisson(link = "log")) #Dispersion test significatif

Mod = glmer.nb(sum_contact ~ Num_passag + Modalite_protocole + area_prairie + nb_parcelle + area_BIO +
                (1| year/Commune),
              data = data_mod) #Dharma ok

Mod = glmer.nb(sum_contact ~ Num_passag + area_prairie + nb_parcelle + area_BIO +
                 (1| year/Commune),
               data = data_mod) #Dharma ok

summary(Mod)
Anova(Mod)

#Résidus 
simulationOutput <- simulateResiduals(fittedModel = Mod)
plot(simulationOutput)

png(file.path(FolderDonnees,FolderSortie,"DHARMAPaysage2000mSumContact.png"),
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

