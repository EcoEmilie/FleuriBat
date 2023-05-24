# Titre : ACPPaysageComposition  
# But : Graphique + ACP avec les variables configuration
# Auteur : Emilie
# Date : 23.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(sf)
library(corrplot)
library(FactoMineR) # Pour effectuer l'ACP
library(factoextra) # Pour visualiser les résultats de l'ACP

# Donnée ------------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"  

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

data_composition = readRDS(file.path(FolderDonnees,FolderInter, "data_composition.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  as.data.frame() 

# 100 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_composition_100 = data_composition %>% 
  filter(dist_buffer == "100") %>% 
  distinct() %>% 
  select(!Shannon_naturel)%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass")

data_composition_100 %>% 
  mutate_if(is.numeric, scale) %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot( method="color", col=col(200),  
            type="upper", order="hclust", 
            addCoef.col = "black", # Ajout du coefficient de corrélation
            tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
            # Combiner avec le niveau de significativité
            diag=FALSE 
  )

ggsave(filename = file.path(FolderDonnees,FolderSortie,"corr_composition_100.png"), device = "png")


## ACP ---------------------------------------------------------------------

resultat_acp_100 <- data_composition_100 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_100$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_100$eig

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_100$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_100)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPeboulie_compo_100.png"), device = "png")

#CP1 x CP2
fviz_pca_var(resultat_acp_100,
             axes = c(1, 2),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_100_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_100,
             axes = c(1, 3),
             repel = TRUE)

ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_100_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_100,
             axes = c(2, 3),
             repel = TRUE)

ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_100_CP2_3.png"), device = "png")

#Représentation avec le traitement
fviz_pca_biplot(resultat_acp_100,
             habillage = data_composition_100$Modalite_protocole,
             addEllipses = TRUE,
             label = "var",
             axes = c(1,2),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_100_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_100,
             habillage = data_composition_100$Modalite_protocole,
             addEllipses = TRUE,
             label = "var",
             axes = c(1,3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_100_CP1_3.png"), device = "png")

fviz_pca_biplot(resultat_acp_100,
             habillage = data_composition_100$Modalite_protocole,
             addEllipses = TRUE,
             label = "var",
             axes = c(2,3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_100_CP2_3.png"), device = "png")

# Contribution 
resultat_acp_100 <- dimdesc(resultat_acp_100, axes = c(1,2), proba = 0.05)

# Description de la dimension 1
resultat_acp_100$Dim.1

# Description de la dimension 2
resultat_acp_100$Dim.2

# 500 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_composition_500 = data_composition %>% 
  st_drop_geometry() %>% 
  filter(dist_buffer == "500") %>% 
  select(!Shannon_naturel) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass")

data_composition_500 %>% 
  mutate_if(is.numeric, scale) %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot( method="color", col=col(200),  
            type="upper", order="hclust", 
            addCoef.col = "black", # Ajout du coefficient de corrélation
            tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
            # Combiner avec le niveau de significativité
            diag=FALSE 
  )

ggsave(file.path(FolderDonnees,FolderSortie,"corr_composition_500.png"),device = "png" )

## ACP ---------------------------------------------------------------------

resultat_acp_500 <- data_composition_500 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_500$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_500$eig

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_500$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_500)

#CP1 x CP2
fviz_pca_var(resultat_acp_500,
             axes = c(1, 2))

ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_500_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_500,
             axes = c(1, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_500_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_500,
             axes = c(2, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_500_CP2_3.png"), device = "png")

#Représentation avec le traitement
fviz_pca_biplot(resultat_acp_500,
                habillage = data_composition_500$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(1,2),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_500_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_500,
                habillage = data_composition_500$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_500_CP1_3.png"), device = "png")

fviz_pca_biplot(resultat_acp_500,
                habillage = data_composition_500$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_500_CP2_3.png"), device = "png")

# Contribution 
resultat_acp_500 <- dimdesc(resultat_acp_500, axes = c(1,2), proba = 0.05)

# Description de la dimension 1
resultat_acp_500$Dim.1

# Description de la dimension 2
resultat_acp_500$Dim.2

# 1000 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_composition_1000 = data_composition %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  filter(dist_buffer == "1000") %>% 
  select(!Shannon_naturel) %>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass")

data_composition_1000_corr = data_composition_1000 %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  corrplot( method="color", col=col(200),  
            type="upper", order="hclust", 
            addCoef.col = "black", # Ajout du coefficient de corrélation
            tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
            # Combiner avec le niveau de significativité
            diag=FALSE 
  )
ggsave(file.path(FolderDonnees,FolderSortie,"corr_composition_1000.png"),device = "png" )

## ACP ---------------------------------------------------------------------

resultat_acp_1000 <- data_composition_1000 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_1000$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_1000$eig

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_1000$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_1000)

#CP1 x CP2
fviz_pca_var(resultat_acp_1000,
             axes = c(1, 2))
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_composition_1000_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_1000,
             axes = c(1, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_1000_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_1000,
             axes = c(2, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_1000_CP2_3.png"), device = "png")

#Représentation avec le traitement
fviz_pca_ind(resultat_acp_1000,
             geom = "point",
             axes = c(1,2))

#Représentation avec le traitement
fviz_pca_biplot(resultat_acp_1000,
                habillage = data_composition_1000$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(1,2),
                repel = TRUE)

ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_1000_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_1000,
                habillage = data_composition_1000$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_1000_CP1_3.png"), device = "png")

fviz_pca_biplot(resultat_acp_1000,
                habillage = data_composition_1000$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_1000_CP2_3.png"), device = "png")


# Contribution 
resultat_acp_1000 <- dimdesc(resultat_acp_1000, axes = c(1,2), proba = 0.05)

# Description de la dimension 1
resultat_acp_1000$Dim.1

# Description de la dimension 2
resultat_acp_1000$Dim.2


# 2000 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_composition_2000 = data_composition %>% 
  st_drop_geometry() %>%  
  distinct() %>% 
  filter(dist_buffer == "2000") %>% 
  select(!Shannon_naturel)  %>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass")

data_composition_2000_corr = data_composition_2000 %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
corrplot( method="color", col=col(200),  
          type="upper", order="hclust", 
          addCoef.col = "black", # Ajout du coefficient de corrélation
          tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
          # Combiner avec le niveau de significativité
          diag=FALSE 
)
ggsave(file.path(FolderDonnees,FolderSortie,"corr_composition_2000.png"),device = "png" )

## ACP ---------------------------------------------------------------------

resultat_acp_2000 <- data_composition_2000 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_2000$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_2000$eig

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_2000$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_2000)

#CP1 x CP2
fviz_pca_var(resultat_acp_2000,
             axes = c(1, 2))
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_2000_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_2000,
             axes = c(1, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_2000_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_2000,
             axes = c(2, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_compo_2000_CP2_3.png"), device = "png")

#Représentation avec le traitement
fviz_pca_biplot(resultat_acp_2000,
                habillage = data_composition_2000$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(1,2),
                repel = TRUE)

ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_2000_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_2000,
                habillage = data_composition_2000$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_2000_CP1_3.png"), device = "png")

fviz_pca_biplot(resultat_acp_2000,
                habillage = data_composition_2000$Modalite_protocole,
                addEllipses = TRUE,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_compo_2000_CP2_3.png"), device = "png")

# Contribution 
resultat_acp_2000 <- dimdesc(resultat_acp_2000, axes = c(1,2), proba = 0.05)

# Description de la dimension 1
resultat_acp_2000$Dim.1

# Description de la dimension 2
resultat_acp_2000$Dim.2
