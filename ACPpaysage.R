# Titre : ACP paysage 
# But : Graphique + ACP avec les variables paysages 
# Auteur : Emilie
# Date : 12.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(sf)
library(FactoMineR) # Pour effectuer l'ACP
library(factoextra) # Pour visualiser les résultats de l'ACP

# Donnée ------------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"  

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

data_paysage = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  select(!dist_eau.1) %>% 
  st_drop_geometry()

# 100 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_paysage_100 = data_paysage %>% 
  filter(dist_buffer == "100") %>% 
  distinct() %>% 
  select(!Shannon_naturel)%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass") 

data_paysage_100 %>% 
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

ggsave(filename = file.path(FolderDonnees,FolderSortie,"corr_paysage_100.png"), device = "png")


## ACP ---------------------------------------------------------------------

resultat_acp_100 <- data_paysage_100 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_100$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_100$eig# On prend 4 composantes principales 

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

fviz_pca_biplot(resultat_acp_100,
                label = "var",
                axes = c(1,4),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_100_CP1_4.png"), device = "png")

fviz_pca_biplot(resultat_acp_100,
                label = "var",
                axes = c(2,4),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_100_CP2_4.png"), device = "png")

fviz_pca_biplot(resultat_acp_100,
                label = "var",
                axes = c(3,4),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_100_CP3_4.png"), device = "png")


# Contribution 
resultat_acp_100_A <- dimdesc(resultat_acp_100, axes = c(1,2), proba = 0.05)
resultat_acp_100_B <- dimdesc(resultat_acp_100, axes = c(1,3), proba = 0.05)
resultat_acp_100_C <- dimdesc(resultat_acp_100, axes = c(1,4), proba = 0.05)

# Description de la dimension 1
resultat_acp_100_A$Dim.1 # area_prairie 

# Description de la dimension 2
resultat_acp_100_A$Dim.2 # area_praitemp

# Description de la dimension 3
resultat_acp_100_B$Dim.3 #moy_area_agri

# Description de la dimension 4
resultat_acp_100_C$Dim.4 #area_foret 

# 500 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_paysage_500 = data_paysage %>% 
  filter(dist_buffer == "500") %>% 
  distinct() %>% 
  select(!Shannon_naturel)%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass") 

data_paysage_500_corr = data_paysage_500 %>% 
  select_if(is.numeric) %>% 
  cor()%>% 
  corrplot( method="color", col=col(200),  
            type="upper", order="hclust", 
            addCoef.col = "black", # Ajout du coefficient de corrélation
            tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
            # Combiner avec le niveau de significativité
            diag=FALSE 
  )
ggsave(file.path(FolderDonnees,FolderSortie,"corr_paysage_500.png"),device = "png" )

## ACP ---------------------------------------------------------------------

resultat_acp_500 <- data_paysage_500 %>%  
  select_if(is.numeric) %>% # Sélection des variables numériques
  PCA(graph = F, # On ne trace rien
      ncp = ncol(.)) # Le nombre de composantes principales

nouveau_tableau <- resultat_acp_500$ind$coord %>% 
  as_tibble()
nouveau_tableau # Même dimension que le tableau initial

summarise_all(nouveau_tableau, var)
resultat_acp_500$eig #4 composantes principales

nouveau_tableau %>% 
  cor() %>% # calcul de la matrice de corrélation
  corrplot() # Représentation graphique (package (corrplot))#Aucune corrélation entre les variables

resultat_acp_500$var$cor[, 1:5] # Correlation des 5 premières nouvelles variables
# avec toutes les anciennes

#Valeur propre
fviz_eig(resultat_acp_500)

#CP1 x CP2
fviz_pca_var(resultat_acp_500,
             axes = c(1, 2),
             repel = TRUE)

ggsave(file.path(FolderDonnees,FolderSortie,"ACP_500_CP1_2.png"), device = "png")

#CP1 x CP3
fviz_pca_var(resultat_acp_500,
             axes = c(1, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_500_CP1_3.png"), device = "png")

#CP2 x CP3
fviz_pca_var(resultat_acp_500,
             axes = c(2, 3),
             repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACP_500_CP2_3.png"), device = "png")

#Représentation avec le traitement
fviz_pca_biplot(resultat_acp_500,
                label = "var",
                axes = c(1,2),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_500_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_500,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_500_CP1_3.png"), device = "png")


fviz_pca_biplot(resultat_acp_500,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_500_CP2_3.png"), device = "png")

fviz_pca_biplot(resultat_acp_500,
                label = "var",
                axes = c(1,4),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_500_CP1_4.png"), device = "png")

fviz_pca_biplot(resultat_acp_500,
                label = "var",
                axes = c(2,4),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_500_CP2_4.png"), device = "png")

fviz_pca_biplot(resultat_acp_500,
                label = "var",
                axes = c(3,4),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_500_CP3_4.png"), device = "png")

# Contribution 
resultat_acp_500_A <- dimdesc(resultat_acp_500, axes = c(1,2), proba = 0.05)
resultat_acp_500_B <- dimdesc(resultat_acp_500, axes = c(1,3), proba = 0.05)
resultat_acp_500_C <- dimdesc(resultat_acp_500, axes = c(1,4), proba = 0.05)

# Description de la dimension 1
resultat_acp_500_A$Dim.1 # haie_density  

# Description de la dimension 2
resultat_acp_500_A$Dim.2 # nb_parcelle

# Description de la dimension 3
resultat_acp_500_B$Dim.3 #area_prairie

# Description de la dimension 4
resultat_acp_500_C$Dim.4 #dist_habitation  

# 1000 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_paysage_1000 = data_paysage %>% 
  filter(dist_buffer == "1000") %>% 
  distinct() %>% 
  select(!Shannon_naturel)%>% 
  remove_rownames() %>% 
  column_to_rownames(var = "carre_year_pass") 

data_paysage_1000_corr = data_paysage_1000 %>% 
  select_if(is.numeric) %>% 
  cor()%>% 
  corrplot( method="color", col=col(200),  
            type="upper", order="hclust", 
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
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_1000_CP1_2.png"), device = "png")

fviz_pca_biplot(resultat_acp_1000,
                label = "var",
                axes = c(1,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_1000_CP1_3.png"), device = "png")


fviz_pca_biplot(resultat_acp_1000,
                label = "var",
                axes = c(2,3),
                repel = TRUE)
ggsave(file.path(FolderDonnees,FolderSortie,"ACPind_1000_CP2_3.png"), device = "png")

# Contribution 
resultat_acp_1000_A <- dimdesc(resultat_acp_1000, axes = c(1,2), proba = 0.05)
resultat_acp_1000_B <- dimdesc(resultat_acp_1000, axes = c(1,3), proba = 0.05)


# Description de la dimension 1
resultat_acp_1000_A$Dim.1 # perimetre_agri  

# Description de la dimension 2
resultat_acp_1000_A$Dim.2 # nb_parcelle

# Description de la dimension 3
resultat_acp_1000_B$Dim.3 #area_BIO



# 2000 m  ------------------------------------------------------------------

## Matrice de corrélation  -------------------------------------------------

data_paysage_2000 = data_paysage %>% 
  filter(dist_buffer == "2000") %>% 
  distinct() %>% 
  select(!Shannon_naturel)%>% 
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
resultat_acp_2000_A$Dim.2 # nb_parcelle

# Description de la dimension 3
resultat_acp_2000_B$Dim.3 #area_BIO


