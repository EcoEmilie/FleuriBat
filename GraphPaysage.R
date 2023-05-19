# Titre : GLMTraitement
# But : Modèle GLM sur l'effet de sbandes sur les chauves souris 
# Auteur : Emilie
# Date : 17.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(sf)
library(units)
library(randomcoloR)

# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_pay = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage.rds")) %>% 
  st_drop_geometry()


# Graphiques --------------------------------------------------------------

data_pay_num = data_pay %>% 
  select(!c(dist_buffer, buffer_area)) %>% 
  select_if(is.numeric) 

names=colnames(data_pay_num)

HistoGraph = function (x, var, name, fill) {
ggplot(x)+
  aes(x = var)+
  geom_histogram(color = "black", fill= fill)+
  labs( x = paste(name), y = "Fréquences", title = paste("Distribution", name))
ggsave(file.path(FolderDonnees,FolderSortie, paste(name, ".png")), device = "png")
}

for (i in 1:ncol(data_pay_num)){
  var = data_pay_num[,i]
  name=names[i]
  fill = randomColor()
  HistoGraph(data_pay_num, var , name, fill)
}
hist(data_pay$nb_parcelle)
