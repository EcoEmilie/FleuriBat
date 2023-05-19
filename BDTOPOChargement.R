# Titre : BDTOPOchargement
# But : Création des différentes cartes issue de BD TOPO 
# Auteur : Emilie PEN
# Date : 15/05/2023


# Library  ----------------------------------------------------------------

library(sf)
library(tidyverse)


# Chemin  -----------------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/1.Donnees_sources/Cartographie/BD_TOPO")
FolderFinal = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")


# Sites -------------------------------------------------------------------

data_site= readRDS(file = file.path(FolderRDS, "data_site.rds")) %>% #A changer avec données site
  mutate(Commune = str_to_upper(Commune), year = as.factor(year)) %>% 
  mutate(Commune = str_replace_all(Commune,"_"," ")) %>% 
  unite(Mod_pass, Modalite_protocole, Num_passag, sep = "_", remove = FALSE)%>% 
  #slice_sample(n = 10) %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% #au départ en WGS 84
  st_transform(2154) 

# Plan d'eau --------------------------------------------------------------

data_plan_eau = st_read("BDT_3-3_GPKG_LAMB93_D028-ED2022-12-15.gpkg", layer =  "plan_d_eau") 

saveRDS(data_plan_eau, file.path(FolderFinal,"data_plan_eau.rds"))