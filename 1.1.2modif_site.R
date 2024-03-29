# Titre : 1.2clean_data_site
# But : Modifier les données data_site
# - les nom des commune en majuscule et ajouter des tiret à la place des espace
# - l'année en facteur
# - création d'une variable Mod_year
# - rename carre_year.1 en carre_year_pass pour que tous les tableaux ai une variable en commun
# Auteur : Emilie
# Date : 21/04/2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(stringr)
library(tidyverse)

# Chargement data ---------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"
FolderSources = "1.Donnees_sources"


# Modification ------------------------------------------------------------

data_site = read.csv(file = file.path(FolderDonnees,FolderSources, "Chiroptères", "Sites","site_chiro_19_20_21.csv"), sep = ";", header = TRUE, dec = ",") %>% 
  # rename(Commune = X...Commune) %>% #La variable commune est renommé 
  mutate(Commune = str_to_upper(Commune)) %>% 
  mutate(Commune = str_replace_all(Commune," ","_"),
         Commune = str_replace(Commune, "’", "_"),
         year = as.factor(year),
         Modalite_protocole = as.factor(Modalite_protocole)) %>%
  unite(Mod_pass, Modalite_protocole, Num_passag, sep = "_", remove = FALSE) %>%  
  rename( carre_year_pass = carre_year.1) %>% 
mutate(Commune = str_to_upper(Commune))%>% 
  mutate(Commune = str_replace(Commune,"MEZIERES_BROUE_1","GERMAINVILLE_OUEST"),
         Commune = str_replace(Commune,"MEZIERES_BROUE_2","GERMAINVILLE_EST" ),
         Commune = str_replace(Commune,"MEZIERE-EN-DROUAIS", "MEZIERE")) %>% 
  filter(!Commune == "SERVILLE_2") 

data_1 = data_site %>% 
  filter(year == "2020" & Commune == "GERMAINVILLE") %>% 
  mutate(Commune = str_replace(Commune,"GERMAINVILLE", "GERMAINVILLE_OUEST"))

data_2 = data_site %>% 
  filter(year == "2021" & Commune == "GERMAINVILLE") %>% 
  mutate(Commune = str_replace(Commune,"GERMAINVILLE", "GERMAINVILLE_EST"))

data_3 = data_site %>% 
  filter(Carre_Point_vigiechiro == "Car780534_Z1") %>% 
  mutate(Commune = str_replace(Commune,"SONCHAMP", "SONCHAMP_1"))

data_4 = data_site %>% 
  filter(Carre_Point_vigiechiro == "Car780542_Z1") %>% 
  mutate(Commune = str_replace(Commune,"SONCHAMP", "SONCHAMP_2"))

data_6 = data_site %>% 
  filter(Carre_Point_vigiechiro == "Car910404_Z5") %>%
  mutate(Commune = str_replace(Commune,"GUILLERVAL", "GUILLERVAL_OUEST"))
  
data_7 = data_site %>% 
  filter(Carre_Point_vigiechiro == "Car910404_Z4") %>%
  mutate(Commune = str_replace(Commune,"GUILLERVAL", "GUILLERVAL_EST"))

data_8 = data_site %>% 
  filter(year == "2020" & Commune == "JANVRY") %>% 
  mutate(Commune = str_replace(Commune,"JANVRY", "JANVRY_EST"))

data_9 = data_site %>% 
  filter(year == "2021" & Commune == "JANVRY") %>% 
  mutate(Commune = str_replace(Commune,"JANVRY", "JANVRY_OUEST"))

data_10 = data_site %>% 
  filter(year == "2021" & Commune == "MAISSE") %>% 
  mutate(Commune = str_replace(Commune,"MAISSE", "MAISSE_EST"))

data_11 = data_site %>% 
  filter(year %in% c("2020","2019") & Commune == "MAISSE") %>% 
  mutate(Commune = str_replace(Commune,"MAISSE", "MAISSE_OUEST"))

data_site_modif = bind_rows(data_site, data_1, data_2, data_3, data_4, data_6, data_7, data_8,data_9,data_10, data_11) %>% 
  filter(!Commune =="GERMAINVILLE") %>% 
  filter(!Commune == "SONCHAMP") %>% 
  filter(!Commune == "GUILLERVAL") %>% 
  filter(!Commune == "JANVRY") %>%
  filter(!Commune == "MAISSE") %>% 
  distinct()

# Ecriture ----------------------------------------------------------------

saveRDS(data_site_modif, file = (file.path(FolderDonnees,FolderInter, "data_site.rds")))

