# Titre : 1.2clean_data_site
# But : Modifier les données data_site
# - les nom des commune en majuscule et ajouter des tiret à la place des espace
# - l'année en facteur
# - création d'une variable Mod_year
# - rename carre_year.1 en carre_year_pass pour que tous les tableaux ai une variable en commun
# Auteur : Emilie
# Date : 21/04/2023


# Library -----------------------------------------------------------------

library(stringr)
library(tidyverse)

# Chargement data ---------------------------------------------------------

Folderpath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")


# Modification ------------------------------------------------------------

data_site = read.csv(file = file.path(Folderpath,"1.Donnees_sources", "Chiroptères", "Sites","site_chiro_19_20_21.csv"), sep = ";", header = TRUE) %>% 
  # rename(Commune = X...Commune) %>% #La variable commune est renommé 
  mutate(Commune = str_to_upper(Commune)) %>% 
  mutate(Commune = str_replace_all(Commune," ","_"),
         Commune = str_replace(Commune, "’", "_"),
         year = as.factor(year)) %>%
  unite(Mod_pass, Modalite_protocole, Num_passag, sep = "_", remove = FALSE) %>%  
  rename( carre_year_pass= carre_year.1) 

# Ecriture ----------------------------------------------------------------

saveRDS(data_site, file = (file.path(Folderpath,"2.Donnees_intermediaire", "data_site.rds")))

