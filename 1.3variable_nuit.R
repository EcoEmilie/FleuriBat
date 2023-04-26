# Titre : 1.3VariableNuit
# But : Création de la variable Nuit
# Auteur : Emilie
# Date : 25/04/2023


# Library ----------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Chargement data ---------------------------------------------------------

Folderpath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
data_chiro = readRDS(file = file.path(Folderpath,"2.Donnees_intermediaire","data_chiro.rds"))
data_site = read.csv(file = file.path(Folderpath,"1.Donnees_sources", "Chiroptères", "Sites","site_chiro_19_20_21.csv"), sep = ";", header = TRUE)

## restructuration de la date et l'heure ----
data_chiro1= data_chiro %>% 
  mutate(heure = as.character(heure)) %>%
  mutate(heure = str_pad(heure, width = 6, side = "left", pad = "0")) %>% #avoir des 0 pour les heures minuits
  unite(date_heure, date,heure) %>% 
  mutate(date_heure = ymd_hms(date_heure))

## séparation des nuits ----
data_chiro2 = data_chiro1 %>% 
  select(carre_year_pass,date_heure) %>% 
  group_by(carre_year_pass) %>% 
  mutate(min_date = min(date_heure),
         min_date_fin = min_date + hours(x = 15)) %>% #On considére que la nuit se termine 15H après le 1er enregistrement 
  mutate(max_date = max(date_heure),
         max_date_debut = max_date - dhours(x = 15)) #On considére que le debut de la deuxièreme nuit commence 15h avant le dernier enregistrement

#Si la date/heure est dans l'intervalle de min_date et min_date_fin alors il fait partie de NUIT1 sinon NUIT2
data_chiro2$Num_Nuit = ifelse(data_chiro2$date_heure %within% interval(data_chiro2$min_date,data_chiro2$min_date_fin) == TRUE,"NUIT1","NUIT2")

#data avec les sites et la variable nuit
data_nuit = data_chiro2 %>% 
  select(carre_year_pass, Num_Nuit)%>% 
  distinct()

# Ajout de la variable au jeu de données  ---------------------------------

data_chiro_nuit = left_join(data_chiro,data_nuit) %>% 
  relocate(Num_Nuit, .after = Num_passag)

data_site_nuit = left_join(data_site, data_nuit)
  
  
