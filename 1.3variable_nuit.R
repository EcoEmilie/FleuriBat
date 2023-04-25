# Titre : 1.3VariableNuit
# But : Création de la variable Nuit
# Auteur : Emilie
# Date : 25/04/2023


# restructuration de la date et l'heure ----
data_total1= data_total %>% 
  mutate(heure = as.character(heure)) %>%
  mutate(heure = str_pad(heure, width = 6, side = "left", pad = "0")) %>% #avoir des 0 pour les heures minuits
  unite(date_heure, date,heure) %>% 
  mutate(date_heure = ymd_hms(date_heure))

# séparation des nuits ----

data_total2 = data_total1 %>% 
  select(point_year_pass,date_heure) %>% 
  group_by(point_year_pass) %>% 
  mutate(min_date = min(date_heure),
         min_date_fin = min_date + hours(x = 15)) %>%
  mutate(max_date = max(date_heure),
         max_date_debut = max_date - dhours(x = 15)) 

data_total2$Num_Nuit = ifelse(data_total2$date_heure %within% interval(data_total2$min_date,data_total2$min_date_fin) == TRUE,"NUIT1","NUIT2")


data_nuit = data_total2 %>% 
  select(point_year_pass, Num_Nuit)%>% 
  distinct()
