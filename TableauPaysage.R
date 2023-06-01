# Titre : TableauPaysage
# But : Création d'un tableau résumant les variables
# Auteur : Emilie
# Date : 28/04/2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(gridExtra)


# Chargement données ------------------------------------------------------

FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_paysage = readRDS(file.path(FolderDonnees,FolderInter, "data_paysage_modif.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  st_drop_geometry() %>% 
  as.data.frame()

data_naturel = readRDS(file.path(FolderDonnees,FolderInter, "data_varpaysage.rds")) %>% 
  mutate(dist_buffer = as.factor(dist_buffer))%>% 
  st_drop_geometry() %>% 
  as.data.frame()%>% 
  filter(!Modalite_protocole == "exclos") 

data_total = left_join(data_paysage, data_naturel)%>% 
  distinct()

# Tableau Figure ----------------------------------------------------------

data_summary1 = data_total %>% 
  dplyr::select(!c(carre_year_pass,year, Modalite_protocole, buffer_area, Shannon_naturel)) %>%
  mutate(dist_foret = as.numeric(dist_foret),
         dist_eau = as.numeric(dist_eau), 
         dist_habitation = as.numeric(dist_habitation),
         dist_haie = as.numeric(dist_haie)) %>% 
  pivot_longer(cols = 2:20, names_to = "Variables", values_to = "Value") %>% 
  mutate(Variables = as.factor(Variables), .before = dist_buffer) %>% 
  drop_na() %>% 
  dplyr::filter(!str_detect(Variables,"dist")) %>% 
  group_by( dist_buffer, Variables) %>% 
  summarise(Moyenne = mean(Value), Médiane = median(Value), Max = max(Value), Min = min(Value)) %>% 
  mutate(Moyenne = round(Moyenne, 2),
         Médiane = round(Médiane, 2),
         Max = round(Max, 2),
         Min = round(Min, 2)) %>% 
  arrange(Variables)

data_summary2 = data_total %>% 
  dplyr::select(!c(carre_year_pass,year, Modalite_protocole, buffer_area, Shannon_naturel)) %>%
  mutate(dist_foret = as.numeric(dist_foret),
         dist_eau = as.numeric(dist_eau), 
         dist_habitation = as.numeric(dist_habitation),
         dist_haie = as.numeric(dist_haie)) %>% 
  pivot_longer(cols = 2:20, names_to = "Variables", values_to = "Value") %>% 
  mutate(Variables = as.factor(Variables), .before = dist_buffer) %>% 
  drop_na() %>% 
  dplyr::filter(str_detect(Variables,"dist"))%>% 
  group_by( Variables) %>% 
  summarise(Moyenne = mean(Value), Médiane = median(Value), Max = max(Value), Min = min(Value)) %>% 
  mutate(Moyenne = round(Moyenne, 2),
         Médiane = round(Médiane, 2),
         Max = round(Max, 2),
         Min = round(Min, 2)) %>% 
  arrange(Variables)

data_summary = bind_rows(data_summary1,data_summary2) %>% 
  replace_na(list(x2="X"))

saveRDS(data_summary, file.path(FolderDonnees, FolderInter, "data_figurePaysage.rds"))


pdf(file.path(FolderDonnees,FolderSortie,"TableauSummaryPaysage.pdf"), width = 6, height = 22)
grid.table(data_summary)
dev.off()

png(file.path(FolderDonnees,FolderSortie,"TableauSummaryPaysage.png"), width = 450, height = 1550)
grid.table(data_summary)
dev.off()


