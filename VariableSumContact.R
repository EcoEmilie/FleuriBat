# Titre : VariableSumContact
# But : Calcule du nombre de contact  
# Auteur : Emilie
# Date : 23.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(gridExtra)

# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_site = readRDS(file.path(FolderDonnees,FolderInter, "data_site.rds"))

data_chiro = readRDS(file.path(FolderDonnees,FolderInter, "data_filtree_seuil08.rds")) %>% 
  dplyr :: select(!Commune)

data_agri = readRDS(file.path(FolderDonnees,FolderInter, "SDCChiroall.rds")) %>% 
  rename(carre_year_pass = carre_year.1)

data_SDC = left_join( data_site,data_agri) %>% 
  select(Commune, SDC) %>% 
  distinct() %>% 
  group_by(SDC) %>% 
  tally()
  
data_total = left_join( data_site,data_chiro) %>% 
  left_join(data_agri) %>% 
  mutate(tadarida_taxon = as.factor(tadarida_taxon)) %>% 
  filter(!Modalite_protocole == "exclos") %>% 
  filter(!is.na(tadarida_taxon))

data_contact = data_total %>% 
  filter(!Modalite_protocole == "exclos")%>% 
  dplyr :: select(carre_year_pass, Modalite_protocole,Num_passag, year, Commune, SDC) %>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = "sum_contact") %>% 
  distinct() %>% 
  mutate(Commune = str_replace(Commune, " ", "_")) 

data_SDC = data.frame(data_contact$Commune, data_contact$SDC) %>% 
  distinct()

data_na = data_total %>% 
  filter(is.na(tadarida_taxon))

saveRDS(data_contact, file.path(FolderDonnees,FolderInter, "data_sumcontact.rds"))

# Figure ------------------------------------------------------------------

data = data_total %>% 
  group_by(tadarida_taxon) %>% 
  tally(name = "sum_contact") %>% 
  arrange(desc(sum_contact)) %>% 
  ungroup() %>% 
  mutate(Pourcentage = (sum_contact*100)/sum(sum_contact)) %>% 
  mutate(Pourcentage = round(data$Pourcentage, digits =2))

data_sumcontact = data_total %>% 
  dplyr :: select(carre_year_pass, Modalite_protocole, tadarida_taxon, Num_passag, year, Commune, SDC)%>% 
  group_by(carre_year_pass) %>% 
  add_tally(name = "sum_contact") %>% 
  group_by(tadarida_taxon,carre_year_pass) %>% 
  add_tally(name = "sum_contact_sp") %>% 
  distinct() %>% 
  filter(tadarida_taxon == "Pippip") %>% 
  mutate(Pourcentage = (sum_contact_sp*100)/sum_contact) %>% 
  ungroup() %>% 
  select(carre_year_pass, Pourcentage)

data_min_max = data_total%>% 
  group_by( tadarida_taxon,carre_year_pass)%>% 
  tally(name = "sum_contact_esp")%>% 
  mutate(Max = max(sum_contact_esp),
         Min = min(sum_contact_esp)) %>%
  select(tadarida_taxon,Min, Max) %>% 
  distinct() %>% 
  mutate("Min-Max" = str_c(Min, Max, sep = "-")) %>% 
  select(tadarida_taxon,"Min-Max")
  
data_occurence = data_total %>% 
  group_by(tadarida_taxon) %>% 
  summarise( N_site = n_distinct(carre_year_pass)) %>% 
  mutate(Occurrence =(N_site*100)/n_distinct(data_total$carre_year_pass))%>% 
  mutate(Occurrence = round(data_occurence$Occurrence, digits = 2))

data_figure = left_join(data, data_occurence) %>% 
  left_join(data_min_max) %>% 
  dplyr::select(!N_site) %>% 
  relocate("Min-Max", .before = Pourcentage ) %>% 
  mutate(Pourcentage = as.character(Pourcentage)) %>% 
  mutate(Pourcentage = str_replace(Pourcentage, "0.0", "<0.1")) %>% 
  rename ("Espèces" = tadarida_taxon,
          "Nombre de contact total" = sum_contact,
          "Pourcentage du nombre total de contact (%)" = Pourcentage,
          "Occurence (%)" = Occurrence) 

saveRDS(data_figure, file.path(FolderDonnees, FolderInter, "data_figureContact.rds"))


pdf(file.path(FolderDonnees,FolderSortie,"TableauSummaryChiro.pdf"), width = 9, height = 5)
grid.table(data_figure)
dev.off()

png(file.path(FolderDonnees,FolderSortie,"TableauSummaryChiro.png"), width = 600, height =300 )
grid.table(data_figure)
dev.off()
