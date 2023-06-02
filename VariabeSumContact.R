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

data_na = data_total %>% 
  filter(is.na(tadarida_taxon))

saveRDS(data_contact, file.path(FolderDonnees,FolderInter, "data_sumcontact.rds"))

# Figure ------------------------------------------------------------------

data = data_total %>% 
  group_by(tadarida_taxon) %>% 
  tally(name = "sum_contact") %>% 
  arrange(desc(sum_contact)) %>% 
  ungroup() %>% 
  mutate(Pourcentage = (sum_contact*100)/sum(sum_contact)) 
  
data_occurence = data_total %>% 
  group_by(tadarida_taxon) %>% 
  summarise( N_site = n_distinct(carre_year_pass)) %>% 
  mutate(Occurrence =(N_site*100)/n_distinct(data_total$carre_year_pass))

data_figure = left_join(data, data_occurence) %>% 
  dplyr::select(!N_site) %>% 
  rename ("Espèces" = tadarida_taxon,
          "Nombre de contact" = sum_contact,
          "Pourcentage du nombre total de contact (%)" = Pourcentage,
          "Occurence (%)" = Occurrence)

saveRDS(data_figure, file.path(FolderDonnees, FolderInter, "data_figureContact.rds"))


pdf(file.path(FolderDonnees,FolderSortie,"TableauSummaryChiro.pdf"), width = 8, height = 4)
grid.table(data_figure)
dev.off()

png(file.path(FolderDonnees,FolderSortie,"TableauSummaryChiro.png"), width = 600, height =300 )
grid.table(data_figure)
dev.off()
