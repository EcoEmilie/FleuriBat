# Titre : 1.2clean_data_chiro
# But : Filtrer les r??sultats de Tadarida, on ne veut garder que les donn??es chiropt??res
# Auteur : Emilie
# Date : 21/04/2023

setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees/Chiropteres")

a_192021 = read.csv("export_192021.csv", header = TRUE)


# Nettoyage  --------------------------------------------------------------

## Liste esp??ce

setwd("/Users/emihui/Library/Mobile Documents/com~apple~CloudDocs/FAC/Master/M2/Stage/Stage_ESE-OFB/Statistiques/_Donnees")
list_esp = read.csv("SpeciesList.csv", sep = ";") %>% 
  filter(Group == "bat")

## On ne garde que les chauves souris 
data_total = a_192021 %>% 
  unite(point_year_pass,Carre_Point_vigiechiro,year,Num_passag, sep = "_", remove = FALSE) %>% 
  mutate(date=ymd(date)) %>% 
  mutate(year = factor(year)) %>% 
  filter(tadarida_taxon %in% list_esp$Esp) %>% #on garde que les CS
  mutate(tadarida_taxon= factor(tadarida_taxon))

