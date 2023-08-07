# Titre : VerifSumContact
# But : Voir Quelle loi suit SumContact
# Auteur : Emilie
# Date : 27.05.2023

rm(list=ls())

# Library -----------------------------------------------------------------

library(tidyverse)
library(ggplot2)


# Données -----------------------------------------------------------------
FolderDonnees = paste("/Users/emihui/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees")
FolderInter= "2.Donnees_intermediaire"
FolderSortie = "3.Sorties"

data_contact = readRDS(file.path(FolderDonnees,FolderInter, "data_sumcontact.rds"))


# Représentation  ---------------------------------------------------------

ggplot(data_contact,aes(sum_contact))+
  geom_histogram(fill="#1E90FF")

# Verification  -----------------------------------------------------------

mean(data_contact$sum_contact)#266.7273

set.seed(1234) # permet de simuler toujours les mêmes comptages.
theoretic_count <-rpois(143,266.7273)

# on incorpore ces comptages théoriques dans un data frame
tc_df <-data.frame(theoretic_count)

# on plot simultanémaent les comptages observés et les comptages théoriques
ggplot(data_contact,aes(sum_contact))+
  geom_histogram(fill="#1E90FF")+
  geom_histogram(data=tc_df, aes(theoretic_count,fill="#1E90FF", alpha=0.5))+
  theme_classic()+
  theme(legend.position="none") 




