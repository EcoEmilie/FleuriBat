# Titre : 1.1.2.EspecesVerifieesParSites
# But : match avec les validation d'amandine
# Auteur : Camille Bernery 
# Modification : Emilie PEN 
# Date : 25/04/2023


# Library  ----------------------------------------------------------------

library(tidyverse)

############DONNEES SYNTHESE D AMANDINE : Tri des espèces par site####################
rm(list = ls())
getwd()
###mise en forme des donnéés d'amandine pour qu'elles soient homegenes
FolderPath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/1.Donnees_sources/Chiroptères/Data_Chiro_Filtrees")
FinalPath = paste("~/Documents sur ordi/Master/Stage_M2_ESE_OFB/R/Repertoire_donnees/2.Donnees_intermediaire")

#Probléme avec les accents 
Bande2019<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","Bande2019.csv"), na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)
Temoin2019<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","Temoin2019.csv"), na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)

BandePass12020<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","BandePass12020.csv"),na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)
BandePass22020<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","BandePass22020.csv"), na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)
TemoinPass12020<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","TemoinPass12020.csv"),na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)
TemoinPass22020<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","TemoinPass22020.csv"), na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)

Pass12021<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","Pass12021.csv"), na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)
Pass22021<-read.csv2(file = file.path(FolderPath,"Tableau_Amandine_utilises","Pass22021.csv"), na.strings=c("","NA"),fileEncoding = "Latin1", check.names = F)

###select seulement les colonnes avec des espèces certaienes
Bande2019ok <- Bande2019 %>% mutate_all(str_replace_all,"X", "Esp.ce.certaine")
Temoin2019ok<-Temoin2019 %>% mutate_all(funs(str_replace(.,"X", "Esp.ce.certaine")))

BandePass12020ok<-BandePass12020 %>% select(c('Site': "Esp.ce.certaine"))
BandePass22020ok<-BandePass22020 %>% select(c('Site': "Esp.ce.certaine"))
TemoinPass12020ok<-TemoinPass12020 %>% select(c('Site': "Esp.ce.certaine"))
TemoinPass22020ok<-TemoinPass22020 %>% select(c('Site': "Esp.ce.certaine"))

Pass12021ok<-Pass12021 %>% select(c('Site': "Esp.ce.certaine"))
Pass22021ok<-Pass22021 %>% select(c('Site': "Esp.ce.certaine"))

colnames(Pass12021)

####___________Merge les donnees d'amandine par date__________####

####2019######
Bande2019ok$Protocole<-"Bande"
Temoin2019ok$Protocole<-"Temoin"

Bande2019ok$Pass<-"PASS1"
Temoin2019ok$Pass<-"PASS1"

D2019all<-bind_rows(Bande2019ok, Temoin2019ok)

#Sélectionner une ligne sur deux pour avoir les infos de noms de site (CarXX) dans une colonne
D2019<-D2019all[ c(TRUE,FALSE), ] #les lignes avec les infos espèces mais pas les infos de code de site
Nomsite<-D2019all[ !c(TRUE,FALSE), ] #les infos code de sites sans les infos espèces
D2019$Nom.site<-Nomsite$X #faire correspondre toutes les infos

D2019$year<-2019

colnames(D2019)[1]<-"Site"

#####2020#####
BandePass12020ok$Protocole<-"Bande"
BandePass22020ok$Protocole<-"Bande"
TemoinPass12020ok$Protocole<-"Temoin"
TemoinPass22020ok$Protocole<-"Temoin"

BandePass12020ok$Pass<-"PASS1"
BandePass22020ok$Pass<-"PASS2"
TemoinPass12020ok$Pass<-"PASS1"
TemoinPass22020ok$Pass<-"PASS2"

D2020all<-bind_rows(BandePass12020ok, BandePass22020ok,TemoinPass12020ok,TemoinPass22020ok)

#Sélectionner une ligne sur deux pour avoir les infos de noms de site (CarXX) dans une colonne
D2020<-D2020all[ c(TRUE,FALSE), ] #les lignes avec les infos espèces mais pas les infos de code de site
Nomsite2<-D2020all[ !c(TRUE,FALSE), ] #les infos code de sites sans les infos espèces
D2020$Nom.site<-Nomsite2$Site #faire correspondre toutes les infos

D2020$year<-2020

colnames(D2020)[1]<-"Site"

######2021########
Pass12021ok$Pass<-"PASS1"
Pass22021ok$Pass<-"PASS2"

D2021all<-bind_rows(Pass12021ok,Pass22021ok)
class(Pass22021ok$Myodau)
#Sélectionner une ligne sur deux pour avoir les infos de noms de site (CarXX) dans une colonne
D2021<-D2021all[ c(TRUE,FALSE, FALSE), ] #les lignes avec les infos espèces mais pas les infos de code de site
Nomsite3<-D2021all[ c(FALSE,TRUE,FALSE), ] #les infos code de sites sans les infos espèces

Proto<- D2021all[ c(FALSE,FALSE,TRUE), ]###besoin de cette colonne pour avoir les modalités de protocole
Proto$Protocole<-NA
Proto[grep("Bande", Proto$Site), "Protocole"]<-"Bande"
Proto[grep("moin", Proto$Site), "Protocole"]<-"Temoin"

D2021$Nom.site<-Nomsite3$Site #faire correspondre toutes les infos
D2021$Nom.site<-Nomsite3$Site

D2021$Protocole<-Proto$Protocole

D2021$year<-2021


######Bind all dates######
ALLDATES<-bind_rows(D2019, D2020,D2021)

##Créer la column pour la faire correspondre aux sites
ALLDATESok<-unite(ALLDATES, Site2, Nom.site, year, Pass, sep="_")
rownames(ALLDATESok) <- ALLDATESok[,"Site2"]

###Créer le tableau avec seulement les espèces
ALLDATESsp<-ALLDATESok[c(2:14,18,19)]
ALLDATESsplog<-!is.na(ALLDATESsp)###METTRE UN TRUE si sp présente


Liste<-list()
  for (i in 1:146) {
  unname(unlist(ALLDATESsplog[i,]))
  l<-colnames(ALLDATESsplog)[unname(unlist(ALLDATESsplog[i,]))]
  Liste[[i]]<-l
  names(Liste)[i]<-row.names(ALLDATESsplog)[i]
}


##save les espèces vérifiées par site
saveRDS(Liste, "D:/1_Postdoc_CHIRO/SCRIPT_R/Trophic_chiro/Synthese_amandine_enforme/ListVerifSpPerSite.RDS")

#####SAVE LE NOMBRE DESPECES PAR SITE######
Nbesp<-merge(ALLDATESsplog, ALLDATESok[,c("Protocole", "Esp.ce.certaine")], by="row.names" )
write.csv2(Nbesp, "D:/1_Postdoc_CHIRO/SCRIPT_R/Trophic_chiro/Synthese_amandine_enforme/NbSpVerif.csv")
