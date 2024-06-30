library(questionr)
library(dplyr)
library(tidyr)
library(stringr)


## La Croix 
LAcroix2 <- fread("01_data_ollion/LaCroix2.csv")
View(LAcroix2)
LeFigaro <- read_csv("Data/LeFigaro.csv")
str(LeFigaro)
library(readr)
LeFigaro <- read_csv("Data/LeFigaro.csv")
LeMonde <- read_csv("Data/LeMonde.csv")
str(LeMonde)
LesEchos <- read_csv("Data/LesEchos.csv")
Liberation <- read_csv("Data/Liberation.csv")
View(Liberation)
Liberation$...1 <- NULL   
str(LesEchos)

## Si besoin de esquisse : 
# esquisse:::esquisser()


# Travail sur les données ------

# I. Nettoyage - La Croix ------
library(readr)
LaCroix <- fread("02_data_travail/lacroix_travail_vf.csv")
str(LaCroix)

LaCroix <- fread("Data/LaCroix.csv")
View(LaCroix)
LaCroix$variable <- paste0("lacroix_", LaCroix$...1)
LaCroix$...1 <- LaCroix$variable
LaCroix$variable <- NULL

### Renommer l'auteur ----
LaCroix$auteur <- LaCroix$authors
LaCroix$authors <- NULL

### Renommer rubrique  ----
LaCroix$rubrique <- LaCroix$doc_matiere
LaCroix$doc_matiere <- NULL
View(LaCroix)
### Renommer nb_mots ----
LaCroix$nb_mots <- LaCroix$wordcount
LaCroix$wordcount<- NULL
View(LaCroix)

### Nom du journal   ----
LaCroix$journal <- "La Croix" 

### Ancienne rubrique suppression  ----
library(lubridate)
LaCroix_2 <- LaCroix
LaCroix_2$date <- dmy(LaCroix_2$date, locale = "fr_FR")

View(LaCroix_3)
base_expo4$date_debut2 <- dmy(base_expo4$date_debut, locale = "fr_FR")
str(LaCroix)


### Diviser les auteurs -----------------
LaCroix$auteurs <- LaCroix$auteur
LaCroix1 <- LaCroix

LaCroix1$aut <- LaCroix1$auteurs
LaCroix1$auteurs <- gsub("Par","",LaCroix1$auteurs)
LaCroix1  <- separate(LaCroix1, aut, sep="(?i)\\bet\\b|,|;|\\bavec\\b|\\|", into=c("auteur1", "auteur2")) 

# Enlever les espaces au début 
LaCroix1$auteurs <- str_trim(LaCroix1$auteurs)
LaCroix1$auteur1 <- str_trim(LaCroix1$auteur1)
LaCroix1$auteur2 <- str_trim(LaCroix1$auteur2)
LaCroix <- LaCroix1
View(LaCroix1)


### Date LaCroix 

# Extraction du mois en texte (format complet) et de l'année
LeMonde$year <- year(LeMonde$date)

# Conversion de mois en lettre (format court)
LeMonde$mois <- format(LeMonde$date, "%b")
LeMonde$mois <- paste(LeMonde$mois,LeMonde$year) 


### Reorganisation ------
library(dplyr)

LaCroix$annee <- LaCroix$date
LaCroix$is_article <- NA
LaCroix$file <- NA
LaCroix$motscle <- NA
LaCroix$chapo <- NA
LaCroix$mois <- NA
LaCroix$year <- LaCroix$annee


### Year au format 4 chiffres  ------
LaCroix$year <- year(LaCroix$date)

## Les dates ------
### Annee en format date  ------
LaCroix <- LaCroix %>% 
  mutate(annee  = cut(date, breaks = "year"))
### Year au format 4 chiffres  ------
LeFigaro$year <- year(LeFigaro$date)
### Conversion de mois en lettre (format court)  ------
LaCroix$mois <- format(LaCroix$date, "%b")
LaCroix$mois <- paste(LaCroix$mois,LaCroix$year) 

LaCroix$mois <- NA
LaCroix <- LaCroix %>%
  select(
    ...1,
    journal,
    date,
    mois, 
    year, 
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )

View(LaCroix)


write.csv(LaCroix, "02_data_travail/lacroix_travail_vf_3.csv", row.names = FALSE)

LaCroix <- fread("02_data_travail/lacroix_travail_vf.csv") 


# II. Le Monde Nettoyage -----------------
LeMonde <- fread("02_data_travail/lemonde_travail_vf.csv")

LeMonde_bis <- fread("02_data_travail/lemonde_clean_V2.csv")
LeMonde$date <- as.Date(LeMonde$date)
LeMonde_bis$date <- as.Date(LeMonde_bis$date)
LeMonde_bis$annee <- as.Date(LeMonde_bis$annee)
LeMonde$annee <- as.Date(LeMonde$annee)
LeMonde <- rbind(LeMonde, LeMonde_bis)
str(LeMonde_bis)

#### Mettre un code -------
LeMonde$variable <- paste0("lemonde_", LeMonde$...1)
LeMonde$...1 <- LeMonde$variable
LeMonde$variable <- NULL

# Date 
LeMonde$date <- LeMonde$publidate
LeMonde$publidate  <- NULL
LeMonde$publiheure <- NULL

## Annee 
LeMonde <- LeMonde %>% 
  mutate(annee  = cut(date, breaks = "year"))


# source 
LeMonde$source <- LeMonde$url
LeMonde$url  <- NULL

View(LeMonde_1)
# Cration variable journal 
LeMonde$journal <- "Le Monde" 
str(LeMonde_1)
LeMonde_1 <- LeMonde
LeMonde$soustitre <- NA
LeMonde$nb_mots <- NA
LeMonde$doc_geo <- NA
LeMonde$doc_pphysique <- NA
LeMonde$doc_pmorale <- NA
LeMonde$doc_oldindexation <- NA
LeMonde$edition <- NA
LeMonde$websection <- NA
LeMonde$section <- NA
LeMonde$type_article <- NA
LeMonde$motscle <- NA
LeMonde_1$annee <- NA

LeMonde$auteurs <- LeMonde$auteur
LeMonde$auteur <- NULL
LeMonde$auteurs <- gsub("Par","",LeMonde$auteurs)
LeMonde$auteurs <- gsub("^Propos Recueillis Par","",LeMonde$auteurs)

# Enlever les espaces au début 
LeMonde$auteurs <- str_trim(LeMonde$auteurs)
# Majuscule au début 
LeMonde$auteurs <- str_to_title(LeMonde$auteurs)

View(LeFigaro)


### Diviser les auteurs -----------------
LeFigaro <- LeMonde
Liberation1$aut <- Liberation1$auteurs
Liberation1  <- separate(Liberation1, aut, sep="(?i)\\bet\\b|,|\\bavec\\b|\\|", into=c("auteur1", "auteur2")) 
Liberation1$auteurs <- gsub("Par","",Liberation1$auteurs)
# Enlever les espaces au début 
Liberation1$auteurs <- str_trim(Liberation1$auteurs)
Liberation1$auteur1 <- str_trim(Liberation1$auteur1)
Liberation1$auteur2 <- str_trim(Liberation1$auteur2)


# Extraction du mois en texte (format complet) et de l'année
LeMonde$year <- year(LeMonde$date)

# Conversion de mois en lettre (format court)
LeMonde$mois <- format(LeMonde$date, "%b")
LeMonde$mois <- paste(LeMonde$mois,LeMonde$year) 


View(LeMonde)
LeMonde <- LeMonde %>%
  select(
    ...1,
    journal,
    date,
    mois, 
    year, 
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )

LeMonde1 <- LeMonde1 %>% 
  mutate(annee  = cut(date, breaks = "year"))

write.csv(LeMonde, "02_data_travail/lemonde_travail_vf_2.csv", row.names = FALSE)



# III. Liberation Nettoyage --------------
Liberation <- fread("02_data_travail/liberation_travail_vf.csv")


Liberation_bis <- fread("02_data_travail/nv_donnees/liberation_clean.csv")
Liberation$date <- as.Date(Liberation$date)
Liberation_bis$date <- as.Date(Liberation_bis$date)
Liberation_bis$annee <- as.Date(Liberation_bis$annee)
Liberation$annee <- as.Date(Liberation$annee)
Liberation <- rbind(Liberation, Liberation_bis)
str(LeMonde_bis)

View(Liberation)

### 1. Mettre un code -------
Liberation$...1 <- seq(1, length.out = nrow(Liberation), by = 1)
Liberation$variable <- paste0("Liberation_", Liberation$...1)
Liberation$...1 <- Liberation$variable
Liberation$variable <- NULL
View(Liberation)

### Date -----
Liberation$date <- Liberation$publidate
Liberation$publidate  <- NULL
Liberation$publiheure <- NULL

# source 
Liberation$source <- Liberation$url
Liberation$url  <- NULL

View(Liberation)


### Diviser les auteurs -----------------
Liberation1 <- Liberation
Liberation1$auteurs <- Liberation1$auteur
Liberation1$auteurs <- gsub("Par","",Liberation1$auteurs)
Liberation1$aut <- Liberation1$auteurs


Liberation1  <- separate(Liberation1, aut, sep="(?i)\\bet\\b|,|;|\\bavec\\b|\\|", into=c("auteur1", "auteur2")) 

# Enlever les espaces au début 
Liberation1$auteurs <- str_trim(Liberation1$auteurs)
Liberation1$auteur1 <- str_trim(Liberation1$auteur1)
Liberation1$auteur2 <- str_trim(Liberation1$auteur2)
View(Liberation1)

# Extraction du mois en texte (format complet) et de l'année
Liberation1$year <- year(Liberation1$date)

Liberation1<- Liberation1 %>% 
  mutate(annee  = cut(date, breaks = "year"))

# Conversion de mois en lettre (format court)
Liberation1$mois <- format(Liberation1$date, "%b")
Liberation1$mois <- paste(Liberation1$mois,Liberation1$year) 



# Cration variable journal 
Liberation$journal <- "Liberation" 
str(Liberation_1)
Liberation$soustitre <- NA
Liberation$nb_mots <- NA
Liberation$doc_geo <- NA
Liberation$doc_pphysique <- NA
Liberation$doc_pmorale <- NA
Liberation$doc_oldindexation <- NA
Liberation$edition <- NA
Liberation$websection <- NA
Liberation$section <- NA
Liberation$type_article <- NA
Liberation$is_article <- NA
Liberation$annee <- NA
Liberation_1 <- Liberation

Liberation1 <- Liberation1 %>%
  select(
    ...1,
    journal,
    date,
    mois, 
    year, 
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )

write.csv(Liberation, "02_data_travail/liberation_travail_vf_2.csv", row.names = FALSE)

# IV. Le Echos Nettoyage --------------


## Importaiton 
LesEchos <- fread("Data/LesEchos.csv")
LesEchos2 <- fread("02_data_travail/lesechos_clean.csv")

View(LesEchos2)


## Couper 

# Supprimer les lignes avec des dates antérieures au 15 février 2019
LesEchos2 <- subset(LesEchos2, as.Date(date) >= as.Date("2019-02-15"))

# Date 
LesEchos$date <- LesEchos$publidate 
LesEchos$publidate  <- NULL
LesEchos$publiheure <- NULL


# source 
LesEchos$source <- LesEchos$url
LesEchos$url  <- NULL

## Diviser les auteurs -----
LesEchos2$auteur1 <- NULL
LesEchos2$auteur2 <- NULL
LesEchos1<- LesEchos2

LesEchos1$auteurs <- gsub("Par","",LesEchos1$auteurs)
LesEchos1$auteurs <- gsub("Propos recueillis par","",LesEchos1$auteurs)

LesEchos1$aut <- LesEchos1$auteurs
LesEchos1  <- separate(LesEchos1, aut, sep="(?i)\\bet\\b|,|;|\\bavec\\b|\\|", into=c("auteur1", "auteur2")) 

# Enlever les espaces au début 
LesEchos1$auteurs <- str_trim(LesEchos1$auteurs)
LesEchos1$auteur1 <- str_trim(LesEchos1$auteur1)
LesEchos1$auteur2 <- str_trim(LesEchos1$auteur2)
View(LesEchos1)
LesEchos <- LesEchos1- 
LesEchos


# Cration variable journal 
LesEchos$journal <- "LesEchos" 
str(LesEchos)
LesEchos$soustitre <- NA
LesEchos$nb_mots <- NA
LesEchos$doc_geo <- NA
LesEchos$doc_pphysique <- NA
LesEchos$doc_pmorale <- NA
LesEchos$doc_oldindexation <- NA
LesEchos$edition <- NA
LesEchos$websection <- NA
LesEchos$section <- NA
LesEchos$type_article <- NA
LesEchos$auteurs <- LesEchos$auteur
LesEchos$file <- LesEchos$source
LesEchos$is_article <- NA
LesEchos$source <- LesEchos$file
LesEchos$rubrique <- NA


## Les dates ------
# Conversion de la colonne "date" en format date
LesEchos$date <- ymd(LesEchos$date)
str(LesEchos2)
LesEchos2 <- LesEchos1
LesEchos2$date <- as.Date(LesEchos2$date)
### Annee en format date  ------
LesEchos<- LesEchos %>% 
  mutate(annee  = cut(date, breaks = "year"))
### Year au format 4 chiffres  ------
LesEchos$year <- year(LesEchos$date)

LesEchos$date 
### Conversion de mois en lettre (format court)  ------
LesEchos$mois <- format(LesEchos$date, "%b")
LesEchos$mois <- paste(LesEchos$mois,OuestFrance$year) 



LesEchos <- LesEchos %>%
  select(
    ...1,
    journal,
    date,
    mois, 
    year, 
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )


## Mettre ensemble les deux fichiers
LesEchos$variable <- paste0("lesechos_", LesEchos$...1)
LesEchos$...1 <- LesEchos$variable
LesEchos$variable <- NULL

LesEchos3 <- rbind(LesEchos, LesEchos1)
LesEchos4 <- LesEchos3

LesEchos4$...1<- seq(1, length.out = nrow(LesEchos3), by = 1)
LesEchos4$variable <- paste0("lesechos_", LesEchos4$...1)
LesEchos4$...1 <- LesEchos4$variable
LesEchos4$variable <- NULL
View(LesEchos4)

write.csv(LesEchos, "02_data_travail/lesechos_travail_vf.csv", row.names = FALSE)
write.csv(LesEchos2, "02_data_travail/lesechos_travail_base2.csv", row.names = FALSE)


# V. Le Parisien Nettoyage -------------------
library(readr)
LeParisien <- fread("02_data_travail/leparisien1_travail.csv")
View(LeParisien)

## Les dates ------
### Annee en format date  ------
LeParisien <- LeParisien %>% 
  mutate(annee  = cut(date, breaks = "year"))
### Year au format 4 chiffres  ------
LeParisien$year <- year(LeParisien$date)
### Conversion de mois en lettre (format court)  ------
LeParisien$mois <- format(LeParisien$date, "%b")
LeParisien$mois <- paste(LeParisien$mois,LeParisien$year) 


LeParisien <- LeParisien %>%
  select(
    ...1,
    journal,
    date,
    mois, 
    year, 
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )


write.csv(LeParisien, "02_data_travail/leparisien1_travail_vf.csv", row.names = FALSE)

# VI. Le Figaro Nettoyage -----------------------
LeFigaro <- fread("02_data_travail/lefigaro_travail_vf.csv")
LeFigaro_bis <- fread("02_data_travail/nv_donnees/lefigaro_clean.csv")
LeFigaro$date <- as.Date(LeFigaro$date)
LeFigaro_bis$date <- as.Date(LeFigaro_bis$date)
LeFigaro_bis$annee <- as.Date(LeFigaro_bis$annee)
LeFigaro$annee <- as.Date(LeFigaro$annee)
LeFigaro <- rbind(LeFigaro, LeFigaro_bis)
str(LeMonde_bis)

View(LeFigaro)
LeFigaro$source <- "Le Figaro"
LeFigaro$journal <- LeFigaro$source 
LeFigaro$source <- NULL
LeFigaro$date <- LeFigaro$dateparution2
LeFigaro$dateparution2 <- NULL
LeFigaro$annee <- LeFigaro$year
LeFigaro$year<- NULL

LeFigaro$texte <- LeFigaro$texte1
LeFigaro$texte1 <- NULL

LeFigaro$V1 <- NULL
LeFigaro$rubrique2 <- NULL
LeFigaro$variable <- paste0("lefigaro_", LeFigaro$...1)
LeFigaro$...1 <- LeFigaro$variable
LeFigaro$variable <- NULL

LeFigaro$signes<- NULL
LeFigaro$numparution<- NULL
LeFigaro$dateparution <- NULL
LeFigaro$auteurs  <- NULL
LeFigaro$auteur <- LeFigaro$auteurssq
LeFigaro$auteurssq <- NULL
LeFigaro$langue <- NULL
LeFigaro$type <- NULL



LeFigaro$chapo <- NA
LeFigaro$edition <- NA
LeFigaro$section <- NA
LeFigaro$type_article <- NA
LeFigaro$websection <- NA
LeFigaro$doc_pphysique <- NA
LeFigaro$doc_geo <- NA
LeFigaro$doc_pmorale <- NA
LeFigaro$doc_oldindexation <- NA
LeFigaro$is_article <- NA
LeFigaro$file <- NA
LeFigaro$soustitre <- NA
LeFigaro$motscle <- NA
LeFigaro$source <- NA
LeFigaro$nb_mots <- str_count(LeFigaro$texte, ' ')


### Diviser les auteurs -----------------
LeFigaro$auteurs <- LeFigaro$auteur
LeFigaro$aut <- LeFigaro$auteurs
LeFigaro  <- separate(LeFigaro, aut, sep="(?i)\\bet\\b|,|\\bavec\\b|\\|", into=c("auteur1", "auteur2")) 
LeFigaro$auteurs <- gsub("Par","",LeFigaro$auteurs)
# Enlever les espaces au début 
LeFigaro$auteurs <- str_trim(LeFigaro$auteurs)
LeFigaro$auteur1 <- str_trim(LeFigaro$auteur1)
LeFigaro$auteur2 <- str_trim(LeFigaro$auteur2)


## Les dates ------
### Annee en format date  ------
LeFigaro <- LeFigaro %>% 
  mutate(annee  = cut(date, breaks = "year"))
### Year au format 4 chiffres  ------
LeFigaro$year <- year(LeFigaro$date)
### Conversion de mois en lettre (format court)  ------
LeFigaro$mois <- format(LeFigaro$date, "%b")
LeFigaro$mois <- paste(LeFigaro$mois,LeFigaro$year) 

LeFigaro <- LeFigaro %>%
  select(
    ...1,
    journal,
    date,
    mois, 
    year, 
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )

View(LeFigaro)

write.csv(LeFigaro, "02_data_travail/lefigaro_travail_vf_2.csv", row.names = FALSE)

# VII. Ouest France -----
OuestFrance <- fread("02_data_travail/ouestfrance_travail_vf.csv")
View(OuestFrance)

## Les dates ------
### Annee en format date  ------
OuestFrance <- OuestFrance %>% 
  mutate(annee  = cut(date, breaks = "year"))
### Year au format 4 chiffres  ------
OuestFrance$year <- year(OuestFrance$date)
### Conversion de mois en lettre (format court)  ------
OuestFrance$mois <- format(OuestFrance$date, "%b")
OuestFrance$mois <- paste(OuestFrance$mois,OuestFrance$year) 

OuestFrance <- OuestFrance %>%
  select(
    ...1,
    journal,
    date,
    mois, 
    year, 
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )
write.csv(OuestFrance, "02_data_travail/ouestfrance_travail_vf.csv")

# Mise en commun de tout le corpus -------------------
library(data.table)
LeParisien <- fread("02_data_travail/leparisien1_travail_vf.csv")
LeMonde <- fread("02_data_travail/lemonde_travail_vf.csv")
LaCroix <- fread("02_data_travail/lacroix_travail_vf.csv")
LeFigaro <- fread("02_data_travail/lefigaro_travail_vf.csv")
Liberation <- fread("02_data_travail/liberation_travail_vf.csv")
LesEchos <- fread("02_data_travail/lesechos_travail_vf.csv")
OuestFrance <- fread("02_data_travail/ouestfrance_travail_vf.csv")
LaTribune <- fread("02_data_travail/latribune_clean.csv")
View(LaCroix)



base_complete <- rbind(LaCroix, LeParisien, LeMonde, LeFigaro, Liberation, LesEchos, OuestFrance, LaTribune)


write.csv(base_complete, "02_data_travail/base_complete_travail_vf.csv", row.names = FALSE)

View(base_complete)
str(LeParisien)
LesEchos$date <- ymd(LesEchos$date)

library(lubridate)
LaCroix$date <- ymd(LaCroix$date)
LeParisien$date <- ymd(LeParisien$date)
LeMonde$date <- ymd(LeMonde$date)
LeFigaro$date <- ymd(LeFigaro$date)
Liberation$date <- ymd(Liberation$date)
LesEchos$date <- ymd(LesEchos$date)
freq(is.na(LesEchos$date))
OuestFrance$date <- ymd(OuestFrance$date)
LaTribune$date <- ymd(LaTribune$date)

LeParisien$annee <- ymd(LeParisien$annee)
LeMonde$annee <- ymd(LeMonde$annee)
LeFigaro$annee <- ymd(LeFigaro$annee)
Liberation$annee <- ymd(Liberation$annee)
LesEchos$annee <- ymd(LesEchos$annee)
OuestFrance$annee <- ymd(OuestFrance$annee)
LaTribune$annee <- ymd(LaTribune$annee)



LaCroix$date <- ymd(LaCroix$date)

LaCroix$date <-  paste0(LaCroix$date, "-01-01")
LaCroix$date <- as.numeric(LaCroix$date)
View(LesEchos)


# Nettoyage de la base complete 

base_complete <- base_complete_travail
base_complete$date1 <- as.numeric(base_complete$date)
class(base_complete$date)
# Créer la colonne année
base_complete$annee <- format(base_complete$date, "%Y")

View(LesEchos)

