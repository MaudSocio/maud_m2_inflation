


# Mise en commun de tout le corpus -------------------
library(data.table)
LeParisien <- fread("02_data_travail/leparisien1_travail_vf.csv")
LaCroix <- fread("02_data_travail/lacroix_travail_vf_3.csv")
LeMonde <- fread("02_data_travail/lemonde_travail_vf_3.csv")
# LaCroix <- fread("02_data_travail/lacroix_travail_vf.csv")
LeFigaro <- fread("02_data_travail/lefigaro_travail_vf_2.csv")
Liberation <- fread("02_data_travail/liberation_travail_vf_2.csv")
LesEchos <- fread("02_data_travail/lesechos_travail_vf.csv")
OuestFrance <- fread("02_data_travail/ouestfrance_travail_vf.csv")
OuestFrance$V1 <- NULL
LaTribune <- fread("02_data_travail/latribune_clean.csv")


## Format date
str(LaCroix)
LaCroix$date <- as.Date(LaCroix$date)
LeParisien$date <- as.Date(LeParisien$date)
LeMonde$date <- as.Date(LeMonde$date)
LeFigaro$date <- as.Date(LeFigaro$date)
Liberation$date <- as.Date(Liberation$date)
OuestFrance$date <- as.Date(OuestFrance$date)
LaTribune$date <- as.Date(LaTribune$date)
LesEchos$date <- as.Date(LesEchos$date)

LeParisien$annee <- as.Date(LeParisien$annee)
LeMonde$annee <- as.Date(LeMonde$annee)
LeFigaro$annee <- as.Date(LeFigaro$annee)
Liberation$annee <- as.Date(Liberation$annee)
OuestFrance$annee <- as.Date(OuestFrance$annee)
LaTribune$annee <- as.Date(LaTribune$annee)
LaTribune$annee <- as.Date(LaTribune$annee)
LesEchos$annee <- as.Date(LesEchos$annee)
LaCroix$annee <- as.Date(LaCroix$annee)


#" Netto
LesEchos$journal <- "Les Echos"

base_complete <- rbind(LeParisien, LeMonde, LeFigaro, Liberation, LesEchos, OuestFrance, LaTribune, LaCroix)

str(LaCroix)
str(LeParisien)




## Nettoyage La Croix 
# Convertir en chaîne de caractères et ajouter "01/01" devant chaque élément
LaCroix$date <- as.integer(LaCroix$date)
LaCroix$date <- paste0("01/01/", LaCroix$date)
class(LaCroix$date)

# Convertir en format date
LaCroix$date <- as.Date(LaCroix$date, format = "%d/%m/%Y")



write.csv(base_complete, "02_data_travail/base_complete_travail_vf_4.csv", row.names = FALSE)
View(LaCroix)

base <- fread("02_data_travail/base_complete_travail_vf_4.csv")

# Création de variables de travail 
base$year1 <- as.character(base$year)


## Date -- précision de 1970 à dec 2023. 


## Nettoyage : enlever les petits articles et ceux qui parlent des pays étrangers. (dans le tritre) ## 
# Exclure les auteurs "Le Monde", "AFP" et "latribune.fr"
base_filtre <- base[!(base$auteur1 %in% c("Le Monde", "AFP", "Latribune.fr", "Les Echos")), ]


# Compter les occurrences de chaque auteur
auteurs_counts <- table(base$auteur1)
auteur1_unique <- unique(base$auteur1)
auteur2_unique <- unique(base$auteur2)

resultats <- unique(resultats)


auteur1_unique  <- gsub(auteur1_unique, "([A-Z][a-z].*\\s)([A-Z][a-z].*)")
resultats <- grep("([A-Z][a-z].*\\s)([A-Z][a-z].*)", auteur1_unique, value = TRUE)
resultats2 <- grep("([A-Z][a-z].*\\s)([A-Z][a-z].*)", auteur2_unique, value = TRUE)
print(resultats2)

resultats1 <- gsub("\\(.*?\\)","", resultats)

resultats1 <- grep("^\\w+\\s\\w+$", resultats1, value = TRUE)

resultats1

# Trier les résultats par ordre décroissant
auteurs_sorted <- sort(auteurs_counts, decreasing = TRUE)
freq(auteurs_sorted)



# Sélectionner les 10 premiers auteurs
top_10_auteurs <- names(auteurs_sorted)[1:10]

## Enlever articles avec des pays dans les titres 



## Nettoyage auteurs ---

base$auteurs <- gsub("Correspondante à Buenos Aires","",base$auteurs)
base$auteur1 <- gsub("Correspondant?","",base$auteur1)
base$auteur2 <- gsub("Correspondant?","",base$auteur2)

base$auteurs <- gsub("(*)","",base$auteurs)
base$auteur1 <- gsub("(*)","",base$auteur1)
base$auteur2 <- gsub("(*)","",base$auteur2)


base$auteurs <- str_trim(base$auteurs)

#Ensuite, on distingue hommes et femmes
#il faut d'abord isoler le prénom des commissaires 
# Testons cela sur la colonne du premier commissaire



library(stringr)
base <- separate(base, auteur1, sep=" ", into=c("prenom_auteur1", "nom_auteur2"))
View(base)
tt6 <- separate(tt6, prenom1, sep=" et", into=c("prenom1", "prenom2"))
View(tt6)
tt6 <- separate(tt6, prenom, sep=" ", into=c("prenom", "nom"))
base_expo6$nul <- NULL
base_expo6$prenom

str(LeParisien)


# Liste des noms de pays à filtrer
pays_a_filtrer <- c("Zimbabwe", "Venezuela", "Argentine", "Allemagne", "Iran", "Brésil", 
                    "Turquie", "Russie", "Hongrie", "Nicaragua", "Angola", "Bolivie", 
                    "Congo", "Équateur", "Ghana", "Mexique", "Indonésie", 
                    "Nigéria","Etats-Unis")
# J'ai choisi de garder l'Ukraine : "Ukraine"



# Filtrage des lignes basé sur les pays à filtrer
base_filtre <- base[!grepl(paste(pays_a_filtrer, collapse = "|"), base$titre, ignore.case = TRUE), ]

## Nettoyage date 

base_filtree <- subset(base_filtree, date <= as.Date("2023-12-31"))

base$nb_mots <- str_count(base$texte, ' ')

boxplot(base$nb_mots)

base_filtree <- subset(base, base$nb_mots > 200)
View(base)

base_filtree <- subset(base_filtree, date <= as.Date("2023-12-31"))


write.csv(base, "02_data_travail/base_complete_travail_vf_6.csv")


### Enlever les dates après 31/12/2024 ------

base_filtree <- subset(base, date <= as.Date("2023-12-31"))

