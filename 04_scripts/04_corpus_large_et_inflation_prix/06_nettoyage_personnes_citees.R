

tt <- ("06_data_base/base_personnes_genre_v3.csv")

# Analyse du genre Haut et Bas --------------
library(ResourceSelection)
library(gt)
library(gtsummary)
library(md)
library(data.table)
library(forcats) # Pour Ftc revlevel
library(dplyr)

df <- fread("06_data_base/haut_base_articles_v6.csv")
str(df)


# Lire les données (supposons que df soit déjà chargé dans l'environnement)
# df <- read.csv("path_to_your_file.csv") # Utilisez cette ligne si vous devez charger les données depuis un fichier

# Sélectionner les colonnes prenom_auteur1 et nom_auteur1
new_df <- df

# Supprimer les doublons
new_df <- unique(new_df)

# Fonction pour nettoyer les chaînes de caractères
clean_string <- function(string) {
  string <- gsub("/", "", string) # Supprimer les /
  string <- gsub("\\(", "", string) # Supprimer les (
  string <- gsub("\\)", "", string) # Supprimer les )
  string <- gsub("\\(.*?\\)", "", string) # Supprimer les éléments entre parenthèses
  string <- trimws(string) # Supprimer les espaces en début et fin de chaîne
  return(string)
}

# Appliquer la fonction de nettoyage aux colonnes prenom_auteur1 et nom_auteur1
new_df$prenom_auteur1 <- sapply(new_df$prenom_auteur1, clean_string)
new_df$nom_auteur1 <- sapply(new_df$nom_auteur1, clean_string)

# Fonction pour traiter les cas spécifiques
update_names <- function(prenom, nom, keyword, phrase) {
  if (grepl(keyword, prenom, ignore.case = TRUE) && grepl(phrase, nom, ignore.case = TRUE)) {
    parts <- unlist(strsplit(nom, phrase))
    if (length(parts) > 1) {
      name_parts <- unlist(strsplit(parts[2], " "))
      if (length(name_parts) >= 2) {
        prenom <- name_parts[1]
        nom <- name_parts[2]
      }
    }
  }
  return(list(prenom, nom))
}

# Appliquer la fonction pour traiter les cas "CHAT", "PROPOS" et "DOSSIER"
for (i in 1:nrow(new_df)) {
  result <- update_names(new_df$prenom_auteur1[i], new_df$nom_auteur1[i], "CHAT", "Modéré Par ")
  new_df$prenom_auteur1[i] <- result[[1]]
  new_df$nom_auteur1[i] <- result[[2]]
  
  result <- update_names(new_df$prenom_auteur1[i], new_df$nom_auteur1[i], "PROPOS", "recueillis par ")
  new_df$prenom_auteur1[i] <- result[[1]]
  new_df$nom_auteur1[i] <- result[[2]]
  
  result <- update_names(new_df$prenom_auteur1[i], new_df$nom_auteur1[i], "DOSSIER", "Réalisé Par ")
  new_df$prenom_auteur1[i] <- result[[1]]
  new_df$nom_auteur1[i] <- result[[2]]
}

new_dt_2 <- new_df
# Assurez-vous d'avoir chargé la bibliothèque dplyr
library(dplyr)

new_df$nom_auteur1
new_df$prenom_auteur1
# Réorganiser les colonnes
new_df <- new_df %>%
  select(titre, journal, date, auteurs, texte, prenom_auteur1, nom_auteur1, everything())

new_df <- new_df %>% arrange(journal, date,auteurs)
# Afficher les premières lignes pour vérifier le résultat
head(new_df)

df <- fread("06_data_base/haut_base_articles_v7.csv")


library(stringi)



# Fonction pour nettoyer et mettre à jour les noms
clean_name <- function(prenom, nom) {
  # Remplacer les caractères indésirables, enlever les accents et mettre en majuscules
  prenom <- toupper(stri_trans_general(prenom, "Latin-ASCII"))
  nom <- toupper(stri_trans_general(nom, "Latin-ASCII"))
  
  # Supprimer les mots indésirables et les caractères spéciaux
  prenom <- gsub("[*\"CRISE|DOSSIER|\\!]", "", prenom)
  nom <- gsub("[*\"CRISE|DOSSIER|\\!]", "", nom)
  
  # Supprimer les éléments après des caractères spécifiques
  nom <- gsub("( À| à| —|;| CORRESPONDANT).*", "", nom)
  
  # Corriger les prénoms et noms en fonction de mots-clés
  if(grepl("PAR", prenom)) {
    prenom <- sub(".*PAR ", "", prenom)
  }
  if(grepl("PROPOS", prenom)) {
    prenom <- sub(".*PROPOS ", "", prenom)
  }
  if(grepl("RECUEILLI", prenom)) {
    prenom <- sub(".*RECUEILLI ", "", prenom)
  }
  if(grepl("LA", prenom)) {
    prenom <- sub(".*LA ", "", prenom)
  }
  
  return(list(prenom = prenom, nom = nom))
}

# Appliquer la fonction à chaque ligne du data frame
for (i in 1:nrow(df)) {
  result <- clean_name(df$prenom_auteur1[i], df$nom_auteur1[i])
  df$prenom_auteur1[i] <- result$prenom
  df$nom_auteur1[i] <- result$nom
}

# Fonction pour convertir en majuscules et supprimer les accents
clean_text <- function(text) {
  text <- toupper(stri_trans_general(text, "Latin-ASCII"))
  return(text)
}

# Appliquer la fonction à chaque ligne du data frame
df$prenom_auteur1 <- sapply(df$prenom_auteur1, clean_text)
df$nom_auteur1 <- sapply(df$nom_auteur1, clean_text)


write.csv(df, "06_data_base/haut_base_articles_v7.csv", row.names = FALSE)




