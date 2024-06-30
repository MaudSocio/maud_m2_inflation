

base <- fread(here("02_data_travail/base_complete_travail_vf_5.csv"))


## Prendre les prénoms -------------
str(base)
  
base  <- base %>%
  mutate(auteur1 = str_replace_all(auteur1, "Le Monde|LIBERATION|AFP|afp|Annecy|Anniversaire|Associations|Assurance|Automobile|Autoroutes|Banque_centrale|Banque|Baromètre", ""), # Suppression de "Le Monde" et "LIBERATION"
         auteur1 = str_trim(auteur1), # Supprime les espaces superflus après la suppression
         prenom1 = str_extract(auteur1, "^[\\w-]+"),
         nom1 = str_replace(auteur1, "^[\\w-]+\\s+", ""),
         prenom1 = str_to_title(prenom1), 
         prenom1 = if_else(str_length(prenom1) == 1, "", prenom1)) # Supprime les prénoms d'une seule lettre) # Premiere lettre en majuscule 
  
  base$prenom1
  freq(is.na(base$prenom1))
  
  ## J'ai quand même 48 068 noms ! 
  
 # Pb à régler :  
  # Al 
  # Ann
  # Ar
  # Art
  # Arts
  
  
  
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
  
  
  write.csv(new_df, "06_data_base/haut_base_articles_v7.csv", row.names = FALSE)
  
  df <- fread("06_data_base/haut_base_articles_v7.csv")
  
  head(df)
  
  