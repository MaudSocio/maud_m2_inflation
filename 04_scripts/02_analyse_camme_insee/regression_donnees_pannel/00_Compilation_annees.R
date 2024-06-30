
# Compilation des données -----------------

# Boucle de 2

# Définir les années de début et de fin
annee_debut <- 2016
annee_fin <- 2016


# Boucle sur les années
for (annee in annee_debut:annee_fin) {
  # Construire le chemin du dossier pour l'année courante
  directory <- paste0("C:/Users/ymaud/dev/inflation_memoire/02_data/insee/", annee, "/Csv/")
  
  # Initialiser une liste pour stocker les dataframes mensuelles
  dfs <- list()
  
  # Boucle sur les mois
  for (mois in 1:12) {
    # Formatage du nom du fichier (avec gestion du format MM pour le mois)
    nom_fichier <- sprintf("archfpr%02d%02d.csv", annee - 2000, mois)
    chemin_complet <- paste0(directory, nom_fichier)
    
    # Vérifier si le fichier existe
    if (file.exists(chemin_complet)) {
      # Lire le fichier CSV et l'ajouter à la liste si le fichier existe
      dfs[[length(dfs) + 1]] <- read.csv2(chemin_complet)
    } else {
      # Imprimer un message si le fichier n'existe pas
      message("Le fichier ", chemin_complet, " n'existe pas et sera ignoré.")
    }
  }
  
  # S'il n'y a pas de données à concaténer, passer à l'année suivante
  if (length(dfs) == 0) next
  
  # Jonction des dataframes mensuelles
  df_annuelle <- do.call(rbind, dfs)
  
  # Enregistrer la dataframe annuelle dans un fichier CSV
  write.csv(df_annuelle, paste0("02_data/travail/Inflation_base_", annee, ".csv"), row.names = FALSE)
  
  # Nettoyer l'environnement des dataframes mensuelles
  rm(dfs)
  
  # Attendre (par exemple, 5 secondes) avant de passer à l'année suivante
  Sys.sleep(5)
}



# Boucle de 1991 à 2003
# Charger la bibliothèque plyr
library(plyr)

# Définir les années de début et de fin
annee_debut <- 1997
annee_fin <- 2003

# Boucle sur les années
for (annee in annee_debut:annee_fin) {
  directory <- paste0("C:/Users/ymaud/dev/inflation_memoire/02_data/insee/", annee, "/Csv/")
  
  # Initialiser une liste pour stocker les dataframes mensuelles
  dfs <- list()
  
  # Les lettres représentant les mois de 'a' à 'l'
  mois_lettres <- letters[1:12]
  
  for (mois_lettre in mois_lettres) {
    nom_fichier <- sprintf("camme%02d%s.csv", annee %% 100, mois_lettre)
    chemin_complet <- paste0(directory, nom_fichier)
    
    if (file.exists(chemin_complet)) {
      # Lire le fichier CSV
      df_temp <- read.csv2(chemin_complet, fileEncoding = "UTF-8")
      # Ajouter le dataframe à la liste
      dfs[[length(dfs) + 1]] <- df_temp
    } else {
      message("Le fichier ", chemin_complet, " n'existe pas et sera ignoré.")
    }
  }
  
  if (length(dfs) == 0) {
    message("Aucune donnée pour l'année ", annee)
    next
  }
  
  # Combinaison des dataframes avec rbind.fill
  df_annuelle <- do.call(rbind.fill, dfs)
  
  # Enregistrer la dataframe annuelle dans un fichier CSV
  write.csv(df_annuelle, paste0("02_data/travail/Inflation_base_", annee, ".csv"), row.names = FALSE)
  
  # Nettoyer l'environnement des dataframes mensuelles
  rm(dfs)
  
  Sys.sleep(5) # Attendre 5 secondes
}




# Cas 2016 ------
# Charger la bibliothèque plyr
library(plyr)

# Définir les années de début et de fin
annee_debut <- 2016
annee_fin <- 2016

# Boucle sur les années
for (annee in annee_debut:annee_fin) {
  directory <- paste0("C:/Users/ymaud/dev/inflation_memoire/02_data/insee/", annee, "/Csv/")
  
  # Initialiser une liste pour stocker les dataframes mensuelles
  dfs <- list()
  
  for (mois in 1:12) {
    # Formatage du nom du fichier (avec gestion du format MM pour le mois)
    nom_fichier <- sprintf("archfpr%02d%02d.csv", annee - 2000, mois)
    chemin_complet <- paste0(directory, nom_fichier)
    
    # Vérifier si le fichier existe
    if (file.exists(chemin_complet)) {
      # Lire le fichier CSV et l'ajouter à la liste si le fichier existe
      dfs[[length(dfs) + 1]] <- read.csv2(chemin_complet)
    } else {
      # Imprimer un message si le fichier n'existe pas
      message("Le fichier ", chemin_complet, " n'existe pas et sera ignoré.")
    }
  }
  
  if (length(dfs) == 0) {
    message("Aucune donnée pour l'année ", annee)
    next
  }
  
  # Combinaison des dataframes avec rbind.fill
  df_annuelle <- do.call(rbind.fill, dfs)
  
  # Enregistrer la dataframe annuelle dans un fichier CSV
  write.csv(df_annuelle, paste0("02_data/travail/Inflation_base_", annee, ".csv"), row.names = FALSE)
  
  # Nettoyer l'environnement des dataframes mensuelles
  rm(dfs)
  
  Sys.sleep(5) # Attendre 5 secondes
}


## Compilation des données ----------------------
# Charger la bibliothèque plyr pour utiliser rbind.fill
library(plyr)

# Fonction pour combiner les fichiers avec gestion avancée des colonnes
combine_csv_files_advanced <- function(start_year, end_year, output_file) {
  library(dplyr)
  
  # Initialiser une liste pour les dataframes et un vecteur pour les noms de colonnes uniques
  dfs <- list()
  all_column_names <- character()
  
  # Lire les fichiers et recueillir tous les noms de colonnes
  for (year in start_year:end_year) {
    file_path <- sprintf("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_%d.csv", year)
    if (file.exists(file_path)) {
      df <- read.csv2(file_path)
      dfs[[length(dfs) + 1]] <- df
      all_column_names <- union(all_column_names, names(df))
    }
  }
  
  # Ajuster chaque dataframe pour qu'il contienne toutes les colonnes, en ajoutant des NA si nécessaire
  dfs_adjusted <- lapply(dfs, function(df) {
    missing_cols <- setdiff(all_column_names, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    df[, all_column_names] # Réordonner les colonnes pour qu'elles soient uniformes
  })
  
  # Combiner les dataframes ajustés
  combined_df <- bind_rows(dfs_adjusted)
  
  # Écrire le résultat dans un fichier
  write.csv(combined_df, output_file, row.names = FALSE)
}

# Combinaison des fichiers
combine_csv_files_advanced(1991, 2003, "C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_91_03.csv")
combine_csv_files_advanced(2004, 2021, "C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_04_21.csv")
