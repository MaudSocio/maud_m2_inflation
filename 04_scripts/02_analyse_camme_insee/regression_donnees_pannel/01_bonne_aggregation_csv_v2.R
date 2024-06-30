library(data.table)

# Fonction pour nettoyer et préparer les data frames
clean_and_align_data <- function(df) {
  # Convertir et nettoyer les colonnes spécifiques
  if ("ANENQ" %in% names(df)) df[, ANENQ := as.integer(ANENQ)]
  if ("MOISENQ" %in% names(df)) df[, MOISENQ := as.character(MOISENQ)]
  if ("SSECH" %in% names(df)) df[, SSECH := as.character(SSECH)]
  if ("NUMFA" %in% names(df)) df[, NUMFA := as.character(NUMFA)]
  
  # Filtrer pour éliminer les lignes où PRIXPLUS est NA si la colonne existe
  if ("PRIXPLUS" %in% names(df)) {
    df <- df[!is.na(PRIXPLUS)]
  }
  
  # Supprimer les colonnes non nécessaires si elles existent
  unnecessary_cols <- c("DEJATRA", "DEJTRACJ", "STATUTCJ", "CLASPRCJ")
  df[, (unnecessary_cols) := NULL, with = FALSE]
  
  return(df)
}

# Fonction pour traiter les données de chaque année et chaque mois
process_data_files <- function(start_year, end_year, directory_base, final_output_file) {
  all_dfs <- list()
  
  for (year in start_year:end_year) {
    for (month in 1:12) {
      file_name <- sprintf("archfpr%02d%02d.csv", year %% 100, month)
      file_path <- file.path(directory_base, as.character(year), "Csv", file_name)
      
      if (file.exists(file_path)) {
        message(sprintf("Traitement du fichier : %s", file_path))
        df <- fread(file_path)
        cleaned_df <- clean_and_align_data(df)
        all_dfs[[length(all_dfs) + 1]] <- cleaned_df
      } else {
        message(sprintf("Le fichier %s n'existe pas.", file_path))
      }
    }
  }
  
  # Concaténer tous les data frames en un seul avec gestion des colonnes manquantes
  if (length(all_dfs) > 0) {
    final_df <- rbindlist(all_dfs, fill = TRUE)  # Utiliser fill=TRUE pour gérer les colonnes inégales
    fwrite(final_df, final_output_file)
    message(sprintf("Les données agrégées ont été enregistrées dans : %s", final_output_file))
  } else {
    message("Aucune donnée à agréger.")
  }
}

# Spécifier les chemins de base pour les répertoires d'entrée et de sortie
input_directory_base <- "C:/Users/ymaud/dev/inflation_memoire/02_data/insee/"
final_output_file <- "C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years.csv"

# Exécuter le traitement des données pour les années spécifiées
process_data_files(2004, 2021, input_directory_base, final_output_file)
