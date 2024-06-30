library(readr)  # Preferred for its fast reading capabilities and handling of different file formats
library(plyr)   # For using rbind.fill
library(dplyr)  # For data manipulation

# Function to read and aggregate data for a given year
process_year_data <- function(year, directory_base) {
  directory <- paste0(directory_base, year, "/Csv/")
  dfs <- list()
  
  # Using the last two digits of the year and two digits for the month
  file_template <- "archfpr%02d%02d.csv"
  
  for (month in 1:12) {
    file_name <- sprintf(file_template, year %% 100, month)  # Corrected to always use last two digits of the year
    file_path <- paste0(directory, file_name)
    
    if (file.exists(file_path)) {
      dfs[[length(dfs) + 1]] <- read_csv(file_path, show_col_types = FALSE)
    } else {
      message("Le fichier ", file_path, " n'existe pas et sera ignoré.")
    }
  }
  
  if (length(dfs) == 0) {
    message("Aucune donnée pour l'année ", year)
    return(NULL)
  }
  
  df_annuelle <- do.call(rbind.fill, dfs)
  return(df_annuelle)
}

# Function to process multiple years
process_years <- function(start_year, end_year, directory_base, output_base) {
  for (year in start_year:end_year) {
    message("Processing year: ", year)
    df_annuelle <- process_year_data(year, directory_base)
    
    if (!is.null(df_annuelle)) {
      output_file_path <- paste0(output_base, "Inflation_base_", year, ".csv")
      write_csv(df_annuelle, output_file_path)
      Sys.sleep(5)  # Optional: Pause for 5 seconds
    }
  }
}

# Set base directories for input and output
input_directory_base <- "C:/Users/ymaud/dev/inflation_memoire/02_data/insee/"
output_directory_base <- "C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/"

# Process the years as needed
process_years(2004, 2021, input_directory_base, output_directory_base)

# Function to aggregate all yearly CSV files into one
aggregate_all_years <- function(start_year, end_year, output_base, final_output_file) {
  all_dfs <- list()
  
  for (year in start_year:end_year) {
    file_path <- paste0(output_base, "Inflation_base_", year, ".csv")
    
    if (file.exists(file_path)) {
      df <- read_csv(file_path, show_col_types = FALSE)  # read_csv uses comma as the default delimiter
      all_dfs[[length(all_dfs) + 1]] <- df
    } else {
      message("Le fichier ", file_path, " n'existe pas et sera ignoré.")
    }
  }
  
  if (length(all_dfs) == 0) {
    message("Aucune donnée à agréger.")
    return(NULL)
  }
  
  combined_df <- do.call(rbind.fill, all_dfs)
  write_csv(combined_df, final_output_file)  # write_csv uses comma as the default delimiter
}

# Set base directories for input and output
input_directory_base <- "C:/Users/ymaud/dev/inflation_memoire/02_data/insee/"
output_directory_base <- "C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/"

# Process the years individually
process_years(2004, 2021, input_directory_base, output_directory_base)

# Aggregate all the processed yearly files into one
final_output_file <- "C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years.csv"
aggregate_all_years(2004, 2021, output_directory_base, final_output_file)

## nettoyage df ------------
df <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years.csv", sep = ";")

# Function to clean the "ANENQ" column
clean_anenq <- function(df) {
  if (!"ANENQ" %in% colnames(df)) return(df)
  
  # Extract four-digit year from the "ANENQ" column
  df$ANENQ <- as.numeric(gsub(".*(\\d{4}).*", "\\1", df$ANENQ))
  
  # Ensure all values are valid four-digit years or NA
  df$ANENQ <- ifelse(is.na(df$ANENQ) | (df$ANENQ >= 1000 & df$ANENQ <= 9999), df$ANENQ, NA)
  
  return(df)
}

# Example usage of the function on an aggregated DataFrame
# Assuming 'combined_df' is the DataFrame resulting from aggregation

# Apply the cleaning function to the combined DataFrame
df <- clean_anenq(df)
rm()

# Save the cleaned DataFrame
fwrite(df,"C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v2.csv", sep = ";", row.names = FALSE)


# Example usage of the function on an aggregated DataFrame
# Assuming 'combined_df' is the DataFrame resulting from aggregation

# Apply the cleaning function to the combined DataFrame
combined_df <- clean_anenq(combined_df)


library(dplyr)

# Vérifier les noms de colonnes en double
duplicate_columns <- names(df)[duplicated(names(df))]
print(duplicate_columns)

# Supprimer les colonnes en double
df <- df %>%
  select(-one_of(duplicate_columns))


# Save the cleaned DataFrame
write_csv(combined_df, "C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_all_years_cleaned.csv")
