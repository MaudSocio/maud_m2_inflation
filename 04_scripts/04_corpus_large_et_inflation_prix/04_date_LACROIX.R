
library(dplyr)
library(lubridate)

base <- fread("06_data_base/base_inflation_prix_v3.csv")

base <- fread("06_data_base/haut_base_articles.csv")

base$date <- as.Date(base$date)

base <- base %>%
  # Opérer ligne par ligne
  rowwise() %>%
  # Mutate avec une fonction personnalisée pour chaque ligne
  mutate(date = if_else(journal == "La Croix",
                        as.Date(paste(year(date), 
                                      sample(1:12, 1), # Mois aléatoire
                                      sample(1:28, 1), # Jour aléatoire, en assumant un maximum de 28 jours pour simplification
                                      sep = "-")),
                        date)) %>%
  # Revenir à une opération sur l'ensemble des données
  ungroup()

# Vérification des premières lignes de la base mise à jour
head(base)

library(dplyr)
library(lubridate)

# Assurez-vous que les données 'date' sont au bon format
base$date <- as.Date(base$date)

base <- base %>%
  mutate(
    mois = if_else(
      journal == "La Croix",
      paste(format(date, "%b", usetz = TRUE, tz = "Europe/Paris"), year(date)),
      mois
    )
  )


write.csv(base_inflation, "06_data_base/base_inflation_prix_v4.csv", row.names = FALSE)
