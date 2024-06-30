## Importation des données

## Importation des donnees brutes
# Annee2021 <- read_csv(here("02_data/travail", "Inflation_base_2021.csv"))
# Annee2020 <- read_csv(here("02_data/travail", "Inflation_base_2020.csv"))
# Annee2019 <- read_csv(here("02_data/travail", "Inflation_base_2019.csv"))
# Annee2018 <- read_csv(here("02_data/travail", "Inflation_base_2018.csv"))
# AnneeGeneral <- read_csv(here("02_data/travail", "Inflation_base_18_21.csv"))

## Importation des donnees recodées
library(data.table)
library(here)


Annee2021 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2021_recodee.csv")



## PONDERATION -----
# library(survey)
# moy_pond <- mean(Annee2021$IWEIGHT)
# Annee2021$POND <- Annee2021$IWEIGHT / moy_pond
# Annee2021_svy <- svydesign(id=~1,weights=~POND, data=Annee2021)
# 
# 
# Annee2021$MOISENQ