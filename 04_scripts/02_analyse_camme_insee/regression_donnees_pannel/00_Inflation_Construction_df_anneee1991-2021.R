### Construction d'une base de données avec tout

library(tidyverse)
library(nycflights13)
library(dplyr)
library(foreign)
library(questionr)
library(stats)
library(survey)
library(gtsummary)
library(factoextra)
library(gplots)
library(FactoMineR)
library(gtsummary)
library(esquisse)
library(ggplot2)
library(viridis)  
library(readr)
library(data.table)
library(here)



# Annee 2021 -------

janv2021 <- fread(here("02_data/insee/2021/Csv/archfpr2101.csv"))
fev2021 <- fread(here("02_data/insee/2021/Csv/archfpr2102.csv"))
mar2021 <- fread(here("02_data/insee/2021/Csv/archfpr2103.csv"))
av2021 <- fread(here("02_data/insee/2021/Csv/archfpr2104.csv"))
mai2021 <- fread(here("02_data/insee/2021/Csv/archfpr2105.csv"))
juin2021 <- fread(here("02_data/insee/2021/Csv/archfpr2106.csv"))
juill2021 <- fread(here("02_data/insee/2021/Csv/archfpr2107.csv"))
aout2021 <- fread(here("02_data/insee/2021/Csv/archfpr2108.csv"))
sept2021 <- fread(here("02_data/insee/2021/Csv/archfpr2109.csv"))
oct2021 <- fread(here("02_data/insee/2021/Csv/archfpr2110.csv"))
nov2021 <- fread(here("02_data/insee/2021/Csv/archfpr2111.csv"))
dec2021 <- fread(here("02_data/insee/2021/Csv/archfpr2112.csv"))

### Jonction des dataframe 
df  <- list(janv2021,fev2021, mar2021, av2021, mai2021, juin2021, juill2021, aout2021, sept2021, oct2021, nov2021, dec2021)
Annee2021 <- do.call(rbind, df)

write.csv(Annee2021, "Inflation_base_2021.csv") 

directory2 <- "C:/Users/ymaud/Documents/dev/inflation_memoire/inflation_data/2020/Csv/"



# Annee 2020 -------
janv2020 <- fread(here("02_data/insee/2020/Csv/archfpr2001.csv"))
fev2020 <- fread(here("02_data/insee/2020/Csv/archfpr2002.csv"))
mar2020 <- fread(here("02_data/insee/2020/Csv/archfpr2003.csv"))
av2020 <- fread(here("02_data/insee/2020/Csv/archfpr2004.csv"))
mai2020 <- fread(here("02_data/insee/2020/Csv/archfpr2005.csv"))
juin2020 <- fread(here("02_data/insee/2020/Csv/archfpr2006.csv"))
juill2020 <- fread(here("02_data/insee/2020/Csv/archfpr2007.csv"))
aout2020 <- fread(here("02_data/insee/2020/Csv/archfpr2008.csv"))
sept2020 <- fread(here("02_data/insee/2020/Csv/archfpr2009.csv"))
oct2020 <- fread(here("02_data/insee/2020/Csv/archfpr2010.csv"))
nov2020 <- fread(here("02_data/insee/2020/Csv/archfpr2011.csv"))
dec2020 <- fread(here("02_data/insee/2020/Csv/archfpr2012.csv"))
### Jonction des dataframe 
df  <- list(janv2020,fev2020, mar2020, av2020, mai2020, juin2020, juill2020, aout2020, sept2020, oct2020, nov2020, dec2020)
Annee2020 <- do.call(rbind, df)

write.csv(Annee2020, "Inflation_base_2020.csv") 

### Jonction des bases 2019
directory3 <- "C:/Users/ymaud/Documents/dev/inflation_memoire/inflation_data/2019/Csv/"


# Annee 2019 -------
janv2019 <- fread(here("02_data/insee/2019/Csv/archfpr1901.csv"))
fev2019 <- fread(here("02_data/insee/2019/Csv/archfpr1902.csv"))
mar2019 <- fread(here("02_data/insee/2019/Csv/archfpr1903.csv"))
av2019 <- fread(here("02_data/insee/2019/Csv/archfpr1904.csv"))
mai2019 <- fread(here("02_data/insee/2019/Csv/archfpr1905.csv"))
juin2019 <- fread(here("02_data/insee/2019/Csv/archfpr1906.csv"))
juill2019 <- fread(here("02_data/insee/2019/Csv/archfpr1907.csv"))
aout2019 <- fread(here("02_data/insee/2019/Csv/archfpr1908.csv"))
sept2019 <- fread(here("02_data/insee/2019/Csv/archfpr1909.csv"))
oct2019 <- fread(here("02_data/insee/2019/Csv/archfpr1910.csv"))
nov2019 <- fread(here("02_data/insee/2019/Csv/archfpr1911.csv"))
dec2019 <- fread(here("02_data/insee/2019/Csv/archfpr1912.csv"))
### Jonction des dataframe 
df  <- list(janv2019,fev2019, mar2019, av2019, mai2019, juin2019, juill2019, aout2019, sept2019, oct2019, nov2019, dec2019)
Annee2019 <- do.call(rbind, df)

## Collage
write.csv(Annee2019, "Inflation_base_2019.csv") 



# Annee 2018 -------
# Directory
directory4 <- "C:/Users/ymaud/Documents/dev/inflation_memoire/inflation_data/2018/Csv/"
## Base
janv2018 <- read.csv2(paste0(directory4,'archfpr1801.csv'))
fev2018 <- read.csv2(paste0(directory4,'archfpr1802.csv'))
mar2018 <- read.csv2(paste0(directory4,'archfpr1803.csv'))
av2018 <- read.csv2(paste0(directory4,'archfpr1804.csv'))
mai2018 <- read.csv2(paste0(directory4,'archfpr1805.csv'))
juin2018 <- read.csv2(paste0(directory4,'archfpr1806.csv'))
juill2018 <- read.csv2(paste0(directory4,'archfpr1807.csv'))
aout2018 <- read.csv2(paste0(directory4,'archfpr1808.csv'))
sept2018 <- read.csv2(paste0(directory4,'archfpr1809.csv'))
oct2018 <- read.csv2(paste0(directory4,'archfpr1810.csv'))
nov2018 <- read.csv2(paste0(directory4,'archfpr1811.csv'))
dec2018 <- read.csv2(paste0(directory4,'archfpr1812.csv'))
### Jonction des dataframe 
df  <- list(janv2018,fev2018, mar2018, av2018, mai2018, juin2018, juill2018, aout2018, sept2018, oct2018, nov2018, dec2018)
Annee2018 <- do.call(rbind, df)

## Collage
write.csv(Annee2018, "Inflation_base_2018.csv") 

### Toutes les bases 
dA  <- list(Annee2018, Annee2019, Annee2020, Annee2021)
AnneeGen <- bind_rows(dA)

write.csv(AnneeGen, "Inflation_base_18_21.csv") 
# AnneeGen <- bind_rows(dA, .id = "Source")
# L'argument .id = "Source" ajoute une colonne nommée "Source" pour indiquer l'origine de chaque ligne


library(data.table)
library(here)

# Fonction pour charger et joindre les données d'une année donnée
load_and_bind_year_data <- function(year) {
  months <- c("janv", "fev", "mar", "av", "mai", "juin", "juill", "aout", "sept", "oct", "nov", "dec")
  month_codes <- sprintf("%02d", 1:12)  # Génère "01", "02", ..., "12"
  filenames <- paste0("archfpr", substr(year, 3, 4), month_codes, ".csv")
  
  monthly_data <- lapply(filenames, function(file) {
    fread(here("02_data/insee", substr(year, 3, 4), "Csv", file))
  })
  
  do.call(rbind, monthly_data)
}

# Boucle sur les années souhaitées et enregistre les données combinées
for (year in 2015:2018) {
  year_data <- load_and_bind_year_data(as.character(year))
  write.csv(year_data, paste0("Inflation_base_", year, ".csv"))
}



