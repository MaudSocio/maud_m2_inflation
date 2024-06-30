### Etape 2 Recodage base de données ########

## Instalation des packages 

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
## install.packages("esquisse")
library(esquisse)
library(ggplot2)
## install.packages("viridis")  # Installer
library(viridis)           # Charge

## install.packages("RColorBrewer")

### Pour afficher toutes les palettes de couleurs dans le package, tapez ceci:
library(RColorBrewer)
library(readr)
## display.brewer.all()


# directory
setwd("C:/Users/ymaud/Documents/dev/inflation_memoire/inflation_script")

### Base 2021###
AnneeGen <- read_csv("Inflation_base_18_21.csv")

## Recodage du SEXE 
AnneeGen$SEXE_rec <- as.character(AnneeGen$SEXE)
AnneeGen$SEXE_rec[AnneeGen$SEXE == "1"] <- "Homme"
AnneeGen$SEXE_rec[AnneeGen$SEXE == "2"] <- "Femme"
table(AnneeGen$SEXE_rec)

AnneeGen %>%
  tbl_summary(include = c("SEXE_rec"))