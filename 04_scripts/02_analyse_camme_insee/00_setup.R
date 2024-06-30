#### PACKAGES ET OPTIONS #####

# PACKAGES ----

## Instalation des packages 
library(shiny)
library(tidyverse)
library(here)
# library(nycflights13)
library(dplyr)
library(foreign)
library(questionr)
# library(stats)
# library(survey)
library(gtsummary)
# library(factoextra)
# library(FactoMineR) # pour l'analyse géométrique des données
## install.packages("bsi")
# library(RColorBrewer) # pour les palettes de couleurs

# library(utils)
library(esquisse)
library(ggplot2)
library(tidyr)
## install.packages("tidyr")  # Installer         # Charge

## install.packages("Rtools")

### Pour afficher toutes les palettes de couleurs dans le package, tapez ceci:
library(RColorBrewer)
library(readr)
library(data.table)

## display.brewer.all()
# 
# library(stringr)
# library(wdman)
# library(tibble)
# library(netstat)
# library(readr)

# library(janitor) # pour des outils de description
# library(broom)
# library(officer)
# library(here)
# library(survey)
# library(knitr)
# library(rmarkdown)
# library(bookdown)
# library(printr)
# library(rmdformats)
# library(rticles)
# library(readxl)
# library(scales)
# library(ggrepel) # pour que les libellés des points ne se chevauchent pas (crucial !)
library(flextable) # pour mettre en forme les tableaux


# library(viridis)  
# library(readr)

## ACM 

# # Importer les packages n?cessaires
# library(FactoMineR)
# library(Factoshiny)
# #install.packages("Factoshiny")
# library(explor)
# # install.packages("ade4", dep = TRUE)
# library(ade4)
# # Pour la stat descriptive
# library(questionr)
# ## Pour le recodage
# library(tidyverse)

# # Regression -----
# library(margins)
# library(car)
# library(lmtest)


# OPTIONS ----

set_flextable_defaults(decimal.mark = ",", big.mark = " ", na_st = "-")

options(OutDec= ",")

options(scipen=999) # pour désactiver l'écriture scientifique des nombres


options(digits = 3)

distrib <- function(var) {
  rep <- cbind("min" = quantile(var, probs = 0.00, na.rm = T), 
               "p01" = quantile(var, probs = 0.01, na.rm = T, type = 1),
               "p05" = quantile(var, probs = 0.05, na.rm = T, type = 1), 
               "p20" = quantile(var, probs = 0.20, na.rm = T, type = 1), 
               "p25" = quantile(var, probs = 0.25, na.rm = T, type = 1), 
               "p40" = quantile(var, probs = 0.40, na.rm = T, type = 1), 
               "p50" = quantile(var, probs = 0.50, na.rm = T, type = 1), 
               "p60" = quantile(var, probs = 0.60, na.rm = T, type = 1), 
               "p75" = quantile(var, probs = 0.75, na.rm = T, type = 1), 
               "p80" = quantile(var, probs = 0.80, na.rm = T, type = 1), 
               "p95" = quantile(var, probs = 0.95, na.rm = T, type = 1), 
               "p99" = quantile(var, probs = 0.99, na.rm = T, type = 1), 
               "max" = quantile(var, probs = 1.00, na.rm = T), 
               "mean"= mean(var, na.rm = T), 
               "sd"  = sd(var, na.rm = T), 
               "n"     = sum(is.na(var)) + sum(!is.na(var)),
               "NA(n)" = sum(is.na(var)),
               "NA(%)" = round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3)
  )
  rownames(rep) <- NULL
  return(rep)
}


graphDistrib <- function(var) {
  data <- data.frame(value = var)
  plot <- ggplot(data = data, aes(x = value)) + geom_density()
  return(plot)
}

## Creation des fonctions  graphDistribCroisee et distribCroisee

distribCroisee <- function(var1, var2) {
  rep <- cbind("min" = tapply(var1, var2, quantile, probs = 0.00, na.rm = T),
               "p01" = tapply(var1, var2, quantile, probs = 0.01, na.rm = T, type = 1),
               "p05" = tapply(var1, var2, quantile, probs = 0.05, na.rm = T, type = 1),
               "p20" = tapply(var1, var2, quantile, probs = 0.20, na.rm = T, type = 1),
               "p25" = tapply(var1, var2, quantile, probs = 0.25, na.rm = T, type = 1),
               "p40" = tapply(var1, var2, quantile, probs = 0.40, na.rm = T, type = 1),
               "p50" = tapply(var1, var2, quantile, probs = 0.50, na.rm = T, type = 1),
               "p60" = tapply(var1, var2, quantile, probs = 0.60, na.rm = T, type = 1),
               "p75" = tapply(var1, var2, quantile, probs = 0.75, na.rm = T, type = 1),
               "p80" = tapply(var1, var2, quantile, probs = 0.80, na.rm = T, type = 1),
               "p95" = tapply(var1, var2, quantile, probs = 0.95, na.rm = T, type = 1),
               "p99" = tapply(var1, var2, quantile, probs = 0.99, na.rm = T, type = 1),
               "max" = tapply(var1, var2, quantile, probs = 1.00, na.rm = T),
               "mean"= tapply(var1, var2, mean, na.rm = T),
               "sd"  = tapply(var1, var2, sd, na.rm = T),
               "n"     = tapply(var1, var2, function(var) {return(sum(is.na(var)) + sum(!is.na(var)))}),
               "NA(n)" = tapply(var1, var2, function(var) {return(sum(is.na(var)))}),
               "NA(%)" = tapply(var1, var2, function(var) {return(round(sum(is.na(var)) / (sum(is.na(var)) + sum(!is.na(var))) * 100, 3))})
  )
  return(rep)
}

graphDistribCroisee <- function(var1, var2) {
  data <- data.frame(value1 = var1, value2 = var2)
  plot <- ggplot(data = data, aes(x = value1, group = value2, color = value2)) + 
    geom_density()
  return(plot)
}
