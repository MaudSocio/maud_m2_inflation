# Regression linéaire sur la perception de l'inflation

library(data.table)
library(here)
library(forcats)
library(lme4) 
library(MASS) # Pour la transformation Box-Cox
library(gtsummary)

theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")

Annee2021 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2021_recodee.csv")



# Situation financière --------------


## Recodage de Annee2021$FINANCES en Annee2021$FINANCES_rec
Annee2021$FINANCES_rec <- Annee2021$FINANCES %>%
  as.character() %>%
  fct_recode(
    "Met de d'argent de côté" = "1",
    "Met de d'argent de côté" = "2",
    "Budget juste" = "3",
    "Tire un peu sur les réserves" = "4",
    "Endettement" = "5",
    NULL = "9"
  )

Annee2021$FINANCES_rec <- as.factor(Annee2021$FINANCES_rec) 

## Réordonnancement de Annee2021$FINANCES_rec
Annee2021$FINANCES_rec <- Annee2021$FINANCES_rec %>%
  fct_relevel(
    "Met de d'argent de côté",
    "Budget juste", "Tire un peu sur les réserves", "Endettement"
  )


## Recodage de Annee2021$Ageind en Annee2021$Ageind_rec
Annee2021$Ageind_tranche <- cut(Annee2021$Ageind,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 26, 36, 46, 56, 66, 76, 121)
)

Annee2021$Ageind_tranche <- as.factor(Annee2021$Ageind_tranche)

## Recodage de Annee2021$Ageind_tranche en Annee2021$Ageind_tranche_rec
Annee2021$Ageind_tranche <- Annee2021$Ageind_tranche %>%
  fct_recode(
    "Moins de 26 ans" = "[0,26)",
    "26 à 36 ans" = "[26,36)",
    "36 à 45 ans" = "[36,46)",
    "46 à 55 ans" = "[46,56)",
    "56 à 65 ans" = "[56,66)",
    "66 à 75 ans" = "[66,76)",
    "76 ans et plus" = "[76,121]"
  )


# Annee2021$SITUAECO_rec_rec <- Annee2021$SITUAECO_rec %>%
#   fct_recode(
#     NULL = "Ne sait pas",
#     "s’est améliorée" = "s’est nettement améliorée",
#     "s’est améliorée" = "s’est un peu améliorée"
#   )
# 
# 
# ## Réordonnancement de Annee2021$SITUAECO_rec_rec
# Annee2021$SITUAECO_rec_rec <- Annee2021$SITUAECO_rec_rec %>%
#   fct_relevel(
#     "est restée stationnaire", "s’est nettement dégradée",
#     "s’est un peu dégradée", "s’est améliorée"
#   )

# Diplome --------------------
Annee2021$DIPLOME_rec <- Annee2021$DIPLOME_rec %>%
  fct_relevel(
    "Aucun diplôme ou certificat d’études primaires", "CAP, BEP ou équivalent",
    "Baccalauréat, brevet professionnel ou équivalent", "Diplôme du supérieur court (niveau bac + 2)",
    "Diplôme du supérieur long (supérieur à bac + 2)"
  )
## Mono --------------
Annee2021$MONOPARENTALE<- as.factor(Annee2021$MONOPARENTALE)

## Réordonnancement de Annee2021$MONOPARENTALE
Annee2021$MONOPARENTALE <- Annee2021$MONOPARENTALE %>%
  fct_relevel(
    "Pas monoparental", "Monoparental"
  )


## Genre -----------
Annee2021$SEXE_rec <- as.factor(Annee2021$SEXE_rec)
Annee2021$SEXE_rec <- relevel(Annee2021$SEXE_rec, "Homme")



## Réordonnancement de Annee2021$MOISENQ_rec
Annee2021$MOISENQ_rec <-as.factor(Annee2021$MOISENQ_rec)
Annee2021$MOISENQ_rec <- Annee2021$MOISENQ_rec %>%
  fct_relevel(
    "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
    "Aout", "Septembre", "Octobre", "Novembre", "Décembre")


## Regression linéaire 

# reg_lineaire <- lm(prixplus_rec ~ MONOPARENTALE + SITUAECO_rec_rec + Ageind + SEXE_rec + IPC + DIPLOME_rec + interaction(factor(MOISENQ_rec),factor(VAGUEE)),data = Annee2021)



## Réordonnancement de Annee2021$PROFESSION
Annee2021$PROFESSION <- as.factor(Annee2021$PROFESSION)
Annee2021$PROFESSION <- Annee2021$PROFESSION %>%
  fct_relevel(
    "Ouvrier, technicien, agent de maîtrise", "Employé", "Ingénieur, cadre, directeur"
  )

# Régression linéaire pondérée
reg_lineaire_ponderee <- lm(prixplus_rec ~ PROFESSION + DIPLOME_rec + FINANCES_rec + Ageind_tranche + SEXE_rec + IPC + interaction(factor(MOISENQ_rec), factor(VAGUEE)), data = Annee2021, weights = Annee2021$IWEIGHT)

# Obtenez le résumé de votre modèle
summary_reg <- summary(reg_lineaire_ponderee)

# Extraire les statistiques pertinentes
f_statistic <- round(summary_reg$fstatistic[1], 2)
r_squared <- round(summary_reg$r.squared, 2)
r_squared_adj <- round(summary_reg$adj.r.squared, 2)

freq(Annee2021$DIPLOME_rec)

# Prédiction du modèle ----------
library(dplyr)

reg_lineaire <- lm(prixplus_rec ~ PROFESSION + DIPLOME_rec + FINANCES_rec + Ageind_tranche + SEXE_rec + IPC, data = Annee2021, weights = Annee2021$IWEIGHT)

new_data <- expand.grid(
  PROFESSION = "Ingénieur, cadre, directeur",
  DIPLOME_rec = "Diplôme du supérieur long (supérieur à bac + 2)",
  FINANCES_rec = levels(Annee2021$FINANCES_rec),
  Ageind_tranche = levels(Annee2021$Ageind_tranche),
  SEXE_rec = levels(Annee2021$SEXE_rec),
  IPC = mean(Annee2021$IPC, na.rm = TRUE))

# Calcul des poids moyens pour le nouveau dataframe, basé sur les modalités de référence
new_data$IWEIGHT <- mean(Annee2021$IWEIGHT)

# Prédictions en utilisant le modèle existant
new_data$predicted_inflation = predict(reg_lineaire, newdata = new_data, type = "response")

# Visualisation des résultats
print(new_data[c("Ageind_tranche", "SEXE_rec", "predicted_inflation")])



library(dplyr)
library(tidyr)

# Solution avec une fonction d'agrégation, ici la moyenne
results_wide <- new_data %>%
  dplyr::select(Ageind_tranche, SEXE_rec, predicted_inflation) %>%
  tidyr::pivot_wider(
    names_from = SEXE_rec,
    values_from = predicted_inflation,
    values_fn = list(predicted_inflation = mean)  # Utiliser la moyenne pour agréger
  )

# Affichage du nouveau dataframe
print(results_wide)

# Optionnel : Renommage des colonnes pour plus de clarté si nécessaire
colnames(results_wide)[2:3] <- c("Homme", "Femme")
print(results_wide)


