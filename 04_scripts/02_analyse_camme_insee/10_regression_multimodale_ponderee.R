
#devtools::install_github("carlganz/svrepmisc")

Annee2021$SEXE_rec <- as.factor(Annee2021$SEXE_rec)
Annee2021$Ageind <- as.numeric(Annee2021$Ageind)
Annee2021$MOISENQ <- as.factor(Annee2021$MOISENQ)
Annee2021$VAGUEE <- as.factor(Annee2021$VAGUEE)


Annee2021$PRIX_rec <- Annee2021$PRIX_rec %>%
  fct_relevel(
    "Stagné ou diminué", "Un peu augmenté", "Modérément augmenté",
    "Fortement augmenté"
  )
# Réordonnancement diplome 
## Réordonnancement de Annee2021$DIPLOME_rec
Annee2021$DIPLOME_rec <- Annee2021$DIPLOME_rec %>%
  fct_relevel(
    "Aucun diplôme ou certificat d’études primaires", "CAP, BEP ou équivalent",
    "Baccalauréat, brevet professionnel ou équivalent", "Diplôme du supérieur court (niveau bac + 2)",
    "Diplôme du supérieur long (supérieur à bac + 2)"
  )
## Recodage de Annee2021$MONOPARENTALE en Annee2021$MONOPARENTALE_rec
Annee2021$MONOPARENTALE <- Annee2021$MONOPARENTALE %>%
  fct_recode(
    "Oui" = "Monoparental",
    "Non" = "Pas monoparental"
  ) 
## Réordonnancement de Annee2021$MOISENQ_rec
Annee2021$MOISENQ_rec <- Annee2021$MOISENQ_rec %>%
  fct_relevel(
    "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
    "Aout", "Septembre", "Octobre", "Novembre", "Décembre"
  )

library(survey)
moy_pond <- mean(Annee2021$IWEIGHT)
Annee2021$POND <- Annee2021$IWEIGHT / moy_pond
Annee2021_svy <- svydesign(id=~1,weights=~POND, data=Annee2021)

# Ordinale 

library(survey)
rego <- svyolr(PRIX_rec ~ MONOPARENTALE + SEXE_rec + Age + DIPLOME_rec  + MOISENQ, design = Annee2021_svy)
summary(rego)

str(summary(rego))

# Création du tableau avec gtsummary
# Extraction des informations nécessaires
coef_df <- summary(rego)$coefficients
rownames(coef_df) <- summary(rego)$coefficients[, 1] # Assurez-vous que cela correspond à vos variables

# Création d'un dataframe pour le tableau
results_df <- data.frame(
  Variable = rownames(coef_df),
  Estimate = coef_df[, "Value"],
  `Std. Error` = coef_df[, "Std. Error"],
  `t value` = coef_df[, "t value"]
)

# Conversion du dataframe en tableau gtsummary
# Assurez-vous que gtsummary est chargé
library(gtsummary)

# Création du tableau avec tbl_df
tableau <- tibble::as_tibble(results_df)

# Affichage du tableau avec des modifications supplémentaires
# Pour ajouter un titre, un sous-titre et une note de bas de page, vous pourriez utiliser les fonctions respectives de gtsummary ou d'autres packages compatibles pour la personnalisation
print(tableau) %>%
  modify_header("Régression Logistique Ordinale Pondérée pour les Perceptions de Prix") %>%
  modify_footnote(all_stat_cols() ~ "Source: Camme 2021 : Données mensuelles agrégées")






### Alternative
dwr <- as.svrepdesign(Annee2021_svy, type = "bootstrap", replicates = 100)


# as.svrepdesign est une fonction qui transforme un objet (dans ce cas, Annee2021_svy) en un design de sondage avec réplication. Ceci est utile pour l'inférence statistique à partir d'échantillons complexes.
# type = "bootstrap" spécifie que la méthode de réplication à utiliser est le bootstrap, une technique courante pour estimer la distribution d'une statistique.
# replicates = 100 indique que 100 répliques bootstrap seront générées. Cela est utilisé pour calculer des estimations de variance pour des échantillons complexes.

library(svrepmisc)
freq(Annee2021$PRIX_rec)
Annee2021$MONOPARENTALE

rego2 <- svyclm(PRIX_rec ~ MONOPARENTALE + SEXE_rec + Age + DIPLOME_rec  + MOISENQ, design = dwr)
summary(rego2)


confint(rego2)
library(broom)
library(tidyverse)
tidy(rego2, exponentiate = TRUE, conf.int = TRUE)

stargazer(rego2, type = "text",
          title = "Régression ordinale sur données pondérées de la perception individuelle de l'inflation",
          header = FALSE, 
          no.space = TRUE,
          single.row = TRUE,
          font.size = "small",
          intercept.bottom = FALSE,
          ci = TRUE,  # Inclure l'intervalle de confiance
          ci.level = 0.95  # Niveau de confiance à 95%
)

# Extraire les résultats du modèle
results <- broom::tidy(rego2)

# Extraire les intervalles de confiance
conf_int <- broom::confint_tidy(rego2)

# Fusionner les résultats et les intervalles de confiance
final_results <- left_join(results, conf_int, by = c("term" = "term"))

## Beau tableau -------------
# Supposons que rego2 est votre modèle de régression
# Extraire les informations nécessaires de rego2
coefficients <- coef(summary(rego2))
variables <- rownames(coefficients)
coefficients_values <- coefficients[, "Estimate"]
std_errors <- coefficients[, "Std. Error"]
p_values <- coefficients[, "Pr(>|t|)"]

# Créer le dataframe avec les résultats
results_df <- data.frame(
  Variable = variables,
  Coefficient = coefficients_values,
  ErreurStandard = std_errors,
  ValeurP = p_values
)
