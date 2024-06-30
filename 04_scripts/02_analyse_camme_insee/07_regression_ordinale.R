# ETAPE 4  : Regression ordinale #######

###Opinion personnelle sur l’inflation (OPI) et caractéristiques des répondants ######
## Revenu 
##" janv2021$REVNUTRA

library(margins)
library(car)
library(lmtest)

# Regression ordinale sur Prix_rec!!  ------

## (trois ou plus modalités qui sont ordonnées (par exemple : modéré, moyen, fort) - ordre hiéarchique (du plus faible au plus fort)
## Réordonnancement de Annee2021$PRIX_rec du plus faible au plus fort
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
## Regression sur les arrangements typiques familiaux ------
library(ordinal)
library(broom)
regm2 <- clm(PRIX_rec ~ MONOPARENTALE + SEXE_rec + Age + DIPLOME_rec  + MOISENQ, data = Annee2021)
# Sur le terminal 
summary(regm2)

# tidy(regm2, exponentiate = TRUE, conf.int = TRUE)

## La belle regression sur un beau tableau
tbl_regression(regm2, exponentiate = TRUE)

## Tout est significatif 


## Le beau dessin 
library(JLutils)
library(GGally)
# ggcoef_model(regm2, exponentiate = TRUE)

