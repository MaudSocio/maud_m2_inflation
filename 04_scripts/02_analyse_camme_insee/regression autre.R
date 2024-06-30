# Perception des prix

#### Un premier modèle de régression sur l'explication par le genre
Perceptionprix <- Annee2021$PRIXPLUS_rec[!is.na(Annee2021$PRIXPLUS_rec)]


## Recodage de Annee2021$PRIX_rec en Annee2021$PRIX_rec_rec
## Annee2021$PRIX_rec2 <- Annee2021$PRIX_rec %>%
## fct_recode(
##  "Fortement augmenté" = "Ne sait pas")
freq(Annee2021$PRIX_rec)
## Recodage de Annee2021$PRIX_rec2 en Annee2021$PRIX_rec2_rec
Annee2021$PRIX_rec3 <- Annee2021$PRIX_rec2 %>% fct_recode(   "Modérément augmenté" = "Un peu augmenté") %>% fct_explicit_na("Stagné")

## Annee2021$PRIX_rec3 <- Annee2021$PRIX_rec2[!is.na(Annee2021$PRIX_rec2)]

### Variable de référence 
Annee2021$PRIX_rec <- relevel(Annee2021$PRIX_rec, "Un peu augmenté")
### install.packages("nnet")
library(nnet)


### Se débarasser des NA ??? Comment ??? : Voir leur cours sur la regression !! 
## Recodage de Annee2021$Age en Annee2021$Age_rec
# Annee2021$Age3 <- Annee2021$Age %>%
#   fct_explicit_na("75 ans et +")
# Normalement il n'y pas plus de NA à Age


## Recodage de Annee2021$PROFESSION en Annee2021$PROFESSION_rec
Annee2021$PROFESSION_rec <- Annee2021$PROFESSION %>%
  fct_explicit_na("Autre")



ggcoef_multinom(
  regm2,
  type = "faceted",
  exponentiate = TRUE
)
### install.packages("ggeffects")
library(ggeffects)
plot(allEffects(regm2))
plot(ggeffect(regm2, "Fortement augmenté"))


### Tu dois faire une regression logistique ordinale !! 
# Régression logistique ordinale ------

## Réordonnancement de Annee2021$PRIX_rec3
Annee2021$PRIX_rec4 <- Annee2021$PRIX_rec3 %>%
  fct_relevel(
    "Diminué", "Stagné", "Modérément augmenté", "Fortement augmenté"
  )
freq(Annee2021$PRIX_rec4)
### install.packages("ordinal")
library(ordinal)

rego <- clm(PRIX_rec3 ~ SEXE_rec + Age3 + DIPLOME_rec, data = Annee2021)
summary(rego)
rego2 <- step(rego)
### install.packages("JLutils")
library(JLutils)
library(GGally)

#### Jolie graphique (même si je comprends pas les écarts ce que ça veut dire)
ggcoef_model(rego2, exponentiate = TRUE)


# Les données manquantes ######

# install.packages("VIM")

# Charger le jeu de données sleep (exemple)

library(VIM)

# Sous RStudio (optionnel sous R)

x11() # Ouvrir fenêtre interactive

# Afficher la matrice

matrixplot(Annee2021,cex.axis=0.6)

# Cliquer dessus pour trier

# Faire échap à la fin
freq(Annee2021$PRIX_rec)
library(ggplot2)

ggplot(Annee2021) +
  aes(x = PRIX_rec, fill = Age) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(SEXE_rec))



## Regression multinomiale sur le genre
Annee2021$PRIX_rec <- relevel(Annee2021$PRIX_rec, "Un peu augmenté")
regm <- multinom(PRIX_rec ~ SEXE_rec + Age + DIPLOME_rec, data = Annee2021)
summary(regm)
odds.ratio(regm)
tbl_regression(regm, exponentiate = TRUE)

