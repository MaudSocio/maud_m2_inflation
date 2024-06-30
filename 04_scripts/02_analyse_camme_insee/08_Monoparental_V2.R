library(dplyr)
library(ggplot2)
library(questionr)

# Recodage pour simplifier les noms des catégories de monoparentalité
Annee2021$MONOPARENTALE <- as.factor(Annee2021$MONOPARENTALE)
levels(Annee2021$MONOPARENTALE) <- c("Famille monoparentale", "Famille non-monoparentale")


## Recodage de Annee2021$SITUAECO_rec en Annee2021$SITUAECO_rec_rec
library(questionr)
library(tidyverse)

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
  fct_relevel("Endettement","Tire un peu sur les réserves","Budget juste","Met de d'argent de côté" )

# Filtre pour exclure les valeurs "Ne sait pas" si c'est nécessaire
Annee2021_filtre <- Annee2021 %>%
  filter(!(FINANCES_rec %in% NA))

# Funtion quartile ---
  quartile_fun <- function(y) {
    return(data.frame(
      y = median(y),
      ymin = quantile(y, probs = 0.25),
      ymax = quantile(y, probs = 0.75)
    ))
  }
  
  

# Fonction médiane ---------
median_fun <- function(y) {
  return(data.frame(y = median(y)))
}


# TRES BON graphique ---------------------------
# Plot avec lignes pour la médiane et les quartiles
ggplot(Annee2021_filtre, aes(x = interaction(MONOPARENTALE, FINANCES_rec, sep = "\n"), y = prixplus_rec, fill = FINANCES_rec)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun = median, geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Ligne médiane
  stat_summary(fun.data = quartile_fun, geom = "errorbar", width = 0.2, color = "black") +
  stat_summary(fun.data = function(x) data.frame(y = quantile(x, probs = 0.25)), geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Quartile inférieur
  stat_summary(fun.data = function(x) data.frame(y = quantile(x, probs = 0.75)), geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Quartile supérieur
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +  # Pour rendre le graphique horizontal
  theme_minimal() +
  scale_y_continuous(limits = c(-10, 35)) + # Définir les limites de l'axe des y de 0 à 35
  labs(
    x = "Composition et situation économique du ménage", 
    y = "Estimation de l'inflation", 
    title = "Perception de l'inflation 
    selon la structure et la situation économique du ménage", 
    subtitle = "Comparaison entre les familles monoparentales et non-monoparentales", 
    fill = "Situation économique",
    caption = "Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021.\nQuestion : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10), 
    plot.caption = element_text(hjust = 0.5, size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size =8), 
    axis.text.y = element_text(hjust = 1, size = 8)
  ) +
  theme(text = element_text(family = "Garamond"))



# Beau graphique pondéré ----------

# Assurez-vous que le paquet Hmisc est installé et chargé
# install.packages("Hmisc")
library(Hmisc)
library(ggplot2)

# Fonction pour calculer les quartiles pondérés et la médiane
weighted_stats <- function(x, weights, prob = c(0.25, 0.5, 0.75)) {
  quantiles <- wtd.quantile(x, weights, probs = prob)
  data.frame(y = quantiles)
}

# Fonction pour calculer quartiles inférieur et supérieur avec pondération
weighted_quartiles <- function(x, weights) {
  q25 <- wtd.quantile(x, weights, probs = 0.25)
  q75 <- wtd.quantile(x, weights, probs = 0.75)
  return(data.frame(y = 0.5 * (q25 + q75), ymin = q25, ymax = q75)) # Retourne le milieu et les extrémités pour ymin et ymax
}


# Fonction pour calculer quartiles inférieur et supérieur avec pondération
weighted_quartiles <- function(x, weights) {
  q25 <- wtd.quantile(x, weights, probs = 0.25)
  q75 <- wtd.quantile(x, weights, probs = 0.75)
  return(data.frame(y = 0.5 * (q25 + q75), ymin = q25, ymax = q75)) # Retourne le milieu et les extrémités pour ymin et ymax
}

# Création du graphique
ggplot(Annee2021_filtre, aes(x = interaction(MONOPARENTALE, SITUAECO_rec_rec, sep = "\n"), y = prixplus_rec, fill = SITUAECO_rec_rec)) +
  geom_violin(trim = FALSE, aes(weight = IWEIGHT)) + 
  stat_summary(fun = function(x) wtd.quantile(x, Annee2021_filtre$IWEIGHT, probs = 0.5), geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Ligne médiane pondérée
  stat_summary(fun.data = function(x) weighted_quartiles(x, Annee2021_filtre$IWEIGHT), geom = "errorbar", width = 0.2, color = "black") + # Lignes horizontales reliant les quartiles
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(limits = c(-10, 35)) +
  labs(
    x = "Composition et situation économique du ménage", 
    y = "Estimation de l'inflation", 
    title = "Perception de l'inflation selon la structure et la situation économique du ménage", 
    subtitle = "Comparaison entre les familles monoparentales et non-monoparentales", 
    fill = "Situation économique",
    caption = "Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021.\nQuestion : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10), 
    plot.caption = element_text(hjust = 0.5, size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
    axis.text.y = element_text(hjust = 1, size = 8),
    text = element_text(family = "Garamond")
  )

## CALCUL MOYENNE ET MEDIANE PONDEREE -------
library(dplyr)
library(Hmisc)

# Calculer les moyennes et médianes pondérées par groupe
resultats <- Annee2021_filtre %>%
  group_by(Groupe = interaction(MONOPARENTALE, FINANCES_rec)) %>%
  summarise(
    Moyenne_ponderee = weighted.mean(prixplus_rec, IWEIGHT, na.rm = TRUE),
    Median_ponderee = wtd.quantile(prixplus_rec, weights = IWEIGHT, probs = 0.5, na.rm = TRUE)
  )

# Afficher les résultats
View(resultats)

# Calculer les effectifs par groupe
effectifs_par_groupe <- Annee2021_filtre %>%
  group_by(Groupe = interaction(MONOPARENTALE, FINANCES_rec)) %>%
  summarise(Effectif = n())

View(effectifs_par_groupe)

# Calcul des médianes et des effectifs par groupe d'interaction
resultats_par_groupe <- Annee2021_filtre %>%
  group_by(Groupe = interaction(MONOPARENTALE, FINANCES_rec)) %>%
  summarise(
    Effectif = n(),
    Moyenne_Inflation = mean(prixplus_rec, na.rm = TRUE), 
    Median_Inflation = median(prixplus_rec, na.rm = TRUE)
  )

# Affichage des résultats
print(resultats_par_groupe)

# Vous pouvez également afficher les résultats dans une fenêtre de visualisation si vous utilisez RStudio
# View(resultats_par_groupe)


