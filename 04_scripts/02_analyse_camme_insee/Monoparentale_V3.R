library(dplyr)
library(ggplot2)
library(forcats)  # pour le recodage et la manipulation des facteurs

# Recodage pour simplifier les noms des catégories de monoparentalité
Annee2021$MONOPARENTALE <- as.factor(Annee2021$MONOPARENTALE)
levels(Annee2021$MONOPARENTALE) <- c("Famille monoparentale", "Famille non-monoparentale")

# Recodage de Annee2021$FINANCES
Annee2021$FINANCES_rec <- recode(Annee2021$FINANCES,
                                 `1` = "Met d'argent de côté", `2` = "Met d'argent de côté",
                                 `3` = "Budget juste", `4` = "Tire un peu sur les réserves", `5` = "Endettement", .default = NA_character_)

Annee2021$FINANCES_rec <- factor(Annee2021$FINANCES_rec, levels = c("Endettement", "Tire un peu sur les réserves", "Budget juste", "Met d'argent de côté"))

# Filtre pour exclure les valeurs "Ne sait pas"
Annee2021_filtre <- Annee2021 %>%
  filter(!is.na(FINANCES_rec))

# Fonctions pour calculer les statistiques descriptives
quartile_fun <- function(y) {
  return(data.frame(
    y = median(y),
    ymin = quantile(y, probs = 0.25),
    ymax = quantile(y, probs = 0.75)
  ))
}

# Graphique amélioré
ggplot(Annee2021_filtre, aes(x = interaction(MONOPARENTALE, FINANCES_rec, sep = "\n"), y = prixplus_rec, fill = FINANCES_rec)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun.y = median, geom = "point", shape = 23, fill = "white", size = 3) +  # Points pour la médiane
  stat_summary(fun.data = quartile_fun, geom = "errorbar", width = 0.2, color = "black") +  # Barres pour les quartiles
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +
  labs(
    x = "Composition et situation économique du ménage",
    y = "Estimation de l'inflation",
    title = "Perception de l'inflation selon la structure et la situation économique du ménage",
    subtitle = "Comparaison entre les familles monoparentales et non-monoparentales",
    fill = "Situation économique",
    caption = "Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021.\nQuestion : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 12),
    plot.caption = element_text(hjust = 0, size = 8),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "black", size = 1)
  ) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
