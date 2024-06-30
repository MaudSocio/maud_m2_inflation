library(dplyr)
library(ggplot2)
library(questionr)

# Assumons que 'Annee2021' est déjà chargée avec les variables appropriées

# Recodage pour simplifier les noms des catégories de monoparentalité
Annee2021$MONOPARENTALE <- factor(Annee2021$MONOPARENTALE, levels = c("1", "2"), 
                                  labels = c("Famille monoparentale", "Famille non-monoparentale"))

# Recodage de Annee2021$FINANCES en Annee2021$FINANCES_rec
Annee2021$FINANCES_rec <- factor(Annee2021$FINANCES, 
                                 levels = c("1", "2", "3", "4", "5"),
                                 labels = c("Met de l'argent de côté", "Met de l'argent de côté", 
                                            "Budget juste", "Tire un peu sur les réserves", "Endettement"))

# Filtre pour exclure les valeurs NA
Annee2021_filtre <- Annee2021 %>%
  filter(!is.na(FINANCES_rec))

# Fonction pour calculer la médiane et les quartiles avec deux chiffres après la virgule
summary_fun <- function(y) {
  if (all(is.na(y))) return(data.frame(y = NA, ymin = NA, ymax = NA))  # Gère les cas de toutes valeurs NA
  quants <- quantile(y, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  data.frame(
    y = round(quants[2], 2),      # Médiane
    ymin = round(quants[1], 2),   # Premier quartile
    ymax = round(quants[3], 2)    # Troisième quartile
  )
}

# Graphique avec lignes pour la médiane et les quartiles
ggplot(Annee2021_filtre, aes(x = interaction(MONOPARENTALE, FINANCES_rec, sep = "\n"), y = prixplus_rec, fill = FINANCES_rec)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun.data = summary_fun, geom = "errorbar", width = 0.2, color = "black", size = 0.5) +  # Médiane et quartiles
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
