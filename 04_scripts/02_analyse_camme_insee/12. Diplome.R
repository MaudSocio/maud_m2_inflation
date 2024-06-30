

library(ggplot2)
library(Hmisc)

# Assurez-vous que les noms de diplômes sont bien définis dans votre dataframe
# Vous pouvez les renommer pour plus de clarté si nécessaire
Annee2021_filtre$DIPLOME_rec <- factor(Annee2021_filtre$DIPLOME_rec, levels = c("Aucun diplôme ou certificat d’études primaires", "Baccalauréat, brevet professionnel ou équivalent", "CAP, BEP ou équivalent", "Diplôme du supérieur court (niveau bac + 2)", "Diplôme du supérieur long (supérieur à bac + 2)"))

# Création du graphique adapté au diplôme
ggplot(Annee2021_filtre, aes(x = DIPLOME_rec, y = prixplus_rec, fill = DIPLOME_rec)) +
  geom_violin(trim = FALSE, aes(weight = IWEIGHT)) +
  stat_summary(fun = function(x) wtd.quantile(x, Annee2021_filtre$IWEIGHT, probs = 0.5), geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Ligne médiane pondérée
  stat_summary(fun.data = function(x) weighted_quartiles(x, Annee2021_filtre$IWEIGHT), geom = "errorbar", width = 0.2, color = "black") + # Lignes horizontales reliant les quartiles
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(limits = c(-10, 35)) +
  labs(
    x = "Niveau de diplôme", 
    y = "Estimation de l'inflation", 
    title = "Perception de l'inflation selon le niveau de diplôme", 
    subtitle = "Répartition par niveau de diplôme", 
    fill = "Niveau de diplôme",
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
