

library(ggplot2)


## Tout premier graphique ---------------------
ggplot(Annee2021) +
 aes(x = PRIXPLUS_rec, y = SEXE_rec, fill = MONOPARENTALE) +
 geom_boxplot() +
 scale_fill_brewer(palette = "RdYlBu", 
 direction = 1) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0))  +
 labs(
   x = "Estimation de l'inflation", y = " Genre et composition de la famille", 
  title = "Perception de l'inflation selon le genre et la composition des familles", subtitle = "Une perception différenciée selon l'imbricaltion des rapports sociaux", 
 fill = "Type de famille",
 caption = "Notes de lecture : Les femmes dans des familles monoparentales estiment l'inflation en valeur médiane 
à 10%, alors que les femmes qui ne sont pas dans des ménages monoparentaux l'estiment en médiane à 8%
Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021
Question : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
 )  +
 coord_flip() +
 theme_linedraw() +
 xlim(0, 30) +
  theme(
    plot.caption = element_text(
      hjust = 0,  # Ajuster la justification horizontale (0 = à gauche, 1 = à droite),  # Couleur du texte
      size = 10,  # Taille du texte
    )
  ) 

library(gt)

## Réordonnancement de Annee2021$SITUAECO_rec
Annee2021$SITUAECO_rec <- factor(Annee2021$SITUAECO_rec,
  levels = c(
    "Ne sait pas", "s’est nettement dégradée", "s’est un peu dégradée",
    "est restée stationnaire", "s’est un peu améliorée", "s’est nettement améliorée"
  )
)

# Deuxième graphique - exposé --------------------

library(dplyr)
library(ggplot2)

## Recodage de Annee2021$MONOPARENTALE en Annee2021$MONOPARENTALE_rec
Annee2021$MONOPARENTALE <- Annee2021$MONOPARENTALE
Annee2021$MONOPARENTALE[Annee2021$MONOPARENTALE == "Monoparental"] <- "Famille monoparentale"
Annee2021$MONOPARENTALE[Annee2021$MONOPARENTALE == "Pas monoparental"] <- "Famille non-monoparentale"

Annee2021 %>%
 filter(!(SITUAECO_rec %in% "Ne sait pas")) %>%
 ggplot() +
 aes(x = prixplus_rec, y = SEXE_rec, fill = SITUAECO_rec) +
 geom_boxplot() +
scale_fill_brewer(palette = "RdYlBu", 
                    direction = 1) + 
 coord_flip() +
 theme_minimal() +
 facet_wrap(vars(MONOPARENTALE)) +
 xlim(0, 45)  + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0))  +
  labs(
    x = "Estimation de l'inflation", y = " Genre et composition de la famille", 
    title = "Perception de l'inflation selon le genre, la structure du ménage et la situation économique", subtitle = "Une perception différenciée selon l'imbrication de rapports sociaux", 
    fill = "Situation économique perçue",
    caption = "Notes de lecture : Les femmes dans des familles monoparentales, dont la situation économique s'est un peu dégradée,
estiment l'inflation en valeur médiane  à 10%, alors que les femmes qui ne sont pas dans des ménages monoparentaux,
dont la situation économique s'est un peu dégradée, l'estiment en médiane à 6%
Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021
Question : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
  )  +
  coord_flip() +
  theme_linedraw() +
  theme(
    plot.caption = element_text(
      hjust = 0,  # Ajuster la justification horizontale (0 = à gauche, 1 = à droite),  # Couleur du texte
      size = 10,  # Taille du texte
    )
  ) 

# Cours Ricci - Comparaison --------------------------
library(dplyr)
library(ggplot2)
library(questionr)

# Recodage pour simplifier les noms des catégories de monoparentalité
Annee2021$MONOPARENTALE <- as.factor(Annee2021$MONOPARENTALE)
levels(Annee2021$MONOPARENTALE) <- c("Famille monoparentale", "Famille non-monoparentale")


## Recodage de Annee2021$SITUAECO_rec en Annee2021$SITUAECO_rec_rec
library(questionr)
library(tidyverse)
Annee2021$SITUAECO_rec_rec  <- as.factor(Annee2021$SITUAECO_rec_rec ) 
Annee2021$SITUAECO_rec_rec <- Annee2021$SITUAECO_rec %>%
  fct_recode(
    "s’est améliorée" = "s’est nettement améliorée",
    "s’est améliorée" = "s’est un peu améliorée"
  )

 

# Assurez-vous que c'est déjà le cas ou ajustez selon votre cadre de données
freq(Annee2021$SITUAECO_rec_rec)


# Filtre pour exclure les valeurs "Ne sait pas" si c'est nécessaire
Annee2021_filtre <- Annee2021 %>%
  filter(!(SITUAECO_rec_rec %in% "Ne sait pas"))
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
## Info ------

ggplot(Annee2021_filtre , aes(x = interaction(MONOPARENTALE, SITUAECO_rec_rec, sep = "\n"), y = prixplus_rec, fill = SITUAECO_rec_rec)) +
  geom_violin(trim = FALSE) +
  # geom_boxplot(width = 0.1, fill = NA, colour = "black", outlier.shape = NA) + # Ajoute un boxplot à l'intérieur de chaque violon pour montrer les quartiles et la médiane
  stat_summary(fun = median, geom = "point", shape = 95, size = 4, color = "white") + # Point for the median
  stat_summary(fun.data = quartile_fun, geom = "errorbar", width = 0.2, color = "white") + # Error bars for quartiles
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +  # Pour rendre le graphique horizontal
  theme_minimal() +
  scale_y_continuous(limits = c(-10, 35)) + # Définir les limites de l'axe des y de 0 à 35
  labs(
    x = "Composition de la famille et Situation économique", 
    y = "Estimation de l'inflation", 
    title = "Perception de l'inflation selon la structure du ménage et la situation économique", 
    subtitle = "Comparaison entre les familles monoparentales et non-monoparentales", 
    fill = "Situation économique",
    caption = "Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021\nQuestion : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Incliner le texte de l'axe des x pour une meilleure lisibilité
  )


library(ggplot2)

# Plot
ggplot(Annee2021_filtre, aes(x = interaction(MONOPARENTALE, SITUAECO_rec_rec, sep = "\n"), y = prixplus_rec, fill = SITUAECO_rec_rec)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 95, size = 4, color = "white") + # Points for the median
  stat_summary(fun.data = quartile_fun, geom = "errorbar", width = 0.2, color = "black") + # Error bars for quartiles
  geom_segment(aes(x = as.numeric(interaction(MONOPARENTALE, SITUAECO_rec_rec, sep = "\n")) - 0.25, 
                   xend = as.numeric(interaction(MONOPARENTALE, SITUAECO_rec_rec, sep = "\n")) + 0.25, 
                   y = median(prixplus_rec), yend = median(prixplus_rec)), 
               color = "black", size = 0.5) +  # Vertical line for the median
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +  # To make the plot horizontal
  theme_minimal() +
  scale_y_continuous(limits = c(-10, 35)) + # Define the limits of the y-axis from 0 to 35
  labs(
    x = "Composition de la famille et Situation économique", 
    y = "Estimation de l'inflation", 
    title = "Perception de l'inflation selon la structure du ménage et la situation économique", 
    subtitle = "Comparaison entre les familles monoparentales et non-monoparentales", 
    fill = "Situation économique",
    caption = "Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021\nQuestion : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


library(ggplot2)

# Plot avec la médiane représentée par une ligne verticale à l'intérieur de chaque violon
ggplot(Annee2021_filtre, aes(x = interaction(MONOPARENTALE, SITUAECO_rec_rec, sep = "\n"), y = prixplus_rec, fill = SITUAECO_rec_rec)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun = median, geom = "errorbar", width = 0.3, aes(ymin = ..y.., ymax = ..y..), color = "black") + # Ligne médiane verticale
  stat_summary(fun.data = calculate_quantiles, geom = "linerange", aes(ymin = ..ymin.., ymax = ..ymax..), color = "black", size = 0.5) + # Lignes verticales pour les quartiles
  stat_summary(fun.data = calculate_quantiles, geom = "crossbar", width = 0.2, aes(y = ..y.., ymin = ..ymin.., ymax = ..ymax..), color = "black", size = 0.5) + # Ligne horizontale reliant les quartiles
  
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +  # Pour rendre le graphique horizontal
  theme_minimal() +
  scale_y_continuous(limits = c(-10, 35)) + # Définir les limites de l'axe des y de 0 à 35
  labs(
    x = "Composition de la famille et Situation économique", 
    y = "Estimation de l'inflation", 
    title = "Perception de l'inflation selon la structure du ménage et la situation économique", 
    subtitle = "Comparaison entre les familles monoparentales et non-monoparentales", 
    fill = "Situation économique",
    caption = "Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021\nQuestion : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



library(ggplot2)


library(ggplot2)

# Bon graphique ---------------------------
# Plot avec lignes pour la médiane et les quartiles
ggplot(Annee2021_filtre, aes(x = interaction(MONOPARENTALE, SITUAECO_rec_rec, sep = "\n"), y = prixplus_rec, fill = SITUAECO_rec_rec)) +
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



library(dplyr)

# Calcul des moyennes pour chaque interaction entre MONOPARENTALE et SITUAECO_rec_rec
mean_table <- Annee2021_filtre %>%
  group_by(MONOPARENTALE, SITUAECO_rec_rec) %>%
  summarise(mean_prixplus_rec = mean(prixplus_rec, na.rm = TRUE)) %>%
  ungroup()

# Affichage du tableau des moyennes
print(mean_table)



## Cours 18/03/2024 - RICCI -------------------------


## Année filtrée ------------
Annee2021_filtre <- Annee2021 %>%
  filter(SITUAECO_rec_rec != "Ne sait pas", !is.na(prixplus_rec))

## Statistique 
stats_par_categorie <- Annee2021 %>%
  group_by(SITUAECO_rec_rec) %>%
  summarise(
    mediane = median(prixplus_rec, na.rm = TRUE),
    q1 = quantile(prixplus_rec, 0.25, na.rm = TRUE),
    q3 = quantile(prixplus_rec, 0.75, na.rm = TRUE)
  )

## Graphique 
ggplot(data = stats_par_categorie, aes(x = mediane, y = SITUAECO_rec_rec)) +
  geom_point() +
  # Ajoutez ici d'autres éléments geom pour représenter les quartiles et la médiane
  theme_minimal() +
  labs(title = "Répartition de l'estimation de l'inflation par situation économique",
       x = "Estimation de l'inflation",
       y = "Situation économique")

# GENRE ---------

###  Fonction Quartile --------------

quartile_fun <- function(y) {
  return(data.frame(
    y = median(y),
    ymin = quantile(y, probs = 0.25),
    ymax = quantile(y, probs = 0.75)
  ))
}

### Fonction médiane ---------
median_fun <- function(y) {
  return(data.frame(y = median(y)))
}

### Graphique ----------------

# Fonction pour calculer la médiane, le premier et le troisième quartile
calculate_quantiles <- function(y) {
  data.frame(
    y = median(y),
    ymin = quantile(y, 0.25),
    ymax = quantile(y, 0.75)
  )
}



ggplot(Annee2021) +
  aes(x = SEXE_rec, y = prixplus_rec, fill = Age_rec_rec) +
  geom_violin(trim= FALSE) +
  stat_summary(fun = median, geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Ligne médiane
  stat_summary(fun.data = quartile_fun, geom = "errorbar", width = 0.2, color = "black") +
  stat_summary(fun.data = function(x) data.frame(y = quantile(x, probs = 0.25)), geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Quartile inférieur
  stat_summary(fun.data = function(x) data.frame(y = quantile(x, probs = 0.75)), geom = "errorbar", width = 0.2, aes(ymin = ..y.., ymax = ..y..), color = "black", size = 0.5) + # Quartile supérieur
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +
  scale_fill_hue(direction = 1)  +
labs( x = "Genre", 
  y = "Age des individus de 25 à 95 ans", 
  title = "Perception de l'inflation en fonction 
 de l'âge des individu et de leur genre", 
  subtitle = "Comparaison entre les familles monoparentales et non-monoparentales", 
  fill = "Situation économique",
  caption = "Source : Enquêtes mensuelles CAMME entre janvier et décembre 2021.\nQuestion : De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ?") +
  theme_minimal() +
  ylim(0,20) +
  theme(text = element_text(family = "Garamond"))

## V2 graphique 
library(ggplot2)
library(RColorBrewer)

# library(ggbeeswarm) # Uncomment if using geom_quasirandom

ggplot(Annee2021, aes(x = SEXE_rec, y = prixplus_rec, fill = Age_rec_rec)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun = median, geom = "point", color = "black", size = 2, aes(group = interaction(SEXE_rec, Age_rec_rec))) + # Médiane pour chaque groupe
  stat_summary(fun.data = quartile_fun, geom = "errorbar", width = 0.2, color = "black", aes(group = interaction(SEXE_rec, Age_rec_rec))) + # Quartiles pour chaque groupe
  
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +
  labs(x = "Genre", 
       y = "Perception de l'inflation (%)", 
       title = "Perception de l'inflation en fonction de l'âge et du genre", 
       subtitle = "Comparaison entre différentes tranches d'âge", 
       fill = "Tranche d'âge",
       caption = "Source: Enquêtes CAMME, 2021. Question sur l'évolution percevée des prix.") +
  theme_minimal() +
  ylim(0,20) +
  theme(text = element_text(family = "Garamond"))




## Calcul des médianes ----------

## Calcul des médianes ----------
## Recodage de Annee2021$SITUAECO_rec en Annee2021$SITUAECO_rec_rec
library(questionr)
library(tidyverse)
Annee2021$SITUAECO_rec_rec  <- as.factor(Annee2021$SITUAECO_rec_rec ) 
Annee2021$SITUAECO_rec_rec <- Annee2021$SITUAECO_rec %>%
  fct_recode(
    "s’est améliorée" = "s’est nettement améliorée",
    "s’est améliorée" = "s’est un peu améliorée"
  )



# Assurez-vous que c'est déjà le cas ou ajustez selon votre cadre de données
freq(Annee2021$SITUAECO_rec_rec)


# Filtre pour exclure les valeurs "Ne sait pas" si c'est nécessaire


Annee2021_filtre <- Annee2021 %>%
  filter(SITUAECO_rec_rec != "Ne sait pas", !is.na(prixplus_rec))


medians <-  Annee2021_filtre.groupby(['MONOPARENTALE', 'SITUAECO_rec_rec'])['prixplus_rec'].median().reset_index()
medians


median_table <- Annee2021_filtre %>%
  group_by(MONOPARENTALE, SITUAECO_rec_rec) %>%
  summarise(median_prixplus_rec = median(prixplus_rec, na.rm = TRUE)) %>%
  ungroup()



## Moyenne ------------
mean_table <- Annee2021_filtre %>%
  group_by(MONOPARENTALE, SITUAECO_rec_rec) %>%
  summarise(mean_prixplus_rec = mean(prixplus_rec, na.rm = TRUE)) %>%
  ungroup()

# Test ANOVA
anova_result <- aov(prixplus_rec ~ interaction(MONOPARENTALE, SITUAECO_rec_rec), data = Annee2021_filtre)
summary(anova_result)

anova_summary <- tidy(anova_result)
# Test de Welch ANOVA
welch_result <- oneway.test(prixplus_rec ~ interaction(MONOPARENTALE, SITUAECO_rec_rec), data = Annee2021_filtre)
print(welch_result)
welch_summary <- tidy(welch_result)

library(knitr)
library(broom)

# Combinez les résultats dans un seul dataframe
results_df <- rbind(
  data.frame(Test = "ANOVA", anova_summary[, -1]),  # Excluez la première colonne de anova_summary
  data.frame(Test = "Welch ANOVA", welch_summary[, -1])  # Excluez la première colonne de welch_summary
)

# Affichez le tableau
knitr::kable(results_df, caption = "Résultats des tests ANOVA et Welch ANOVA")

# Supposons que anova_summary et welch_summary sont déjà créés avec broom::tidy()
# Adaptation pour assurer la compatibilité des structures de données

# Pour l'ANOVA
anova_df <- data.frame(
  Test = "ANOVA",
  Statistic = anova_summary$statistic,
  Df = anova_summary$df,
  P.Value = anova_summary$p.value
)

# Comme le test de Welch ANOVA retourne des résultats un peu différents,
# vous pouvez adapter le dataframe pour qu'il corresponde. Par exemple :
welch_df <- data.frame(
  Test = "Welch ANOVA",
  Statistic = welch_result$statistic,
  Df = welch_result$parameter,  # 'parameter' peut contenir les degrés de liberté pour Welch
  P.Value = welch_result$p.value
)

# Maintenant, combiner les deux dataframes
results_df <- rbind(anova_df, welch_df)

# Supposons que results_df est votre dataframe contenant les résultats des tests
gt_table <- gt(results_df) %>%
  tab_header(
    title = "*Résultats des Tests Statistiques*",
    subtitle = "Comparaison des moyennes de prixplus_rec"
  ) %>%
  cols_label(
    Test = "Test Statistique",
    Statistic = "Valeur de la Statistique",
    Df = "Degrés de Liberté",
    P.Value = "Valeur-P"
  ) %>%
  fmt_number(
    columns = c(Statistic, P.Value),
    decimals = 6
  ) %>%
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body(columns = everything())
  ) 

library(gt)
# Supposons que results_df est votre dataframe contenant les résultats des tests
gt_table <- gt(results_df) %>%
  tab_header(
    title = "Résultats des Tests Statistiques",
    subtitle = "Comparaison des moyennes de prixplus_rec"
  ) %>%
  cols_label(
    Test = "Test Statistique",
    Statistic = "Valeur de la Statistique",
    Df = "Degrés de Liberté",
    P.Value = "Valeur-P"
  ) %>%
  fmt_number(
    columns = vars(Statistic, P.Value),
    decimals = 3
  ) %>%
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_footnote(
    footnote = "Les tests ANOVA et Welch ANOVA ont été utilisés pour analyser les différences des moyennes.",
    locations = cells_title(groups = "title")
  )

# Afficher le tableau gt
print(gt_table)


# Afficher le tableau gt
print(gt_table)















## Graphique genre et age V3 ----------------------------
Annee2021$prixplus_rec
ggplot(Annee2021, aes(x = SEXE_rec, y = prixplus_rec, fill = Age_rec_rec)) +
  geom_violin(trim = FALSE) +
  stat_summary(fun.data = calculate_quantiles, geom = "errorbar", aes(ymin = ymin, ymax = ymax, y = y), color = "black", linewidth = 0.5) +
  stat_summary(fun.data = calculate_quantiles, geom = "crossbar", width = 0.2, aes(y = y, ymin = ymin, ymax = ymax), color = "black", linewidth = 0.5) +
  scale_fill_brewer(palette = "RdYlBu", direction = 1) +
  coord_flip() +
  labs(x = "Genre", 
       y = "Perception de l'inflation (%)", 
       title = "Perception de l'inflation en fonction de l'âge et du genre", 
       subtitle = "Comparaison entre différentes tranches d'âge", 
       fill = "Tranche d'âge",
       caption = "Source: Enquêtes CAMME, 2021. Question sur l'évolution percevée des prix.") +
  theme_minimal() +
  theme(text = element_text(family = "Garamond"))

