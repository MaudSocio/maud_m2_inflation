


library(tidyverse)
library(dplyr)
library(foreign)
library(questionr)
library(stats)
library(survey)
library(gtsummary)
library(gtsummary)
library(data.table)
library(here)
library(dplyr)


# Chargement des données et recodage ------------
df_final <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v5.csv")

# Création du graphique
p <- ggplot(df_final, aes(x = Ageind_tranche, y = prixplus_pond, group = ID_GROUPE, color = ID_GROUPE)) +
  geom_line() +  # Utilisez geom_line pour tracer des lignes
  geom_point() + # Ajoutez des points pour chaque donnée
  scale_color_manual(values = rainbow(length(unique(df_final$ID_GROUPE)))) + # Couleurs différentes pour chaque groupe
  labs(title = "Perception de l'inflation par tranche d'âge et par groupe",
       x = "Tranche d'âge",
       y = "Perception moyenne de l'inflation",
       color = "Groupe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Pour une meilleure lisibilité des étiquettes sur l'axe X

# Affichage du graphique
print(p)


# Préparation des données
df_final_3 <- df_final %>%
  mutate(YearMonth = paste(ANENQ, formatC(MOISENQ, width = 2, format = "d", flag = "0"))) %>%
  arrange(ID_GROUPE, YearMonth, Ageind_tranche)

# Création du graphique
p <- ggplot(df_final_3, aes(x = YearMonth, y = prixplus_pond, group = ID_GROUPE, color = ID_GROUPE)) +
  geom_line() +  # Utilisez geom_line pour tracer des lignes connectant les points par groupe à travers le temps
  geom_point() + # Ajoutez des points pour chaque donnée
  scale_color_manual(values = rainbow(length(unique(df_final$ID_GROUPE)))) + # Couleurs différentes pour chaque groupe
  labs(title = "Perception de l'inflation par tranche d'âge et par groupe au fil du temps",
       x = "Année et Mois",
       y = "Perception moyenne de l'inflation",
       color = "Groupe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Les étiquettes des x tournées pour mieux lire les dates

# Affichage du graphique
print(p)


## Graphique 2 ------

# Trier les données pour préparer pour un pseudo panel
df_final <- df_filtre %>%
  arrange(ID_GROUPE, ANNAISS)

df_final <- df_final %>%
  select(ID_GROUPE, ANNAISS, SEXE_rec, PROFESSION, prixplus_rec, everything())

df_final <- df_final %>%
  group_by(ID_GROUPE, ANENQ, MOISENQ, Ageind_tranche) %>%
  mutate(N_observations = n()) %>%
  ungroup()


df_annual <- df_final %>%
  group_by(ID_GROUPE, ANENQ, Ageind_tranche) %>%
  summarise(
    prixplus_pond = weighted.mean(prixplus_rec, w = N_observations, na.rm = TRUE),
    .groups = "drop"
  )

# Création du graphique
ggplot(df_annual, aes(x = as.factor(ANENQ), y = prixplus_pond, group = ID_GROUPE, color = ID_GROUPE)) +
  geom_line() +  # Lignes pour relier les points des mêmes groupes à travers les années
  geom_point() + # Points pour chaque donnée annuelle
  scale_x_discrete(breaks = seq(min(df_annual$ANENQ), max(df_annual$ANENQ), by = 5)) + # Étiquettes tous les 5 ans
  scale_color_manual(values = rainbow(length(unique(df_annual$ID_GROUPE)))) + # Couleurs différentes pour chaque groupe
  labs(title = "Perception moyenne de l'inflation par année et groupe d'âge",
       x = "Année",
       y = "Perception moyenne de l'inflation (pondérée)",
       color = "Groupe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotation des étiquettes pour une meilleure lisibilité
        plot.title = element_text(hjust = 0.5)) # Centrage du titre


## IDEE 2 - Graphique 2 ------


