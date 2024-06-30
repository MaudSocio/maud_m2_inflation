
library(ggplot2)

ggplot(Annee2021) +
 aes(x = PRIX_rec, fill = SEXE_rec) +
 geom_bar(position = "dodge") +
 scale_fill_hue(direction = 1) +
 labs(x = "Perception de l'inflation", y = "Effectif", subtitle = "Perception de l'inflation en fonction du genre", 
 fill = "Genre") +
 theme_minimal()

ggplot(Annee2021) +
 aes(x = PRIXPLUS_rec, y = date, fill = SEXE_rec) +
  geom_curve() +
 scale_fill_hue(direction = -1) +
 labs(x = "Perception de l'inflation", y = "Effectif", subtitle = "Perception de l'inflation en fonction du genre", 
 fill = "Genre") +
 theme_light() +
 xlim(-10, 25)

help(ggplot)


Annee2021$date <- as.Date(Annee2021$date)

Annee2021$MOISENQ

ggplot(Annee2021, aes(x = MOISENQ, y = PRIXPLUS_rec, color = SEXE_rec)) +
  geom_line() +  # Utilisez geom_line pour tracer des courbes
  scale_color_hue(direction = -1) +  # Utilisez scale_color_hue pour la distinction des couleurs
  labs(x = "Date", y = "Perception de l'inflation", 
       subtitle = "Perception de l'inflation en fonction du genre", 
       color = "Genre") +
  theme_light()  # Ajustez selon le format de votre date

Annee2021$MOISENQ_rec
## Gerne 
View(Annee2021)


library(ggalluvial)
library(ggplot2)

Annee2021$PRIX_rec


library(dplyr)
library(questionr)

Annee2021 <- Annee2021 %>%
  group_by(MOISENQ_rec, PRIX_rec, SEXE_rec) %>%
  summarise(freq = n(), .groups = 'drop')

install.packages("ggplot2")

ggplot(data = Annee2021,
       aes(axis1 = MOISENQ_rec, axis2 = PRIX_rec, y = freq)) +
  geom_alluvium(aes(fill = SEXE_rec)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Perception de l'inflation par mois et par genre",
       x = "",
       y = "Fréquence",
       fill = "Genre")

Annee2021$MOISENQ_rec <- as.factor(Annee2021$MOISENQ_rec)
iorder(Annee2021$MOISENQ_rec)

## Réordonnancement de Annee2021$MOISENQ_rec
Annee2021$MOISENQ_rec <- factor(Annee2021$MOISENQ_rec,
  levels = c(
    "Septembre", "Octobre", "Novembre", "Décembre", "Janvier",
    "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Aout"
  )
)

ggplot(data = Annee2021, aes(axis1 = MOISENQ_rec, axis2 = PRIX_rec, y = freq)) +
  geom_alluvium(aes(fill = SEXE_rec)) +
  geom_stratum() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Perception de l'inflation par mois et par genre",
       x = "",
       y = "Fréquence",
       fill = "Genre")


ggplot(data = Annee2021, aes(axis1 = MOISENQ_rec, axis2 = PRIX_rec, y = freq)) +
  geom_alluvium(aes(fill = SEXE_rec)) +
  geom_stratum() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Perception de l'inflation par mois et par genre",
       x = "",
       y = "Fréquence",
       fill = "Genre") +
  scale_x_discrete(limits = levels(Annee2021$MOISENQ_rec)) # Assurez-vous que les mois apparaissent comme des étiquettes


library(ggplot2)
library(ggalluvial)


