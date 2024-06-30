

## Sociologie des personnes qui répondent qu'elles ne savent pas
Annee2021 %>%
  filter(PRIX_rec %in% "Ne sait pas") %>%
  ggplot() +
  aes(x = PRIX_rec, fill = Age, colour = DIPLOME_rec) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(x = "Sociologie des personnes qui répondent qu'elles ne savent pas", 
       y = "Effectif") +
  theme_minimal() +
  facet_wrap(vars(SEXE_rec))





Annee2021 %>%
  filter(is.na(Age)) %>%
  ggplot() +
  aes(x = Age, fill = PROFESSION) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(SEXE_rec))




### Regardons les Ne sait pas 

library(dplyr)
library(ggplot2)

Annee2021 %>%
  filter(EVOLPRIX_rec %in% "Ne sait pas") %>%
  ggplot() +
  aes(x = EVOLPRIX_rec, fill = Age, colour = DIPLOME_rec) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(x = "Anticipation des évolutions d'inflation", 
       y = "Population") +
  theme_minimal() +
  facet_wrap(vars(SEXE_rec))

Annee2021 %>%
  filter(EVOLPRIX_rec %in% "Ne sait pas") %>%
  ggplot() +
  aes(x = EVOLPRIX_rec, fill = Age) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(x = "Anticipation des évolutions d'inflation", y = "Population") +
  theme_minimal() +
  facet_wrap(vars(SEXE_rec))
