library(dplyr)
library(tidyr)

# Filtrer les données 
Annee2021_filtre <- Annee2021 %>%
  filter(SITUAECO_rec_rec != "Ne sait pas", !is.na(prixplus_rec))

# Exemple de statistiques pour deux catégories
# Remplacez ces valeurs par vos calculs réels
stats_par_categorie <- Annee2021_filtre %>%
  group_by(SITUAECO_rec_rec) %>%
  summarise(
    mediane = median(prixplus_rec, na.rm = TRUE),
    q1 = quantile(prixplus_rec, 0.25, na.rm = TRUE),
    q3 = quantile(prixplus_rec, 0.75, na.rm = TRUE)
  )
# Fonction pour générer les points d'un quart de cercle
generate_points <- function(radius, n = 100, start_angle = 0, end_angle = pi/2) {
  theta <- seq(start_angle, end_angle, length.out = n)
  x <- radius * cos(theta)
  y <- radius * sin(theta)
  data.frame(x = x, y = y)
}

# Générer les points pour chaque catégorie et chaque statistique
data_points <- lapply(1:nrow(stats_par_categorie), function(i) {
  cat <- stats_par_categorie$SITUAECO_rec_rec[i]
  # Générer des points pour la médiane, q1 et q3 avec des rayons ajustés selon les statistiques
  median_points <- generate_points(stats_par_categorie$mediane[i])
  q1_points <- generate_points(stats_par_categorie$q1[i])
  q3_points <- generate_points(stats_par_categorie$q3[i])
  
  # Combiner les points en un seul DataFrame, en ajoutant la catégorie et le type de statistique
  rbind(
    mutate(median_points, category = cat, type = "mediane"),
    mutate(q1_points, category = cat, type = "q1"),
    mutate(q3_points, category = cat, type = "q3")
  )
}) %>% bind_rows()

# Utiliser ggplot2 pour tracer les points
# Ce bloc est pour illustrer comment vous pourriez commencer à tracer; ajustez selon le besoin
ggplot(data_points, aes(x = x, y = y, group = category, fill = category)) +
  geom_polygon(color = "black") +
  scale_fill_brewer(palette = "Set3") +
  coord_fixed() +
  labs(title = "Répartition de l'estimation de l'inflation par situation économique",
       x = "Coordonnée X",
       y = "Coordonnée Y") +
  theme_minimal()

# Fonction pour générer les points d'un quart de cercle avec ajustement de quadrant
generate_points <- function(radius, quadrant = 1, n = 100) {
  theta <- seq(0, pi/2, length.out = n) # Angle pour un quart de cercle
  # Calcul de base pour les coordonnées
  x <- radius * cos(theta)
  y <- radius * sin(theta)
  
  # Ajuster les signes des coordonnées en fonction du quadrant
  if(quadrant == 2) {
    x <- -x
  } else if(quadrant == 3) {
    x <- -x
    y <- -y
  } else if(quadrant == 4) {
    y <- -y
  }
  
  data.frame(x = x, y = y)
}

# Générer les points pour chaque catégorie et statistique en ajustant pour les quadrants
data_points <- lapply(1:nrow(stats_par_categorie), function(i) {
  cat <- stats_par_categorie$SITUAECO_rec_rec[i]
  # Supposons que vous vouliez positionner chaque catégorie dans un quadrant différent
  # Exemple : Quadrant 1 pour la première catégorie, etc.
  quadrant <- i %% 4 + 1 # Cela attribue un quadrant de 1 à 4 en boucle
  
  # Générer des points pour la médiane (ajustez ceci selon les besoins pour q1 et q3 aussi)
  median_points <- generate_points(stats_par_categorie$mediane[i], quadrant)
  
  mutate(median_points, category = cat, type = "mediane")
}) %>% bind_rows()

# Continuer avec le code ggplot pour dessiner
ggplot(data_points, aes(x = x, y = y, group = category, fill = category)) +
  geom_polygon(color = "black") +
  scale_fill_brewer(palette = "Set3") +
  coord_fixed() +
  labs(title = "Répartition de l'estimation de l'inflation par situation économique",
       x = "Coordonnée X",
       y = "Coordonnée Y") +
  theme_minimal()
