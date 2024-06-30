# Expliquer l'estimation de l'inflation #####


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
df <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v4.csv")

## Recodage de df$FINANCES en df$FINANCES_rec
df$FINANCES_rec <- df$FINANCES %>%
  as.character() %>%
  fct_recode(
    "Met de d'argent de côté" = "1",
    "Met de d'argent de côté" = "2",
    "Budget juste" = "3",
    "Tire un peu sur les réserves" = "4",
    "Endettement" = "5",
    NULL = "9"
  )

df$FINANCES_rec <- as.factor(df$FINANCES_rec) 

## Réordonnancement de df$FINANCES_rec
df$FINANCES_rec <- df$FINANCES_rec %>%
  fct_relevel(
    "Met de d'argent de côté",
    "Budget juste", "Tire un peu sur les réserves", "Endettement"
  )


## Recodage de df$Ageind en df$Ageind_rec
df$Ageind_tranche <- cut(df$Ageind,
                         include.lowest = TRUE,
                         right = FALSE,
                         dig.lab = 4,
                         breaks = c(0, 26, 36, 46, 56, 66, 76, 121)
)

df$Ageind_tranche <- as.factor(df$Ageind_tranche)

## Recodage de df$Ageind_tranche en df$Ageind_tranche_rec
df$Ageind_tranche <- df$Ageind_tranche %>%
  fct_recode(
    "Moins de 26 ans" = "[0,26)",
    "26 à 36 ans" = "[26,36)",
    "36 à 45 ans" = "[36,46)",
    "46 à 55 ans" = "[46,56)",
    "56 à 65 ans" = "[56,66)",
    "66 à 75 ans" = "[66,76)",
    "76 ans et plus" = "[76,121]"
  )


### Recodage diplome --------------------
df$DIPLOME_rec <- df$DIPLOME_rec %>%
  fct_relevel(
    "Aucun diplôme ou certificat d’études primaires", "CAP, BEP ou équivalent",
    "Baccalauréat, brevet professionnel ou équivalent", "Diplôme du supérieur court (niveau bac + 2)",
    "Diplôme du supérieur long (supérieur à bac + 2)"
  )
### Mono --------------
df$MONOPARENTALE<- as.factor(df$MONOPARENTALE)
df$MONOPARENTALE <- df$MONOPARENTALE %>%
  fct_relevel(
    "Pas monoparental", "Monoparental"
  )

### Genre -----------
df$SEXE_rec <- as.factor(df$SEXE_rec)
df$SEXE_rec <- relevel(df$SEXE_rec, "Homme")

### Réordonnancement de df$MOISENQ_rec ---
df$MOISENQ_rec <-as.factor(df$MOISENQ_rec)
df$MOISENQ_rec <- df$MOISENQ_rec %>%
  fct_relevel(
    "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
    "Aout", "Septembre", "Octobre", "Novembre", "Décembre")



## Réordonnancement de df$FINANCES_rec
df$FINANCES_rec <- df$FINANCES_rec %>%
  fct_relevel(
    "Tire sur les réserves", "S'endette", "Budget juste", "Peu d'argent de côté",
    "Beaucoup d'argent de côté"
  )

df$SEXE_rec <- as.factor(df$SEXE_rec)
df$Ageind <- as.numeric(df$Ageind)
df$MOISENQ <- as.factor(df$MOISENQ)
df$VAGUEE <- as.factor(df$VAGUEE)


df$MOISENQ <- as.factor(df$MOISENQ)

# 2. Constitution du pseudo-panel -------------------
library(dplyr)

df_filtre <- df


library(dplyr)

# Mise à jour des groupes d'âge et des identifiants de groupe
df_filtre <- df_filtre %>%
  mutate(
    GROUPE_AGE = case_when(
      ANNAISS < 1930 ~ "Avant_1929",
      ANNAISS >= 1981 ~ "Apres_1980",
      TRUE ~ as.character(floor((ANNAISS - 1930) / 10) * 10 + 1930)  # Groupe d'âge de 10 ans entre 1930 et 1980
    ),
    GROUPE_AGE_FIN = case_when(
      ANNAISS < 1930 ~ "1929",
      ANNAISS >= 1981 ~ "Post",
      TRUE ~ as.character(floor((ANNAISS - 1930) / 10) * 10 + 1939)  # Fin des groupes de 10 ans
    ),
    ID_GROUPE = ifelse(GROUPE_AGE == "Avant_1929" | GROUPE_AGE == "Apres_1980",
                       paste0(GROUPE_AGE, "_", SEXE_rec),
                       paste0(GROUPE_AGE, "_", GROUPE_AGE_FIN, "_", SEXE_rec))
  ) 

df_filtre$ID_GROUPE <-  as.factor(df_filtre$ID_GROUPE)


## Réordonnancement de df_filtre$ID_GROUPE
df_filtre$ID_GROUPE <- df_filtre$ID_GROUPE %>%
  fct_relevel(
    "Avant_1929_Femme", "Avant_1929_Homme", "1930_1939_Femme",
    "1930_1939_Homme", "1940_1949_Femme", "1940_1949_Homme", "1950_1959_Femme",
    "1950_1959_Homme", "1960_1969_Femme", "1960_1969_Homme", "1970_1979_Femme",
    "1970_1979_Homme", "1980_1989_Femme", "1980_1989_Homme", "Apres_1980_Femme",
    "Apres_1980_Homme"
  )


# Trier les données pour préparer pour un pseudo panel
df_final <- df_filtre %>%
  arrange(ID_GROUPE, ANNAISS)

df_final <- df_final %>%
  select(ID_GROUPE, ANNAISS, SEXE_rec, PROFESSION, prixplus_rec, everything())

# 2. Calcul de la moyenne pondérée de prixplus_rec pour chaque groupe, mois et année ----
library(dplyr)

# Assurez-vous que df_final contient une colonne N_observations calculée correctement
df_final <- df_final %>%
  group_by(ID_GROUPE, ANENQ, MOISENQ, Ageind_tranche) %>%
  mutate(N_observations = n()) %>%
  ungroup()

# Calcul de la moyenne pondérée tout en conservant toutes les données et colonnes
df_final <- df_final %>%
  group_by(ID_GROUPE, ANENQ, MOISENQ, Ageind_tranche) %>%
  mutate(
    prixplus_pond = weighted.mean(prixplus_rec, w = N_observations, na.rm = TRUE)
  ) %>%
  ungroup()  # Dégrouper pour éviter toute confusion dans les manipulations ultérieures


# Affichage des premières lignes pour vérification
summary(df_final$prixplus_pond)



write.csv(df_final, "C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v5.csv", row.names = FALSE)


## Idée 2 - Avant 80 ou après 80 ------------------
df_filtre_2 <- df

# Mise à jour des groupes d'âge et des identifiants de groupe
df_filtre_2 <- df_filtre_2 %>%
  mutate(
    ID_GROUPE = case_when(
      ANNAISS >= 1980 & SEXE_rec == "Femme" ~ "Apres_1980_Femme",
      ANNAISS >= 1980 & SEXE_rec == "Homme" ~ "Apres_1980_Homme",
      ANNAISS < 1980 & SEXE_rec == "Femme" ~ "Avant_1980_Femme",
      ANNAISS < 1980 & SEXE_rec == "Homme" ~ "Avant_1980_Homme",
      TRUE ~ "Autre" ))


# 2. Calcul de la moyenne pondérée de prixplus_rec pour chaque groupe, mois et année ----

df_filtre_2$ID_GROUPE <- as.factor(df_filtre_2$ID_GROUPE)
    
    # Trier les données pour préparer pour un pseudo panel
    df_filtre_2 <- df_filtre_2 %>%
      arrange(ID_GROUPE, ANNAISS)
    
    # Assurez-vous que df_final contient une colonne N_observations calculée correctement
    df_filtre_2 <- df_filtre_2 %>%
      group_by(ID_GROUPE, ANENQ) %>%
      mutate(N_observations = n()) %>%
      ungroup()
    
    # Calcul de la moyenne pondérée tout en conservant toutes les données et colonnes
    df_filtre_2 <- df_filtre_2 %>%
      group_by(ID_GROUPE, ANENQ) %>%
      mutate(
        prixplus_pond = weighted.mean(prixplus_rec, w = N_observations, na.rm = TRUE)
      ) %>%
      ungroup()  # Dégrouper pour éviter toute confusion dans les manipulations ultérieures
    
    df_filtre_2 <-df_filtre_2 %>%
      select(ID_GROUPE, ANNAISS, SEXE_rec, PROFESSION, prixplus_rec,prixplus_pond, everything())
    


## Création du graphique Idée 2 ----------
    q <- ggplot(df_annual, aes(x = as.factor(ANENQ), y = prixplus_pond, group = ID_GROUPE, color = ID_GROUPE)) +
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
    
    print(q)
    
    
## IDEE 3 - Effet genre et généraiton ------------
    
    library(dplyr)
    library(ggplot2)
    
    # Filtre initial des données
    df_filtre_2 <- df
    
    # Mise à jour des groupes d'âge avec distinction de genre
    df_filtre_2 <- df_filtre_2 %>%
      mutate(
        ID_GROUPE = case_when(
          ANNAISS >= 1980 & SEXE_rec == "Femme" ~ "Apres_1980_Femme",
          ANNAISS >= 1980 & SEXE_rec == "Homme" ~ "Apres_1980_Homme",
          ANNAISS < 1980 & SEXE_rec == "Femme" ~ "Avant_1980_Femme",
          ANNAISS < 1980 & SEXE_rec == "Homme" ~ "Avant_1980_Homme",
          TRUE ~ "Autre"  # Ce cas servira de contrôle pour identifier des erreurs éventuelles de saisie des données
        ))
    
    # Transformation du facteur pour la nouvelle variable ID_GROUPE
    df_filtre_2$ID_GROUPE <- as.factor(df_filtre_2$ID_GROUPE)
    
    # Calcul de la moyenne pondérée de l'inflation perçue
    df_filtre_2 <- df_filtre_2 %>%
      group_by(ID_GROUPE, ANENQ) %>%
      mutate(N_observations = n()) %>%
      ungroup() %>%
      group_by(ID_GROUPE, ANENQ) %>%
      mutate(
        prixplus_pond = weighted.mean(prixplus_rec, w = N_observations, na.rm = TRUE)
      ) %>%
      ungroup() 
    
    # Sélection des colonnes pertinentes pour éviter la redondance
    df_filtre_2 <- df_filtre_2 %>%
      select(ID_GROUPE, ANNAISS, SEXE_rec, PROFESSION, prixplus_rec, prixplus_pond, everything())
    
    
    ## Graphique IDEE 4 ------------
    library(ggplot2)
    library(dplyr)
    
    ## Recodage de df_filtre_2$Ageind_tranche en df_filtre_2$Ageind_tranche_rec
df_filtre_2$Ageind_tranche_rec <- df_filtre_2$Ageind_tranche %>%
  fct_recode(
    "Moins de 36 ans" = "Moins de 26 ans",
    "Moins de 36 ans" = "26 à 36 ans"
  )


# Calcul de la moyenne pondérée de l'inflation perçue
df_filtre_2 <- df_filtre_2 %>%
  group_by(ID_GROUPE, ANENQ) %>%
  mutate(N_observations = n()) %>%
  ungroup() %>%
  group_by(ID_GROUPE, ANENQ) %>%
  mutate(
    prixplus_pond = weighted.mean(prixplus_rec, w = N_observations, na.rm = TRUE)
  ) %>%
  ungroup() 

# Sélection des colonnes pertinentes pour éviter la redondance
df_filtre_2 <- df_filtre_2 %>%
  select(ID_GROUPE, ANNAISS, SEXE_rec, PROFESSION, prixplus_rec, prixplus_pond, everything())


# Mappage des couleurs et des styles de ligne
    age_colors <- c("Moins de 36 ans" = "blue", 
                    "36 à 45 ans" = "red", 
                    "46 à 55 ans" = "orange", 
                    "56 à 65 ans" = "brown", 
                    "66 à 75 ans" = "black", 
                    "76 ans et plus" = "grey")
    
    
    group_line_types <- c("Apres_1980_Femme" = "solid",
                          "Apres_1980_Homme" = "twodash",
                          "Avant_1980_Femme" = "dotted",
                          "Avant_1980_Homme" = "dotdash")
    
    
    
    ## Recodage âge ------------
    
  
    
    # Assurez-vous que df_filtre_2 a les bonnes transformations
    df_filtre_2 <- df_filtre_2 %>%
      mutate(Ageind_tranche_rec = factor(Ageind_tranche_rec, levels = names(age_colors)),
             ID_GROUPE = factor(ID_GROUPE, levels = names(group_line_types)))
    
    # Création du graphique avec lignes et points colorés selon l'âge
    ggplot(df_filtre_2, aes(x = as.factor(ANENQ), y = prixplus_pond, group = interaction(ID_GROUPE, Ageind_tranche_rec))) +
      geom_line(aes(color = Ageind_tranche_rec, linetype = ID_GROUPE)) +  # Lignes colorées par âge, style par groupe
      geom_point(aes(color = Ageind_tranche_rec), size = 3) +  # Points colorés par âge
      scale_color_manual(values = age_colors) +
      scale_linetype_manual(values = group_line_types) +
      labs(title = "Perception moyenne de l'inflation par année, groupe d'âge/gendre et tranche d'âge",
           x = "Année d'enquête",
           y = "Perception moyenne de l'inflation (pondérée)",
           color = "Tranche d'âge",
           linetype = "Groupe") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
    
    
    
# IDEE 5 ------------------
    library(ggplot2)
    library(dplyr)
    
    # Filtre initial des données et transformation du groupe
    df_filtre_2 <- df %>%
      mutate(
        ID_GROUPE = case_when(
          ANNAISS >= 1980 & SEXE_rec == "Femme" ~ "Apres_1980_Femme",
          ANNAISS >= 1980 & SEXE_rec == "Homme" ~ "Apres_1980_Homme",
          ANNAISS < 1980 & SEXE_rec == "Femme" ~ "Avant_1980_Femme",
          ANNAISS < 1980 & SEXE_rec == "Homme" ~ "Avant_1980_Homme",
          TRUE ~ "Autre"
        )) %>%
      mutate(ID_GROUPE = as.factor(ID_GROUPE))
      ## Recodage de df_filtre_2$Ageind_tranche en df_filtre_2$Ageind_tranche_rec
      df_filtre_2$Ageind_tranche_rec <- df_filtre_2$Ageind_tranche %>%
      fct_recode(
        "Moins de 36 ans" = "Moins de 26 ans",
        "Moins de 36 ans" = "26 à 36 ans"
      )
    
    # Calcul de la moyenne pondérée
    df_filtre_2 <- df_filtre_2 %>%
      group_by(ID_GROUPE, ANENQ, Ageind_tranche_rec) %>%
      mutate(N_observations = n()) %>%
      ungroup() %>%
      group_by(ID_GROUPE, ANENQ, Ageind_tranche_rec) %>%
      mutate(prixplus_pond = weighted.mean(prixplus_rec, w = N_observations, na.rm = TRUE)) %>%
      ungroup()
    
    # Mappage des couleurs et des styles de ligne
    age_colors <- c("Moins de 36 ans" = "blue", "36 à 45 ans" = "red", "46 à 55 ans" = "orange", 
                    "56 à 65 ans" = "brown", "66 à 75 ans" = "black", "76 ans et plus" = "grey")
    group_line_types <- c("Apres_1980_Femme" = "solid", "Apres_1980_Homme" = "twodash", 
                          "Avant_1980_Femme" = "dotted", "Avant_1980_Homme" = "dotdash")
    
    # Préparation du graphique avec distinction de genre pour les points
    ggplot(df_filtre_2, aes(x = as.factor(ANENQ), y = prixplus_pond, group = interaction(ID_GROUPE, Ageind_tranche_rec))) +
      geom_line(aes(color = Ageind_tranche_rec, linetype = ID_GROUPE)) +
      geom_point(aes(color = Ageind_tranche_rec, shape = SEXE_rec), size = 3) +
      scale_color_manual(values = age_colors) +
      scale_shape_manual(values = c("Femme" = 4, "Homme" = 16)) +  # 4 = cross, 16 = filled circle
      scale_linetype_manual(values = group_line_types) +
      labs(title = "Perception moyenne de l'inflation par année, groupe d'âge/gendre et tranche d'âge",
           x = "Année d'enquête",
           y = "Perception moyenne de l'inflation (pondérée)",
           color = "Tranche d'âge",
           linetype = "Groupe",
           shape = "Genre") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5))
    
    
## IDEE 6 - AGE EN ABSCISE ------------------------------
    
    library(dplyr)
    library(ggplot2)
    
    # Supposons df_filtre_2 est ton dataframe préparé avec 'Age', 'prixplus_rec', 'ANENQ', 'ID_GROUPE'
    
    # Filtre initial des données et transformation du groupe
    df_filtre_2 <- df %>%
      mutate(
        ID_GROUPE = case_when(
          ANNAISS >= 1980 & SEXE_rec == "Femme" ~ "Apres_1980_Femme",
          ANNAISS >= 1980 & SEXE_rec == "Homme" ~ "Apres_1980_Homme",
          ANNAISS < 1980 & SEXE_rec == "Femme" ~ "Avant_1980_Femme",
          ANNAISS < 1980 & SEXE_rec == "Homme" ~ "Avant_1980_Homme",
          TRUE ~ "Autre"
        )) %>%
      mutate(ID_GROUPE = as.factor(ID_GROUPE))
   
     df_filtre_2$Ageind_tranche_rec <- df_filtre_2$Ageind_tranche %>%
      fct_recode(
        "Moins de 36 ans" = "Moins de 26 ans",
        "Moins de 36 ans" = "26 à 36 ans"
      )

    
    # Calcul de la moyenne pondérée
    df_filtre_2 <- df_filtre_2 %>%
      group_by(ID_GROUPE, Ageind_tranche_rec) %>%
      mutate(N_observations = n()) %>%
      ungroup()
    
    df_filtre_2 <-  df_filtre_2 %>%
      group_by(ID_GROUPE, Ageind_tranche_rec) %>%
      summarize(prixplus_pond = weighted.mean(prixplus_rec, w = N_observations, na.rm = TRUE)) %>%
      ungroup()
    
    
    # Création du graphique
    ggplot(df_filtre_2, aes(x = Ageind_tranche_rec, y = prixplus_pond, group = ID_GROUPE, color = ID_GROUPE)) +
      geom_line() +
      geom_point() +
      scale_x_discrete() +
      labs(title = "Perception moyenne de l'inflation par tranche d'âge et groupe générationnel",
           x = "Tranche d'âge",
           y = "Perception moyenne de l'inflation (pondérée)",
           color = "Groupe Générationnel") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    
## IDEE 7 ------------
    
    library(dplyr)
    library(ggplot2)
    
    # Préparation des données avec filtrage de l'âge jusqu'à 50 ans
    df_filtre_2 <- df %>%
      mutate(
        Age = ANENQ - ANNAISS,
        ID_GROUPE = case_when(
          ANNAISS >= 1970 & SEXE_rec == "Femme" ~ "Apres_1970_Femme",
          ANNAISS >= 1970 & SEXE_rec == "Homme" ~ "Apres_1970_Homme",
          ANNAISS < 1970 & SEXE_rec == "Femme" ~ "Avant_1970_Femme",
          ANNAISS < 1970 & SEXE_rec == "Homme" ~ "Avant_1970_Homme",
          TRUE ~ "Autre"
        )
      ) 
    
    # Calcul de la moyenne pondérée par le nombre d'observations
    df_filtre_2 <- df_filtre_2 %>%
      group_by(ID_GROUPE, Age) %>%
      summarise(
        N_observations = n(),
        prixplus_pond = if_else(N_observations > 1, 
                                weighted.mean(prixplus_rec, w = rep(1, N_observations), na.rm = TRUE), 
                                mean(prixplus_rec, na.rm = TRUE)), # Utilisez mean si une seule observation
        .groups = 'drop'
      )
    
    # Création du graphique
    ggplot(df_filtre_2, aes(x = Age, y = prixplus_pond, group = ID_GROUPE, color = ID_GROUPE)) +
      geom_line() +
      geom_point() +
      labs(title = "Perception moyenne de l'inflation par âge et groupe générationnel",
           x = "Âge (jusqu'à 50 ans)",
           y = "Perception moyenne de l'inflation pondérée",
           color = "Groupe Générationnel") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlim(0, 60)  # Limite de l'axe des x jusqu'à 50 ans
    