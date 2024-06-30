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

## COMMENCER AU RECODAGE ------------
# 0 Importation Données préalablement agrégées entre 2004 et 2021 ----------------

df <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v3.csv")



# 1. Préparation des données ----------------


# 1.1 Identifiant de la vague -------------
# La grande difficulté ici a été de créer un identifiant poiur chaque panel. 
# Ajouter une conversion si nécessaire
library(dplyr)

# Prétraitement des données : conversion des colonnes MOISENQ et VAGUEE en format numérique.
# Cela est nécessaire car les données peuvent être importées en tant que texte.
df <- df %>%
  dplyr::mutate(
    MOISENQ = as.numeric(as.character(MOISENQ)),  # Convertir MOISENQ en numérique pour assurer une manipulation correcte
    VAGUEE = as.numeric(as.character(VAGUEE))      # Convertir VAGUEE en numérique pour assurer une manipulation correcte
  )

# Organiser les données par année (ANENQ), mois d'enquête (MOISENQ) et par vague d'enquête (VAGUEE) décroissante.
# Cela prépare les données pour l'attribution des identifiants uniques et séquentiels.
df <- df %>%
  dplyr::arrange(ANENQ, MOISENQ, dplyr::desc(VAGUEE)) %>%
  dplyr::group_by(ANENQ, MOISENQ) %>%
  dplyr::mutate(
    id_vague = paste(ANENQ, MOISENQ, VAGUEE, sep = "_"),  # Créer un identifiant unique pour chaque vague en combinant année, mois et numéro de vague
    cycle_id = dplyr::row_number()  # Attribuer un numéro séquentiel à chaque enregistrement au sein du même groupe (année, mois)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    cycle_id_global = cumsum(cycle_id == 1),  # Créer un identifiant global qui s'incrémente à chaque début de nouveau groupe de données
    cycle = paste("id", cycle_id_global, sep = "")  # Formater l'identifiant global en ajoutant un préfixe "id"
  )

# Finaliser le dataframe en sélectionnant l'ordre des colonnes, mettant en avant les identifiants créés pour chaque vague et cycle.
df <- df %>%
  dplyr::select(cycle, id_vague, everything())  # Réarranger les colonnes pour afficher les identifiants en premier

# Le dataframe 'df' est maintenant prêt avec des identifiants ajoutés pour chaque vague et chaque cycle, utilisable pour des analyses ultérieures.


# Ajouter l'IPC ----------------
# Inflation -----------------------------
inflation_values <- fread("02_data/inflation/Inflation_OCDE_1945_2023.csv")


# Prendre certaines varibales 
inflation_values <- inflation_values %>%
  select(TIME_PERIOD, OBS_VALUE)
inflation_values[, `:=`(
  ANENQ = as.numeric(substr(TIME_PERIOD, 1, 4)),
  MOISENQ = as.numeric(substr(TIME_PERIOD, 6, 7))
)]

# Rename OBS_VALUE to IPC
setnames(inflation_values, "OBS_VALUE", "IPC")

# Sélectionner uniquement les colonnes nécessaires dans inflation_values
inflation_values <- inflation_values %>%
  select(ANENQ, MOISENQ, IPC)

# Fusionner les deux dataframes en utilisant left_join
df <- df %>%
  left_join(inflation_values, by = c("ANENQ", "MOISENQ"))

## 1.2 Date -------------
df <- df %>%
  mutate(
    # Créer une variable unique pour le mois et l'année
    date = paste(ANENQ, MOISENQ, sep = "_"),
    # Assurer que la variable date est de type factor pour plm
    date = as.factor(date),
    # Transformer id_vague pour le modèle plm
    id_vague = as.factor(cycle)
  )

df <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v4.csv")

## 1.3 Recodage des variables explicatives -----------------

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


### Réordonnancement de df$PROFESSION ----
df$PROFESSION <- as.factor(df$PROFESSION)
df$PROFESSION <- df$PROFESSION %>%
  fct_relevel(
    "Ouvrier, technicien, agent de maîtrise", "Employé", "Ingénieur, cadre, directeur"
  )

### J'enlève les NA ----------
df <- df %>%
  arrange(id_vague, date) %>%  # Assurez-vous que les données sont triées
  group_by(id_vague) %>%
  mutate(lag_prixplus_rec = lag(prixplus_rec)) %>%
  ungroup()


df_clean <- df %>%
  filter(!is.na(prixplus_rec) & !is.na(lag_prixplus_rec) & !is.na(PROFESSION) & !is.na(DIPLOME_rec) & 
           !is.na(FINANCES_rec) & !is.na(Ageind_tranche) & !is.na(SEXE_rec) & !is.na(IPC))
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
# Les poids ---
df_clean <- df_clean %>%
  group_by(id_vague) %>%
  mutate(adjusted_weight = IWEIGHT / sum(IWEIGHT)) %>%
  ungroup()

df_clean$adjusted_weight[df_clean$adjusted_weight <= 0] <- 1
df_clean <- df_clean[!is.na(df_clean$adjusted_weight), ] 


# 2. Les modèles de regression -----------------------
library(plm)
## 2.1 Modèle pooled -----------------


# J'invente un identifiant : ??? df_clean$observation_id <- seq_len(nrow(df_clean))
# Modèle Pooled avec variable retardée
# Utilisation dans plm
reg_pooled <- plm(prixplus_rec ~ lag_prixplus_rec + PROFESSION + DIPLOME_rec + FINANCES_rec + Ageind_tranche + SEXE_rec + IPC,
                        data = df_clean,
                        model = "pooling",
                        index = c("id_vague", "date"),
                        effect  = "twoways",
                        weights = adjusted_weight)

# Modèle within
reg_within <- plm(prixplus_rec ~ lag_prixplus_rec + PROFESSION + DIPLOME_rec + FINANCES_rec + Ageind_tranche + SEXE_rec + IPC,
                  index = c("id_vague", "date"),
                  data = df_clean, 
                  model = "within", 
                  effect  = "twoways",
                  weights = adjusted_weight)


ptest <- pFtest(reg_within, reg_pooled)
print(ptest)

## Une valeur F de 10.646 indique que le rapport entre la variance expliquée par le modèle à effets fixes et la variance résiduelle est significativement élevé, ce qui suggère que les variables incluses dans le modèle (et les effets fixes par individu) expliquent une part significative de la variation dans la variable dépendante, prixplus_rec.

## La p-value très faible (<  indique que vous pouvez rejeter l'hypothèse nulle selon laquelle ajouter des effets fixes pour chaque individu (ou pour chaque individu et période si c'est un effet fixe à deux dimensions) n'améliore pas significativement le modèle. En termes simples, cela signifie que les variations intra-individus expliquent significativement une partie de la variation observée dans prixplus_rec
## Comparaison des deux modèles -----------

tbl_regression(reg_within, 
               # intercept = FALSE,  # Il est possible que vous deviez désactiver l'affichage de l'intercept
               include = c("lag_prixplus_rec", "PROFESSION", "DIPLOME_rec", "FINANCES_rec", "Ageind_tranche", "SEXE_rec", "IPC"))

# 3. Présentation -------------
library(md)
tbl_regression(reg_within, 
               intercept = FALSE,  # Il est possible que vous deviez désactiver l'affichage de l'intercept
               include = c("lag_prixplus_rec", "PROFESSION", "DIPLOME_rec", "FINANCES_rec", "Ageind_tranche", "SEXE_rec", "IPC"),
               label = list(
                 lag_prixplus_rec ~ "Prédiction précédente sur le prix",
                 PROFESSION ~ "Profession",
                 DIPLOME_rec ~ "Niveau de diplôme",
                 FINANCES_rec ~ "Situation financière actuelle du foyer",
                 Ageind_tranche ~ "Âge de l'individu",
                 SEXE_rec ~ "Genre",
                 IPC ~ "Indice des prix à la consommation"
               )) %>%
  modify_header(label = "",
                estimate ~ "Coefficient",
                std.error ~ "Erreur standard",
                p.value ~ "P-valeur") %>% 
  bold_labels() %>%
  add_significance_stars(
    hide_ci = FALSE,
    hide_p = TRUE,
    hide_se = TRUE ) %>% 
  as_gt() %>%
  gt::tab_options(table.font.names = "Garamond", table.font.size = 11) %>%
  gt::tab_header(
    title = "Figure 3. Les déterminants de l'inflation perçue", 
    subtitle = "Régression linéaire à effet fixe sur l'estimation de l'inflation") %>%
  gt::tab_style(style = list(gt::cell_text(weight = "bold", align = "left", size = 15)),
     locations = gt::cells_title(groups = "title")) %>%
  gt::tab_style(style = list(gt::cell_text(align = "left", size = 12)),locations = gt::cells_title(groups = "subtitle")) %>%
  gt::tab_footnote(
    footnote = md(
      paste(
        "**Source** : Données CAMME agrégée et pondérée de 2004 à 2021 de l'INSEE.",
        "\n\n **Note de lecture** : À situation économique, âge de la vie, genre, IPC par année, et niveau de diplôme équivalent, l'estimation de l'inflation baisse de 1,9 points si l'individu a une profession de cadre, d'ingénieur ou de direction. Cet effet est statistiquement significatif au seuil de 0,001."))
  )
