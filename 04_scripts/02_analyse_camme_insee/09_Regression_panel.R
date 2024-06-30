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


Annee2021 <- fread(here("02_data/travail/Inflation_base_2021_recodee.csv"))
"02_data/agregees/Inflation_base_04_21.csv"

# 1. Préparation des données ----------------


# 1.1 Identifiant de la vague -------------
# La grande difficulté ici a été de créer un identifiant poiur chaque panel. 
# Ajouter une conversion si nécessaire
Annee2021 <- Annee2021 %>%
  mutate(
    MOISENQ = as.numeric(as.character(MOISENQ)), # Assurer que MOISENQ est numérique
    VAGUEE = as.numeric(as.character(VAGUEE))    # Assurer que VAGUEE est numérique
  )

# Ajouter des identifiants de cycle en fonction des exigences
Annee2021 <- Annee2021 %>%
  arrange(ANENQ, MOISENQ, desc(VAGUEE)) %>%
  group_by(ANENQ, MOISENQ) %>%
  mutate(
    id_vague = paste(ANENQ, MOISENQ, VAGUEE, sep = "_"),
    cycle_id = row_number()   # Attribue un numéro séquentiel inverse à chaque vague chaque mois
  ) %>%
  ungroup() %>%
  arrange(ANENQ, MOISENQ, cycle_id) %>%
  mutate(
    cycle_id_global = cumsum(cycle_id == 1)  # Crée un identifiant global qui s'incrémente à chaque nouvelle vague la plus récente chaque mois
  ) %>%
  mutate(
    cycle = paste("id", cycle_id_global, sep = "")
  )


Annee2021 <- Annee2021 %>%
  select(cycle, id_vague, everything())



## 1.2 Date -------------
Annee2021 <- Annee2021 %>%
  mutate(
    # Créer une variable unique pour le mois et l'année
    date = paste(ANENQ, MOISENQ, sep = "_"),
    # Assurer que la variable date est de type factor pour plm
    date = as.factor(date),
    # Transformer id_vague pour le modèle plm
    id_vague = as.factor(cycle)
  )


## 1.3 Recodage des variables explicatives -----------------

## Recodage de Annee2021$FINANCES en Annee2021$FINANCES_rec
Annee2021$FINANCES_rec <- Annee2021$FINANCES %>%
  as.character() %>%
  fct_recode(
    "Met de d'argent de côté" = "1",
    "Met de d'argent de côté" = "2",
    "Budget juste" = "3",
    "Tire un peu sur les réserves" = "4",
    "Endettement" = "5",
    NULL = "9"
  )

Annee2021$FINANCES_rec <- as.factor(Annee2021$FINANCES_rec) 

## Réordonnancement de Annee2021$FINANCES_rec
Annee2021$FINANCES_rec <- Annee2021$FINANCES_rec %>%
  fct_relevel(
    "Met de d'argent de côté",
    "Budget juste", "Tire un peu sur les réserves", "Endettement"
  )


## Recodage de Annee2021$Ageind en Annee2021$Ageind_rec
Annee2021$Ageind_tranche <- cut(Annee2021$Ageind,
                                include.lowest = TRUE,
                                right = FALSE,
                                dig.lab = 4,
                                breaks = c(0, 26, 36, 46, 56, 66, 76, 121)
)

Annee2021$Ageind_tranche <- as.factor(Annee2021$Ageind_tranche)

## Recodage de Annee2021$Ageind_tranche en Annee2021$Ageind_tranche_rec
Annee2021$Ageind_tranche <- Annee2021$Ageind_tranche %>%
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
Annee2021$DIPLOME_rec <- Annee2021$DIPLOME_rec %>%
  fct_relevel(
    "Aucun diplôme ou certificat d’études primaires", "CAP, BEP ou équivalent",
    "Baccalauréat, brevet professionnel ou équivalent", "Diplôme du supérieur court (niveau bac + 2)",
    "Diplôme du supérieur long (supérieur à bac + 2)"
  )
### Mono --------------
Annee2021$MONOPARENTALE<- as.factor(Annee2021$MONOPARENTALE)
Annee2021$MONOPARENTALE <- Annee2021$MONOPARENTALE %>%
  fct_relevel(
    "Pas monoparental", "Monoparental"
  )

### Genre -----------
Annee2021$SEXE_rec <- as.factor(Annee2021$SEXE_rec)
Annee2021$SEXE_rec <- relevel(Annee2021$SEXE_rec, "Homme")

### Réordonnancement de Annee2021$MOISENQ_rec ---
Annee2021$MOISENQ_rec <-as.factor(Annee2021$MOISENQ_rec)
Annee2021$MOISENQ_rec <- Annee2021$MOISENQ_rec %>%
  fct_relevel(
    "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
    "Aout", "Septembre", "Octobre", "Novembre", "Décembre")


### Réordonnancement de Annee2021$PROFESSION ----
Annee2021$PROFESSION <- as.factor(Annee2021$PROFESSION)
Annee2021$PROFESSION <- Annee2021$PROFESSION %>%
  fct_relevel(
    "Ouvrier, technicien, agent de maîtrise", "Employé", "Ingénieur, cadre, directeur"
  )

### J'enlève les NA ----------
Annee2021 <- Annee2021 %>%
  arrange(id_vague, date) %>%  # Assurez-vous que les données sont triées
  group_by(id_vague) %>%
  mutate(lag_prixplus_rec = lag(prixplus_rec)) %>%
  ungroup()

df <- Annee2021
df_clean <- df %>%
  filter(!is.na(prixplus_rec) & !is.na(lag_prixplus_rec) & !is.na(PROFESSION) & !is.na(DIPLOME_rec) & 
           !is.na(FINANCES_rec) & !is.na(Ageind_tranche) & !is.na(SEXE_rec) & !is.na(IPC))

# Les poids ---
df_clean <- df_clean %>%
  group_by(id_vague) %>%
  mutate(adjusted_weight = IWEIGHT / sum(IWEIGHT)) %>%
  ungroup()


# 2. Les modèles de regression -----------------------
## 2.1 Modèle pooled -----------------
model_pooled_lag <- plm(prixplus_rec ~ lag_prixplus_rec + PROFESSION + DIPLOME_rec + FINANCES_rec + Ageind_tranche + SEXE_rec + IPC,
                        data = df_clean,
                        model = "pooling",
                        index = c("id_vague", "date"),
                        weights = adjusted_weight)




## Regression effet fixe --------
library(AER)
library(plm)

# Modèle Pooled avec lag
model_pooled_lag <- plm(prixplus_rec ~  PROFESSION + DIPLOME_rec + FINANCES_rec + Ageind_tranche + SEXE_rec + IPC,
                        data = df_clean,
                        model = "pooling",
                        index = c("id_vague", "date"),
                        weights = adjusted_weight)


summary(model_pooled_lag)

# Modèle within --------------
# Préalable : soit factor soit numérique 
Annee2021$FINANCES_rec <- as.factor(Annee2021$FINANCES_rec)

iorder(Annee2021$FINANCES_rec)



## Réordonnancement de Annee2021$FINANCES_rec
Annee2021$FINANCES_rec <- Annee2021$FINANCES_rec %>%
  fct_relevel(
    "Tire sur les réserves", "S'endette", "Budget juste", "Peu d'argent de côté",
    "Beaucoup d'argent de côté"
  )

Annee2021$SEXE_rec <- as.factor(Annee2021$SEXE_rec)
Annee2021$Ageind <- as.numeric(Annee2021$Ageind)
Annee2021$MOISENQ <- as.factor(Annee2021$MOISENQ)
Annee2021$VAGUEE <- as.factor(Annee2021$VAGUEE)


Annee2021$MOISENQ <- as.factor(Annee2021$MOISENQ)
## Créer un identifiant 
dt[, id_vague := paste(ANENQ, MOISENQ, VAGUEE, sep="_")]

# Correction de la syntaxe pour le modèle
# La fonction plm nécessite que les données soient de type pdata.frame pour pouvoir utiliser l'argument index correctement
# L'argument index doit être fourni sous forme de vecteur avec le nom de l'individu puis le nom du temps
pdata <- pdata.frame(Annee2021, index = c("MOISENQ", "VAGUEE"))

# Regression linéaire 
reg_lineaire <- lm(prixplus_rec ~ MONOPARENTALE + SITUAECO_rec + Ageind + SEXE_rec + IPC + DIPLOME_rec + interaction(factor(MOISENQ),factor(VAGUEE)),
                  data = Annee2021)

reg_lineaire %>%
  tbl_regression(intercept = TRUE) + 
  modify_header("Régression linéaire sur ")

# Modèle within
reg_within <- plm(prixplus_rec ~ SITUAECO_rec + Ageind + SEXE_rec + MOISENQ + VAGUEE,
                  index = c("VAGUEE", "MOISENQ"),
                  data = Annee2021, 
                  model = "within")


## Modèle within 
reg_within <- plm(prixplus_rec ~ SITUAECO_rec + Ageind + SEXE_rec,
                  data = Annee2021, 
                  # Ne pas se tromper dans l'ordre de l'indexation
                  index = c("VAGUEE", "MOISENQ"), 
                  model = "within")
summary(reg_within)

# Regression simple 
model_lm <- lm(prixplus_rec ~ SITUAECO_rec + Ageind + SEXE_rec + factor(MOISENQ) + factor(VAGUEE) + 
                 interaction(factor(MOISENQ), factor(VAGUEE)), 
               data = Annee2021)

summary(model_lm)


# 3. Présentation -------------

tbl_regression(model_pooled_lag, 
               intercept = TRUE, 
               include = c(PROFESSION, DIPLOME_rec, FINANCES_rec, Ageind_tranche, SEXE_rec, IPC),
               label = list(
                 all_intercepts() ~ "Constante",
                 PROFESSION ~ "Profession",
                 FINANCES_rec ~ "Situation financière actuelle du foyer",
                 Ageind_tranche ~ "Âge de l'individu",
                 SEXE_rec ~ "Genre",
                 IPC ~ "Indice des prix à la consommation",
                 DIPLOME_rec ~ "Niveau de diplôme")) %>%
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
    subtitle = "Régression linéaire sur l'estimation de l'inflation") %>%
  gt::tab_style(style = list(gt::cell_text(weight = "bold", align = "left", size = 15)),
                locations = gt::cells_title(groups = "title")) %>%
  gt::tab_style(style = list(gt::cell_text(align = "left", size = 12)),locations = gt::cells_title(groups = "subtitle")) %>%
  gt::tab_footnote(
    footnote = md(
      paste(
        "**Source** : Données CAMME 2021, INSEE avec données mensuelles agrégées et pondérées. Nous avons pris en compte l'effet d'interaction entre la vague et le mois, même s'il n'est pas affiché sur ce tableau.",
        "\n\n **Note de lecture** : À situation économique, âge de la vie, genre, IPC par année, et niveau de diplôme équivalent, l'estimation de l'inflation baisse de 1,9 points si l'individu a une profession de cadre, d'ingénieur ou de direction. Cet effet est statistiquement significatif au seuil de 0,001.",
        "\n\n **F-test** :", f_statistic,
        "\n **R2** :", r_squared,
        "\n **R2 ajusté** :", r_squared_adj
      )
    )
  )
