##The Gender of Academic Capital
#Clean script for matching predictions
rm(list=ls())
#History
#Latest version: Jan, 12th 2024 [EO]
#20231125+ Etienne revise
#20230627 Senmiao + Etienne

library(tidyverse)
library(RColorBrewer)
library(plotly)
library(scales)
library(gender)

`%notin%` <- Negate(`%in%`)


all <- fread("06_data_base/base_generale_ollion_net.csv")


base_ollion$prenom_auteur1["Camille"]
View(base_ollion)


# Descriptive stats on Base
#Remove a few journals, select on years > 2000
# all <- all[!all$revue %in% c("Revue Défense Nationale", "L’Ouest Saharien", "Revue internationale d'intelligence économique"),]
# all <- all[all$annee>2000 & all$annee<2023,]
# all$revue <- gsub("&amp;", "&", all$revue)
# all$revue <- gsub("Vingtième Siècle. Revue d'histoire", "20 & 21. Revue d'histoire", all$revue)
# all$revue <- gsub("Revue Tiers Monde", "Revue internationale des études du développement (ex RTM)", all$revue)

plot(table(all$year))
table(all$journal)
#write.csv(table(all$revue, all$annee), "/home/leo/Downloads/RevuexAnnees_27Nov2023.csv")


# Nettoyage base Ollion - 

library(dplyr)

all$auteurs[all$auteurs == "Le Monde"] <- NA
all$auteur1[all$auteur1 == "Le Monde"] <- NA
all$auteur1[all$auteur2 == "Le Monde"] <- NA

all$auteurs[all$auteurs == "Liberation.fr"] <- NA
all$auteur1[all$auteur1 == "Liberation.fr"] <- NA
all$auteur1[all$auteur2 == "Liberation.fr"] <- NA




library(tidyr)
all$V1 <- NULL
all$auteura <- all$auteur1

# Identifier les noms de colonnes en double
nom_colonnes <- names(all)
noms_doubles <- nom_colonnes[duplicated(nom_colonnes) | duplicated(nom_colonnes, fromLast = TRUE)]


# Renommer les colonnes en double, par exemple en ajoutant un suffixe pour les rendre uniques
# Ceci est juste un exemple. Vous devrez peut-être ajuster la logique en fonction de votre situation spécifique.
noms_uniques <- make.unique(nom_colonnes)
names(all) <- noms_uniques

all <- all %>%
  tidyr::separate(col = "auteura", into = c("prenom_auteur1", "nom_auteur1"), sep = " ", extra = "merge", fill = "right")

all$prenom_auteur1 <- toupper(all$prenom_auteur1)




all$auteurb <- all$auteur2

all <- all %>%
  tidyr::separate(col = "auteurb", into = c("prenom_auteur2", "nom_auteur2"), sep = " ", extra = "merge", fill = "right")

write.csv(all, here("06_data_base/base_ollion_genre.csv"))



## Changement Camille -----------
base <- fread("06_data_base/base_personnes_genre.csv")

# Mettre à jour la base de données en fonction des conditions
base <- base %>%
  mutate(sexe = ifelse(prenom_auteur1 == "CAMILLE" & (nom_auteur1 == "Landais" | nom_auteur1 == "Mialot"), 1, sexe),
         genre = ifelse(prenom_auteur1 == "CAMILLE" & (nom_auteur1 == "Landais" | nom_auteur1 == "Mialot"), "Homme", genre))



write.csv(base, "06_data_base/base_personnes_genre.csv", row.names = FALSE)





## Etude du genre : corpus général restreint ----------------------



base_filtered <- base %>%
  filter(date >= as.Date("1991-01-01") & date <= as.Date("2019-01-01")) %>%
  mutate(month = cut(date, "month")) 

base_filtered <- base_filtered %>%
  filter(!is.na(base_filtered$sexe))

# write.csv(base_filtered, "06_data_base/corpus_generale_genre_restreint.csv", row.names = FALSE)


freq(base_filtered$genre)

library(ggplot2)

library(ggplot2)

# Exemple de données
base_filtered <- data.frame(
  date = rep(seq(as.Date("1991-01-01"), as.Date("2019-01-01"), by = "year"), 2),
  genre = rep(c("Homme", "Femme"), each = 29),
  count = sample(1:100, 58, replace = TRUE)
)



library(ggplot2)

## Graphique sur le genre
library(md)
ggplot(base_filtered) +
 aes(x = date, fill = genre) +
 geom_histogram(bins = 30L, position = "dodge") +
 scale_fill_hue(direction = 1) +
  scale_fill_manual(values = c("#b2182b", "#f4a582")) +
  theme_minimal()  +
ggtitle("Figure 8. Répartitition du genre dans le corpus") +
  labs(x = "Année de publication de l'article", 
       y = "Nombre d'articles de presse",
       fill= "Genre",
       caption = "Source : Corpus principal, données d'Etienne Ollion
                  Champ : Corpus général entre 1991 et 2019 avec les articles dont le genre du premier auteur ou de la première autrice a pu être identifié.") +
  theme_minimal(base_size = 14) +  # Thème plus jolie 
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(text = element_text(family = "Garamond"),
        plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text( size = 12)) # Mettre le titre en gras+
  theme_minimal()


  library(gt)
  
## gt -----
  
  library(ggplot2)
  library(gt)
  
  # Création du graphique avec ggplot2
  graph <- ggplot(base_filtered) +
    aes(x = date, fill = genre) +
    geom_histogram(bins = 30L, position = "dodge") +
    scale_fill_manual(values = c("#b2182b", "#f4a582")) +
    theme_minimal(base_size = 14) +
    ggtitle("Figure 8. Répartition du genre dans le corpus") +
    labs(x = "Année de publication de l'article", 
         y = "Nombre d'articles de presse",
         fill = "Genre") +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    theme(text = element_text(family = "Garamond"),
          plot.title = element_text(face = "bold", size = 16),
          axis.title.x = element_text(face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          legend.title = element_text(face = "bold", size = 12),
          legend.text = element_text(size = 12))
  
  # Convertir le graphique ggplot en gt
  graph_table <- as_gt(graph)
  
  # Ajouter une note de bas de page formatée en Markdown
  graph_table <- graph_table %>%
    tab_spanner(label = "**Note :**",
                columns = everything(),
                locations = cells_column_labels()) %>%
    tab_footnote(
      footnote = "Source : Corpus principal, données d'Etienne Ollion\nChamp : Corpus général entre 1991 et 2019 avec les articles dont le genre du premier auteur ou de la première autrice a pu être identifié.",
      locations = cells_footnote(),
      align = "left"
    )
  
  # Afficher la table graphique
  graph_table
  
##

# Création de la population pyramid
ggplot(base_filtered, aes(x = date, y = ifelse(genre == "Homme", -count, count), fill = genre)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("#f4a582","#b2182b")) +
  theme_minimal() 
  ggtitle("Population Pyramid") +
  labs(x = "Année de publication de l'article", 
       y = "Nombre d'articles de presse",
       fill= "Genre",
       caption = "Source : Corpus principal d'Etienne Ollion") +
  theme_minimal(base_size = 14) +  # Thème plus jolie 
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(text = element_text(family = "Garamond"),
        plot.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text( size = 10)) # Mettre le titre en gras+





library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# Agréger les données par année et genre pour obtenir les effectifs absolus
aggregated_data <- base_filtered %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year, genre) %>%
  summarise(count = sum(count))

# Création de la population pyramid inversée avec les effectifs absolus
ggplot(aggregated_data, aes(x = year, y = ifelse(genre == "Homme", count, -count), fill = genre)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("#f4a582", "#b2182b")) +
  theme_minimal(base_size = 14) +
  ggtitle("Population Pyramid") +
  labs(x = "Année de publication de l'article", 
       y = "Nombre d'articles de presse",
       fill = "Genre",
       caption = "Source : Corpus principal d'Etienne Ollion") +
  theme(text = element_text(family = "Garamond"),
        plot.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10))  # Mettre le titre en gras


## Barcode plot -----------------
library(ggplot2)

# Exemple de données
set.seed(123)
base_filtered <- data.frame(
  date = sample(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by="day"), 100, replace = TRUE),
  genre = sample(c("Homme", "Femme"), 100, replace = TRUE)
)

# Création du Barcode plot
library(ggplot2)

library(ggplot2)

# Création de la Circle Timeline avec des barres temporelles et des cercles superposés
ggplot(base_filtered, aes(x = date, y = genre, fill = genre)) +
  geom_tile(aes(height = 1), color = "white") +  # Barres temporelles
  geom_point(size = 3, shape = 21, color = "black") +  # Cercles superposés
  scale_fill_manual(values = c("#f4a582", "#92c5de")) +  # Couleurs pour Homme et Femme
  theme_minimal() +
  labs(x = "Date", y = "Genre", fill = "Genre") +
  ggtitle("Circle Timeline des Articles en fonction du Genre") +
  theme(legend.position = "bottom", 
        legend.title = element_text(face = "bold")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")  # Pour ajuster les étiquettes de l'axe des x


library(ggplot2)
library(dplyr)




# #############################################################
# ####                  2.Merge files                      ####
# #############################################################
# tomerge_genre <- 
#   paste0("/home/leo/ownCloud/ARSS/PlanQuinqua/BERT_Predictions/Prediction/", "2024-01-08-tiggerbert-2023-05-29-abstracts-gastonEtienne_Genre4_20240108.csv")
# 
# genre_pred <-
#   read_csv(tomerge_genre) %>%
#   mutate(
#     genre_2cl = case_when(bertpred_pas_genre > bertpred_genre ~ "0", TRUE ~ "1"), # create gender label
#     genre_prob = apply(.[, 2:3], 1, max)) %>% # extract prediction proabilitiy
#   select(idg, genre_2cl, genre_prob) %>% 
#   rename("nid" = "idg")
# 
# tomerge_classe <- 
#   paste0("/home/leo/ownCloud/ARSS/PlanQuinqua/BERT_Predictions/Prediction/", "2023-06-27-classe.csv")
# 
# classe_pred <-
#   read_csv(tomerge_classe) %>%
#   mutate(
#     classe_2cl = case_when(bertpred_pas_classe > bertpred_classe ~ "0", TRUE ~ "1"), # create classe label
#     classe_prob = apply(.[, 2:3], 1, max)) %>% # extract prediction proabilitiy
#   select(idg, classe_2cl, classe_prob) %>% 
#   rename("nid" = "idg")
# 
# # tomerge_race <- 
# #   paste0("/home/leo/ownCloud/ARSS/PlanQuinqua/BERT_Predictions/Prediction/", PRED)
# # 
# # race_pred <-
# #   read_csv(tomerge_race) %>%
# #   mutate(
# #     race_2cl = case_when(bertpred_pas_race > bertpred_race ~ "0", TRUE ~ "1"), # create race label
# #     race_prob = apply(.[, 2:3], 1, max)) %>% # extract prediction probabilitiy
# #   select(idg, race_2cl, race_prob) %>% 
# #   rename("nid" = "idg")
# 
# tomerge_inclusif <- 
#   paste0("/home/leo/ownCloud/ARSS/PlanQuinqua/BERT_Predictions/Prediction/", "Etienne_inclusif_provisoire_20240113.csv")
# 
# inclusif_pred <-
#   read_csv(tomerge_inclusif) %>%
#   mutate(
#     inclusif_2cl = case_when(bertpred_PasInclusif > bertpred_Inclusif ~ "0", TRUE ~ "1"), # create label
#     inclusif_prob = apply(.[, 2:3], 1, max)) %>% # extract prediction proabilitiy
#   select(idg, inclusif_2cl, inclusif_prob) %>% 
#   rename("nid" = "idg")
# 
# tomerge_genre_varstat <- 
#   paste0("/home/leo/ownCloud/ARSS/PlanQuinqua/BERT_Predictions/Prediction/", "varTRESrestrictive_20240113.csv")
# 
# genre_varstat_pred <-
#   read_csv(tomerge_genre_varstat) %>%
#   mutate(
#     varstat_2cl = case_when(bertpred_autre > bertpred_variable ~ "0", TRUE ~ "1"), # create label
#     varstat_prob = apply(.[, 2:3], 1, max)) %>% # extract prediction proabilitiy
#   select(idg, varstat_2cl, varstat_prob) %>% 
#   rename("nid" = "idg")
# 
# 
# ###Add all prediction files, and add rows belows to the pred_db file
# 
# pred_db <- all %>% 
#   left_join(genre_pred, by = "nid") %>%   
#   left_join(classe_pred, by = "nid") %>%   
#   left_join(inclusif_pred, by = "nid") %>%   
#   left_join(genre_varstat_pred, by = "nid")
# 
# #View(pred_db)
# hist(pred_db$genre_prob)
# hist(pred_db$classe_prob)
# hist(pred_db$inclusif_prob)
# hist(pred_db$varstat_prob)
# 
# pred_db$genre_2cl <- as.numeric(pred_db$genre_2cl)
# pred_db$classe_2cl <- as.numeric(pred_db$classe_2cl)
# pred_db$inclusif_2cl <- as.numeric(pred_db$inclusif_2cl)
# pred_db$varstat_prob <- as.numeric(pred_db$varstat_prob)
# 
# #pred_db$race_2cl <- as.numeric(pred_db$race_2cl)


#############################################################
####                  2.Detect Author's Gender          ####
#############################################################
#1. Extract First Names

# for pattern 1: Last Name1, First Name1 | Last Name2, First Name2

## define a function to extract first/last names
# extract_element <- function(x, i) {
#   if (length(x) >= i) {
#     return(x[[i]])
#   } else {
#     return(NA)
#   }
# }
# 
# auteur_1 <- pred_db %>%
#   filter(source != "OpenEdition") %>%
#   mutate(nom_complet = str_split(auteur, patter = " \\| ")) %>%
#   unnest(nom_complet) %>%
#   mutate(
#     prenom = sapply(str_split(nom_complet, pattern = ", "), extract_element, i = 2),
#     # keep first names for gender prediction
#     prenom = sapply(str_split(prenom, pattern = " "), extract_element, i = 1)
#   ) # remove letters (middle names) for better gender predictions

## Transform the first pattern into the second one, to keep consistency

names_list <-
  str_split(auteur_1$nom_complet, ", ") ### split the names

auteur_1 <- auteur_1 %>%
  mutate(
    nom_complet = paste(
      sapply(names_list, extract_element, 2),
      # extract given name
      sapply(names_list, extract_element, 1)
    ),
    # extract family name & transform into 'First Last' format
    nom_complet = ifelse(nom_complet == "NA NA", NA, nom_complet),
    nom_complet = gsub("NA ", "", nom_complet)
  ) # remove 'NA ', to keep the author name when it's not a person but an organization, for example

# for pattern 2: First Name1 Last Name1 et First Name2 Last Name2

## Multiple authors
auteur_2 <- pred_db %>%
  filter(source == "OpenEdition") %>% 
  slice(grep(" et ", auteur)) %>% # only keep the rows with multiple authors
  mutate(nom_complet = auteur) %>%
  separate_rows(nom_complet, sep = " et |, ") %>% # separate the last author, and then the former ones divided by comma
  group_by(nid) %>%
  mutate(prenom = sapply(strsplit(nom_complet, " "), "[", 1)) %>% # keeping the first names for gender prediction
  ungroup()

# Single author
auteur_3 <- pred_db %>%
  filter(source == "OpenEdition") %>%
  slice(-grep(" et ", auteur)) %>%
  mutate(nom_complet = auteur,
         prenom = sapply(strsplit(nom_complet, " "), "[", 1))

# merge the three datasets
auteur <- bind_rows(auteur_1, auteur_2, auteur_3)
View(auteur)

#2. Gender coding (1st name)


# Genre corpus Ollion ------------------

library(dplyr)
library(data.table) # Pour lire les fichiers
library(stringi) # Pour la manipulation des chaînes de caractères
names(all) <- make.unique(names(all))

# Chargement des données
insee_sexe <- fread("01_data_ollion/INSEE_genre.csv")
all <- fread("06_data_base/base_ollion_genre.csv")

insee_sexe <- insee_sexe %>% 
  group_by(sexe, preusuel) %>% 
  summarise(n = sum(nombre)) %>% # calculate the sum of each gender for each first name
  group_by(preusuel) %>%
  filter(n == max(n)) %>% # only keep the rows where the n is bigger when there're 2 values of gender for one first name
  drop_na(preusuel) # drop the NA as it's coded as female in the db


# Préparation de all avec une gestion explicite des NA pour prenom_auteur1
all <- all %>%
  mutate(
    prenom_auteur1_upper = if_else(is.na(prenom_auteur1) | prenom_auteur1 == "", NA_character_, toupper(prenom_auteur1)),
    prenom_auteur1_no_accents = if_else(is.na(prenom_auteur1_upper), NA_character_, stringi::stri_trans_general(prenom_auteur1_upper, "Latin-ASCII"))
  )

# Jointure pour déterminer le genre, avec gestion des NA
all <- all %>%
  left_join(insee_sexe, by = c("prenom_auteur1_no_accents" = "preusuel"))

# Conversion du code de sexe en genre textuel
all <- all %>%
  mutate(
    genre = case_when(
      sexe == 1 ~ "Homme",
      sexe == 2 ~ "Femme",
      TRUE ~ NA_character_
    )
  )


write.csv(all, "06_data_base/base_ollion_genre.csv", row.names = FALSE)

freq(all$genre)

## Autre 





all <- all %>%
  mutate(preusuel = toupper(prenom_auteur1), # create a all caps column for first names
         preusuel_no = stringi::stri_trans_general(preusuel, "Latin-ASCII")) %>% # remove all latin accents to improve accuracy
  left_join(insee_sexe %>% select(preusuel, sexe), by = "preusuel") %>% # add the gender label
  left_join(insee_sexe %>% select(preusuel, sexe), by = c("preusuel_no" = 'preusuel')) %>% 
  mutate(sexe = ifelse(is.na(sexe.x), sexe.y, sexe.x)) %>% 
  select(-c(sexe.x, sexe.y))

# make a list of distinctive first names
prenom_caps_NA <- auteur %>% 
  filter(is.na(sexe)) %>% 
  group_by(preusuel_no) %>% 
  summarise(n = n()) %>%
  filter(!str_detect(preusuel_no, "^[A-Za-z]\\.")) # removing names like "A."

# Préparation de all
all <- all %>%
  mutate(prenom_auteur1_upper = toupper(prenom_auteur1), # Convertir en majuscules
         prenom_auteur1_no_accents = stringi::stri_trans_general(prenom_auteur1_upper, "Latin-ASCII"), # Enlever les accents
         prenom_auteur1_no_accents = if_else(prenom_auteur1_no_accents == "", NA_character_, prenom_auteur1_no_accents)) # Mettre NA si vide

# Jointure pour déterminer le genre
all <- all %>%
  left_join(insee_sexe, by = c("prenom_auteur1_no_accents" = "preusuel"))

# Conversion du code de sexe en genre textuel avec gestion des NA
library(questionr)
all <- all %>%
  mutate(genre = case_when(
    sexe == 1 ~ "Homme",
    sexe == 2 ~ "Femme",
    TRUE ~ NA_character_ # Gère les cas où sexe est NA ou prenom_auteur1 est vide
  ))






# Use Genderize API
##Do it once only
# sexe_NA1 <- gender(prenom_caps_NA$preusuel_no[1:1000], method = "genderize")
# Use VPN to bypass API request limits
# sexe_NA2 <- gender(prenom_caps_NA$preusuel_no[1001:length(prenom_caps_NA$preusuel_no)], method = "genderize")

# sexe_NA <- bind_rows(sexe_NA1, sexe_NA2) %>%
# select(name, gender) %>%
# rename("preusuel_no" = "name",
# "sexe.NA" = "gender") %>%
# mutate(sexe.NA = case_when(sexe.NA == "male" ~ 1,
# sexe.NA == "female" ~ 2))













auteur <- auteur %>%
  left_join(read_csv("/home/leo/ownCloud/ARSS/PlanQuinqua/data/sexe_NA.csv"), by = "preusuel_no") %>% 
  mutate(sexe = ifelse(is.na(sexe), sexe.NA, sexe)) %>% 
  select(-sexe.NA)

# a list of uncoded first names
auteur %>% 
  filter(is.na(sexe),
         !is.na(prenom)) %>% 
  group_by(prenom) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

View(auteur)
#Prop per year
aggregate(as.character(auteur$sexe), by=list(auteur$annee), FUN=table)

# Gender ratio in authors over time
auteur %>% 
  group_by(annee, sexe) %>% 
  summarise(n = n()) %>% 
  group_by(annee) %>% 
  mutate(total = sum(n),
         p = n / total) %>% 
  mutate(sexe = case_when(sexe == 1 ~ "Homme",
                          sexe == 2 ~ "Femme",
                          TRUE ~ "NA")) %>% 
  ungroup() %>% 
  ggplot(aes(
    y = p,
    x = annee,
    col = sexe
  )) + 
  geom_line() +
  theme_minimal() + 
  labs(x = "\nAnnée",
       y = "Pourcentage\n",
       col = "Sexe") +
  scale_y_continuous(labels = percent_format()) +
  scale_color_brewer(palette = "Paired") + 
  theme(legend.position = "bottom")


## Graphique corpus en fonction ---------------

corpus_large <- fread("06_data_base/base_personnes_genre_v3.csv")
corpus_large %>%
  filter(!(journal %in% "La Croix"))  %>%
  ggplot() +
  aes(x = year, fill = genre) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Paired", direction = 1) +
  labs(
    x = "Année de parution",
    y = "Nombre d'articles",
    title = "Figure 7. Genre de l'auteur des articles du corpus général",
    caption = "Source : Corpus d'Etienne Ollion",
    fill = "Genre" ) +
  theme_minimal() +  # Thème plus jolie 
  theme(text = element_text(family = "Garamond"),
        plot.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(size = 8),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text( size = 10)) # Mettre le titre en gras



library(dplyr)
library(ggplot2)

all %>%
 filter(!(journal %in% "La Croix")) %>%
 filter(!is.na(genre)) %>%
 ggplot() +
 aes(x = year, fill = genre) +
 geom_bar(position = "dodge") +
 scale_fill_brewer(palette = "Paired", 
 direction = 1) +
 labs(x = "Année de parution", y = "Nombre d'articles", title = "Figure 7. Genre de l'auteur des articles du corpus général", 
 caption = "Source : Corpus d'Etienne Ollion", fill = "Genre") +
theme_minimal(base_size = 14) +  # Thème plus jolie 
  scale_x_discrete(breaks = seq(min(base$year), max(base$year), by = 5)) +
  theme(text = element_text(family = "Garamond"),
        plot.title = element_text(face = "bold", size = 13),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text( size = 10)) # Mettre le titre en gras



#############################################################
####                  3.Select journals                 ####
#############################################################
#Filter out journals with < 5 full years w/ abstracts
tab_publish <- as.data.frame(table(pred_db$annee, pred_db$revue))
colnames(tab_publish) <- c("annee", "revue", "nb")
#View(tab_publish)
vec <- names(which(table(tab_publish[tab_publish$nb !=0,]$revue)>5))

auteur <- auteur[auteur$revue %in% vec, ]

#Also remove selected journals
auteur <- auteur[!auteur$revue %in% c("Revue internationale d'intelligence économique", "Agone", "Dix-huitième siècle", "Dix-septième siècle", "A contrario"),]
auteur <- droplevels(auteur)

##Add label for selected journals
soc <- c("Actes de la recherche en sciences sociales", "L'Année sociologique", "Revue française de sociologie", "Sociologie", "Sociétés contemporaines", "Sociologie du travail")
select <- c("Archives de sciences sociales des religions", "Actes de la recherche en sciences sociales","Annales. Histoire, Sciences Sociales", "Ethnologie française","Genèses","Population", "Réseaux", "Revue française de science politique", "20 & 21. Revue d'histoire")

auteur$socio <- NA
auteur$socio[auteur$revue %in% soc] <- "socio"
auteur$select <- NA
auteur$select[auteur$revue %in% select] <- "select"



#############################################################
####                  4.Export                           ####
#############################################################
write.csv(auteur, "auteur_jan2024_clean&matched.csv")
articles <- auteur[!duplicated(auteur$nid),]
write.csv(articles, "articles_jan2024_clean.csv")


######################## OTHER #################################################
######Extract all tagged as Gender for 2nd classifier [Jan, 8th, 2024-01-08-tiggerbert-2023-05-29-abstracts-gastonEtienne_Genre4_20240108.csv]
# predicted_gender_only <- pred_db[pred_db$genre_2cl==1,]
# View(predicted_gender_only)
# write.csv(predicted_gender_only, "/home/leo/ownCloud/ARSS/PlanQuinqua/BERT_Predictions/Prediction/SubBaseGenre8jan24.csv")
# 

