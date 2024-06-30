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

