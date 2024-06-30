# Nettoyage des quatre bases -------------


library(questionr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(here)


nb_articles_per_year <- fread("01_data_ollion/nb_articles_per_year.csv")
str(nb_articles_per_year )
str(base_ollion)

# BASE 1 ------ Génération de la base ---------------------


LeMonde <- fread(here("02_data_travail/lemonde_travail_vf_3.csv"))
lemonde <- subset(LeMonde, LeMonde$date < as.Date("2022-04-29")

                  

lacroix <- fread(here("02_data_travail/lacroix_travail_vf_3.csv"))
LaCroix <- fread(here("01_data_ollion/LaCroix2.csv"))

Liberation <- fread(here("02_data_travail/liberation_travail_vf_2.csv"))
liberation <- subset(Liberation, Liberation$date < as.Date("2022-05-16"))



LeFigaro <- fread(here("02_data_travail/lefigaro_travail_vf_2.csv"))
LeFigaro$date <- as.Date(LeFigaro$date)

lefigaro <- subset(LeFigaro, LeFigaro$date < as.Date("2021-04-24")) # Change rien 


# J'avais pas fait l'ajout sur la base complete pour Libération ? 

LesEchos <- fread(here("02_data_travail/lesechos_travail_vf.csv"))
LesEchos$date <- as.Date(LesEchos$date)
lesechos <- subset(LesEchos, LesEchos$date < as.Date("2019-02-16"))


## Format date

class(lemonde$annee)
lacroix$date <- as.Date(lacroix$date)
lemonde$date <- as.Date(lemonde$date)
lefigaro$date <- as.Date(lefigaro$date)
liberation$date <- as.Date(liberation$date)
lesechos$date <- as.Date(lesechos$date)


lacroix$annee <- as.Date(lacroix$annee)
lemonde$annee <- as.Date(lemonde$annee)
lefigaro$annee <- as.Date(lefigaro$annee)
liberation$annee <- as.Date(liberation$annee)
lesechos$annee <- as.Date(lesechos$annee)


LesEchos$journal <- "Les Echos"

base_complete <- rbind(lemonde, lefigaro, liberation, lesechos, lacroix)




write.csv(base_complete, here("01_data_ollion/base_generale_ollion.csv"))


## Nettoyage BASE 1 

base_complete <- base_ollion

base_complete$nb_mots <- str_count(base_complete$texte, ' ')

base_complete <- subset(base_complete, base_complete$nb_mots < 9999)
base_complete  <- subset(base_complete, base_complete$nb_mots > 199 )







# Liste des noms de pays à filtrer
pays_a_filtrer <- c("Zimbabwe", "Venezuela", "Argentine", "Allemagne", "Iran", "Brésil", 
                    "Turquie", "Russie", "Hongrie", "Nicaragua", "Angola", "Bolivie", 
                    "Congo", "Équateur", "Ghana", "Mexique", "Indonésie", 
                    "Nigéria","Etats-Unis","États-Unis")
# J'ai choisi de garder l'Ukraine : "Ukraine"



# Filtrage des lignes basé sur les pays à filtrer
base_filtre <- base_complete[!grepl(paste(pays_a_filtrer, collapse = "|"), base_complete$titre, ignore.case = TRUE), ]


base_filtre <- subset(base_filtre, base_filtre$year != "2023")



write.csv(base_filtre, here("06_data_base/base_generale_ollion_net.csv"))


# Ajout des nombres d'articles en fonction de l'année -----------

# Chargement du package dplyr
library(dplyr)

# Votre dataframe base_ollion semble avoir 'year' comme chr (caractère), il faudrait le convertir en int pour correspondre au type dans nb_articles_per_year
base_ollion$year <- as.integer(base_ollion$year)

# Jointure des dataframes sur les colonnes 'year' et 'journal' (correspondant à 'newspaper')
base_ollion <- base_ollion %>%
  left_join(nb_articles_per_year, by = c("journal" = "newspaper", "year" = "year")) %>%
  # Renommage de la colonne n_articles en nb_article_total
  mutate(nb_article_total = n_articles) %>%
  # Vous pouvez choisir de retirer la colonne n_articles si vous ne voulez pas la conserver
  select(-n_articles)

# Note : Cette opération présume que 'journal' dans base_ollion correspond exactement à 'newspaper' dans nb_articles_per_year
# et que la conversion de 'year' de chr à int ne cause pas de problèmes de correspondance


base <- base %>%
  group_by(year, journal) %>%
  mutate(rec_politique_y = sum(politique, na.rm = TRUE)) %>%
  ungroup()

# Pour calculer prop_politique_year, assurez-vous d'avoir déjà joint nb_article_total comme montré précédemment
base <- base %>%
  mutate(prop_politique_year = (rec_politique_y / nb_article_total) * 100)

### Base 2 --------------
base <- fread(here("02_data_travail/base_complete_travail_vf_5.csv"))

base$inflation <- str_detect(base$texte, "\\binflation(s)?\\b") 
base$prix <- str_detect(base$texte, "prix") 

base_2 <- subset(base, base$inflation == TRUE & base$prix == TRUE )

pays_a_filtrer <- c("Zimbabwe", "Venezuela", "Argentine", "Allemagne", "Iran", "Brésil", 
                    "Turquie", "Russie", "Hongrie", "Nicaragua", "Angola", "Bolivie", 
                    "Congo", "Équateur", "Ghana", "Mexique", "Indonésie", 
                    "Nigéria","Etats-Unis","États-Unis")

base_3 <- base_2[!grepl(paste(pays_a_filtrer, collapse = "|"), base_2$titre, ignore.case = TRUE), ]

base_3 <- subset(base_3, base_3$nb_mots < 9999)
base_3 <- subset(base_3, base_3$nb_mots > 199 )

write.csv(base_3, here("06_data_base/base_inflation_prix.csv"))


## Base inflation --------------


base$inflation <- str_detect(base$texte, "\\binflation(s)?\\b") 
base_inflation <- subset(base, base$inflation == TRUE)

# Jointure des dataframes sur les colonnes 'year' et 'journal' (correspondant à 'newspaper')
base_inflation <- base_inflation %>%
  left_join(nb_articles_per_year, by = c("journal" = "newspaper", "year" = "year")) %>%
  # Renommage de la colonne n_articles en nb_article_total
  mutate(nb_article_total = n_articles) %>%
  # Vous pouvez choisir de retirer la colonne n_articles si vous ne voulez pas la conserver
  select(-n_articles)

write.csv(base_inflation, here("06_data_base/base_inflation_ollion.csv"))


# Jointure des dataframes sur les colonnes 'year' et 'journal' (correspondant à 'newspaper')
base_ollion <- base_ollion %>%
  left_join(nb_articles_per_year, by = c("journal" = "newspaper", "year" = "year")) %>%
  # Renommage de la colonne n_articles en nb_article_total
  mutate(nb_article_total = n_articles) %>%
  # Vous pouvez choisir de retirer la colonne n_articles si vous ne voulez pas la conserver
  select(-n_articles)

nb_articles_per_year <- nb_articles_per_year %>%
  mutate(newspaper = recode(newspaper, "Libération" = "Liberation"))

freq(nb_articles_per_year$newspaper)


## Base personnes -----------------
base_personnes <- fread("02_data_travail/base_personnes.csv")

# Filtrage des données avec des conditions spécifiques pour chaque journal
base_personnes <- base_personnes %>%
  filter(
    (journal == "Le Monde" & date < as.Date("2022-04-29")) |
      (journal == "Liberation" & date < as.Date("2022-05-16")) |
      (journal == "Le Figaro" & date < as.Date("2021-04-24")) |
      (journal == "Les Echos" & date < as.Date("2019-02-16")) |
        (journal == "La Croix")
  ) %>%
  # Exclure certains journaux
  filter(
    !journal %in% c("Ouest-France", "La Tribune", "Le Parisien")
  )

pays_a_filtrer <- c("Zimbabwe", "Venezuela", "Argentine", "Allemagne", "Iran", "Brésil", 
                    "Turquie", "Russie", "Hongrie", "Nicaragua", "Angola", "Bolivie", 
                    "Congo", "Équateur", "Ghana", "Mexique", "Indonésie", 
                    "Nigéria","Etats-Unis","États-Unis")

base_personnes <- base_personnes[!grepl(paste(pays_a_filtrer, collapse = "|"), base_personnes$titre, ignore.case = TRUE), ]

base_personnes <- subset(base_personnes, base_personnes$nb_mots < 9999)
base_personnes <- subset(base_personnes, base_personnes$nb_mots > 199 )
base_personnes <- subset(base_personnes, base_personnes$year != "2023")

write.csv(base_personnes, "06_data_base/base_personnes_v2.csv", row.names = FALSE)

base_joined <- left_join(base, base_personnes %>% select(titre, personnes), by = "titre")

base_sans_correspondance <- anti_join(base, base_personnes, by = "titre")
