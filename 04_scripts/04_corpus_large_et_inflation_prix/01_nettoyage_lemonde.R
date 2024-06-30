library(readr)
LeMonde <- read_csv("02_data_travail/lemonde_travail.csv")

LeMonde$auteurs <- LeMonde$auteur
LeMonde$auteur <- NULL
LeMonde$auteurs <- gsub("Par","",LeMonde$auteurs)
LeMonde$auteurs <- gsub("^Propos Recueillis Par","",LeMonde$auteurs)

# Enlever les espaces au début 
LeMonde$auteurs <- str_trim(LeMonde$auteurs)
# Majuscule au début 
LeMonde$auteurs <- str_to_title(LeMonde$auteurs)

View(LeMonde1)


LeMonde <- fread("02_data_travail/lemonde_travail_vf.csv")
### Diviser les auteurs -----------------
LeMonde1 <- LeMonde
LeMonde1$aut <- LeMonde1$auteurs
LeMonde1  <- separate(LeMonde1, aut, sep="(?i)\\bet\\b|,|\\bavec\\b|\\|", into=c("auteur1", "auteur2")) 
LeMonde1$auteurs <- gsub("Par","",LeMonde1$auteurs)
# Enlever les espaces au début 
LeMonde1$auteurs <- str_trim(LeMonde1$auteurs)
LeMonde1$auteur1 <- str_trim(LeMonde1$auteur1)
LeMonde1$auteur2 <- str_trim(LeMonde1$auteur2)


LeMonde1 <- LeMonde1 %>%
  select(
    ...1,
    journal,
    date,
    annee,
    auteurs,
    auteur1, 
    auteur2,
    titre,
    soustitre,
    chapo,
    texte,
    edition,
    section,
    type_article,
    websection,
    source,
    rubrique,
    nb_mots,
    doc_geo,
    doc_pphysique,
    doc_pmorale,
    doc_oldindexation,
    is_article,
    file, 
    motscle
  )
View(LaCroix)

write.csv(LeMonde, "02_data_travail/lemonde_travail_vf_3.csv", row.names = FALSE)

## Nettoyage 2 ----


LeMonde <- fread("02_data_travail/lemonde_travail_vf_2.csv")
library(stringr)

LeMonde$inflation <- str_detect(LeMonde$texte, "\\binflation?\\b") 
View(LeMonde)

# LeMonde$inflation <- as.character(LeMonde$inflation)
# LeMonde_inflation <- LeMonde %>% filter(inflation == "TRUE")

str(LeMonde)






