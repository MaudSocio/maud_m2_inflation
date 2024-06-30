

# Fréquence d'un mot dans le temps
library(data.table)
library(here)
library(stringr)
library(ggplot2)
library(esquisse)
article_raw <- fread(here("data/articles_clean2.csv"))

# Ukraine et COVID -------------------
article_raw$ukraine <- str_detect(article_raw$texte_titre, "Ukraine")

article_raw$ukraine <- as.numeric(article_raw$ukraine)  
ar_u <- subset(article_raw, article_raw$ukraine == TRUE)


# Agrégation par date
occurrences_par_date <- article_raw %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2024-12-31")) %>%
  group_by(date_hebdo) %>%
  summarise(occurrences = sum(ukraine, na.rm = TRUE)) %>%
  arrange(date_hebdo)


# Création de la courbe reliant les points
ggplot(occurrences_par_date, aes(x = date_hebdo, y = occurrences)) +
  geom_line(color = "red") +  # Relie les points avec une ligne rose
  theme_minimal() +
  theme(text = element_text(family = "Garamond"),  # Typographie Garamond
        plot.title = element_text(face = "bold")) +  # Titre en gras
  labs(title = "Figure 13. Occurrences du mot Ukraine' dans le texte par date",
       x = "Date",
       y = "Nombre d'occurrences")



# Création de la courbe reliant les points de manière lisse avec typographie Garamond









library(ggplot2)
library(dplyr)
library(stringr)

# Supposons que votre DataFrame s'appelle article_raw et qu'il a déjà une colonne 'date'
# Marquage des occurrences de "Ukraine" et "Covid" dans les titres
article_raw$ukraine <- str_detect(article_raw$texte_titre, "Ukraine")
article_raw$covid <- str_detect(article_raw$texte_titre, "Covid")

# Agrégation par date
occurrences_par_date <- article_raw %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2024-12-31")) %>%
  group_by(date_hebdo) %>%
  summarise(occurrences_ukraine = sum(ukraine, na.rm = TRUE),
            occurrences_covid = sum(covid, na.rm = TRUE)) %>%
  arrange(date_hebdo)

# Création de la courbe reliant les points pour Ukraine
ggplot(occurrences_par_date, aes(x = date_hebdo)) +
  geom_line(aes(y = occurrences_ukraine), color = "red", size = 1)  +
  # Ajoute une courbe et des points pour Covid
  geom_line(aes(y = occurrences_covid), color = "blue", size = 1) +
  theme_minimal() +
  theme(text = element_text(family = "Garamond"),  # Typographie Garamond
        plot.title = element_text(face = "bold")) +  # Titre en gras
  labs(title = "Occurrences des mots 'Ukraine' et 'Covid' dans les titres par date",
       x = "Date",
       y = "Nombre d'occurrences",
       color = "Mot")


## Historiogramme 

ggplot(ar_u) +
 aes(x = date, fill = journal) +
 geom_histogram(bins = 57L) +
 scale_fill_hue(direction = 1) +
 labs(title = "Figure. Article mentionnant le pays \"Ukraine\" de 2022 à 2023") +
 theme_minimal()

ggplot(ar_u) +
 aes(x = date, fill = journal) +
 geom_histogram(bins = 57L, position = "fill") +
 scale_fill_hue(direction = 1) +
 labs(title = "Figure. Articles mentionnant le pays \"Ukraine\" de 2022 à 2023 en fonction du journal") +
 theme_minimal()


## Evolution des Ukraines ---------------------
ggplot(ar_u) +
 aes(x = date) +
 geom_histogram(bins = 57L, fill = "#112446") +
 labs(x = "Année", y = "Nombre d'articles", 
 title = "Figure. Evolution du nombre d'articles mentionnant le pays \"Ukraine\" en fonction de l'année", 
 subtitle = "Données du corpus restreint de de 2022 à 2023") +
 theme_minimal()


## Regarder les récurrences de mots 
library(tidyverse)
library(rvest)
library(xml2)


searchMots <- function(data, varTime = "date", mots, period = "month", xlim = NULL, ylim = 0:1) {
  
  
  data <- data %>% mutate(date_break = cut(get(varTime), breaks = period, ordered_result = F),
                          date_break = as.Date(date_break))
  
  data_mots <- unique(data$date_break) %>% sort %>% data.frame(date_break = .)
  data_mots$n_articles <- table(data$date_break)
  
  for (i in 1:length(mots)) {
    data[, paste0("n_citations", i)] <- grepl(pattern = mots[i], x = data$texte_clean)
    data_mots <- cbind(data_mots, tapply(data[, paste0("n_citations", i)], data$date_break, sum))
    colnames(data_mots)[i + 2] <- paste0("n_citations", i)
  }
  
  
  data_mots_long <- data_mots %>%
    pivot_longer(cols = starts_with("n_citations"), values_to = "freq", names_to = "index_word", names_prefix = "n_citations") %>%
    mutate(freq = freq/n_articles)
  
  data_mots_long$mot <- sapply(data_mots_long$index_word, function(x) {mots[as.numeric(x)]})
  
  plot <- ggplot(data_mots_long) +
    aes(x = date_break, y = freq, color = mot) +
    geom_line(linewidth = 0.8) +
    scale_y_continuous(name = "Proportion d'articles mentionnant le mot correspondant", 
                       breaks = 0:10*(0.1), limits = ylim) +
    scale_x_date(breaks = x_breaks, labels = x_breaks %>% format("%b %Y"), limits = xlim) +
    theme_minimal() +
    theme(axis.title.y       = element_text(size = 13, face = "bold"),
          plot.title         = element_text(size = 16, face = "bold")) +
    ggtitle("Proportion d'articles mentionnant le mot correspondant au cours du temps") +
    xlab("")
  plot
  
  return(plot)
}

tt <- article_raw


searchMots(data = tt, period = "month",
           mots = "inflation")

searchMots(data = tt, mots = "énergie", period = "3 months")

searchMots(data = tt, mots = "spectre", period = "3 months")

searchMots(data = tt, period = "3 months",
           mots = c("Ukrain|ukrain", "énerg", "carbur", 
                    "import", "russ|Russ", "gaz", "électri", "conflit|guerre"))

searchMots(data = tt, period = "3 months",
           mots = c("crise", "Chin", "sanit", "retard", "repris", 
                    "covid|Covid|corona", "confin"))


searchMots(data = tt, period = "6 months",
           mots = c("réform", "retrait", "consomm", "course", "ménage"))

searchMots(data = tt, period = "3 months",
           mots = c("banque", "réform", "monétair|monnaie", 
                    "échang", "ménage", "entrepris|firm", "smic|Smic|SMIC"))


searchMots(data = tt, period = "quarter",
           mots = c("Ukraine", "production", "énergie", "consommation", "ménage", "courses", "baisse", "accélère"))



