
# Mise en page des résultats de la classification du corpus inflation-titre ------------------------------------

library(data.table)
library(here)
library(tidyverse)
library(knitr)
library(questionr)
library(gt)
library(gtsummary)
library(labelled)
library(R.temis)
# library(wildeR)



corpus <- read_rds(file = "02_data/01_corpus_inflation_titre/cut_corpus_Articles_clean.rds")
article_raw <- fread(here("02_data/01_corpus_inflation_titre/articles_clean2.csv"))
articles_clean <- read_rds(here("02_data/01_corpus_inflation_titre/articles_clean2.rds"))
figures <- read_rds(here("02_data/01_corpus_inflation_titre/figures_redaction.rds"))
dtm <- read_rds(here("02_data/01_corpus_inflation_titre/cut_dtm_Articles_clean.rds"))


# 1. Description corpus -----------

## 1.1 Par Journal -----------------

tableau_1 <- article_raw %>%
  tbl_summary(include = journal,
              percent = "column",
              # statistic = all_categorical() ~ "{n}",
              missing = "no") %>%
  bold_labels()  %>%
  modify_header(label ="", 
                list(all_stat_cols(stat_0 = FALSE) ~ "n={N}",
                     stat_0 ~ "**Effectif total \n N = {N}**")) %>%
  modify_caption("**Tableau 1. Répartition du corpus par journaux**")   %>%
  modify_footnote(update = stat_0  ~ 
                    "**Note de lecture** : Sur 3640 articles, 47 ont été publié par le journal *l'Humanité*, ce qui représente 1,3% du corpus.\n
                               **Champ** : Articles ayant pour titre l'inflation de 2020 à 2023 \n
                              **Source** : Corpus restreint, Données d'Europressen")  %>%
  gtsummary::modify_table_body(~.[-1, ]) %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Garamond")

## 1.2 Par Annee ------------
library(md)
library(gtsummary)
library(gt)
library(dplyr)

tableau_2 <- article_raw %>%
  tbl_summary(include = date_year,
              percent = "column",
              missing = "no") %>%
  bold_labels()  %>%
  modify_header(label ="", 
                list(all_stat_cols(stat_0 = FALSE) ~ "n={N}",
                     stat_0 ~ "**Effectif total \n N = {N}**")) %>%
  modify_footnote(update = stat_0  ~ 
                    "**Note de lecture** : Sur 3640 articles, 47 ont été publié par le journal *l'Humanité*, ce qui représente 1,3% du corpus.\n
                               **Champ** : Articles ayant pour titre l'inflation de 2020 à 2023 \n
                              **Source** : Corpus restreint, Données d'Europressen")  %>%
  gtsummary::modify_table_body(~.[-1, ]) %>%
  as_gt() %>%
  gt::tab_header(
    title = "Figure 10. Répartition du corpus par année",
    subtitle = "Analyse du corpus restreint réalisé avec Baptiste Yzern") %>%
  gt::tab_options(table.font.names = "Garamond")

## 2. Graphique - Répartition paragraphe par année----- 

# Load necessary libraries
library(gt)
library(dplyr)
library(md)
library(purrr)

# Assuming figures$rai_cut_meta$date_semestre and figures$rai_cut_meta$cluster_10 are already defined
classes_date <- table(figures$rai_cut_meta$date_semestre, 
                      figures$rai_cut_meta$cluster_10 %>% 
                        factor(x = ., labels = sapply(rainette::rainette_stats(., dtm = dtmQ), function(x) paste0(x[1,1], "-", x[2,1])))) %>%
  lprop() %>% as.data.frame.matrix()

rownames(classes_date) <- c("S1 2020", "S2 2020", "S1 2021", "S2 2021", "S1 2022", "S2 2022", "S1 2023", "S2 2023", "Ensemble")

# Create the gt table with custom styling
gt_table <- classes_date %>%
  gt(rownames_to_stub = TRUE, locale = "fr")  %>% 
  tab_header(title = md("**Figure 18. Répartition des paragraphes publiés par semestre selon la classe affectée par la classification Reinert**")) %>%
  fmt_percent(columns = 2:11, decimals = 1, scale_values = FALSE, incl_space = TRUE) %>%
  fmt_percent(columns = 12, decimals = 0, scale_values = FALSE, incl_space = TRUE) %>%
  tab_footnote(md("**Note de lecture** : 9,1% des paragraphes de l'ensemble des articles publiés au premier semestre 2020 appartiennent à la classe 1 (acheter-petit), c'est-à-dire à la classe constituée des paragraphes qui traitent le sujet du pouvoir d'achat des consommateurs.
                  \n**Source** : Corpus inflation-titre réalisé avec Baptiste Yzern.")) %>% 
  tab_options(table.width = pct(100)) %>%
  cols_width(1 ~ pct(10))  %>%
  tab_style(
    style = cell_text(font = "Garamond"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(font = "Garamond"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(font = "Garamond"),
    locations = cells_stub()
  ) %>%
  tab_style(
    style = cell_text(font = "Garamond", weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(font = "Garamond"),
    locations = cells_footnotes()
  )

# Exporter le tableau en LaTeX
gtsave(gt_table, filename = "maud/table.tex", as = "latex")



## Journaux 
articles_raw$journal_rec <- case_when(
  str_detect(string = articles_raw$journal, pattern = "Ouest-France")           ~ "Ouest-France",
  str_detect(string = articles_raw$journal, pattern = "Le Parisien")            ~ "Le Parisien",
  str_detect(string = articles_raw$journal, pattern = "La Dépêche du Midi")     ~ "La Dépêche du Midi",
  str_detect(string = articles_raw$journal, pattern = "L'Est Républicain")      ~ "L'Est Républicain",
  str_detect(string = articles_raw$journal, pattern = "Aujourd'hui en France")  ~ "Aujourd'hui en France",
  str_detect(string = articles_raw$journal, pattern = "La Tribune")             ~ "La Tribune",
  str_detect(string = articles_raw$journal, pattern = "l'Humanité")             ~ "L'Humanité",
  TRUE ~ articles_raw$journal)
labelled::var_label(articles_raw$journal_rec) <- "Journal"

articles_clean$journal_rec <- articles_clean$journal
labelled::var_label(articles_clean$journal_rec) <- "Journal"

# Classification REINERT ---------------
library(tidyverse)
library(rvest)
library(xml2)
library(rainette)
library(shiny)
library(quanteda)
library(here)

dtm <- read_rds(here("data/cut_dtm_Articles_clean.rds"))


# dfm <- as.dfm(dtm)
# 
# resrai <- rainette(dfm, k = 10)
# 
# 
# n_terms <- rowSums(dtm %>% as.matrix())
# dtmQ <- dtm[n_terms > 9, ] %>% quanteda::as.dfm()
# res <- rainette::rainette_plot(figures$rai_cut_res, dtm = dtmQ, k = 10)
# 
# figures$rai_cut_res %>% rainette_plot(dfm, k = 10)
# 
# meta_cut <- NLP::meta(corpus)
# meta_cut$cluster_2 <-  cutree_rainette(resrai, k = 2)  %>% as.factor()
# meta_cut$cluster_3 <-  cutree_rainette(resrai, k = 3)  %>% as.factor()
# meta_cut$cluster_4 <-  cutree_rainette(resrai, k = 4)  %>% as.factor()
# meta_cut$cluster_5 <-  cutree_rainette(resrai, k = 5)  %>% as.factor()
# meta_cut$cluster_6 <-  cutree_rainette(resrai, k = 6)  %>% as.factor()
# meta_cut$cluster_7 <-  cutree_rainette(resrai, k = 7)  %>% as.factor()
# meta_cut$cluster_8 <-  cutree_rainette(resrai, k = 8)  %>% as.factor()
# meta_cut$cluster_9 <-  cutree_rainette(resrai, k = 9)  %>% as.factor()
# meta_cut$cluster_10 <- cutree_rainette(resrai, k = 10) %>% as.factor()
# 
# figures$rai_cut_meta <-   meta_cut




figures <- read_rds(here("data/figures_redaction.rds"))


# Calcul du tableau de répartition
result_table <- table(
  figures$rai_cut_meta$journal, 
  figures$rai_cut_meta$cluster_10 %>% 
    factor(x = ., labels = sapply(rainette::rainette_stats(., dtm = dtmQ), function(x) paste0(x[1,1], "-", x[2,1])))
) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame.matrix()



df <- figures$rai_cut_meta %>% 
  group_by(html) %>% 
  summarise(
    n_para = n(),
    n_para_1 = sum(cluster_10 == 1),
    n_para_2 = sum(cluster_10 == 2),
    n_para_3 = sum(cluster_10 == 3),
    n_para_4 = sum(cluster_10 == 4),
    n_para_5 = sum(cluster_10 == 5),
    n_para_6 = sum(cluster_10 == 6),
    n_para_7 = sum(cluster_10 == 7),
    n_para_8 = sum(cluster_10 == 8),
    n_para_9 = sum(cluster_10 == 9),
    n_para_10 = sum(cluster_10 == 10),
  ) %>% 
  ungroup() %>% 
  mutate(
    main_cluster = case_when(
      n_para_1 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 1,
      n_para_2 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 2,
      n_para_3 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 3,
      n_para_4 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 4,
      n_para_5 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 5,
      n_para_6 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 6,
      n_para_7 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 7,
      n_para_8 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 8,
      n_para_9 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 9,
      n_para_10 == pmax(n_para_1, n_para_2, n_para_3, n_para_4, n_para_5, n_para_6, n_para_7, n_para_8, n_para_9, n_para_10 ) ~ 10,
    )
    
  )

## Recodage de df$main_cluster en df$main_cluster_rec
df$main_cluster_rec <- df$main_cluster %>%
  as.character() %>%
  fct_recode(
    "Groupe 1" = "1",
    "Groupe 1" = "2",
    "Groupe 1" = "3",
    "Groupe 1" = "4",
    "Groupe 1" = "5",
    "Groupe 2" = "6",
    "Groupe 2" = "7",
    "Groupe 2" = "8",
    "Groupe 2" = "9",
    "Groupe 2" = "10"
  )
library(questionr)
freq(df$main_cluster_rec)


## Regrouper les données ----------
library(esquisse)
library(ggplot2)

dr <- left_join(articles_clean, df$main_cluster)



articles_clean_2 <- merge(articles_clean, df[, c("html", "main_cluster", "main_cluster_rec")], by = "html", all.x = TRUE)

articles_clean_2$main_cluster <- as.factor(articles_clean_2$main_cluster)






df_2 <- merge(df, articles_clean[, c("html", "journal")], by = "html", all.x = TRUE)

df_2$main_cluster <- as.factor(df_2$main_cluster)

library(questionr)
freq(df$main_cluster)

## Créer des sous-catégories d'articles ----------

articles_clean_3 <- subset(articles_clean_2,main_cluster == "4")

library(stringr)
articles_clean_3$bouclier <- str_detect(articles_clean_3$texte_titre, "bouclier")
freq(articles_clean_3$bouclier )


## Analyse de la classe 5 -----

articles_clean_5 <- subset(articles_clean_2,main_cluster == "5")
articles_clean_6 <- subset(articles_clean_2,main_cluster == "6")
## Prouver que ce sont les journaux locaux qui amènent cette thématique --------------------

# Charger les packages nécessaires
library(dplyr)
library(ggplot2)
library(lubridate)

# Supposons que articles_clean_2 soit déjà chargé dans votre environnement

# 1. Extraire le mois de la date
articles_clean_2 <- articles_clean_2 %>%
  mutate(month = floor_date(ymd(date), "month"))

# 2. Filtrer les données pour conserver uniquement celles du “Groupe 1”
groupe1_articles <- articles_clean_2 %>%
  filter(main_cluster_rec == "Groupe 1")

# 3. Agréger les données par journal et par mois
monthly_counts <- groupe1_articles %>%
  group_by(journal, month) %>%
  summarise(article_count = n(), .groups = 'drop')

# 4. Créer le graphique avec ggplot2
ggplot(monthly_counts, aes(x = month, y = article_count, color = journal)) +
  geom_line() +
  labs(title = "Nombre d'articles avec une vision par le bas par mois et par journal",
       x = "Mois",
       y = "Nombre d'articles") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) %>% 
  as_gt() 



# Installer et charger les packages nécessaires

library(showtext)
library(dplyr)
library(ggplot2)
library(lubridate)

# Charger la police Garamond
font_add("Garamond", regular = "GARA.TTF") # Assurez-vous que le fichier de la police Garamond est disponible dans votre répertoire de travail
showtext_auto()

# Supposons que articles_clean_2 soit déjà chargé dans votre environnement

# 1. Définir les journaux d'intérêt
journaux_interet <- c("Aujourd’hui en France", "L’Est républicain", "La Dépêche du Midi", "Le Parisien", "Ouest-France")

# 2. Extraire le mois de la date
articles_clean_2 <- articles_clean_2 %>%
  mutate(month = floor_date(ymd(date), "month"))

# 3. Filtrer les données pour conserver uniquement celles du “Groupe 1”
groupe1_articles <- articles_clean_2 %>%
  filter(main_cluster_rec == "Groupe 1")

# 4. Mettre les journaux en facteur et définir l'ordre
groupe1_articles <- groupe1_articles %>%
  mutate(journal = factor(journal, levels = c(journaux_interet, setdiff(unique(journal), journaux_interet))))

# 5. Agréger les données par journal et par mois
monthly_counts <- groupe1_articles %>%
  group_by(journal, month) %>%
  summarise(article_count = n(), .groups = 'drop')

# 6. Créer le graphique avec ggplot2
ggplot(monthly_counts, aes(x = month, y = article_count, color = journal)) +
  geom_line(size = 1) +
  labs(title = "Nombre d'articles avec une vision par le bas par mois et par journal",
       x = "Mois",
       y = "Nombre d'articles",
       color = "Journal") +
  theme_minimal(base_family = "Garamond") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = "Garamond"), 
        plot.title = element_text(face = "bold", family = "Garamond"))


# 2. Graphique haut et bas en fonction de l'analyse lexicale -------------------------
# Installer et charger les packages nécessaires
if (!require("showtext")) install.packages("showtext", dependencies = TRUE)
library(showtext)
library(dplyr)
library(ggplot2)
library(lubridate)

# Charger la police Garamond
font_add("Garamond", regular = "GARA.TTF") # Assurez-vous que le fichier de la police Garamond est disponible dans votre répertoire de travail
showtext_auto()

# Supposons que articles_clean_2 soit déjà chargé dans votre environnement

# 1. Extraire le mois de la date
articles_clean_2 <- articles_clean_2 %>%
  mutate(month = floor_date(ymd(date), "month"))

articles_clean_2$date <- as.Date(articles_clean_2$date, format = "%d/%m/%Y")

articles_clean_2$date <- as.Date(articles_clean_2$date, format = "%d/%m/%Y")

# Vérifiez que la conversion a été effectuée correctement
str(articles_clean_2$date)

# Filtrer les données après le 1er novembre 2023
articles_clean_3 <- subset(articles_clean_2, date < as.Date("2023-11-30"))

# 2. Filtrer les données pour conserver uniquement celles des “Groupe 1” et “Groupe 2”
groupes_articles <- articles_clean_3 %>%
  filter(main_cluster_rec %in% c("Groupe 1", "Groupe 2")) %>%
  mutate(main_cluster_rec = recode(main_cluster_rec, 
                                   "Groupe 1" = "Vision par le bas", 
                                   "Groupe 2" = "Vision par le haut"))

# 3. Agréger les données par groupe et par mois
monthly_counts <- groupes_articles %>%
  group_by(main_cluster_rec, month) %>%
  summarise(article_count = n(), .groups = 'drop')

# 4. Créer le graphique avec ggplot2
ggplot(monthly_counts, aes(x = month, y = article_count, color = main_cluster_rec, linetype = main_cluster_rec)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Vision par le bas" = "#542788", "Vision par le haut" = "#fdb863")) +
  scale_linetype_manual(values = c("Vision par le bas" = "dashed", "Vision par le haut" = "solid")) +
  labs(title = "Nombre d'articles par mois pour les visions 'par le bas' et 'par le haut'",
       x = "Mois",
       y = "Nombre d'articles",
       color = "Vision",
       linetype = "Vision") +
  theme_minimal(base_family = "Garamond") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Garamond"),
        text = element_text(family = "Garamond"),
        plot.title = element_text(face = "bold", family = "Garamond"),
        legend.title = element_text(face = "bold", family = "Garamond"))





# Conversion en pourcentage et affichage
gt(result_table) %>%
  tab_header(title = md("**Répartition des paragraphes publiés par journal selon la classe affectée par la classification Reinert**")) %>%
  fmt_percent(columns = everything(), decimals = 1, scale_values = TRUE, incl_space = TRUE) %>%
  tab_footnote(md("**Note de lecture** : 29,3% des paragraphes de l'ensemble des articles publiés par le journal *Aujourd'hui en France* appartiennent à la classe 1 (acheter-petit), c'est-à-dire à la classe constituée des paragraphes qui traitent le sujet du pouvoir d'achat des consommateurs.")) %>% 
  tab_options(table.width = pct(100)) %>%
  cols_width(1 ~ pct(10)) %>%
  tab_footnote(md("**Source** : corpus Europresse constitué par nos soins."))


classes_date <- table(figures$rai_cut_meta$date_semestre, 
                      figures$rai_cut_meta$cluster_10 %>% 
                        factor(x = ., labels = sapply(rainette::rainette_stats(., dtm = dtmQ), function(x) paste0(x[1,1], "-", x[2,1])))) %>%
  lprop() %>% as.data.frame.matrix() 
rownames(classes_date) <- c("S1 2020", "S2 2020", "S1 2021", "S2 2021", "S1 2022", "S2 2022", "S1 2023", "S2 2023", "Ensemble")

classes_date %>% 
  gt(rownames_to_stub = T, locale = "fr")  %>% 
  tab_header(title = md("**Répartition des paragraphes publiés par semestre selon la classe affectée par la classification Reinert**")) %>%
  fmt_percent(columns = 2:11, decimals = 1, scale_values = F, incl_space = T) %>%
  fmt_percent(columns = 12, decimals = 0, scale_values = F, incl_space = T) %>%
  tab_footnote(md("**Note de lecture** : 9,1% des paragraphes de l'ensemble des articles publiés au premier semestre 2020 appartiennent à la classe 1 (acheter-petit), c'est-à-dire à la classe constituée des paragraphes qui traitent le sujet du pouvoir d'achat des consommateurs.")) %>% 
  tab_options(table.width = pct(100)) %>%
  cols_width(1 ~ pct(10)) %>%
  tab_footnote(md("**Source** : corpus Europresse constitué par nos soins."))


library(xtable)
## Selon le journal. 
library(openxlsx)
library(dplyr)
library(gt)
library(magrittr)
library(rainette)

tab <- table(figures$rai_cut_meta$journal, 
      figures$rai_cut_meta$cluster_10 %>% 
        factor(x = ., labels = sapply(rainette::rainette_stats(., dtm = dtmQ), function(x) paste0(x[1,1], "-", x[2,1])))) %>%
  lprop() %>% as.data.frame.matrix() %>% 
  gt(rownames_to_stub = T, locale = "fr")  %>% 
  tab_header(title = md("**Répartition des paragraphes publiés par journal selon la classe affectée par la classification Reinert**")) %>%
  fmt_percent(columns = 2:11, decimals = 1, scale_values = F, incl_space = T) %>%
  fmt_percent(columns = 12, decimals = 0, scale_values = F, incl_space = T) %>%
  tab_footnote(md("**Note de lecture** : 29,3% des paragraphes de l'ensemble des articles publiés par le journal *Aujourd'hui en France* appartiennent à la classe 1 (acheter-petit), c'est-à-dire à la classe constituée des paragraphes qui traitent le sujet du pouvoir d'achat des consommateurs.")) %>% 
  tab_options(table.width = pct(100)) %>%
  cols_width(1 ~ pct(10)) %>%
  tab_footnote(md("**Source** : corpus Europresse constitué par nos soins."))

latex_code <- as_latex(tab)

cat(as.character(latex_code), file = "table.tex")
# Convertir le tableau gt en data frame
df <- as.data.frame.matrix(tab)

# Sauvegarder le tableau en Excel
write.xlsx(df, "maud/tableau_repartition_paragraphes.xlsx")

library(openxlsx)
library(dplyr)
library(gt)
library(magrittr)
library(rainette)

# Créer le tableau comme vous l'avez fait
raw_tab <- table(figures$rai_cut_meta$journal, 
                 figures$rai_cut_meta$cluster_10 %>% 
                   factor(x = ., labels = sapply(rainette::rainette_stats(., dtm = dtmQ), function(x) paste0(x[1,1], "-", x[2,1])))) %>%
  lprop() %>% as.data.frame.matrix()

# Ensuite, vous pouvez utiliser les fonctions de gt pour générer le tableau pour l'affichage
tab <- raw_tab/100 %>% 
  gt(rownames_to_stub = TRUE, locale = "fr")  %>% 
  tab_header(title = md("**Répartition des paragraphes publiés par journal selon la classe affectée par la classification Reinert**")) %>%
  fmt_percent(columns = 2:11, decimals = 1, scale_values = FALSE, incl_space = TRUE) %>%
  fmt_percent(columns = 12, decimals = 0, scale_values = FALSE, incl_space = TRUE) %>%
  tab_footnote(md("**Note de lecture** : 29,3% des paragraphes de l'ensemble des articles publiés par le journal *Aujourd'hui en France* appartiennent à la classe 1 (acheter-petit), c'est-à-dire à la classe constituée des paragraphes qui traitent le sujet du pouvoir d'achat des consommateurs.")) %>% 
  tab_options(table.width = pct(100)) %>%
  cols_width(1 ~ pct(10)) %>%
  tab_footnote(md("**Source** : corpus Europresse constitué par nos soins."))

# Sauvegarder le tableau brut en Excel
write.xlsx(raw_tab/100, "maud/tableau_repartition_paragraphes.xlsx")

