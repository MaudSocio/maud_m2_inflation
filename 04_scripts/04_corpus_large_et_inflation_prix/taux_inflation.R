

inflation <- fread("02_data_travail/inflation/Inflation_OCDE_Annee.csv")
base <- fread("06_data_base/base_personnes_genre_v3.csv")



inflation$annee <- as.integer(inflation$annee)

str(inflation)

# Inflation annuelle --------------


base_inflation <- base_inflationn %>%
  left_join(inflation, by = c("journal" = "newspaper", "year" = "annee")) %>%
  # Renommage de la colonne n_articles en nb_article_total
  mutate(nb_article_total = n_articles) %>%
  # Vous pouvez choisir de retirer la colonne n_articles si vous ne voulez pas la conserver
  select(-n_articles)


## Apariement avec la base de Ollion 
inflation[, INFLATION := as.numeric(gsub(",", ".", INFLATION))]

# Jointure de 'base' avec 'base_inflation' sur la colonne 'year'
base <- base[inflation, on = .(year = annee), nomatch = 0]


# Inflation mensuelle -----------------


base_monthly <- fread("02_data_travail/inflation/Inflation_OCDE_1945_2023_v2.csv")


# Conversion de la colonne 'date' de 'base' en IDate
base[, date := as.IDate(date, format = "%Y-%m-%d")]

# Jointure de 'base' avec 'base_monthly' sur la colonne de date
base <- base[base_monthly, on = .(date = TIME), nomatch = NA]

