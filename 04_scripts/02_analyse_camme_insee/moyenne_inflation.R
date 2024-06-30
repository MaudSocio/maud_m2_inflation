

library(questionr)

Annee1991 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1991.csv")



## Années 1991 
Annee1991 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1991.csv")

View(Annee1991)

## Chef de ménage 

Annee1991$sexch

# Q4 évolution des prix depuis 6 1 augmenté // mois 2 peu varié // 3 légèrement diminué // 9 ne sait pas
irec(Annee1991$q4)


# intérêts d'achats important 1 oui le moment est plutôt
# actuellement (meubles, favorable
# machines à laver, 2 le moment n'est ni
# télévision ...) particulièrement favorable
# ni particulière ment
# défavorable
# 3 le moment est plutôt
# défavorable, achat à
# reporter
# 9 ne sait pas

# REVTRA : revenu par tranche 



Annee1992 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1992.csv")
Annee1993 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1993.csv")
Annee1994 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1994.csv")
Annee1995 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1995.csv")
Annee1996 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1996.csv")
Annee1997 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1997.csv")
Annee1998 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1998.csv")
Annee1999 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_1999.csv")
Annee2000 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2000.csv")
Annee2001 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2001.csv")
Annee2002 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2002.csv")



Annee2003 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2003.csv")

# de 2004 à 2022 ---------------

library(data.table)
library(dplyr)
library(forcats)



# Dataframe de départ 
annees <- 1991:2021
df <- data.frame(Annee = annees, moyenne = NA, mediane = NA,
                 moyenne_h = NA, mediane_h = NA, moyenne_f = NA, mediane_f = NA)

# Boucle de 2004 à 2022 pour calculer les statistiques
for (annee in 2004:2021) {
  fichier <- sprintf("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_%d.csv", annee)
  dt <- fread(fichier)
  
  # Recodage du genre
  dt$SEXE_rec <- as.character(dt$SEXE) %>%
    fct_recode(
      "Homme" = "1",
      "Femme" = "2"
    )
  
  # Calcul des statistiques
  moyenne <- mean(dt$PRIXPLUS, na.rm = TRUE)
  mediane <- median(dt$PRIXPLUS, na.rm = TRUE)
  moyenne_h <- mean(dt$PRIXPLUS[dt$SEXE_rec == "Homme"], na.rm = TRUE)
  mediane_h <- median(dt$PRIXPLUS[dt$SEXE_rec == "Homme"], na.rm = TRUE)
  moyenne_f <- mean(dt$PRIXPLUS[dt$SEXE_rec == "Femme"], na.rm = TRUE)
  mediane_f <- median(dt$PRIXPLUS[dt$SEXE_rec == "Femme"], na.rm = TRUE)
  
  # Mise à jour de df
  df$moyenne[df$Annee == annee] <- moyenne
  df$mediane[df$Annee == annee] <- mediane
  df$moyenne_h[df$Annee == annee] <- moyenne_h
  df$mediane_h[df$Annee == annee] <- mediane_h
  df$moyenne_f[df$Annee == annee] <- moyenne_f
  df$mediane_f[df$Annee == annee] <- mediane_f
  
  # Suppression de dt pour libérer de la mémoire
  rm(dt)
  gc()
}

# Exportation de df en CSV
write.csv(df, "C:/Users/ymaud/dev/inflation_memoire/02_data/travail/moyennes.csv", row.names = FALSE)















Annee2004 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2004.csv")

# Recodage du genre 

Annee2004$SEXE_rec <- Annee2004$SEXE %>%
  as.character() %>%
  fct_recode(
    "Homme" = "1",
    "Femme" = "2"
  )

# Pop générale Moyenne et médiane 
moyenne2004 <- mean(Annee2004$PRIXPLUS, na.rm = TRUE)
mediane2004 <- median(Annee2004$PRIXPLUS, na.rm = TRUE)
moyenne_h_2004 <- mean(Annee2004$PRIXPLUS[Annee2004$SEXE_rec == "Homme"], na.rm = TRUE)
moyenne_f_2004 <- mean(Annee2004$PRIXPLUS[Annee2004$SEXE_rec == "Femme"], na.rm = TRUE)
mediane_h_2004 <- mean(Annee2004$PRIXPLUS[Annee2004$SEXE_rec == "Homme"], na.rm = TRUE)
mediane_f_2004 <- mean(Annee2004$PRIXPLUS[Annee2004$SEXE_rec == "Femme"], na.rm = TRUE)

# Mettre dans le dataframe 
df$moyenne[df$Annee == 2004] <- moyenne2004 
df$mediane[df$Annee == 2004]  <- mediane2004
df$moyenne_f[df$Annee == 2004] <- moyenne_f_2004
df$moyenne_h[df$Annee == 2004] <- moyenne_h_2004
df$mediane_h[df$Annee == 2004] <- mediane_h_2004
df$mediane_f[df$Annee == 2004] <- mediane_f_2004 





df$moyenne_h_2004 <- case_when(Annee2004$SEXE_rec == "Homme", mean(!is.na(Annee2004$PRIXPLUS)))
df$mediane_h_2004 <- median(!is.na(Annee2004$PRIXPLUS))












Annee2005 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2005.csv")
Annee2006 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2006.csv")
Annee2007 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2007.csv")
Annee2008 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2008.csv")
Annee2009 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2009.csv")
Annee2010 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2010.csv")
Annee2011 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2011.csv")
Annee2012 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2012.csv")
Annee2013 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2013.csv")
Annee2014 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2014.csv")
Annee2015 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2015.csv")
Annee2016 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2016.csv")
Annee2017 <- fread("C:/Users/ymaud/dev/inflation_memoire/02_data/travail/Inflation_base_2017.csv")


