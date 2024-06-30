### Etape 2 Recodage base de données ########

# ETAPE 1 : Recodage de variables explicatives ########
# I. Variable socio-démographique ########
## 1. Recodage sexe ########
## Recodage de df$SEXE en df$SEXE_rec
df$SEXE_rec <- as.character(df$SEXE)
df$SEXE_rec[df$SEXE == "1"] <- "Homme"
df$SEXE_rec[df$SEXE == "2"] <- "Femme"




## 2. Recodage Actif de df$NBACTIF en df$NBACTIF_rec ########
## Recodage de df$NBACTIF en df$NBACTIF_rec
df$NBACTIF_rec <- df$NBACTIF %>%
  as.character() %>%
  fct_recode(
    "2 ou plus" = "2",
    "2 ou plus" = "3",
    "2 ou plus" = "4",
    "2 ou plus" = "5"
  )

## On  a des effectifs qui sont assez équilibré, ce qui est assez interessant. 

## 3. Recodage du Diplome et diplome du conjoint ########
## Recodage de df$DIPLOME en df$DIPLOME_rec
df$DIPLOME_rec <- as.character(df$DIPLOME)
df$DIPLOME_rec[df$DIPLOME == "1"] <- "Aucun diplôme ou certificat d’études primaires"
df$DIPLOME_rec[df$DIPLOME == "2"] <- "Aucun diplôme ou certificat d’études primaires"
df$DIPLOME_rec[df$DIPLOME == "3"] <- "Aucun diplôme ou certificat d’études primaires"
df$DIPLOME_rec[df$DIPLOME == "4"] <- "CAP, BEP ou équivalent"
df$DIPLOME_rec[df$DIPLOME == "5"] <- "Baccalauréat, brevet professionnel ou équivalent"
df$DIPLOME_rec[df$DIPLOME == "6"] <- "Baccalauréat, brevet professionnel ou équivalent"
df$DIPLOME_rec[df$DIPLOME == "7"] <- "Baccalauréat, brevet professionnel ou équivalent"
df$DIPLOME_rec[df$DIPLOME == "8"] <- "Diplôme du supérieur court (niveau bac + 2)"
df$DIPLOME_rec[df$DIPLOME == "9"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOME_rec[df$DIPLOME == "10"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOME_rec[df$DIPLOME == "11"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOME_rec[df$DIPLOME == "12"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOME_rec[df$DIPLOME == "99"] <- "Aucun diplôme ou certificat d’études primaires"

## Réordonnancement de df$DIPLOME_rec
df$DIPLOME_rec <- df$DIPLOME_rec %>%
  fct_relevel(
    "Aucun diplôme ou certificat d’études primaires", "CAP, BEP ou équivalent",
    "Baccalauréat, brevet professionnel ou équivalent", "Diplôme du supérieur court (niveau bac + 2)",
    "Diplôme du supérieur long (supérieur à bac + 2)"
  )

## C'est assez équilibré on garde comme ça. 

## Composition des 75 Ne sait pas  : des gens pas diplomes ? 

df$DIPLOMEC_rec <- as.character(df$DIPLOMEC)
df$DIPLOMEC_rec[df$DIPLOMEC == "1"] <- "Aucun diplôme ou certificat d’études primaires"
df$DIPLOMEC_rec[df$DIPLOMEC == "2"] <- "Aucun diplôme ou certificat d’études primaires"
df$DIPLOMEC_rec[df$DIPLOMEC == "3"] <- "Aucun diplôme ou certificat d’études primaires"
df$DIPLOMEC_rec[df$DIPLOMEC == "4"] <- "CAP, BEP ou équivalent"
df$DIPLOMEC_rec[df$DIPLOMEC == "5"] <- "Baccalauréat, brevet professionnel ou équivalent"
df$DIPLOMEC_rec[df$DIPLOMEC == "6"] <- "Baccalauréat, brevet professionnel ou équivalent"
df$DIPLOMEC_rec[df$DIPLOMEC == "7"] <- "Baccalauréat, brevet professionnel ou équivalent"
df$DIPLOMEC_rec[df$DIPLOMEC == "8"] <- "Diplôme du supérieur court (niveau bac + 2)"
df$DIPLOMEC_rec[df$DIPLOMEC == "9"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOMEC_rec[df$DIPLOMEC == "10"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOMEC_rec[df$DIPLOMEC == "11"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOMEC_rec[df$DIPLOMEC == "12"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
df$DIPLOMEC_rec[df$DIPLOMEC == "99"] <- "Ne sait pas"


## On peut postuler que les NA sont en réalité des ne sait pas. 
## On peut aussi ne pas utiliser cette variable. 

## 4. Recodage niveau d'étude ######
## Recodage de df$NIVETUDE en df$NIVETUDE_rec
## Les Ne sait pas : souvent ils ont pas fait d'étude. 
## Recodage de df$NIVETUDE en df$NIVETUDE_rec
df$NIVETUDE_rec <- df$NIVETUDE %>%
  as.character() %>%
  fct_recode(
    "Pas d’études ou étude primaire" = "1",
    "Pas d’études ou étude primaire" = "2",
    "Secondaire" = "3",
    "Supérieur" = "4",
    "Pas d’études ou étude primaire" = "9"
  ) %>%
  fct_explicit_na("Pas d’études ou étude primaire")


df$ETUDECONJ_rec <- df$ETUDECONJ  %>%
  as.character() %>%
  fct_recode(
    "Pas d’études" = "1",
    "Primaire" = "2",
    "Secondaire" = "3",
    "Supérieur" = "4",
    "Pas d’études" = "9"
  ) %>%
  fct_explicit_na("NA")



## 5. Recodage situation économique ########
## Recodage de df$SITUAECO en df$SITUAECO_rec
df$SITUAECO_rec <- as.character(df$SITUAECO)
df$SITUAECO_rec[df$SITUAECO == "1"] <- "s’est nettement améliorée"
df$SITUAECO_rec[df$SITUAECO == "2"] <- "s’est un peu améliorée"
df$SITUAECO_rec[df$SITUAECO == "3"] <- "est restée stationnaire"
df$SITUAECO_rec[df$SITUAECO == "4"] <- "s’est un peu dégradée"
df$SITUAECO_rec[df$SITUAECO == "5"] <- "s’est nettement dégradée"
df$SITUAECO_rec[df$SITUAECO == "9"] <- "Ne sait pas"
## Réordonnancement de df$SITUAECO_rec
## Réordonnancement de df$SITUAECO_rec
df$SITUAECO_rec <- df$SITUAECO_rec %>%
  fct_relevel(
    "s’est nettement dégradée.", "s’est un peu dégradée",
    "est restée stationnaire", "s’est améliorée"
  )
## Réordonnancement de df$SITUAECO_rec
df$SITUAECO_rec <- df$SITUAECO_rec %>%
  fct_relevel(
    "s’est nettement dégradée.", "s’est un peu dégradée",
    "est restée stationnaire", "s’est améliorée"
  )
## On impute les NA à une vision assez standard. 


## 6. Recodage Mois ####
## Recodage de df$MOISENQ en df$MOISENQ_rec
df$MOISENQ_rec <- df$MOISENQ %>%
  as.character() %>%
  fct_recode(
    "Janvier" = "1",
    "Février" = "2",
    "Mars" = "3",
    "Avril" = "4",
    "Mai" = "5",
    "Juin" = "6",
    "Juillet" = "7",
    "Aout" = "8",
    "Septembre" = "9",
    "Octobre" = "10",
    "Novembre" = "11",
    "Décembre" = "12"
  )

## Réordonnancement de df$MOISENQ_rec
df$MOISENQ_rec <- df$MOISENQ_rec %>%
  fct_relevel(
    "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
    "Aout", "Septembre", "Octobre", "Novembre", "Décembre"
  )

## 7.Recodage nb enfants moins 14 ans #####

## Recodage de df$NBENFANT en df$NBENFANT_rec
df$NBENFANT_rec <- df$NBENFANT %>%
  as.character() %>%
  fct_recode(
    "3 ou plus" = "3",
    "3 ou plus" = "4",
    "3 ou plus" = "5",
    "3 ou plus" = "6"
  )


## Je gare le petit effectif car il m'interesse pour les familles monoparentales

## 8. Création de la variable enfant total  #######
## Recodage de df$NBENFANT2 en df$NBENFANT2_rec
NBENFANT2_rec <- as.factor(df$NBENFANT2)
## Recodage de NBENFANT2_rec en NBENFANT2_rec_rec
NBENFANT2_rec_rec <- NBENFANT2_rec %>%
  fct_explicit_na("0")
NBENFANT2_rec_rec_rec <- as.integer(NBENFANT2_rec_rec)
### Nombre d'enfant total 
df$NBENFANTTOTAL <- NBENFANT2_rec_rec_rec + df$NBENFANT


## Recodage de df$NBENFANTTOTAL en df$NBENFANTTOTAL_rec
df$NBENFANTTOTAL_rec <- df$NBENFANTTOTAL %>%
  as.character() %>%
  fct_recode(
    "4 ou plus" = "4",
    "4 ou plus" = "5",
    "4 ou plus" = "6",
    "4 ou plus" = "7",
    "4 ou plus" = "8"
  )
## Recodage de df$NBENFANTTOTAL en df$NBENFANTTOTAL_rec


## 9. Recodage profession #######

## Recodage de df$CLASPRO2 en df$CLASPRO2_rec
df$PROFESSION <- df$CLASPRO2 %>%
  as.character() %>%
  fct_recode(
    "Ouvrier, technicien, agent de maîtrise" = "1",
    "Ouvrier, technicien, agent de maîtrise" = "2",
    "Ouvrier, technicien, agent de maîtrise" = "3",
    "Ouvrier, technicien, agent de maîtrise" = "4",
    "Ingénieur, cadre, directeur" = "5",
    "Employé" = "6",
    "Ingénieur, cadre, directeur" = "7"
  )
## Réordonnancement de df$PROFESSION
df$PROFESSION <- df$PROFESSION %>%
  fct_relevel( "Ouvrier, technicien, agent de maîtrise",
               "Employé", "Ingénieur, cadre, directeur"
  )



## 10. Recodage profession conjoint #######
### Classification professionnel du conjoint : CLASprc2

df$PROFCONJOINT <- df$CLASPRC2 %>%
  as.character() %>%
  fct_recode(
    "Ouvrier, technicien, agent de maîtrise" = "1",
    "Ouvrier, technicien, agent de maîtrise" = "2",
    "Ouvrier, technicien, agent de maîtrise" = "3",
    "Ouvrier, technicien, agent de maîtrise" = "4",
    "Ingénieur, cadre, directeur" = "5",
    "Employé" = "6",
    "Ingénieur, cadre, directeur" = "7"
  )

## Réordonnancement de df$PROFESSION
df$PROFCONJOINT <- df$PROFCONJOINT %>%
  fct_relevel( "Ouvrier, technicien, agent de maîtrise",
               "Employé", "Ingénieur, cadre, directeur"
  )

## Recodage : Votre conjoint(e) a-t-il(elle) déjà travaillé ?
## Recodage de df$DEJTRACJ en df$DEJTRACJ_rec
df$DEJTRACJ_rec <- df$DEJTRACJ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )

## Variable non prise en compte 

## 11. Temps complet ou partiel  #######
## Recodage de df$QUOTITE2 en df$QUOTITE2_rec
df$QUOTITE2_rec <- df$QUOTITE2 %>%
  as.character() %>%
  fct_recode(
    "Temps complet" = "1",
    "Temps partiel" = "2"
  )

## 11. Temps complet ou partiel conjoint #######
## Recodage conjoint : de df$QUOTITC2 en df$QUOTITC2_rec
df$QUOTITC2_rec <- df$QUOTITC2 %>%
  as.character() %>%
  fct_recode(
    "Temps complet" = "1",
    "Temps partiel" = "2"
  )


## 12. Recodage Type d'emploi : CDD ou CDI ####
## Recodage de df$TYPEMPL2 en df$TYPEMPL2_rec
df$TYPEMPL2_rec <- df$TYPEMPL2 %>%
  as.character() %>%
  fct_recode(
    "CDI" = "1",
    "CDD" = "2"
  )


## 13. Recodage Type d'emploi : CDD ou CDI ####

## Recodage de df$TYPEMPC2 en df$TYPEMPC2_rec
df$TYPEMPC2_rec <- as.character(df$TYPEMPC2)
df$TYPEMPC2_rec[df$TYPEMPC2 == "1"] <- "CDI"
df$TYPEMPC2_rec[df$TYPEMPC2 == "2"] <- "CDD"



## 14. Recodage nombre d'habitant dans le logement #######
## Recodage de df$NBPERS en df$NBPERS_rec
## Recodage de df$NBPERS en df$NBPERS_rec
df$NBPERS_rec <- df$NBPERS %>%
  as.character() %>%
  fct_recode(
    "5 ou plus" = "5",
    "5 ou plus" = "6",
    "5 ou plus" = "7",
    "5 ou plus" = "8",
    "5 ou plus" = "9",
    "5 ou plus" = "10",
    "5 ou plus" = "11"
  )

## Réordonnancement de df$NBPERS_rec
df$NBPERS_rec <- df$NBPERS_rec %>%
  fct_relevel(
    "1", "2", "3", "4", "5 ou plus"
  )



## 15. Recodage de l'âge ########

### a. Ageind : age de l'individu, variable numérique ######

df$ANNAISS <- ifelse(df$ANNAISS == 9998, NA, df$ANNAISS)
df$Ageind <- 2021 - df$ANNAISS

### b. Recodage de l'âge en variable catégorielle #####
df$Age_rec <- cut(df$Ageind,
                         include.lowest = TRUE,
                         right = TRUE,
                         dig.lab = 8,
                         breaks = c(18, 34, 44, 54, 64, 74, 130))
df$Age_rec_rec <- 
  df$Age_rec %>%
  fct_recode(
    "18-34 ans" = "[18,34]",
    "35-44 ans" = "(34,44]",
    "45-54 ans" = "(44,54]",
    "55-64 ans" = "(54,64]",
    "65-74 ans" = "(64,74]",
    "75 ans et +" = "(74,130]")
df$Age <- df$Age_rec_rec
df <- subset(df, !is.na(Age))


### Explication des 66 NA de l'âge 




## 16. Recodage de l'âge du conjoint Age du conjoint (on va voir si ça se recoupe) #######

df$ANNAISCJ <- ifelse(df$ANNAISCJ == 9998, NA, df$ANNAISCJ)
###Il y a 7530 NA !!! 
# ### Il y a beaucoup plus de NA 
# df$Ageconjseul <- 2021 - df$ANNAISC
# ## Recodage de df$Age en df$Age_rec
# df$Ageconj_rec <- cut(df$Ageconjseul,
#                          include.lowest = TRUE,
#                          right = TRUE,
#                          dig.lab = 8,
#                          breaks = c(18, 34, 44, 54, 64, 74, 130))
# 
# df$Ageconj_rec <- df$Ageconj_rec %>%
#   fct_recode(
#     "18-34 ans" = "[18,34]",
#     "35-44 ans" = "(34,44]",
#     "45-54 ans" = "(44,54]",
#     "55-64 ans" = "(54,64]",
#     "65-74 ans" = "(64,74]",
#     "75 ans et +" = "(74,130]")
# 
# df$Ageconj <- df$Ageconj_rec
# freq(df$Ageconj)




## 17. Recodage vie en couple : Vivez-vous en couple ?#######

## Recodage de df$CONJOINT en df$CONJOINT_rec
df$CONJOINT_rec <- df$CONJOINT %>%
  as.character() %>%
  fct_recode(
    "En couple" = "1",
    "Pas en couple" = "2"
  )



## 18. Création variable Famille monoparentale ######

df$Monoparental <- NA
df$Monoparental[df$SEXE_rec == "Homme" & df$NBENFANT == 0 & df$CONJOINT_rec == "Pas en couple"] <- "Homme seul sans enfant"
df$Monoparental[df$SEXE_rec == "Homme" & df$NBENFANT >= 1 & df$CONJOINT_rec == "Pas en couple"] <- "Homme seul avec enfant"
df$Monoparental[df$SEXE_rec == "Femme" & df$NBENFANT == 0 & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule sans enfant"
df$Monoparental[df$SEXE_rec == "Femme" & df$NBENFANT == 1  & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule un enfant"
df$Monoparental[df$SEXE_rec == "Femme" & df$NBENFANT == 2 & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec deux enfants"
df$Monoparental[df$SEXE_rec == "Femme" & df$NBENFANT >= 3 & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec trois enfants ou plus"
## Réordonnancement de df$MONOPARENTALE
df$Monoparental <- df$Monoparental %>%
  fct_relevel(
    "Femme seule avec trois enfants ou plus", "Femme seule avec deux enfants",
    "Femme seule un enfant", "Femme seule sans enfant", "Homme seul avec enfant",
    "Homme seul sans enfant"
  )


### 18. Création de la variable MONOPARENTALE : oui ou non ! 

df$MONOPARENTALE <- NA
df$MONOPARENTALE[df$NBENFANT == 0 & df$CONJOINT_rec == "Pas en couple"] <- "Pas monoparental"
df$MONOPARENTALE[df$NBENFANT >= 1 & df$CONJOINT_rec == "Pas en couple"] <- "Monoparental"
df$MONOPARENTALE[df$CONJOINT_rec == "En couple"] <- "Pas monoparental"


## 19. Création variable familles monoparentale 2 avec des enfants aussi de plus de 14 ans #####

df$Monoparental2 <- NA
df$Monoparental2[df$SEXE_rec == "Homme" & df$NBENFANTTOTAL_rec == 0 & df$CONJOINT_rec == "Pas en couple"] <- "Homme seul sans enfant"
df$Monoparental2[df$SEXE_rec == "Homme" & df$NBENFANTTOTAL_rec >= 1 & df$CONJOINT_rec == "Pas en couple"] <- "Homme seul avec enfant"
df$Monoparental2[df$SEXE_rec == "Femme" & df$NBENFANTTOTAL_rec == 0 & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule sans enfant"
df$Monoparental2[df$SEXE_rec == "Femme" & df$NBENFANTTOTAL_rec == 1  & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule un enfant"
df$Monoparental2[df$SEXE_rec == "Femme" & df$NBENFANTTOTAL_rec == 2 & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec deux enfants"
df$Monoparental2[df$SEXE_rec == "Femme" & df$NBENFANTTOTAL_rec >= 3 & df$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec trois enfants ou plus"

freq(df$NBENFANTTOTAL)
## Réordonnancement de df$Monoparental
df$Monoparental <- df$Monoparental %>%
  fct_relevel(
    "Femme seule avec trois enfants ou plus", "Femme seule avec deux enfants",
    "Femme seule un enfant", "Femme seule sans enfant", "Homme seul avec enfant",
    "Homme seul sans enfant"
  )


# II. Variable dépenses et conjoncture ########

## 1. Recodage Achat logement ########
## Recodage de df$LOGEMENT en df$LOGEMENT_rec
df$LOGEMENT_rec <- df$LOGEMENT %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Oui" = "2",
    "Non, probablement pas" = "3",
    "Non, certainement pas" = "4",
    "Non, certainement pas" = "9"
  )


## S'ils ne savent pas, c'est qu'ils ne vont pas en acheter. 
## 2. Recodage dépenses logement ####
## Recodage de df$DEPENSLO en df$DEPENSLO_rec
df$DEPENSLO_rec <- df$DEPENSLO %>%
  as.character() %>%
  fct_recode(
    "Oui, certainement" = "1",
    "Oui, peut-être" = "2",
    "Non, probablement pas" = "3",
    "Non, certainement pas" = "4",
    "Non, certainement pas" = "9"
  )


## 3. Recodage Achat future de voiture #####
## Recodage de df$AUTO en df$AUTO_rec
df$AUTO_rec <- df$AUTO %>%
  as.character() %>%
  fct_recode(
    "Oui, certainement" = "1",
    "Oui, peut-être" = "2",
    "Non, probablement pas" = "3",
    "Non, certainement pas" = "4",
    "Non, certainement pas" = "9"
  )


## 4. Recodage dépenses future ####

## Recodage de df$DEPENSES en df$DEPENSES_rec
## Recodage de df$DEPENSES en df$DEPENSES_rec
df$DEPENSES_rec <- df$DEPENSES %>%
  as.character() %>%
  fct_recode(
    "Beaucoup plus" = "1",
    "Un peu plus" = "2",
    "Autant" = "3",
    "Moins" = "4",
    "Moins" = "5",
    "Ne sait pas" = "9"
  ) %>%
  fct_explicit_na("Ne sait pas")



## 5. ACHATS #####
## Dans la situation économique actuelle, pensez-vous que les gens aient intérêt à faire des achats importants ?
## Recodage de df$ACHATS en df$ACHATS_rec
df$ACHATS_rec <- df$ACHATS %>%
  as.character() %>%
  fct_recode(
    "Oui, le moment est plutôt favorable" = "1",
    "Le moment n’est ni favorable ni défavorable" = "2",
    "Non, le moment est plutôt défavorable" = "3",
    "Ne sait pas" = "9"
  )


## 6. EPARGNER ####
## Recodage de df$EPARGNER en df$EPARGNER_rec
df$EPARGNER_rec <- df$EPARGNER %>%
  as.character() %>%
  fct_recode(
    "Oui, certainement" = "1",
    "Oui, peut-être" = "2",
    "Non, probablement pas." = "3",
    "Non, certainement pas" = "4",
    "Ne sait pas" = "9"
  )


## 7. FINANCES ####

## Recodage de df$FINANCES en df$FINANCES_rec
df$FINANCES_rec <- df$FINANCES %>%
  as.character() %>%
  fct_recode(
    "Beaucoup d'argent de côté" = "1",
    "Peu d'argent de côté" = "2",
    "Budget juste" = "3",
    "Tire sur les réserves" = "4",
    "S'endette" = "5",
    "Budget juste" = "9"
  )


## 8. FINANPAS ####
## Recodage de df$FINANPAS en df$FINANPAS_rec
df$FINANPAS_rec <- df$FINANPAS %>%
  as.character() %>%
  fct_recode(
    "s’est nettement améliorée" = "1",
    "s’est un peu améliorée" = "2",
    "est restée stationnaire" = "3",
    "s’est un peu dégradée." = "4",
    "s’est nettement dégradée" = "5",
    "est restée stationnaire" = "9"
  )


## 8. FINANFUT ####
## Recodage de df$FINANFUT en df$FINANFUT_rec
df$FINANFUT_rec <- df$FINANFUT %>%
  as.character() %>%
  fct_recode(
    "nettement améliorer" = "1",
    "un peu améliorer" = "2",
    "rester stationnaire" = "3",
    "un peu se dégrader" = "4",
    "nettement se dégrader" = "5",
    "Ne sait pas" = "9"
  )

# ETAPE 2 : Recodage des six variables sur le prix ##########


## 1. PRIX_rec : Variable qualitative : "Pensez-vous qu’au cours des douze derniers mois les prix ont…" ########

### a. Recodage de df$PRIX en df$PRIX_rec #####
df$PRIX_rec <- as.character(df$PRIX)
df$PRIX_rec[df$PRIX == "1"] <- "Fortement augmenté"
df$PRIX_rec[df$PRIX == "2"] <- "Modérément augmenté"
df$PRIX_rec[df$PRIX == "3"] <- "Un peu augmenté"
df$PRIX_rec[df$PRIX == "4"] <- "Stagné ou diminué"
df$PRIX_rec[df$PRIX == "5"] <- "Stagné ou diminué"
df$PRIX_rec[df$PRIX == "9"] <- "Stagné ou diminué"
## b.Réordonnancement de df$PRIX_rec #####

## Réordonnancement de df$PRIX_rec
## Réordonnancement de df$PRIX_rec
df$PRIX_rec <- df$PRIX_rec %>%
  fct_relevel(
    "Stagné ou diminué", "Un peu augmenté", "Modérément augmenté",
    "Fortement augmenté"
  )




## 2. PRIXPLUS : Variable quantitative de l'évolution des prix #####
### De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ? #####
df$prixplus_rec <-  as.numeric(df$PRIXPLUS)



# 3 PRIXBAIS : Variable quantitative : De quel pourcentage pensez-vous que les prix ont baissé au cours des douze derniers mois ? #####

df$PRIXBAIS_rec <- as.numeric(df$PRIXBAIS)

## Il y a la moitié des cases qui sont vides ... 
### Il n'y a pas assez d'information sur cette variable. C'est du grand n'importe quoi 
### Elle ne sera pas conservée pour la suite. 



fwrite(df,"C:/Users/ymaud/dev/inflation_memoire/02_data/agregees/Inflation_base_all_years_v3.csv", row.names = FALSE)

