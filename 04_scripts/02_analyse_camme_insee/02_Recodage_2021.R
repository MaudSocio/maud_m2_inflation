### Etape 2 Recodage base de données ########

# ETAPE 1 : Recodage de variables explicatives ########
# I. Variable socio-démographique ########
## 1. Recodage sexe ########
## Recodage de Annee2021$SEXE en Annee2021$SEXE_rec
Annee2021$SEXE_rec <- as.character(Annee2021$SEXE)
Annee2021$SEXE_rec[Annee2021$SEXE == "1"] <- "Homme"
Annee2021$SEXE_rec[Annee2021$SEXE == "2"] <- "Femme"




## 2. Recodage Actif de Annee2021$NBACTIF en Annee2021$NBACTIF_rec ########
## Recodage de Annee2021$NBACTIF en Annee2021$NBACTIF_rec
Annee2021$NBACTIF_rec <- Annee2021$NBACTIF %>%
  as.character() %>%
  fct_recode(
    "2 ou plus" = "2",
    "2 ou plus" = "3",
    "2 ou plus" = "4",
    "2 ou plus" = "5"
  )

## On  a des effectifs qui sont assez équilibré, ce qui est assez interessant. 

## 3. Recodage du Diplome et diplome du conjoint ########
## Recodage de Annee2021$DIPLOME en Annee2021$DIPLOME_rec
Annee2021$DIPLOME_rec <- as.character(Annee2021$DIPLOME)
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "1"] <- "Aucun diplôme ou certificat d’études primaires"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "2"] <- "Aucun diplôme ou certificat d’études primaires"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "3"] <- "Aucun diplôme ou certificat d’études primaires"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "4"] <- "CAP, BEP ou équivalent"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "5"] <- "Baccalauréat, brevet professionnel ou équivalent"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "6"] <- "Baccalauréat, brevet professionnel ou équivalent"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "7"] <- "Baccalauréat, brevet professionnel ou équivalent"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "8"] <- "Diplôme du supérieur court (niveau bac + 2)"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "9"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "10"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "11"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "12"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOME_rec[Annee2021$DIPLOME == "99"] <- "Aucun diplôme ou certificat d’études primaires"

## Réordonnancement de Annee2021$DIPLOME_rec
Annee2021$DIPLOME_rec <- Annee2021$DIPLOME_rec %>%
  fct_relevel(
    "Aucun diplôme ou certificat d’études primaires", "CAP, BEP ou équivalent",
    "Baccalauréat, brevet professionnel ou équivalent", "Diplôme du supérieur court (niveau bac + 2)",
    "Diplôme du supérieur long (supérieur à bac + 2)"
  )

## C'est assez équilibré on garde comme ça. 

## Composition des 75 Ne sait pas  : des gens pas diplomes ? 

Annee2021$DIPLOMEC_rec <- as.character(Annee2021$DIPLOMEC)
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "1"] <- "Aucun diplôme ou certificat d’études primaires"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "2"] <- "Aucun diplôme ou certificat d’études primaires"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "3"] <- "Aucun diplôme ou certificat d’études primaires"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "4"] <- "CAP, BEP ou équivalent"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "5"] <- "Baccalauréat, brevet professionnel ou équivalent"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "6"] <- "Baccalauréat, brevet professionnel ou équivalent"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "7"] <- "Baccalauréat, brevet professionnel ou équivalent"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "8"] <- "Diplôme du supérieur court (niveau bac + 2)"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "9"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "10"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "11"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "12"] <- "Diplôme du supérieur long (supérieur à bac + 2)"
Annee2021$DIPLOMEC_rec[Annee2021$DIPLOMEC == "99"] <- "Ne sait pas"


## On peut postuler que les NA sont en réalité des ne sait pas. 
## On peut aussi ne pas utiliser cette variable. 

## 4. Recodage niveau d'étude ######
## Recodage de Annee2021$NIVETUDE en Annee2021$NIVETUDE_rec
## Les Ne sait pas : souvent ils ont pas fait d'étude. 
## Recodage de Annee2021$NIVETUDE en Annee2021$NIVETUDE_rec
Annee2021$NIVETUDE_rec <- Annee2021$NIVETUDE %>%
  as.character() %>%
  fct_recode(
    "Pas d’études ou étude primaire" = "1",
    "Pas d’études ou étude primaire" = "2",
    "Secondaire" = "3",
    "Supérieur" = "4",
    "Pas d’études ou étude primaire" = "9"
  ) %>%
  fct_explicit_na("Pas d’études ou étude primaire")


Annee2021$ETUDECONJ_rec <- Annee2021$ETUDECONJ  %>%
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
## Recodage de Annee2021$SITUAECO en Annee2021$SITUAECO_rec
Annee2021$SITUAECO_rec <- as.character(Annee2021$SITUAECO)
Annee2021$SITUAECO_rec[Annee2021$SITUAECO == "1"] <- "s’est nettement améliorée"
Annee2021$SITUAECO_rec[Annee2021$SITUAECO == "2"] <- "s’est un peu améliorée"
Annee2021$SITUAECO_rec[Annee2021$SITUAECO == "3"] <- "est restée stationnaire"
Annee2021$SITUAECO_rec[Annee2021$SITUAECO == "4"] <- "s’est un peu dégradée"
Annee2021$SITUAECO_rec[Annee2021$SITUAECO == "5"] <- "s’est nettement dégradée"
Annee2021$SITUAECO_rec[Annee2021$SITUAECO == "9"] <- "Ne sait pas"
## Réordonnancement de Annee2021$SITUAECO_rec
## Réordonnancement de Annee2021$SITUAECO_rec
Annee2021$SITUAECO_rec <- Annee2021$SITUAECO_rec %>%
  fct_relevel(
    "s’est nettement dégradée.", "s’est un peu dégradée",
    "est restée stationnaire", "s’est améliorée"
  )
## Réordonnancement de Annee2021$SITUAECO_rec
Annee2021$SITUAECO_rec <- Annee2021$SITUAECO_rec %>%
  fct_relevel(
    "s’est nettement dégradée.", "s’est un peu dégradée",
    "est restée stationnaire", "s’est améliorée"
  )
## On impute les NA à une vision assez standard. 


## 6. Recodage Mois ####
## Recodage de Annee2021$MOISENQ en Annee2021$MOISENQ_rec
Annee2021$MOISENQ_rec <- Annee2021$MOISENQ %>%
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

## Réordonnancement de Annee2021$MOISENQ_rec
Annee2021$MOISENQ_rec <- Annee2021$MOISENQ_rec %>%
  fct_relevel(
    "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet",
    "Aout", "Septembre", "Octobre", "Novembre", "Décembre"
  )

## 7.Recodage nb enfants moins 14 ans #####

## Recodage de Annee2021$NBENFANT en Annee2021$NBENFANT_rec
Annee2021$NBENFANT_rec <- Annee2021$NBENFANT %>%
  as.character() %>%
  fct_recode(
    "3 ou plus" = "3",
    "3 ou plus" = "4",
    "3 ou plus" = "5",
    "3 ou plus" = "6"
  )


## Je gare le petit effectif car il m'interesse pour les familles monoparentales

## 8. Création de la variable enfant total  #######
## Recodage de Annee2021$NBENFANT2 en Annee2021$NBENFANT2_rec
NBENFANT2_rec <- as.factor(Annee2021$NBENFANT2)
## Recodage de NBENFANT2_rec en NBENFANT2_rec_rec
NBENFANT2_rec_rec <- NBENFANT2_rec %>%
  fct_explicit_na("0")
NBENFANT2_rec_rec_rec <- as.integer(NBENFANT2_rec_rec)
### Nombre d'enfant total 
Annee2021$NBENFANTTOTAL <- NBENFANT2_rec_rec_rec + Annee2021$NBENFANT
## Recodage de Annee2021$NBENFANTTOTAL en Annee2021$NBENFANTTOTAL_rec
Annee2021 %>% 
  tbl_summary(include = "NBENFANTTOTAL")

## Recodage de Annee2021$NBENFANTTOTAL en Annee2021$NBENFANTTOTAL_rec
Annee2021$NBENFANTTOTAL_rec <- Annee2021$NBENFANTTOTAL %>%
  as.character() %>%
  fct_recode(
    "4 ou plus" = "4",
    "4 ou plus" = "5",
    "4 ou plus" = "6",
    "4 ou plus" = "7",
    "4 ou plus" = "8"
  )
## Recodage de Annee2021$NBENFANTTOTAL en Annee2021$NBENFANTTOTAL_rec


## 9. Recodage profession #######

## Recodage de Annee2021$CLASPRO2 en Annee2021$CLASPRO2_rec
Annee2021$PROFESSION <- Annee2021$CLASPRO2 %>%
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
## Réordonnancement de Annee2021$PROFESSION
Annee2021$PROFESSION <- Annee2021$PROFESSION %>%
  fct_relevel( "Ouvrier, technicien, agent de maîtrise",
    "Employé", "Ingénieur, cadre, directeur"
  )



## 10. Recodage profession conjoint #######
     ### Classification professionnel du conjoint : CLASprc2

  Annee2021$PROFCONJOINT <- Annee2021$CLASPRC2 %>%
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

## Réordonnancement de Annee2021$PROFESSION
Annee2021$PROFCONJOINT <- Annee2021$PROFCONJOINT %>%
  fct_relevel( "Ouvrier, technicien, agent de maîtrise",
               "Employé", "Ingénieur, cadre, directeur"
  )

## Recodage : Votre conjoint(e) a-t-il(elle) déjà travaillé ?
## Recodage de Annee2021$DEJTRACJ en Annee2021$DEJTRACJ_rec
Annee2021$DEJTRACJ_rec <- Annee2021$DEJTRACJ %>%
  as.character() %>%
  fct_recode(
    "Oui" = "1",
    "Non" = "2"
  )

## Variable non prise en compte 

## 11. Temps complet ou partiel  #######
## Recodage de Annee2021$QUOTITE2 en Annee2021$QUOTITE2_rec
Annee2021$QUOTITE2_rec <- Annee2021$QUOTITE2 %>%
  as.character() %>%
  fct_recode(
    "Temps complet" = "1",
    "Temps partiel" = "2"
  )

## 11. Temps complet ou partiel conjoint #######
## Recodage conjoint : de Annee2021$QUOTITC2 en Annee2021$QUOTITC2_rec
Annee2021$QUOTITC2_rec <- Annee2021$QUOTITC2 %>%
  as.character() %>%
  fct_recode(
    "Temps complet" = "1",
    "Temps partiel" = "2"
  )


## 12. Recodage Type d'emploi : CDD ou CDI ####
## Recodage de Annee2021$TYPEMPL2 en Annee2021$TYPEMPL2_rec
Annee2021$TYPEMPL2_rec <- Annee2021$TYPEMPL2 %>%
  as.character() %>%
  fct_recode(
    "CDI" = "1",
    "CDD" = "2"
  )


## 13. Recodage Type d'emploi : CDD ou CDI ####

## Recodage de Annee2021$TYPEMPC2 en Annee2021$TYPEMPC2_rec
Annee2021$TYPEMPC2_rec <- as.character(Annee2021$TYPEMPC2)
Annee2021$TYPEMPC2_rec[Annee2021$TYPEMPC2 == "1"] <- "CDI"
Annee2021$TYPEMPC2_rec[Annee2021$TYPEMPC2 == "2"] <- "CDD"



## 14. Recodage nombre d'habitant dans le logement #######
## Recodage de Annee2021$NBPERS en Annee2021$NBPERS_rec
## Recodage de Annee2021$NBPERS en Annee2021$NBPERS_rec
Annee2021$NBPERS_rec <- Annee2021$NBPERS %>%
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

## Réordonnancement de Annee2021$NBPERS_rec
Annee2021$NBPERS_rec <- Annee2021$NBPERS_rec %>%
  fct_relevel(
    "1", "2", "3", "4", "5 ou plus"
  )



## 15. Recodage de l'âge ########

### a. Ageind : age de l'individu, variable numérique ######

Annee2021$ANNAISS <- ifelse(Annee2021$ANNAISS == 9998, NA, Annee2021$ANNAISS)
Annee2021$Ageind <- 2021 - Annee2021$ANNAISS

### b. Recodage de l'âge en variable catégorielle #####
Annee2021$Age_rec <- cut(Annee2021$Ageind,
  include.lowest = TRUE,
  right = TRUE,
  dig.lab = 8,
  breaks = c(18, 34, 44, 54, 64, 74, 130))
Annee2021$Age_rec_rec <- 
  Annee2021$Age_rec %>%
            fct_recode(
              "18-34 ans" = "[18,34]",
              "35-44 ans" = "(34,44]",
              "45-54 ans" = "(44,54]",
              "55-64 ans" = "(54,64]",
              "65-74 ans" = "(64,74]",
              "75 ans et +" = "(74,130]")
Annee2021$Age <- Annee2021$Age_rec_rec
Annee2021 <- subset(Annee2021, !is.na(Age))


### Explication des 66 NA de l'âge 




## 16. Recodage de l'âge du conjoint Age du conjoint (on va voir si ça se recoupe) #######

Annee2021$ANNAISCJ <- ifelse(Annee2021$ANNAISCJ == 9998, NA, Annee2021$ANNAISCJ)
###Il y a 7530 NA !!! 
# ### Il y a beaucoup plus de NA 
# Annee2021$Ageconjseul <- 2021 - Annee2021$ANNAISC
# ## Recodage de Annee2021$Age en Annee2021$Age_rec
# Annee2021$Ageconj_rec <- cut(Annee2021$Ageconjseul,
#                          include.lowest = TRUE,
#                          right = TRUE,
#                          dig.lab = 8,
#                          breaks = c(18, 34, 44, 54, 64, 74, 130))
# 
# Annee2021$Ageconj_rec <- Annee2021$Ageconj_rec %>%
#   fct_recode(
#     "18-34 ans" = "[18,34]",
#     "35-44 ans" = "(34,44]",
#     "45-54 ans" = "(44,54]",
#     "55-64 ans" = "(54,64]",
#     "65-74 ans" = "(64,74]",
#     "75 ans et +" = "(74,130]")
# 
# Annee2021$Ageconj <- Annee2021$Ageconj_rec
# freq(Annee2021$Ageconj)




## 17. Recodage vie en couple : Vivez-vous en couple ?#######

## Recodage de Annee2021$CONJOINT en Annee2021$CONJOINT_rec
Annee2021$CONJOINT_rec <- Annee2021$CONJOINT %>%
  as.character() %>%
  fct_recode(
    "En couple" = "1",
    "Pas en couple" = "2"
  )



## 18. Création variable Famille monoparentale ######

Annee2021$Monoparental <- NA
Annee2021$Monoparental[Annee2021$SEXE_rec == "Homme" & Annee2021$NBENFANT == 0 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Homme seul sans enfant"
Annee2021$Monoparental[Annee2021$SEXE_rec == "Homme" & Annee2021$NBENFANT >= 1 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Homme seul avec enfant"
Annee2021$Monoparental[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANT == 0 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule sans enfant"
Annee2021$Monoparental[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANT == 1  & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule un enfant"
Annee2021$Monoparental[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANT == 2 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec deux enfants"
Annee2021$Monoparental[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANT >= 3 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec trois enfants ou plus"
## Réordonnancement de Annee2021$MONOPARENTALE
Annee2021$Monoparental <- Annee2021$Monoparental %>%
  fct_relevel(
    "Femme seule avec trois enfants ou plus", "Femme seule avec deux enfants",
    "Femme seule un enfant", "Femme seule sans enfant", "Homme seul avec enfant",
    "Homme seul sans enfant"
  )


### 18. Création de la variable MONOPARENTALE : oui ou non ! 

Annee2021$MONOPARENTALE <- NA
Annee2021$MONOPARENTALE[Annee2021$NBENFANT == 0 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Pas monoparental"
Annee2021$MONOPARENTALE[Annee2021$NBENFANT >= 1 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Monoparental"
Annee2021$MONOPARENTALE[Annee2021$CONJOINT_rec == "En couple"] <- "Pas monoparental"


## 19. Création variable familles monoparentale 2 avec des enfants aussi de plus de 14 ans #####

Annee2021$Monoparental2 <- NA
Annee2021$Monoparental2[Annee2021$SEXE_rec == "Homme" & Annee2021$NBENFANTTOTAL_rec == 0 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Homme seul sans enfant"
Annee2021$Monoparental2[Annee2021$SEXE_rec == "Homme" & Annee2021$NBENFANTTOTAL_rec >= 1 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Homme seul avec enfant"
Annee2021$Monoparental2[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANTTOTAL_rec == 0 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule sans enfant"
Annee2021$Monoparental2[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANTTOTAL_rec == 1  & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule un enfant"
Annee2021$Monoparental2[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANTTOTAL_rec == 2 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec deux enfants"
Annee2021$Monoparental2[Annee2021$SEXE_rec == "Femme" & Annee2021$NBENFANTTOTAL_rec >= 3 & Annee2021$CONJOINT_rec == "Pas en couple"] <- "Femme seule avec trois enfants ou plus"

freq(Annee2021$NBENFANTTOTAL)
## Réordonnancement de Annee2021$Monoparental
Annee2021$Monoparental <- Annee2021$Monoparental %>%
  fct_relevel(
    "Femme seule avec trois enfants ou plus", "Femme seule avec deux enfants",
    "Femme seule un enfant", "Femme seule sans enfant", "Homme seul avec enfant",
    "Homme seul sans enfant"
  )


# II. Variable dépenses et conjoncture ########

## 1. Recodage Achat logement ########
## Recodage de Annee2021$LOGEMENT en Annee2021$LOGEMENT_rec
Annee2021$LOGEMENT_rec <- Annee2021$LOGEMENT %>%
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
## Recodage de Annee2021$DEPENSLO en Annee2021$DEPENSLO_rec
Annee2021$DEPENSLO_rec <- Annee2021$DEPENSLO %>%
  as.character() %>%
  fct_recode(
    "Oui, certainement" = "1",
    "Oui, peut-être" = "2",
    "Non, probablement pas" = "3",
    "Non, certainement pas" = "4",
    "Non, certainement pas" = "9"
  )


## 3. Recodage Achat future de voiture #####
## Recodage de Annee2021$AUTO en Annee2021$AUTO_rec
Annee2021$AUTO_rec <- Annee2021$AUTO %>%
  as.character() %>%
  fct_recode(
    "Oui, certainement" = "1",
    "Oui, peut-être" = "2",
    "Non, probablement pas" = "3",
    "Non, certainement pas" = "4",
    "Non, certainement pas" = "9"
  )


## 4. Recodage dépenses future ####

## Recodage de Annee2021$DEPENSES en Annee2021$DEPENSES_rec
## Recodage de Annee2021$DEPENSES en Annee2021$DEPENSES_rec
Annee2021$DEPENSES_rec <- Annee2021$DEPENSES %>%
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
## Recodage de Annee2021$ACHATS en Annee2021$ACHATS_rec
Annee2021$ACHATS_rec <- Annee2021$ACHATS %>%
  as.character() %>%
  fct_recode(
    "Oui, le moment est plutôt favorable" = "1",
    "Le moment n’est ni favorable ni défavorable" = "2",
    "Non, le moment est plutôt défavorable" = "3",
    "Ne sait pas" = "9"
  )


## 6. EPARGNER ####
## Recodage de Annee2021$EPARGNER en Annee2021$EPARGNER_rec
Annee2021$EPARGNER_rec <- Annee2021$EPARGNER %>%
  as.character() %>%
  fct_recode(
    "Oui, certainement" = "1",
    "Oui, peut-être" = "2",
    "Non, probablement pas." = "3",
    "Non, certainement pas" = "4",
    "Ne sait pas" = "9"
  )


## 7. FINANCES ####

## Recodage de Annee2021$FINANCES en Annee2021$FINANCES_rec
Annee2021$FINANCES_rec <- Annee2021$FINANCES %>%
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
## Recodage de Annee2021$FINANPAS en Annee2021$FINANPAS_rec
Annee2021$FINANPAS_rec <- Annee2021$FINANPAS %>%
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
## Recodage de Annee2021$FINANFUT en Annee2021$FINANFUT_rec
Annee2021$FINANFUT_rec <- Annee2021$FINANFUT %>%
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

### a. Recodage de Annee2021$PRIX en Annee2021$PRIX_rec #####
Annee2021$PRIX_rec <- as.character(Annee2021$PRIX)
Annee2021$PRIX_rec[Annee2021$PRIX == "1"] <- "Fortement augmenté"
Annee2021$PRIX_rec[Annee2021$PRIX == "2"] <- "Modérément augmenté"
Annee2021$PRIX_rec[Annee2021$PRIX == "3"] <- "Un peu augmenté"
Annee2021$PRIX_rec[Annee2021$PRIX == "4"] <- "Stagné ou diminué"
Annee2021$PRIX_rec[Annee2021$PRIX == "5"] <- "Stagné ou diminué"
Annee2021$PRIX_rec[Annee2021$PRIX == "9"] <- "Stagné ou diminué"
## b.Réordonnancement de Annee2021$PRIX_rec #####

## Réordonnancement de Annee2021$PRIX_rec
## Réordonnancement de Annee2021$PRIX_rec
Annee2021$PRIX_rec <- Annee2021$PRIX_rec %>%
  fct_relevel(
    "Stagné ou diminué", "Un peu augmenté", "Modérément augmenté",
    "Fortement augmenté"
  )




## 2. PRIXPLUS : Variable quantitative de l'évolution des prix #####
### De quel pourcentage pensez-vous que les prix ont augmenté au cours des douze derniers mois ? #####
Annee2021$prixplus_rec <-  as.numeric(Annee2021$PRIXPLUS)


### Tableau statistique de cette variable quantitative 
summary(na.omit(as.numeric(Annee2021$PRIXPLUS)))
Annee2021 %>%
  tbl_summary(include = "PRIXPLUS_rec")
pourcentage_cases <- data.frame("Cases Vides" = mean(is.na(Annee2021$PRIXPLUS_rec)) * 100, "Cases Non Vides" = mean(!is.na(Annee2021$PRIXPLUS_rec)) * 100)
pourcentage_cases
## Sur 20 000, il y a 10 000 cases qui sont vides !! Mais bon on va faire avec.
## Il n'y pas pas beaucoup de variable rempli : plus tout de même que pour la baisse des prix. 

# La moyenne est de 10 ce qui est très elvé par rapport à l'inflation réelle. 
## La médiane à 6 : donc toujours élevé mais un peu moins
sum(1584 +1425 + 1687 + 693 + 2520 + 1624)

Perceptionprix <- Annee2021$PRIXPLUS_rec[!is.na(Annee2021$PRIXPLUS_rec)]
summary(Perceptionprix)
## Recodage de Perceptionprix en Perceptionprix_rec
Perceptionprix_rec <- cut(Perceptionprix,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 3, 5, 6, 10, 20, 99.9)
)


       


# 3 PRIXBAIS : Variable quantitative : De quel pourcentage pensez-vous que les prix ont baissé au cours des douze derniers mois ? #####

Annee2021$PRIXBAIS_rec <- as.numeric(Annee2021$PRIXBAIS)
Annee2021 %>%
  tbl_summary(include = "PRIXBAIS_rec")
## Il y a la moitié des cases qui sont vides ... 
### Il n'y a pas assez d'information sur cette variable. C'est du grand n'importe quoi 
### Elle ne sera pas conservée pour la suite. 


# 4. EVOLPRIX : variable qualitative : subjective sur le future !!  Par rapport aux douze derniers mois, quelle sera à votre avis l’évolution des prix au cours des douze prochains mois ?#####
### Evolution des prix 

## Recodage de Annee2021$EVOLPRIX en Annee2021$EVOLPRIX_rec #########
Annee2021$EVOLPRIX_rec <- as.character(Annee2021$EVOLPRIX)
Annee2021$EVOLPRIX_rec[Annee2021$EVOLPRIX == "1"] <- "La hausse va être plus rapide"
Annee2021$EVOLPRIX_rec[Annee2021$EVOLPRIX == "2"] <- "La hausse va se poursuivre au même rythme"
Annee2021$EVOLPRIX_rec[Annee2021$EVOLPRIX == "3"] <- "La hausse va être moins rapide"
Annee2021$EVOLPRIX_rec[Annee2021$EVOLPRIX == "4"] <- "Les prix vont rester stationnaires ou diminuer"
Annee2021$EVOLPRIX_rec[Annee2021$EVOLPRIX == "5"] <- "Les prix vont rester stationnaires ou diminuer"
Annee2021$EVOLPRIX_rec[Annee2021$EVOLPRIX == "9"] <- "La hausse va se poursuivre au même rythme"
## Quand on sait pas : on pense que ça va continuer comme ça. 
## Réordonnancement de Annee2021$EVOLPRIX_rec
## Réordonnancement de Annee2021$EVOLPRIX_rec
Annee2021$EVOLPRIX_rec <- Annee2021$EVOLPRIX_rec %>%
  fct_relevel(
    "Les prix vont rester stationnaires ou diminuer", "La hausse va être moins rapide",
    "La hausse va se poursuivre au même rythme", "La hausse va être plus rapide"
  )
## Réordonnancement de Annee2021$EVOLPRIX_rec
Annee2021$EVOLPRIX_rec <- Annee2021$EVOLPRIX_rec %>%
  fct_relevel(
    "La hausse va être moins rapide", "Les prix vont rester stationnaires ou diminuer",
    "La hausse va se poursuivre au même rythme", "La hausse va être plus rapide"
  )




# 4. EVPRIPLUS : variable qualitative : subjective sur le future !!  Par rapport aux douze derniers mois, quelle sera à votre avis l’évolution des prix au cours des douze prochains mois ?#####
### Evolution des prix 
# Annee2021$EVPRIPLU_rec <- as.numeric(Annee2021$EVPRIPLU)

## Il y a la moitié de case vide. 
# summary(na.omit(Annee2021$EVPRIPLU_rec))
## Recodage de Annee2021$EVPRIPLU_rec en Annee2021$EVPRIPLU_rec_rec
# Annee2021$EVPRIPLU_rec <- cut(Annee2021$EVPRIPLU_rec,
#   include.lowest = TRUE,
#   right = FALSE,
#   dig.lab = 4,
#   breaks = c(0, 2, 4, 5, 10, 15, 99.9))
# 
# Annee2021 %>%
#   tbl_summary(include = "EVPRIPLU_rec")
# ## Les gens pensent que l'inflation va continuer d'être de 5%. 
# 
# #### 5. EVPRIBAI : variable qualitative : subjective sur le future !!  Par rapport aux douze derniers mois, quelle sera à votre avis l’évolution des prix au cours des douze prochains mois ?#####
# 
# Annee2021$EVPRIBAI_rec <- as.numeric(Annee2021$EVPRIBAI)
# 
# Annee2021 %>%
#   tbl_summary(include = "EVPRIBAI_rec")
# 

### Aucune information !! Tout est NA !!! On supprimer la variable sans intérêt. 

# 6. Conclusion des effectifs 



##" Construction de la variable inflation réelle par mois. 
## Idéee si c'est de créer une nouvelle variable de l'inflation réelle. 

# Créez un vecteur de valeurs d'inflation pour chaque mois
inflation_values <- c(0.5548115, 0.5644313, 1.109093, 1.243305, 1.422978, 1.479149, 1.150299, 1.865068, 2.161645, 2.621759, 2.778574, 2.75343)
# Utilisez la fonction match() pour obtenir les indices correspondant aux mois dans MOISENQ_rec
month_indices <- match(Annee2021$MOISENQ_rec, c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))

# Créez la nouvelle variable IPC en utilisant les indices pour obtenir les valeurs d'inflation correspondantes
Annee2021$IPC <- inflation_values[month_indices]


# Exportation de la base de données recodées -----


# write.csv(Annee2021, "02_data/travail/Inflation_base_2021_recodee.csv") 


