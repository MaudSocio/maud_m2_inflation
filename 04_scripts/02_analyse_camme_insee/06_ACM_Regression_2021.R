# ETAPE 5 : ACM ########

### Analyse des correspondances multiples ###################################################





d <-   d %>% mutate(
  BLUGRASS2 = case_when(
    BLUGRASS == 5 ~ "BLUGRASS_dislike",
    BLUGRASS == 4 ~ "BLUGRASS_dislike",
    BLUGRASS == 3 ~ "BLUGRASS_neutral",
    BLUGRASS == 2 ~ "BLUGRASS_like",
    BLUGRASS == 1 ~ "BLUGRASS_like_a_lot"   ))

## NB : Si on ne precise rien (pour BLUGRASS == 8 ou 9) ...
# ... alors mutate cree une valeur NA par defaut.

# Verification
freq(d$BLUGRASS2)


################################################################""""
## Etape suivante : on veut faire la meme chose pour toutes les variables de genre musical

# 1. creer un objet contenant la liste de toutes les variables de genre musical (cf. script pr?c?dent)

list <- c("BLUGRASS", "COUNTRY", "BLUES", "MUSICALS", "CLASSICL", "FOLK", "GOSPEL", "JAZZ",
          "LATIN", "MOODEASY", "NEWAGE", "OPERA", "RAP", "REGGAE",
          "CONROCK", "OLDIES", "HVYMETAL")

# 2. On applique le recodage a cet objet (utiliser le langage dplyr)

# D'abord, on regroupe les modalites selon leur signification (like / neutral / dislike)
d <- d %>% mutate(    
  across(all_of(list), 
         function(x) case_when(
           x == 1 ~ "like_a_lot",
           x == 2 ~ "like",
           x == 3 ~ "neutral",
           x == 4 ~ "dislike",
           x == 5 ~ "dislike"  ),
         .names = "{.col}.char")
) 

View(d)
# test : tri ? plat sur la variable BLUES :
freq(d$BLUES.char)

# Quel probleme va se poser selon vous lorsqu'on fera l'ACM ?
### Reponse : seul le nom des modalites vont s'afficher (et non le nom des variables)
### or toutes les variables ont les memes modalites, dont on ne pourra pas les distinguer

# Quelles modifications doit-on faire pour que le probleme ne se pose pas ?
### Reponse : ajouter le nom de la variable dans l'etiquette de chaque modalite

# Exemple sur le Blues :
d$BLUES2 <- paste0("BLUES", sep ="_", d$BLUES.char)
freq(d$BLUES2)

list.char <- c("BLUGRASS.char", "COUNTRY.char", "BLUES.char", "MUSICALS.char", "CLASSICL.char", "FOLK.char", "GOSPEL.char", "JAZZ.char",
               "LATIN.char", "MOODEASY.char", "NEWAGE.char", "OPERA.char", "RAP.char", "REGGAE.char",
               "CONROCK.char", "OLDIES.char", "HVYMETAL.char")

# On fait la meme chose pour toute les variables de la liste
# de maniere automatisee grace au code suivant :
for (v in list.char) {
  d[[v]] <- paste0(v, sep="_", d[[v]]) 
}

freq(d$JAZZ.char)
freq(d$BLUES.char)

#View(d)
## Mise en oeuvre de l'ACM

## Commencer par une ACM avec uniquement les variables actives :
View(Annee2021)
# var_active <- d[,28:44]

var_active <- data.frame( Annee2021$PRIX_rec, Annee2021$SEXE_rec,Annee2021$NBACTIF_rec,Annee2021$NBENFANTTOTAL,Annee2021$DIPLOME_rec, Annee2021$MOISENQ_rec, Annee2021$NBPERS_rec,  Annee2021$PROFESSION, Annee2021$PROFCONJOINT, Annee2021$TYPEMPL2_rec)
View(var_active)
var_active <- data.frame( Annee2021$PRIX_rec,Annee2021$NBENFANTTOTAL, Annee2021$MOISENQ_rec,  Annee2021$PROFESSION,Annee2021$TYPEMPL2_rec)
View(var_active)

# Commande de mise en oeuvre de l'ACM
# (base avec uniquement les variables actives)

res_ACM <- MCA(var_active, graph =F)
explor(res_ACM)

## Que remarque-t-on ? 
##Que faire pour resoudre ce probleme ?

res <- explor::prepare_results(res_ACM)
explor::MCA_var_plot(res, xax = 5, yax = 2, var_sup = FALSE, var_sup_choice = ,
                     var_lab_min_contrib = 0, col_var = "Variable", symbol_var = NULL, size_var = NULL,
                     size_range = c(10, 300), labels_size = 10, point_size = 56, transitions = TRUE,
                     labels_positions = NULL, labels_prepend_var = FALSE, xlim = c(-1.14, 1.45),
                     ylim = c(-1.24, 1.36))

# Reponse : on a une forte opposition sur l'axe 1 entre les non reponses pour tous les
# styles musicaux, et les autres reponses
# Cela s'explique par une forte correlation entre toutes les non reponses.

# Pour resoudre ce probleme, on a 4 possibilit?s :
# 1) on garde les modalit?s NA car on juge qu'elles ont du sens et un effectif suffisant (c'est une modalit? ? part enti?re) 
#+ ce ne sont pas les m?mes individus qui ont r?pondu NA ? toutes les questions 

# 2) on ne garde dans l'echantillon que les personnes qui ont repondu a toutes les var actives

# 3) on indique a R qu'il s'agit de non reponse (NA) et qu'il faut imputer ces donnees manquantes
#on ?vacue alors les donn?es de l'analyse, en enlevant leur effet sur l'analyse.

# 4) On fait une ACM sp?cifique qui bascule les modalit?s NA en suppl?mentaire mais garde les autres modalit?s comme actives
#cela permet de projeter les NA dans l'espace et donc de voir o? ils sont situ?s.

### Nous appliquons ici la solution 3.

#on va avoir besoin de ce package
library(missMDA)

## On a d'abord besoin de recoder tous les NA en vrais NA:
#  freq(d$JAZZ.char)
#>> on ne veut plus la modalit? "JAZZ.char_NA" mais que ce sindividus soient identifi?s par R comme des NA

# On refait l'etape 1 qu'on a faite plus haut pour recoder les variables, cette fois en les appelant X.imp
# list <- c("BLUGRASS", "COUNTRY", "BLUES", "MUSICALS", "CLASSICL", "FOLK", "GOSPEL", "JAZZ",
# "LATIN", "MOODEASY", "NEWAGE", "OPERA", "RAP", "REGGAE",
#  "CONROCK", "OLDIES", "HVYMETAL")

# d <- d %>% mutate(    
# across(all_of(list), 
function(x) case_when(
  x == 1 ~ "like_a_lot",
  x == 2 ~ "like",
  x == 3 ~ "neutral",
  x == 4 ~ "dislike",
  x == 5 ~ "dislike"  ),
.names = "{.col}.imp")
) 

#View(d)

list.imp <- c("BLUGRASS.imp", "COUNTRY.imp", "BLUES.imp", "MUSICALS.imp", "CLASSICL.imp", "FOLK.imp", "GOSPEL.imp", "JAZZ.imp",
              "LATIN.imp", "MOODEASY.imp", "NEWAGE.imp", "OPERA.imp", "RAP.imp", "REGGAE.imp",
              "CONROCK.imp", "OLDIES.imp", "HVYMETAL.imp")

# Puis on ajoute une condition dans le recodage des modalites 
#pour que toute sles modalit?s NA de toutes les variables soit cod?es NA et plus JAZZ_NA ou BLUES_NA
# NB : ! signifie "contraire de"

for (v in list.imp) {
  d[[v]][!is.na(d[[v]])] <- paste0(v,sep = "_", d[[v]][!is.na(d[[v]])])
}

# Test de verification
freq(d$JAZZ.imp)
# Maintenant on a bien une colonne "NA" a la place de "JAZZ_NA"

## On reconstitue la base de donnees des variables actives :

# var_active2 <- select(d,"BLUGRASS.imp", "COUNTRY.imp", "BLUES.imp", "MUSICALS.imp", "CLASSICL.imp", "FOLK.imp", "GOSPEL.imp", "JAZZ.imp",
"LATIN.imp", "MOODEASY.imp", "NEWAGE.imp", "OPERA.imp", "RAP.imp", "REGGAE.imp",
"CONROCK.imp", "OLDIES.imp", "HVYMETAL.imp")

#  View(var_active2)

#On impute ensuite les valeurs manquantes grâce ? la fonction imputeMCA()
impMCA_var_act <- data.frame(imputeMCA(var_active))

#on obtient un nouveau tableau avec valeurs imputées
View(impMCA_var_act)

# On execute enfin l'ACM sur ce nouveau tableau:
res_ACM2 <- MCA(impMCA_var_act2$completeObs, graph = F)

#et on observe les r?sultats
#explor(res_ACM2)


#Interpr?tation: regarder les % d'inertie
#on peut obtenir les taux modifi?s gr?ce au package GDAtools
#install.packages("GDAtools")
library(GDAtools)


#avec la fonction  modif.rates
modif.rate(res_ACM)


# Ajout des elements supplementaires dans l'analyse

## Etape prealable : renommer les modalites des variables supplementaires categorielles 
#on reprend ce qu'on a d?j? fait dans le script pr?c?dent

# Pour simplifier, je me limite a 3 variables supplementaires :
# 1 variable quantitative : AGE
# 2 variables cat?gorielles : SEXE et DEGREE (= diplome)

# SEXE
# Niveau de diplome (on recode le niveau de diplome des parents en meme temps)
# DEGREE : niveau de diplome de la personne interrogee
# MADEG : niveau de diplome de sa mere
# PADEG : niveau de diplome de son pere


##############################################"
## Nouvelle base avec ajout des variables suppl?mentaires (quali et quanti) :


# PREMIERE ETAPE : on cree une base de donnees contenant les variables illustratives 

Annee2021$SEXE_rec <- as.factor(Annee2021$SEXE_rec)
Annee2021$DIPLOME_rec <- as.factor(Annee2021$DIPLOME_rec)
var_illustrative <- select(Annee2021$Age,Annee2021$SEXE_rec,Annee2021$DIPLOME_rec)
# DEUXIEME ETAPE : on fusionne la base des variables actives et illustratives
## NB : on n'a pas modifie l'ordre des lignes apres la creation de var_actives et var_illustratives,
# donc on peut fusionner les deux bases sans risque

#Attention, on reprnd bien la bse avec NA imput?es.

baseACM <- cbind.data.frame(impMCA_var_act2$completeObs,var_illustrative)
View(baseACM)

## Exportation de la base complete et propre
#write.csv(baseACM,"baseACM.csv")

# Commande de mise en oeuvre de l'ACM
# (base complete)
colnames(baseACM)
res_ACM3 <- MCA(baseACM, quanti.sup = 18, quali.sup = c(19,20), graph=F)

#explor(res_ACM3)




