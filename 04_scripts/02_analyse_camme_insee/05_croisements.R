# ETAPE 3 Croisement des variables ######

library(tidyverse)
library(nycflights13)
library(dplyr)
library(foreign)
library(questionr)
library(stats)
library(survey)
library(gtsummary)
library(factoextra)
library(gplots)
library(FactoMineR)
library(gtsummary)
library(esquisse)
library(ggplot2)
library(viridis)  
library(readr)
library(dplyr)
library(ggplot2)

## Environnement de travail 
setwd("C:/Users/ymaud/Documents/dev/inflation_memoire/inflation_script")

## Importation 
Annee2021 <- read_csv("Inflation_base_2021_recodee.csv")

# Croisement en fonction des catégories ######

## 1. CROISEMENT AVEC LE GENRE  #####

### 1.1 Tableaux croisés  #####
#### 1.1.1 PRIX_rec  #####

tabprix_genre <- table(Annee2021$PRIX_rec,Annee2021$SEXE_rec)
rprop(tabprix_genre)

Annee2021 %>% 
  tbl_summary(include=c('SEXE_rec'), by ='PRIX_rec')  %>% 
  add_p()
## Très significatif. 
## Tableau de fréquence sur 2020 avec du gras et de l'italique des belles notes  ####

tab1 <- table(Annee2021$PRIX_rec, Annee2021$SEXE_rec)
cprop(tab1)
rprop(tab1)
### 1.B Graphiques  #####

## Estimation de l'évolution des prix selon le genre
Annee2021 %>%
  filter(!(PRIX_rec %in% c("Ne sait pas", "Diminué"))) %>%
  ggplot() +
  aes(x = PRIX_rec, fill = SEXE_rec) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(x = "Estimation de l'évolution des prix selon le genre", y = "Effectif", subtitle = "Estimation de l'évolution des prix seon le genre", 
       fill = "Genre") +
  theme_minimal()






#Dispersion des estimations passées d'inflation en fonction du genre
#### 1.1.2 PRIXPLUS_rec  #####
ggplot(Annee2021) +
  aes(x = PRIXPLUS_rec, y = SEXE_rec) +
  geom_violin(fill = "#5E8EB5") +
  labs(y = "Estimation de l'inflation 
       sur ces douze derniers mois", 
       title = "Dispersion des estimations passées d'inflation en fonction du genre") +
  coord_flip() +
  theme_minimal() +
  xlim(-3, 50)

library(wildR)

ggplot(Annee2021) +
  aes(x = PRIXPLUS_rec, y = SEXE_rec) +
  geom_boxplot(fill = "#5E8EB5") +
  labs(y = "Estimation de l'inflation sur ces douze derniers mois", 
       title = "Dispersion des estimations passées d'inflation en fonction du genre") +
  coord_flip() +
  theme_minimal() +
  xlim(0, 40)

## 2. Croisement avec l'âge ######
### 2.1 Croisement avec un tableau croisé #######
Annee2021 %>% 
  tbl_summary(include=c('Age'), by ='PRIX_rec') %>%
  add_p()
## Très significatif


### Autre graphique 

library(ggplot2)
freq(PRIX_rec)
ggplot(Annee2021) +
  aes(x = Age, fill = PRIX_rec) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(Ne sait pas = "#11A11F", Diminué = "#E14CF0", 
                               Stagné = "#3A68E7", `Un peu augmenté` = "#00C19F", `Modérément augmenté` = "#FE9A2F", `Fortement augmenté` = "#EB1E1E"
  )) +
  coord_flip() +
  theme_gray()

freq(Annee2021$PRIX_rec)

ggplot(Annee2021) +
  aes(x = Age, fill = PRIX_rec) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(Diminué = "#E14CF0", 
                               Stagné = "#3A68E7", `Un peu augmenté` = "#00C19F", `Modérément augmenté` = "#FE9A2F", `Fortement augmenté` = "#EB1E1E"
  )) +
  coord_flip() +
  theme_gray()

## On a une belle courbe. 
## Comparaison des dispersions

library(dplyr)
library(ggplot2)

Annee2021 %>%
  filter(Age) %>%
  ggplot() +
  aes(x = PRIXPLUS_rec, y = Age) +
  geom_boxplot(fill = "#FF8C00") +
  labs(x = "Estimation de l'inflation ces douze derniers mois", 
       y = "Classe d'âge", title = "Dispersion de l'évaluation de l'inflation selon la classe d'âge") +
  theme_minimal() +
  xlim(0, 40)


### Comparaison : Ageind et PrixPlusrec


### Pas une mauvaise idée mais regarder seulement de 20 à 70 ans 

# ggplot(Annee2021) +
#  aes(x = PRIXPLUS_rec) +
#  geom_histogram(bins = 30L, fill = "#112446") +
#  theme_minimal() +
#  facet_wrap(vars(Ageind))

### je crois que ce graphique ne me dit rien du tout 

ggplot(Annee2021) +
  aes(x = Ageind, y = PRIXPLUS_rec, fill = SEXE_rec) +
  geom_area() +
  scale_fill_hue(direction = 1) +
  labs(x = "Age des individus de 25 à 95 ans", y = "Estimation de l'augmentation des prix ces 12 derniers mois", 
       title = "Perception de l'inflation en fonction de l'âge des individu et de leur genre") +
  theme_minimal() +
  xlim(20, 95) + 
  ylim(0, 40)

### Perception de l'inflation en fonction de l'âge des individu et de leur genre"
ggplot(Annee2021) +
  aes(x = Ageind, y = EVOLPRIX_rec, fill = SEXE_rec) +
  geom_violin() +
  scale_fill_hue(direction = 1) +
  labs(x = "Age des individus de 25 à 95 ans", y = "Estimation de l'augmentation des prix ces 12 derniers mois", 
       title = "Perception de l'inflation en fonction 
 de l'âge des individu et de leur genre") +
  theme_minimal() +
  xlim(10, 95)

ggplot(Annee2021) +
  aes(x = Ageind, y = PRIXPLUS_rec) +
  geom_jitter(size = 0.5) +
  labs(x = "Age des individus de 25 à 95 ans", 
       y = "Estimation de l'augmentation des prix ces 12 derniers mois", title = "Perception de l'inflation en fonction de l'âge des individu et de leur genre") +
  coord_flip() +
  theme_minimal() +
  xlim(20, 95) +
  ylim(0, 30)

## 3. Croisement configuration familiale ------
## Perception de l'inflation des femmes pas en couple avec des enfants


### 3.1 Tableau croisée : nombre d'enfant ######
Annee2021 %>% 
  tbl_summary(include=c('NBENFANT_rec'), by ='EVOLPRIX_rec') %>% 
  add_p() %>% 
  add_overall()
  
## 4. Croisement évolution des prix et famille monoparentale  #####
Annee2021 %>% 
  tbl_summary(include=c('MONOPARENTALE'), by ='EVOLPRIX_rec') %>% 
  add_p()

rprop(table(Annee2021$MONOPARENTALE, Annee2021$EVOLPRIX_rec))

Annee2021 %>%
  filter(!is.na(Monoparental)) %>%
  ggplot() +
  aes(x = PRIXPLUS_rec, y = Monoparental) +
  geom_boxplot(fill = "#EF562D") +
  labs(x = "Estimation inflation", 
       y = "Situation familiale avec enfant de moins 14 ans ", title = "Dispersion de l'estimation de l'inflation de ces douze derniers mois en fonction du genre ") +
  theme_minimal() +
  xlim(0, 60)

Annee2021 %>%
  filter(!is.na(Monoparental)) %>%
  ggplot() +
  aes(x = EVOLPRIX_rec, y = Monoparental) +
  geom_boxplot(fill = "#EF562D") +
  labs(x = "Estimation inflation", 
       y = "Situation familiale avec enfant de moins 14 ans ", title = "Dispersion de l'estimation de l'inflation de ces douze derniers mois en fonction du genre ") +
  theme_minimal() +
  xlim(0, 60)

Annee2021 %>%
  filter(!is.na(Monoparental2)) %>%
  ggplot() +
  aes(x = PRIXPLUS_rec, y = Monoparental2) +
  geom_boxplot(fill = "#EF562D") +
  labs(x = "Estimation inflation", 
       y = "Situation familiale enfant avec tous les âges ", title = "Dispersion de l'estimation de l'inflation de ces douze derniers mois en fonction du genre ") +
  theme_minimal() +
  xlim(0, 60)

##" Les femmes seules avec un emploi à mi-temps

Annee2021 %>%
  filter(!is.na(Monoparental)) %>%
  ggplot() +
  aes(x = EVPRIPLU_rec, y = Monoparental) +
  geom_boxplot(fill = "#EB102D") +
  labs(x = "Estimation inflation", 
       y = "Situation familiale ", title = "Dispersion de l'estimation  
       de l'inflation dans les douze 
       prochains mois en fonction du genre ") +
  theme_minimal() +
  xlim(0, 60)

## Ce tableau ne sert à rien : il dit seulement qu'il y a plus de femme seule que d'homme seule dans l'échantillon 
Annee2021 %>%
  filter(Monoparental %in% c("Femme seule sans enfant", "Homme seul sans enfant")) %>%
  ggplot() +
  aes(x = PRIX_rec) +
  geom_bar(fill = "#FF8C00") +
  labs(x = "Estimation inflation", 
       y = "Situation familiale ", title = "Dispersion de l'estimation prochaine de l'inflation en fonction du genre ") +
  theme_gray() +
  facet_wrap(vars(Monoparental), scales = "free_x")


### Les vieux locataire 
### NORUEE1 RESIDPR

# View(Annee2021)


### Evolution au cours des mois 

library(ggplot2)

## Prix Bais ---------------

### http://127.0.0.1:42445/graphics/dd2af82e-6b2d-4bdb-8888-96e6822d733c.png

## 5. Croisement profession -------

Annee2021 %>% 
  tbl_summary(include=c('PROFESSION'), by ='EVOLPRIX_rec') %>% 
  add_p() %>%
  add_overall()

iorder(PRIX_rec)

Annee2021 %>% 
  tbl_summary(include=c('PROFESSION'), by ='PRIX_rec') %>% 
  add_p() %>%
  add_overall()


##" graphique très interessant sur les perception de l'augmentation de l'inflation en fonction de la profession. 
## Pourquoi pour les employés c'est très différent ? 
Annee2021 %>%
  filter(!is.na(Age_rec)) %>%
  filter(!is.na(Age_rec_rec)) %>%
  filter(!is.na(Age)) %>%
  ggplot() +
  aes(x = MOISENQ_rec, y = PRIXPLUS_rec, fill = PROFESSION) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(x = "Mois", y = "Perception de l'inflation", title = "Perception de l'augmentation de l'inflation en fonction de la profession") +
  theme_minimal() +
  ylim(0, 25)


## 6. Type de contrat -----
Annee2021$TYPEMPL2_rec
Annee2021 %>% 
  tbl_summary(include=c('TYPEMPL2_rec'), by ='EVOLPRIX_rec') %>% 
  add_p() %>%
  add_overall()
## Pas du tout significatif 
Annee2021 %>% 
  tbl_summary(include=c('TYPEMPL2_rec'), by ='PRIX_rec') %>% 
  add_p() %>%
  add_overall()
## Pas du tout significatif 


## C'est 


## Situation Eco et Mois 




library(ggplot2)

ggplot(Annee2021) +
  aes(x = MOISENQ_rec, fill = SITUAECO_rec) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "Mois", y = "Situation économique ", title = "Situation économique en fonction des mois", 
       fill = "Situation économique ") +
  theme_minimal()


## 7. Croisement Mois -------

## Tableau variable quali en fonction du mois -------
Annee2021 %>%
  tbl_summary(
    include = c("PRIX_rec"),
    by = MOISENQ_rec )  %>%
  add_p()
  add_overall()

## Graphique sur la position sur l'évolution des prix en fonction du mois -------
ggplot(Annee2021) +
  aes(x = PRIX_rec) +
  geom_bar(position = "dodge", fill = "#EF562D") +
  labs(x = "Perception des prix ", 
       title = "Graphique de la perception des prix en fonction du mois", subtitle = "Perception de l'évolution des prix ces douze derniers mois ", 
       caption = "Notes : La question posée est : \"Pensez-vous qu’au cours des douze derniers mois les prix ont…\"") +
  theme_minimal() +
  theme(plot.caption = element_text(face = "bold.italic", hjust = 0.5)) +
  facet_wrap(vars(MOISENQ_rec))

## Déplacement des perceptions de l'inflation au file des mois -------
ggplot(Annee2021) +
  aes(x = MOISENQ_rec, fill = PRIX_rec) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "Mois", y = "Réponse sur la perception des augmentation de l'inflation", title = "Déplacement des perceptions de l'inflation au fil des mois", 
       fill = "Perception") +
  theme_minimal()
# Il y a presque 50% de l'échantillon qui dit que cela a un peu ou modérément augmenté ce qui est assez, juste 

### d. Graphique des fréquences des perceptionssur l'évolution des prix sur les 12 dernier mois #####
plot(table(Annee2021$PRIX_rec), col = "darkblue", main = "Graphique des fréquences des perceptions 
     sur l'évolution des prix sur l'année 2021", ylab = "Effectif")

# 8. Croisement diplome ######

## Beaucoup pense que ça va rester pareil !! 
##" Il serait intéressant de croiser cela avec le niveau de diplome !! 

Annee2021 %>% 
  tbl_summary(include=c('SEXE_rec','DIPLOME_rec'), by ='EVOLPRIX_rec') %>% 
  add_overall(last = TRUE) %>% 
  add_p()

## Surrestimation importante 
cprop(table(Annee2021$PRIX_rec,Annee2021$MOISENQ_rec))

# Commentaire : belle loi normale ! 



## PRIXrec : Croisements 

ggplot(Annee2021) +
  aes(x = PRIX_rec, fill = DIPLOME_rec) +
  geom_bar() +
  scale_fill_manual(values = c(`Aucun diplôme ou certificat d’études primaires` = "#F8766D", 
                               `Baccalauréat, brevet professionnel ou équivalent` = "#EBDC0C", `CAP, BEP ou équivalent` = "#00BB4C", 
                               `Diplôme du supérieur court (niveau bac +
                                                              2)` = "#28E2FE", `Diplôme du supérieur long (supérieur à bac +
                                                                                                             2)` = "#0E02E7", 
                               `Ne sait pas` = "#FEBEDE")) +
  labs(x = "Perception de l'inflation ", y = "Diplome", title = "Perception du l'inflation en fonction du diplome ") +
  theme_classic()
### Différents mois et perception de l'inflation  et le genre
ggplot(Annee2021) +
  aes(x = PRIXPLUS_rec, fill = SEXE_rec) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(MOISENQ_rec)) +
  xlim(-4, 50) 



Annee2021 %>%
  filter(!is.na(Monoparental2)) %>%
  ggplot() +
  aes(x = EVOLPRIX_rec, fill = Monoparental2) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(x = "Perception de l'évolution future des prix ", y = "Effectif", title = "Perception des prix selon la situation monoparentale") +
  theme_minimal()

Annee2021 %>%
  filter(!is.na(Monoparental2)) %>%
  ggplot() +
  aes(x = EVOLPRIX_rec, fill = SITUAECO_rec) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(x = "Perception de l'évolution des prix ", y = "Effectif", title = "Perception des prix selon la situation monoparentale") +
  theme_minimal()

Annee2021 %>%
  filter(!is.na(Monoparental2)) %>%
  ggplot() +
  aes(x = EVOLPRIX_rec, fill = SITUAECO_rec) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(x = "Perception de l'évolution des prix ", y = "Effectif", title = "Perception des prix selon la situation financière", 
       fill = "Situation financière") +
  theme_minimal()

## EVOLPRIX : croisements  #######


### graphique des perceptions de l'inflation en fonction du diplôme


## 9. Anticipation de l'évolution des prix dans les douze prochains mois selon le mois, et la situation écononomique ------

ggplot(Annee2021) +
  aes(x = PRIXPLUS_rec, y = MOISENQ_rec, fill = SITUAECO_rec) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PuRd", 
                    direction = 1) +
  labs(x = "Mois", y = "Anticipation", title = "Anticipation de l'évolution des prix dans les douze prochains mois selon le mois, et la situation écononomique") +
  coord_flip() +
  theme_gray() +
  xlim(0, 25)



# 1. PRIX_rec : variable qualitative #####


## 5. ACHATS #####
## Dans la situation économique actuelle, pensez-vous que les gens aient intérêt à faire des achats importants ?
## Recodage de Annee2021$ACHATS en Annee2021$ACHATS_rec
Annee2021 %>% 
tbl_summary(include=c('ACHATS_rec'), 
                  by='PRIX_rec') %>% 
add_p() %>%
add_overall()



# Prix_rec en fonction du Genre et de l'âge. 



