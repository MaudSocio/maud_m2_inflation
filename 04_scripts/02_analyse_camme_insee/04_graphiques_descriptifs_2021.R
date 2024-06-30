# ETAPE 3 Graphiques descriptifs ######

## Importatio




# Prix rec --------------------
# Prix plus rec --------------------

## Courbe de densité 
ggplot(Annee2021) +
  aes(x = PRIXPLUS_rec) +
  geom_density(adjust = 0.9) +
  theme_gray() +
  geom_vline(xintercept = 1.6, color = "red") +
  labs(x = "Estimation de l'évolution des prix",
       title = "Courbe de densité de la perception 
      des augmentations des prix ces douze derniers mois")

## Histogramme
hist(
  as.numeric(na.omit(Annee2021$PRIXPLUS_rec)), 
  main = "Histogramme des effectifs 
     de la perception des augmentations des prix", 
  xlim = c(0, 20),
  xlab = "Chiffre donné entre 0 et 20 pour une inflation en moyenne à 1,6. 
  L'inflation moyenne est sur la ligne rouge verticale",
  breaks = 100,
  col = "lightblue")
abline(v = 1.6, col = "red")

lines(density(na.omit(Annee2021$PRIXPLUS_rec), bw = 0.0001), col = "blue",lwd=2 )


# EVOLPRIX rec --------------------
# EVOLPRIXPLUS rec --------------------

## Evolution prix 

## Graphique avec l'IPC ----------

ggplot(Annee2021) +
  aes(x = MOISENQ_rec, y = PRIXPLUS_rec) +
  geom_boxplot() +
  geom_line(aes(x = MOISENQ_rec, y = IPC), color = "blue", size = 1.5) + # Augmentez la taille de la ligne
  labs(x = "Mois", y = "Perception de l'inflation", title = "Évolution des perceptions de l'inflation") +
  theme_minimal() +
  ylim(-3, 30)

### Super graphique de la perception de l'inflation ces douze derniers mois
ggplot(Annee2021) +
  aes(x = MOISENQ_rec, y = PRIXPLUS_rec, fill = IPC) +
  geom_boxplot() +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  labs(x = "Mois", y = "Estimation", title = "Estimation de l'évolution des prix ces douze derniers mois 
       selon le mois et l'indice d'inflation") +
  theme_minimal() +
  ylim(-3, 40)


#### Estimation de l'évolution des prix dans les douze prochains mois selon le mois, le genre et l'indice d'inflation

ggplot(Annee2021) +
  aes(x = PRIXPLUS_rec, y = MOISENQ_rec, fill = IPC) +
  geom_boxplot() +
  scale_fill_distiller(palette = "PuRd", 
                       direction = 1) +
  labs(x = "Mois", y = "Anticipation", title = "Estimation de l'évolution des prix ces douze derniers mois 
       selon le mois, l'indice d'inflation et le genre") +
  coord_flip() +
  theme_gray() +
  facet_wrap(vars(SEXE_rec)) +
  xlim(0, 25)


## cOURBE CHELOU 

# PRIX_rec x GEnre  : Croisement genre et variable quali 
graphDistribCroisee(Annee2021$PRIX_rec,Annee2021$SEXE_rec) +
  ggtitle("Répartition de la perception du prix selon le genre")

## Croisement avec le genre et variable quanti ######
graphDistribCroisee(Annee2021$PRIXPLUS_rec,Annee2021$SEXE_rec)

## Genre et évolution prix quali ######
graphDistribCroisee(Annee2021$EVOLPRIX_rec,Annee2021$SEXE_rec) +
  ggtitle("Répartition des anticipations de prix selon le genre")

## Genre et évolution prix quanti ######
graphDistribCroisee(Annee2021$EVPRIPLU,Annee2021$SEXE_rec) +
  ggtitle("Répartition des anticipations de prix selon le genre")


distribCroisee(Annee2021$PRIXPLUS_rec,Annee2021$SEXE_rec)

### Croisement avec une courbe #### ------- 
##### Les vieux qui répondent beaucoup plus et les jeunes qui surrestiment l'inflation. 
graphDistribCroisee(Annee2021$PRIXPLUS_rec,Annee2021$Age)


###
