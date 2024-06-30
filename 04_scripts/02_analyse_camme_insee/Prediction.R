library(dplyr)
library(ggplot2)

predictions <- expand.grid(Ageind_tranche = levels(Annee2021$Ageind_tranche),
                           SEXE_rec = levels(Annee2021$SEXE_rec))
predictions$prix_rec_pred <- predict(modele, newdata = predictions)

# Affichage des prédictions
print(predictions)