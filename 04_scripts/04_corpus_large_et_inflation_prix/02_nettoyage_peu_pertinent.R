library(questionr)
library(dplyr)
library(tidyr)
library(stringr)



base$inflation<- str_detect(base$texte, "coût de la vie")
freq(base$cout)

base_cout <- subset(base, base$cout==TRUE )


base_mot <- subset(base, base$nb_mots > 10000 )
